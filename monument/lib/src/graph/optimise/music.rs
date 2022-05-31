use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::{
    graph::{Chunk, ChunkId, Graph, StandardChunkId},
    music::Breakdown,
    Query,
};

/// How many chunks will be searched to determine which chunk patterns generate the required music
const ITERATION_LIMIT: usize = 10_000;

pub(super) fn required_music_min(graph: &mut Graph, query: &Query) {
    // log::debug!("\n\n\n");

    // For each `MusicType`, maps its index to minimum count
    let min_music_counts = query
        .music_types
        .iter()
        .enumerate()
        .filter_map(|(i, ty)| ty.count_range.min.map(|min| (i, min)))
        .collect::<HashMap<_, _>>();

    if min_music_counts.is_empty() {
        return; // If there are no music bounds, then there's nothing to do
    }

    // The `(ChunkId, Chunk)` pairs of chunks which contribute to the types of music that we care
    // about.
    let (required_chunks, non_required_chunks) = graph
        .chunks()
        .filter(|(_id, chunk)| {
            min_music_counts
                .iter()
                .any(|(ty_idx, _)| chunk.music().counts[*ty_idx] > 0)
        })
        .map(|(id, chunk)| (id.standard().unwrap(), chunk))
        .partition::<Vec<_>, _>(|(_id, chunk)| chunk.required);

    // Determine how much music is contributed by required chunks.
    let mut required_music_counts = vec![0; query.music_types.len()];
    for (idx, min) in &min_music_counts {
        required_music_counts[*idx] = *min;
    }
    let mut counts_needed_from_non_required_chunks = Breakdown {
        score: OrderedFloat(0.0),
        counts: required_music_counts.into(),
    };
    for (_id, chunk) in required_chunks {
        // Non-required chunks aren't required to get music which the required chunks can already
        // achieve
        counts_needed_from_non_required_chunks.saturating_sub_assign(chunk.music());
    }

    // Do tree search over the non-required interesting chunks, determining which combinations of
    // the chunks satisfy the required music output.
    let chunk_combinations = search_chunk_combinations(
        &counts_needed_from_non_required_chunks,
        &non_required_chunks,
    );
    for vs in &chunk_combinations {
        log::debug!("{:?}", vs.iter().sorted().collect_vec());
    }

    // Any chunks which are in every possible combination are required for the composition to
    // generate the required music.
    let new_required_chunks = non_required_chunks
        .iter()
        .filter(|(id, _chunk)| {
            chunk_combinations
                .iter()
                .all(|combination| combination.contains(id))
        })
        .map(|(id, _chunk)| (*id).clone())
        .collect_vec();
    let no_required_chunks = new_required_chunks.is_empty();
    log::debug!("required: {:?}", new_required_chunks);
    for required_id in new_required_chunks {
        graph
            .get_chunk_mut(&ChunkId::Standard(required_id))
            .unwrap()
            .required = true;
    }

    // If there aren't any chunks to mark as required, then we can pick a chunk to condition on and
    // create two graphs to optimise: one where that chunk is required, and the other where that
    // chunk is removed.
    if no_required_chunks {
        log::warn!("No required chunks made");
    }
}

/// Remove any chunk which exceeds the max count for any music type.  Usually this max count will be
/// 0 (i.e. any chunks with that music should be removed).
pub(crate) fn remove_chunks_exceeding_max_count(graph: &mut Graph, query: &Query) {
    let mut counts_from_required_chunks = Breakdown::zero(query.music_types.len());
    for chunk in graph.chunks.values() {
        if chunk.required {
            counts_from_required_chunks += &chunk.music;
        }
    }

    for (music_ty_idx, count_from_required) in counts_from_required_chunks
        .counts
        .iter()
        .copied()
        .enumerate()
    {
        let music_type = &query.music_types[music_ty_idx];
        if let Some(count_limit) = music_type.count_range.max {
            let max_count_left_per_chunk = count_limit.checked_sub(count_from_required).expect(
                "Search can't be completed because the required chunks exceed a maximum music count.",
            );
            // Remove any chunks which exceed the count on their own
            graph
                .chunks
                .retain(|_id, chunk| chunk.music.counts[music_ty_idx] <= max_count_left_per_chunk);
        }
    }
}

/// Search every combination of the musical chunks, adding any working sets of chunks to
/// `chunk_patterns`.
// TODO: Why is this returning duplicates?
fn search_chunk_combinations<'gr>(
    counts_needed_from_non_required_chunks: &Breakdown,
    non_required_chunks: &[(&'gr StandardChunkId, &'gr Chunk)],
) -> Vec<HashSet<&'gr StandardChunkId>> {
    let mut chunk_patterns = Vec::<HashSet<&StandardChunkId>>::new();
    let mut chunks_used = HashSet::<&StandardChunkId>::new();
    let mut iter_count_down = ITERATION_LIMIT;
    search_chunks(
        non_required_chunks.iter(),
        counts_needed_from_non_required_chunks,
        non_required_chunks,
        &mut chunks_used,
        &mut chunk_patterns,
        &mut iter_count_down,
    );
    chunk_patterns
}

/// Recursively attempt to add any subset of [`ChunkId`]s taken from `chunks`, adding any working
/// patterns to `chunk_patterns`.
fn search_chunks<'iter, 'graph: 'iter>(
    mut chunks: impl Iterator<Item = &'iter (&'graph StandardChunkId, &'graph Chunk)> + Clone,

    counts_needed: &Breakdown,
    non_required_chunks: &[(&'graph StandardChunkId, &'graph Chunk)],

    chunks_used: &mut HashSet<&'graph StandardChunkId>,
    chunk_patterns: &mut Vec<HashSet<&'graph StandardChunkId>>,
    // Counter which is **decremented** every time this function is called, and the search is
    // terminated when this reaches 0.
    iters_left: &mut usize,
) {
    // Terminate once the iteration limit is reached
    match iters_left.checked_sub(1) {
        Some(v) => *iters_left = v,
        None => return,
    }
    // If the required count is reached without this chunk, then don't bother exploring further
    if counts_needed.counts.iter().all(|cnt| *cnt == 0) {
        chunk_patterns.push(chunks_used.clone());
        return;
    }

    let (id, chunk) = match chunks.next() {
        Some(v) => v,
        None => return, // No more chunks to test
    };

    // Test if other chunks can get the required score without including this one
    search_chunks(
        chunks.clone(),
        counts_needed,
        non_required_chunks,
        chunks_used,
        chunk_patterns,
        iters_left,
    );

    for false_id in chunk.false_chunks() {
        if chunks_used.contains(false_id) {
            return; // Don't add this chunk if it's false
        }
    }

    // Add the chunk
    chunks_used.insert(id);
    let counts_needed_with_this_chunk = counts_needed.saturating_sub(chunk.music());
    // Continue searching, assuming that this chunk is used
    search_chunks(
        chunks.clone(),
        &counts_needed_with_this_chunk,
        non_required_chunks,
        chunks_used,
        chunk_patterns,
        iters_left,
    );
    // Remove this chunk before returning
    assert!(chunks_used.remove(id));
}
