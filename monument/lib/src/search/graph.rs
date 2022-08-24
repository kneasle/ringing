use std::collections::HashMap;

use crate::{
    graph::LinkSide,
    group::{PartHead, PhRotation},
    utils::{Counts, PerPartLength, Score, TotalLength},
    CallIdx, Query,
};
use bit_vec::BitVec;
use ordered_float::OrderedFloat;

/// An immutable version of [`monument_graph::Graph`] which can be traversed without hash table
/// lookups.
#[derive(Debug, Clone)]
pub(super) struct Graph {
    pub starts: StartVec<(ChunkIdx, crate::graph::LinkId, PartHead)>,
    pub chunks: ChunkVec<Chunk>,
}

#[derive(Debug, Clone)]
pub(super) struct Chunk {
    pub id: crate::graph::ChunkId,

    pub score: Score,
    pub music_counts: Counts, // PERF: Not used in search

    pub per_part_length: PerPartLength, // PERF: Not used in search
    pub total_length: TotalLength,
    pub method_counts: Counts,
    /// Minimum number of rows required to go from the end of `self` to rounds
    pub min_len_to_rounds: TotalLength,

    // Indices must be aligned with those from the source graph
    pub succs: SuccVec<SuccLink>,
    // If this chunk is added to a composition, these bits denote the set of chunks will be marked
    // as unreachable.  This includes `Self`, because every chunk is guaranteed to be false against
    // itself.
    pub falseness: BitVec,
}

/// A link between a chunk and its successor
#[derive(Debug, Clone)]
pub(super) struct SuccLink {
    pub call: Option<CallIdx>,
    pub next: LinkSide<ChunkIdx>,
    pub score: Score,
    pub ph_rotation: PhRotation,
}

///////////////////////////////////////////
// CONVERSION FROM monument_graph::Graph //
///////////////////////////////////////////

impl Graph {
    pub fn new(source_graph: &crate::graph::Graph, query: &Query) -> Self {
        let num_chunks = source_graph.chunks.len();

        // Assign each chunk ID to a unique `ChunkIdx`, and vice versa.  This way, we can now label
        // the set of chunks with numbers that can be used to index into a BitVec for falseness
        // computation.
        let mut index_to_id = ChunkVec::<(crate::graph::ChunkId, &crate::graph::Chunk)>::new();
        let mut id_to_index = HashMap::<crate::graph::ChunkId, ChunkIdx>::new();
        for (id, chunk) in &source_graph.chunks {
            let index = index_to_id.push((id.to_owned(), chunk));
            id_to_index.insert(id.to_owned(), index);
        }

        // Now convert chunks from `monument_graph::Chunk` to `self::Chunk`
        let chunks: ChunkVec<_> = (0..num_chunks)
            .map(|index| {
                // Get the source chunk and its ChunkId
                let index = ChunkIdx::new(index);
                let (from_id, source_chunk) = index_to_id[index].clone();

                // Generate a BitVec with a 1 for every chunk which is false against this chunk
                let mut falseness = BitVec::from_elem(num_chunks, false);
                for false_id in &source_chunk.false_chunks {
                    let false_chunk_idx = id_to_index[false_id];
                    falseness.set(false_chunk_idx.index(), true);
                }

                let succs = source_chunk
                    .successors
                    .iter()
                    .filter_map(|link_id| {
                        let link = source_graph.links.get(*link_id)?;
                        let next = match &link.to {
                            LinkSide::Chunk(ch_id) => LinkSide::Chunk(*id_to_index.get(ch_id)?),
                            LinkSide::StartOrEnd => LinkSide::StartOrEnd,
                        };
                        Some(SuccLink {
                            call: link.call,
                            score: link_score(source_chunk, link, query),
                            next,
                            ph_rotation: link.ph_rotation,
                        })
                    })
                    .collect();

                Chunk {
                    id: from_id,

                    score: source_chunk.music.score,
                    music_counts: source_chunk.music.counts.clone(),

                    per_part_length: source_chunk.per_part_length,
                    total_length: source_chunk.total_length,
                    method_counts: source_chunk.method_counts.clone(),
                    min_len_to_rounds: source_chunk.lb_distance_to_rounds,

                    succs,
                    falseness,
                }
            })
            .collect();

        // Compute the list of start chunks and their labels
        let mut starts = StartVec::new();
        for (start_link_id, start_chunk_id) in &source_graph.starts {
            let start_link = match source_graph.links.get(*start_link_id) {
                Some(l) => l,
                None => continue, // Skip any dangling start `LinkId`s
            };
            if source_graph.chunks.contains_key(start_chunk_id) {
                starts.push((
                    id_to_index[start_chunk_id],
                    *start_link_id,
                    PartHead::rounds() * start_link.ph_rotation,
                ));
            }
        }

        Graph { starts, chunks }
    }
}

/// Gets the total [`Score`] generated by a given [`Link`].  For end links, this **doesn't**
/// include the [`Score`] from splices over the part end.
fn link_score(
    source_chunk: &crate::graph::Chunk,
    link: &crate::graph::Link,
    query: &Query,
) -> Score {
    const ZERO: Score = OrderedFloat(0.0);

    let is_splice = match (&link.from, &link.to) {
        // A link between chunks is a splice iff c2's RowIdx directly
        // follows from c1's (i.e. it's the same method and is one row
        // later).  For example:
        // - (Bristol, 16) -> (Bristol, 17)   isn't a splice
        // - (Bristol, 31) -> (Bristol, 0)    isn't a splice (it wraps round the lead end)
        // - (Bristol, 31) -> (Cambridge, 0)  **is** a splice (method changes)
        // - (Bristol, 17) -> (Bristol, 0)    **is** a splice (it skips half a lead)
        (LinkSide::Chunk(c1), LinkSide::Chunk(c2)) => {
            let sub_lead_idx_after_prev_chunk = query.methods[c1.method]
                .add_sub_lead_idx(c1.sub_lead_idx, source_chunk.per_part_length);
            let is_continuation =
                c1.method == c2.method && sub_lead_idx_after_prev_chunk == c2.sub_lead_idx;
            !is_continuation
        }
        // If either side is a start/end, then no splice occurs
        _ => false,
    };
    let call_weight = match link.call {
        Some(idx) => Score::from(query.calls[idx].weight),
        None => ZERO, // Plain leads have no weight
    };
    let splice_weight = if is_splice { query.splice_weight } else { ZERO };
    (call_weight + splice_weight) * query.num_parts() as f32
}

index_vec::define_index_type! { pub struct ChunkIdx = usize; }
index_vec::define_index_type! { pub struct StartIdx = usize; }
index_vec::define_index_type! { pub struct SuccIdx = usize; }
type ChunkVec<T> = index_vec::IndexVec<ChunkIdx, T>;
type StartVec<T> = index_vec::IndexVec<StartIdx, T>;
type SuccVec<T> = index_vec::IndexVec<SuccIdx, T>;
