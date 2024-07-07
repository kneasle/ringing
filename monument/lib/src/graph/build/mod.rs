//! Code for building the initial unoptimised [`Graph`]

mod falseness;
mod layout;

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::Deref,
    sync::Arc,
    time::Instant,
};

use bellframe::{music::AtRowPositions, Block, Mask, Row, RowBuf, Stroke, StrokeSet};
use itertools::Itertools;

use crate::{
    group::{PartHeadGroup, PhRotation},
    parameters::{Call, Method, MethodIdx, MethodVec, Parameters},
    search::Config,
    utils::counts::Counts,
};

use super::{Chunk, ChunkId, Graph, LinkSet, LinkSide, PerPartLength, RowIdx, TotalLength};

impl Graph {
    /// Generate a graph of all chunks which are reachable within a given length constraint.
    pub(crate) fn unoptimised(params: &Parameters, config: &Config) -> crate::Result<Self> {
        log::debug!("Building unoptimised graph:");
        let graph_build_start = Instant::now();

        check_params(params)?;

        // Generate chunk layout
        let start = Instant::now();
        let layout::ChunkLengths {
            chunk_lengths,
            links,
            mut chunk_equiv_map,

            call_sequence_length,
        } = self::layout::chunk_lengths(params, config)?;
        log::debug!("  Chunk layout generated in {:.2?}", start.elapsed());

        // TODO: Combine overlapping chunks

        // Build actual chunks
        let mut chunks = chunk_lengths
            .into_iter()
            .map(|(id, per_part_length): (ChunkId, PerPartLength)| {
                let chunk = expand_chunk(&id, per_part_length, params);
                (id, chunk)
            })
            .collect::<HashMap<_, _>>();

        // Assign `successor`/`predecessor` links
        let start = Instant::now();
        for (link_id, link) in links.iter() {
            if let LinkSide::Chunk(id) = &link.from {
                if let Some(chunk) = chunks.get_mut(id) {
                    chunk.successors.push(*link_id);
                }
            }
            if let LinkSide::Chunk(id) = &link.to {
                if let Some(chunk) = chunks.get_mut(id) {
                    chunk.predecessors.push(*link_id);
                }
            }
        }
        log::debug!(
            "  Successor/predecessor links set in {:.2?}",
            start.elapsed()
        );

        // Assign falseness links
        if params.require_truth {
            falseness::set_links(&mut chunks, &mut chunk_equiv_map, params);
        }

        // Count music
        let start = Instant::now();
        let relies_on_stroke = params
            .music_types
            .iter()
            .any(|ty| ty.strokes() != StrokeSet::Both);
        let start_strokes = get_start_strokes(&chunks, &links, params);
        if start_strokes.is_none() && relies_on_stroke {
            return Err(crate::Error::InconsistentStroke);
        }
        // Now we know the starting strokes, count the music on each chunk
        let method_caches: MethodVec<MethodCacheData> = params
            .methods
            .iter()
            .map(|m| MethodCacheData::new(m, params))
            .collect();
        for (id, chunk) in &mut chunks {
            count_scores(id, chunk, &method_caches, &start_strokes, params);
        }
        log::debug!("  Music counted in {:.2?}", start.elapsed());

        log::debug!(
            "Graph build completed in {:.3?}",
            graph_build_start.elapsed()
        );

        // Compute start/end links
        use LinkSide::*;
        let mut starts = Vec::new();
        let mut ends = Vec::new();
        for (link_id, link) in links.iter() {
            match (&link.from, &link.to) {
                (StartOrEnd, Chunk(chunk_id)) => starts.push((*link_id, chunk_id.clone())),
                (Chunk(chunk_id), StartOrEnd) => ends.push((*link_id, chunk_id.clone())),
                (Chunk(_), Chunk(_)) => {} // Internal link
                (StartOrEnd, StartOrEnd) => unreachable!("Can't have 0-length start->end links"),
            }
        }

        // Finally construct the graph
        let graph = Graph {
            chunks,
            links,

            starts,
            ends,

            call_sequence_length,
            required_chunk_sets: HashSet::new(),
        };
        Ok(graph)
    }
}

struct MethodCacheData<'params> {
    double_plain_course: Block<bellframe::method::RowAnnot<'params>>,
    lead_head_weights: Vec<(Mask, f32)>,
}

impl<'params> MethodCacheData<'params> {
    fn new(method: &'params Method, params: &'params Parameters) -> Self {
        let mut double_plain_course = method.plain_course();
        // Add another plain course.  We use 'double plain courses' so that the range
        // covered by a chunk is always a contiguous section of these cached plain
        // courses (otherwise, a chunk could wrap over the end of one plain course).
        double_plain_course.extend_from_within(..);

        Self {
            double_plain_course,
            lead_head_weights: method.lead_head_weights(params),
        }
    }
}

/// Creates a blank [`Chunk`] from a [`ChunkId`] and corresponding [`PerPartLength`].
fn expand_chunk(id: &ChunkId, per_part_length: PerPartLength, params: &Parameters) -> Chunk {
    let total_length = per_part_length.as_total(&params.part_head_group);

    Chunk {
        per_part_length,
        total_length,

        // All the length goes to the method rung in this chunk
        method_counts: Counts::single_count(
            total_length.as_usize(),
            id.method.index(),
            params.methods.len(),
        ),

        // Filled in separate graph build passes
        predecessors: Vec::new(),
        successors: Vec::new(),
        false_chunks: Vec::new(),
        score: 0.0,
        music_counts: index_vec::index_vec![AtRowPositions::ZERO; params.music_types.len()],

        // Used by optimisation passes
        lb_distance_from_rounds: TotalLength::ZERO,
        lb_distance_to_rounds: TotalLength::ZERO,
    }
}

/// Attempt to assign a single starting [`Stroke`] to every [`Chunk`].  If such a mapping is
/// ambiguous (i.e. there's a cycle in the graph which doesn't preserve [`Stroke`]) then [`None`]
/// is returned.
// TODO: Add `Stroke` as part of `ChunkId`, so that the same chunk on two different strokes are
// treated differently
fn get_start_strokes(
    chunks: &HashMap<ChunkId, Chunk>,
    links: &LinkSet,
    params: &Parameters,
) -> Option<HashMap<ChunkId, Stroke>> {
    let mut start_strokes = HashMap::<ChunkId, Stroke>::with_capacity(chunks.len());
    let mut frontier = Vec::<(ChunkId, Stroke)>::new();
    // Populate the frontier by setting each starting chunk with its respective start stroke
    //
    // `params.start_row` refers to the first **non-start** row of the composition, consistent with
    // how ringers view ringing as starting at the first non-rounds row.  However, Monument
    // considers the `start_row` to be part of the composition (so that leads go from lead head to
    // end, inclusive), so we need to invert `params.start_row` to convert.
    let stroke_of_start_row = !params.start_stroke;
    for link in links.values() {
        if let (LinkSide::StartOrEnd, LinkSide::Chunk(id)) = (&link.from, &link.to) {
            frontier.push((id.clone(), stroke_of_start_row));
        }
    }
    // Run depth-first search over the graph, assigning strokes to chunks as we go.  Branching
    // terminates whenever we assign a `Stroke` to a chunk that already has one.  In this case, the
    // new stroke either agrees with the old and everything is fine.  Or it disagrees, in which
    // case we have a consistency error.  If we successfully complete the search, then all chunk
    // starts are unambiguous.
    while let Some((id, new_stroke)) = frontier.pop() {
        match start_strokes.insert(id.clone(), new_stroke) {
            Some(s) if s != new_stroke => return None, // Disagreement over stroke
            Some(_) => {}                              // Strokes agree
            None => {
                // Chunk hasn't been expanded before, so continue the search
                if let Some(chunk) = chunks.get(&id) {
                    let stroke_after_chunk = new_stroke.offset(chunk.per_part_length.as_usize());
                    for succ_link_id in &chunk.successors {
                        let succ_link = &links[*succ_link_id];
                        assert_eq!(succ_link.from, LinkSide::Chunk(id.clone()));
                        if let LinkSide::Chunk(succ_id) = &succ_link.to {
                            frontier.push((succ_id.to_owned(), stroke_after_chunk));
                        }
                    }
                }
            }
        }
    }

    Some(start_strokes)
}

/// Count the [`Score`] contributed by this [`Chunk`].  This includes both music and course head
/// weights.
fn count_scores(
    id: &ChunkId,
    chunk: &mut Chunk,
    method_caches: &MethodVec<MethodCacheData>,
    start_strokes: &Option<HashMap<ChunkId, Stroke>>,
    params: &Parameters,
) {
    // Always set music to `0`s, even if the chunk is unreachable.  If we don't, then an
    // optimisation pass could see this chunk and run `zip_eq` on the `MusicType`s, thus causing a
    // panic.
    chunk.score = 0.0;
    chunk.music_counts = index_vec::index_vec![AtRowPositions::ZERO; params.music_types.len()];

    let start_stroke = match start_strokes {
        Some(map) => match map.get(id) {
            Some(stroke) => *stroke,
            // If `map` exists but doesn't contain `id`, then there can't be a path from the start
            // to this chunk (because `map` is generated by running breadth-first search forward
            // through the graph).  Thus, this chunk is guaranteed to never be used in a
            // composition, and there's no point computing an exact score.
            None => return,
        },
        // If the nodes are inconsistent but the `MusicType`s don't rely on stroke, then
        // `start_strokes == None` and we can just give every chunk an arbitrary
        // start stroke
        None => Stroke::Back,
    };
    let method_cache = &method_caches[id.method];

    for part_head in params.part_head_group.rows() {
        let lead_head_in_part = part_head * id.lead_head.as_ref();
        let start_row = &lead_head_in_part
            * method_cache
                .double_plain_course
                .get_row(id.sub_lead_idx)
                .unwrap();
        // Determine the rows that this chunk contains
        let mut rows = Block::empty(params.stage);
        rows.extend_range(
            &method_cache.double_plain_course,
            id.sub_lead_idx..(id.sub_lead_idx + chunk.per_part_length.as_usize()),
        );
        rows.pre_multiply(&start_row);
        // Count weight from music
        for (count_so_far, music_type) in chunk.music_counts.iter_mut().zip_eq(&params.music_types)
        {
            let counts = music_type.count_block(&rows, start_stroke);
            chunk.score += music_type.as_overall_score(counts);
            *count_so_far += counts;
        }
        // Count weight from `course_weights`.  `course_weights` apply to every row of every course
        // which contains a lead head matching that mask, so we have to transpose the mask by every
        // lead head to check every lead in the course.  For example, for Plain Bob lead-head
        // methods, `xxxxxx78` will expand into masks
        // `[xxxxxx78, xxxxx8x7, xxx8x7xx, x8x7xxxx, x78xxxxx, xx7x8xxx, xxxx7x8x]` (every one of
        // those leads is included in the course for `xxxxxx78`)
        for (mask, weight) in &method_cache.lead_head_weights {
            if mask.matches(&lead_head_in_part) {
                // Weight applies to each row
                chunk.score += *weight * chunk.per_part_length.as_usize() as f32;
            }
        }
    }
}

////////////////////
// params CHECKING //
////////////////////

/// Check a [`Parameters`] for obvious errors before starting to build the [`Graph`]
fn check_params(params: &Parameters) -> crate::Result<()> {
    // Different start/end rows aren't well defined for multi-parts
    if params.is_multipart() && params.start_row != params.end_row {
        return Err(crate::Error::DifferentStartEndRowInMultipart);
    }

    // Two methods using the same shorthand
    for (i1, m1) in params.methods.iter_enumerated() {
        for m2 in &params.methods[..i1] {
            if m1.shorthand() == m2.shorthand() {
                return Err(crate::Error::DuplicateShorthand {
                    shorthand: m1.shorthand(),
                    title1: m1.title(),
                    title2: m2.title(),
                });
            }
        }
    }

    // Too short calling positions
    for call in &params.calls {
        if call.calling_positions.len() != params.stage.num_bells() {
            return Err(crate::Error::WrongCallingPositionsLength {
                call_symbol: call.symbol,
                calling_position_len: call.calling_positions.len(),
                stage: params.stage,
            });
        }
    }

    // Calls referring to non-existent labels
    let defined_labels = params.lead_labels_used();
    for call in &params.calls {
        for lead_label in [&call.label_from, &call.label_to] {
            if !defined_labels.contains(lead_label) {
                return Err(crate::Error::UndefinedLabel {
                    call_symbol: call.symbol,
                    label: lead_label.clone(),
                });
            }
        }
    }

    // Two calls with the same name at the same lead location
    check_for_duplicate_call_names(params)?;

    // Check which extra course head masks where added while expanding different part heads
    let mut extra_masks = BTreeMap::<Mask, BTreeMap<RowBuf, Vec<MethodIdx>>>::new();
    for (method_idx, method) in params.methods.iter_enumerated() {
        let (_lhms, extra_masks_for_method) = method.allowed_lead_head_masks_with_extras(params);
        for (specified_ch_mask, part_head) in extra_masks_for_method {
            extra_masks
                .entry(specified_ch_mask)
                .or_default()
                .entry(part_head)
                .or_default()
                .push(method_idx);
        }
    }
    // Print these masks in a human-readable form
    for (specified_mask, methods_per_part) in extra_masks {
        println!("Note: For course mask {specified_mask}, adding extra masks for other parts:");
        for (part_head, methods) in methods_per_part {
            print!("  {} (in part {part_head}", &part_head * &specified_mask);
            if params.is_spliced() {
                print!(" for {}", params.method_list_string(&methods));
            }
            println!(")");
        }
    }

    Ok(())
}

/// Check for two [`Call`]s which assign the same `symbol` at the same `label`.
fn check_for_duplicate_call_names(params: &Parameters) -> crate::Result<()> {
    let sorted_calls = params
        .calls
        .iter()
        .map(|call: &Call| (call.symbol, &call.place_notation))
        .sorted_by_key(|&(sym, _pn)| sym);
    for ((sym1, pn1), (sym2, pn2)) in sorted_calls.tuple_windows() {
        if sym1 == sym2 {
            return Err(crate::Error::DuplicateCall {
                symbol: sym1,
                pn1: pn1.clone(),
                pn2: pn2.clone(),
            });
        }
    }
    Ok(())
}

///////////////
// UTILITIES //
///////////////

/// [`ChunkId`] that refers to a chunk _in a single part_.  I.e. before being used in the graph, it
/// needs to be turned into the [`ChunkId`] referring to the 'equivalence class' containing that
/// [`Chunk`] (using [`ChunkEquivalenceMap`]).
#[derive(Debug, Clone)]
struct UnnormalizedChunkId {
    lead_head: RowBuf,
    row_idx: RowIdx,
}

impl Deref for UnnormalizedChunkId {
    type Target = RowIdx;

    fn deref(&self) -> &Self::Target {
        &self.row_idx
    }
}

#[derive(Debug)]
struct ChunkEquivalenceMap<'params> {
    part_head_group: &'params PartHeadGroup,
    // NOTE: The `PhRotation` represents what's required to go from the concrete part head to the
    // `Arc<Row>` representing its equivalence class.
    normalisation: HashMap<RowBuf, (Arc<Row>, PhRotation)>,
}

impl<'params> ChunkEquivalenceMap<'params> {
    fn new(part_head_group: &'params PartHeadGroup) -> Self {
        Self {
            part_head_group,
            normalisation: HashMap::new(),
        }
    }

    fn normalise(&mut self, id: &UnnormalizedChunkId) -> (ChunkId, PhRotation) {
        // If this lead head hasn't been normalised yet, add it and each of its equivalent copies
        // in other parts to the normalisation mapping.
        if !self.normalisation.contains_key(&id.lead_head) {
            let arc_lead_head = id.lead_head.to_arc();
            for (part_head, element) in self.part_head_group.rotations() {
                self.normalisation
                    .insert(part_head * &id.lead_head, (arc_lead_head.clone(), element));
            }
        }
        // Now normalise the ChunkId (by normalising its `lead_head` and preserving the row index)
        let (normalised_lead_head, rotation) = self.normalisation[&id.lead_head].clone();
        (ChunkId::new(normalised_lead_head, id.row_idx), rotation)
    }
}
