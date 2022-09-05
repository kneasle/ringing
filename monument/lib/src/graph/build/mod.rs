//! Code for building the initial unoptimised [`Graph`]

mod falseness;
mod layout;

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::Arc,
    time::Instant,
};

use bellframe::{Bell, Block, Mask, Row, RowBuf, Stroke};
use itertools::Itertools;

use crate::{
    group::{PartHeadGroup, PhRotation},
    query::{self, CallVec, MethodVec, Query, StrokeSet},
    search::Config,
    utils::{Boundary, Counts, MusicBreakdown},
};

use super::{Chunk, ChunkId, Graph, LinkSet, LinkSide, PerPartLength, RowIdx, TotalLength};

impl Graph {
    /// Generate a graph of all chunks which are reachable within a given length constraint.
    pub fn unoptimised(query: &Query, config: &Config) -> crate::Result<Self> {
        log::debug!("Building unoptimised graph:");
        let graph_build_start = Instant::now();

        check_query(query)?;

        let fixed_bells = fixed_bells(query);
        let method_datas = query
            .methods
            .iter()
            .map(|m| MethodData::new(m, &fixed_bells, query))
            .collect::<MethodVec<_>>();

        // Generate chunk layout
        let start = Instant::now();
        let (mut chunk_equiv_map, chunk_lengths, links) =
            self::layout::chunk_lengths(query, &method_datas, config)?;
        log::debug!("  Chunk layout generated in {:.2?}", start.elapsed());

        // TODO: Combine overlapping chunks

        // Build actual chunks
        let mut chunks = chunk_lengths
            .into_iter()
            .map(|(id, per_part_length): (ChunkId, PerPartLength)| {
                let chunk = expand_chunk(&id, per_part_length, query);
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
        if query.require_truth {
            falseness::set_links(
                &mut chunks,
                &mut chunk_equiv_map,
                query,
                &method_datas,
                &fixed_bells,
            );
        }

        // Count music
        let start = Instant::now();
        let relies_on_stroke = query
            .music_types
            .iter()
            .any(|ty| ty.strokes != StrokeSet::Both);
        let start_strokes = get_start_strokes(&chunks, &links, query);
        if start_strokes.is_none() && relies_on_stroke {
            return Err(crate::Error::InconsistentStroke);
        }
        // Now we know the starting strokes, count the music on each chunk
        for (id, chunk) in &mut chunks {
            count_scores(id, chunk, &method_datas, &start_strokes, query);
        }
        log::debug!("  Music counted in {:.2?}", start.elapsed());

        log::debug!(
            "Graph build completed in {:.3?} ({} chunks and {} links)",
            graph_build_start.elapsed(),
            chunks.len(),
            links.len(),
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
        Ok(Graph {
            chunks,
            links,

            starts,
            ends,
        })
    }
}

/// Check a [`Query`] for obvious errors before starting to build the [`Graph`]
fn check_query(query: &Query) -> crate::Result<()> {
    // Different start/end rows aren't well defined for multi-parts
    if query.is_multipart() && query.start_row != query.end_row {
        return Err(crate::Error::DifferentStartEndRowInMultipart);
    }

    // Two methods using the same shorthand
    for (i1, m1) in query.methods.iter_enumerated() {
        for m2 in &query.methods[..i1] {
            if m1.shorthand == m2.shorthand {
                return Err(crate::Error::DuplicateShorthand {
                    shorthand: m1.shorthand.clone(),
                    title1: m1.title().to_owned(),
                    title2: m2.title().to_owned(),
                });
            }
        }
    }

    // Too short calling positions
    for call in &query.calls {
        if call.calling_positions.len() != query.stage.num_bells() {
            return Err(crate::Error::WrongCallingPositionsLength {
                call_name: call.debug_symbol.clone(),
                calling_position_len: call.calling_positions.len(),
                stage: query.stage,
            });
        }
    }

    // Calls referring to non-existent labels
    let mut defined_labels = HashSet::<&String>::new();
    for m in &query.methods {
        for annot in m.plain_course().annots() {
            defined_labels.extend(annot.labels);
        }
    }
    for call in &query.calls {
        for lead_label in [&call.label_from, &call.label_to] {
            if !defined_labels.contains(lead_label) {
                return Err(crate::Error::UndefinedLabel {
                    call_name: call.debug_symbol.clone(),
                    label: lead_label.clone(),
                });
            }
        }
    }

    // Course head masks which don't exist in other parts
    //
    // TODO: Remove this?
    // TODO: Make this refer to lead head masks
    //
    // For every CH mask ...
    for method in &query.methods {
        for mask_in_first_part in &method.ch_masks {
            // ... for every part ...
            for part_head in query.part_head_group.rows() {
                // ... check that the CH mask in that part is covered by some CH mask
                let mask_in_other_part = part_head * mask_in_first_part;
                let is_covered = method
                    .ch_masks
                    .iter()
                    .any(|mask| mask_in_other_part.is_subset_of(mask));
                if !is_covered {
                    return Err(crate::Error::NoCourseHeadInPart {
                        mask_in_first_part: mask_in_first_part.clone(),
                        part_head: part_head.to_owned(),
                        mask_in_other_part,
                    });
                }
            }
        }
    }

    Ok(())
}

/// Creates a blank [`Chunk`] from a [`ChunkId`] and corresponding [`PerPartLength`].
fn expand_chunk(id: &ChunkId, per_part_length: PerPartLength, query: &Query) -> Chunk {
    let total_length = per_part_length.as_total(&query.part_head_group);
    Chunk {
        per_part_length,
        total_length,
        // All the length goes to the method rung in this chunk
        method_counts: Counts::single_count(
            total_length.as_usize(),
            id.method.index(),
            query.methods.len(),
        ),

        // Filled in separate graph build passes
        predecessors: Vec::new(),
        successors: Vec::new(),
        false_chunks: Vec::new(),
        music: MusicBreakdown::zero(0),

        // Used by optimisation passes
        required: false,
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
    query: &Query,
) -> Option<HashMap<ChunkId, Stroke>> {
    let mut start_strokes = HashMap::<ChunkId, Stroke>::with_capacity(chunks.len());
    let mut frontier = Vec::<(ChunkId, Stroke)>::new();
    // Populate the frontier by setting each starting chunk with its respective start stroke
    //
    // `query.start_row` refers to the first **non-start** row of the composition, consistent with
    // how ringers view ringing as starting at the first non-rounds row.  However, Monument
    // considers the `start_row` to be part of the composition (so that leads go from lead head to
    // end, inclusive), so we need to invert `query.start_row` to convert.
    let stroke_of_start_row = !query.start_stroke;
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
    method_datas: &MethodVec<MethodData>,
    start_strokes: &Option<HashMap<ChunkId, Stroke>>,
    query: &Query,
) {
    // Always set music to `0`s, even if the chunk is unreachable.  If we don't, then an
    // optimisation pass could see this chunk and run `zip_eq` on the `MusicType`s, thus causing a
    // panic.
    chunk.music = MusicBreakdown::zero(query.music_types.len());

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
    let plain_course = &method_datas[id.method].plain_course;

    for part_head in query.part_head_group.rows() {
        let lead_head_in_part = part_head * id.lead_head.as_ref();
        let row_iter = (0..chunk.per_part_length.as_usize()).map(|offset| {
            let index = (id.sub_lead_idx + offset) % plain_course.len();
            plain_course.get_row(index).unwrap()
        });
        // Count weight from music
        chunk.music += &MusicBreakdown::from_rows(
            row_iter,
            &lead_head_in_part,
            query.music_types.as_raw_slice(),
            start_stroke,
        );
        // Count weight from CH masks
        for (mask, weight) in &query.course_weights {
            if mask.matches(&lead_head_in_part) {
                // Weight applies to each row
                chunk.music.score += *weight * chunk.per_part_length.as_usize() as f32;
            }
        }
    }
}

///////////////
// UTILITIES //
///////////////

/// [`ChunkId`] that refers to a chunk _in a single part_.  I.e. before being used in the graph, it
/// needs to be turned into the [`ChunkId`] referring to the 'equivalence class' containing that
/// [`Chunk`] (using [`ChunkEquivalenceMap`]).
#[derive(Debug, Clone)]
struct ChunkIdInFirstPart {
    lead_head: RowBuf,
    row_idx: RowIdx,
}

impl Deref for ChunkIdInFirstPart {
    type Target = RowIdx;

    fn deref(&self) -> &Self::Target {
        &self.row_idx
    }
}

#[derive(Debug)]
struct ChunkEquivalenceMap<'query> {
    part_head_group: &'query PartHeadGroup,
    // NOTE: The `PhRotation` represents what's required to go from the concrete part head to the
    // `Arc<Row>` representing its equivalence class.
    normalisation: HashMap<RowBuf, (Arc<Row>, PhRotation)>,
}

impl<'query> ChunkEquivalenceMap<'query> {
    fn new(part_head_group: &'query PartHeadGroup) -> Self {
        Self {
            part_head_group,
            normalisation: HashMap::new(),
        }
    }

    fn normalise(&mut self, id: &ChunkIdInFirstPart) -> (ChunkId, PhRotation) {
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

/// Cached data about a single [`Method`], used to speed up chunk generation.
#[derive(Debug)]
struct MethodData<'query> {
    method: &'query query::Method,
    plain_course: Block<bellframe::method::RowAnnot<'query>>,
    lead_head_masks: Vec<Mask>,
    start_indices: Vec<usize>,
    end_indices: Vec<usize>,
}

impl<'query> MethodData<'query> {
    fn new(method: &'query query::Method, fixed_bells: &[(Bell, usize)], query: &Query) -> Self {
        // Convert *course* head masks into *lead* head masks (course heads are convenient for the
        // user, but the whole graph is based on lead heads).
        let mut lead_head_masks = HashSet::new();
        'mask_loop: for ch_mask in &method.ch_masks {
            let mut ch_masks_with_fixed_bells = ch_mask.to_owned();
            // Add the fixed bells to this CH mask
            for (bell, pos) in fixed_bells {
                if ch_masks_with_fixed_bells.set_bell(*bell, *pos).is_err() {
                    log::debug!(
                        "Discarding CH mask {} because it fixes {} in the wrong place",
                        ch_mask,
                        bell
                    );
                    // Discard any CH masks which require fixed bells in impossible places
                    continue 'mask_loop;
                }
            }
            for lead_head in method.lead_head().closure() {
                lead_head_masks.insert(&ch_masks_with_fixed_bells * &lead_head);
            }
        }
        // Remove any lh masks which are a subset of others (for example, if `xx3456` and `xxxx56`
        // are present, then `xx3456` can be removed because it is implied by `xxxx56`).  This is
        // useful to speed up the falseness table generation.  Making `lead_head_masks` a `HashSet`
        // means that perfect duplicates have already been eliminated, so we only need to check for
        // strict subset-ness.
        let mut filtered_lead_head_masks = Vec::new();
        for mask in &lead_head_masks {
            let is_implied_by_another_mask = lead_head_masks
                .iter()
                .any(|mask2| mask.is_strict_subset_of(mask2));
            if !is_implied_by_another_mask {
                filtered_lead_head_masks.push(mask.clone());
            }
        }

        // Compute exact `{start,end}indices`
        let wrap_sub_lead_indices = |sub_lead_indices: &[isize]| -> Vec<usize> {
            sub_lead_indices
                .iter()
                .map(|idx| {
                    let lead_len_i = method.lead_len() as isize;
                    (*idx % lead_len_i + lead_len_i) as usize % method.lead_len()
                })
                .collect_vec()
        };
        let mut start_indices = wrap_sub_lead_indices(&method.start_indices);
        let mut end_indices = match &method.end_indices {
            Some(indices) => wrap_sub_lead_indices(indices),
            None => (0..method.lead_len()).collect_vec(),
        };
        // If ringing a multi-part, the `{start,end}_indices` have to match.   Therefore, it makes
        // no sense to generate any starts/ends which don't have a matching end/start.  To achieve
        // this, we set both `{start,end}_indices` to the union between `start_indices` and
        // `end_indices`.
        if query.is_multipart() {
            let union = start_indices
                .iter()
                .filter(|idx| end_indices.contains(idx))
                .copied()
                .collect_vec();
            start_indices = union.clone();
            end_indices = union;
        }

        Self {
            method,
            plain_course: method.plain_course(),
            lead_head_masks: filtered_lead_head_masks,

            start_indices,
            end_indices,
        }
    }

    /// Checks if `row` is a valid lead head in this method (according to the CH masks provided).
    fn is_lead_head(&self, lead_head: &Row) -> bool {
        self.lead_head_masks.iter().any(|m| m.matches(lead_head))
    }

    fn start_or_end_indices(&self, boundary: Boundary) -> &[usize] {
        match boundary {
            Boundary::Start => &self.start_indices,
            Boundary::End => &self.end_indices,
        }
    }
}

/// Returns the place bells which are always preserved by plain leads and all calls of all methods
/// (e.g. hunt bells in non-variable-hunt compositions).
fn fixed_bells(query: &Query) -> Vec<(Bell, usize)> {
    let mut fixed_bells = query.stage.bells().collect_vec();
    for m in &query.methods {
        let f = fixed_bells_of_method(m, &query.calls);
        fixed_bells.retain(|b| f.contains(b));
    }
    // Currently, these `fixed_bells` assume that the start_row is rounds
    fixed_bells
        .iter()
        .map(|b| (query.start_row[b.index()], b.index()))
        .collect_vec()
}

/// Returns the place bells which are always preserved by plain leads and all calls of a single
/// method (e.g. hunt bells in non-variable-hunt compositions).
fn fixed_bells_of_method(method: &query::Method, calls: &CallVec<query::Call>) -> HashSet<Bell> {
    // Start the set with the bells which are fixed by the plain lead of every method
    let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
    for call in calls {
        // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
        // Grandsire is unaffected by a plain lead, but affected by calls)
        filter_bells_fixed_by_call(method, call, &mut fixed_bells);
    }
    fixed_bells
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(
    method: &bellframe::Method,
    call: &query::Call,
    set: &mut HashSet<Bell>,
) {
    // Note that all calls are required to only substitute one piece of place notation.
    for sub_lead_idx_after_call in method.label_indices(&call.label_from) {
        // TODO: Handle different from/to locations
        let idx_before_call = (sub_lead_idx_after_call + method.lead_len() - 1) % method.lead_len();
        let idx_after_call = idx_before_call + 1; // in range `1..=method.lead_len()`

        // The row before a call in this location in the _first lead_
        let row_before_call = method.first_lead().get_row(idx_before_call).unwrap();
        // The row after a plain call in this location in the _first lead_
        let row_after_no_call = method.first_lead().get_row(idx_after_call).unwrap();
        // The row after a call in this location in the _first lead_
        let mut row_after_call = row_before_call.to_owned();
        call.place_not.permute(&mut row_after_call).unwrap();

        // A bell is _affected_ by the call iff it's in a different place in `row_after_call` than
        // `row_after_no_call`.  These should be removed from the set, because they are no longer
        // fixed.
        for (bell_after_no_call, bell_after_call) in
            row_after_no_call.bell_iter().zip(&row_after_call)
        {
            if bell_after_call != bell_after_no_call {
                set.remove(&bell_after_call);
            }
        }
    }
}
