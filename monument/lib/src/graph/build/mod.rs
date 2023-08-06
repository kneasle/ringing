//! Code for building the initial unoptimised [`Graph`]

mod falseness;
mod layout;

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::Arc,
    time::Instant,
};

use bellframe::{PlaceNot, Row, RowBuf, Stroke};
use itertools::Itertools;

use crate::{
    atw::AtwTable,
    group::{PartHeadGroup, PhRotation},
    parameters::{Call, StrokeSet},
    query::Query,
    search::Config,
    utils::{counts::Counts, MusicBreakdown},
};

use super::{Chunk, ChunkId, Graph, LinkSet, LinkSide, PerPartLength, RowIdx, TotalLength};

impl Graph {
    /// Generate a graph of all chunks which are reachable within a given length constraint.
    pub(crate) fn unoptimised(query: &Query, config: &Config) -> crate::Result<(Self, AtwTable)> {
        log::debug!("Building unoptimised graph:");
        let graph_build_start = Instant::now();

        check_query(query)?;

        // Generate chunk layout
        let start = Instant::now();
        let (mut chunk_equiv_map, chunk_lengths, links) =
            self::layout::chunk_lengths(query, config)?;
        log::debug!("  Chunk layout generated in {:.2?}", start.elapsed());

        let atw_table = AtwTable::new(query, &chunk_lengths);

        // TODO: Combine overlapping chunks

        // Build actual chunks
        let mut chunks = chunk_lengths
            .into_iter()
            .map(|(id, per_part_length): (ChunkId, PerPartLength)| {
                let chunk = expand_chunk(&id, per_part_length, query, &atw_table);
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
            falseness::set_links(&mut chunks, &mut chunk_equiv_map, query);
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
            count_scores(id, chunk, &start_strokes, query);
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
        };
        Ok((graph, atw_table))
    }
}

/// Creates a blank [`Chunk`] from a [`ChunkId`] and corresponding [`PerPartLength`].
fn expand_chunk(
    id: &ChunkId,
    per_part_length: PerPartLength,
    query: &Query,
    atw_table: &AtwTable,
) -> Chunk {
    let total_length = per_part_length.as_total(&query.part_head_group);
    // A `Chunk` is a duffer iff it's a duffer in any part
    let duffer = query.part_head_group.rows().any(|part_head| {
        let lead_head_in_part = part_head * &*id.lead_head;
        query.methods[id.method].is_lead_head_duffer(&lead_head_in_part)
    });

    Chunk {
        per_part_length,
        total_length,

        // All the length goes to the method rung in this chunk
        method_counts: Counts::single_count(
            total_length.as_usize(),
            id.method.index(),
            query.methods.len(),
        ),
        atw_bitmap: atw_table.bitmap_for_chunk(query, id, per_part_length),

        // Filled in separate graph build passes
        predecessors: Vec::new(),
        successors: Vec::new(),
        false_chunks: Vec::new(),
        music: MusicBreakdown::zero(0),

        // Used by optimisation passes
        required: false,
        lb_distance_from_rounds: TotalLength::ZERO,
        lb_distance_to_rounds: TotalLength::ZERO,

        duffer,
        lb_distance_from_non_duffer: PerPartLength::ZERO,
        lb_distance_to_non_duffer: PerPartLength::ZERO,
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
    let plain_course = &query.methods[id.method].plain_course;
    let lead_heads = query.methods[id.method].inner.lead_head().closure();

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
        // Count weight from `course_weights`.  `course_weights` apply to every row of every course
        // which contains a lead head matching that mask, so we have to transpose the mask by every
        // lead head to check every lead in the course.  For example, for Plain Bob lead-head
        // methods, `xxxxxx78` will expand into masks
        // `[xxxxxx78, xxxxx8x7, xxx8x7xx, x8x7xxxx, x78xxxxx, xx7x8xxx, xxxx7x8x]` (every one of
        // those leads is included in the course for `xxxxxx78`)
        for (mask, weight) in &query.course_weights {
            for lead_head in &lead_heads {
                if (mask * lead_head).matches(&lead_head_in_part) {
                    // Weight applies to each row
                    chunk.music.score += *weight * chunk.per_part_length.as_usize() as f32;
                }
            }
        }
    }
}

////////////////////
// QUERY CHECKING //
////////////////////

/// Check a [`Query`] for obvious errors before starting to build the [`Graph`]
fn check_query(query: &Query) -> crate::Result<()> {
    // Different start/end rows aren't well defined for multi-parts
    if query.is_multipart() && query.start_row != query.end_row {
        return Err(crate::Error::DifferentStartEndRowInMultipart);
    }

    // Two methods using the same shorthand
    for (i1, m1) in query.methods.iter_enumerated() {
        for m2 in &query.methods[..i1] {
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
    for call in &query.calls {
        if call.calling_positions.len() != query.stage.num_bells() {
            return Err(crate::Error::WrongCallingPositionsLength {
                call_name: call.symbol.clone(),
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
                    call_name: call.symbol.clone(),
                    label: lead_label.clone(),
                });
            }
        }
    }

    // Two calls with the same name at the same lead location
    check_for_duplicate_call_names(query)?;

    // Course head masks which don't exist in other parts
    //
    // TODO: Make this actually output a mask specified by the user
    //
    // For every CH mask ...
    for method in &query.methods {
        for mask_in_first_part in &method.specified_course_head_masks {
            // ... for every part ...
            for part_head in query.part_head_group.rows() {
                // ... check that the CH mask in that part is covered by some lead mask
                let mask_in_other_part = part_head * mask_in_first_part;
                let is_covered = method
                    .allowed_lead_masks
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

/// Check for two [`Call`]s which assign the same `symbol` at the same `label`.
fn check_for_duplicate_call_names(query: &Query) -> crate::Result<()> {
    let sorted_calls = query
        .calls
        .iter()
        .map(|call: &Call| -> (&str, &str, &PlaceNot) {
            (&call.symbol, &call.label_from, &call.place_notation)
        })
        .sorted_by_key(|&(sym, lead_loc, _pn)| (sym, lead_loc));
    for ((sym1, label1, pn1), (sym2, label2, pn2)) in sorted_calls.tuple_windows() {
        if sym1 == sym2 && label1 == label2 {
            return Err(crate::Error::DuplicateCall {
                symbol: sym1.to_owned(),
                label: label1.to_owned(),
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
