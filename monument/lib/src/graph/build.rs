//! Code for building the initial unoptimised [`Graph`]

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
    convert::identity,
    fmt::{Display, Formatter},
    ops::Deref,
    sync::Arc,
    time::Instant,
};

use bellframe::{Bell, Block, Mask, Row, RowBuf, Stage, Stroke};
use itertools::Itertools;

use crate::{
    music::{Breakdown, StrokeSet},
    utils::{
        group::{PartHeadGroup, PhRotation},
        Boundary, Counts, FrontierItem,
    },
    CallIdx, CallVec, Config, Method, MethodIdx, MethodVec, Query, SpliceStyle,
};

use super::{
    falseness::FalsenessTable, Chunk, ChunkId, Graph, Link, LinkSet, LinkSide, PerPartLength,
    RowIdx, TotalLength,
};

/// The different ways that graph building can fail
#[derive(Debug)]
pub enum BuildError {
    /// The given maximum graph size limit was reached
    SizeLimit(usize),
    /// Different start/end rows were specified in a multi-part
    DifferentStartEndRowInMultipart,
    /// The same [`Chunk`] could start at two different strokes, and some
    /// [`MusicType`](crate::music::MusicType) relies on that
    InconsistentStroke,
    /// Some [`Call`](crate::Call) doesn't have enough calling positions to cover the [`Stage`]
    WrongCallingPositionsLength {
        call_name: String,
        calling_position_len: usize,
        stage: Stage,
    },

    /// Some [`Call`](crate::Call) refers to a lead location that doesn't exist
    UndefinedLeadLocation { call_name: String, label: String },
    /// [`Query`] didn't define any [`Method`]s
    NoMethods,
    /// Two [`Method`]s use the same shorthand
    DuplicateShorthand {
        shorthand: String,
        title1: String,
        title2: String,
    },
    NoCourseHeadInPart {
        mask_in_first_part: Mask,
        part_head: RowBuf,
        mask_in_other_part: Mask,
    },
}

/// Top-level function go build an unoptimised [`Graph`] for a given [`Query`].
pub(super) fn build(query: &Query, config: &Config) -> Result<Graph, BuildError> {
    log::debug!("Building unoptimised graph:");
    let graph_build_start = Instant::now();

    check_query(query)?;

    let fixed_bells = fixed_bells(query);
    let method_datas = query
        .methods
        .iter()
        .map(|m| MethodData::new(m, &fixed_bells))
        .collect::<MethodVec<_>>();

    // Generate chunk layout
    let start = Instant::now();
    let (mut chunk_equiv_map, chunk_lengths, links) =
        generate_chunk_lengths(query, &method_datas, config)?;
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
    if !query.allow_false {
        compute_falseness(&mut chunks, &mut chunk_equiv_map, query, &method_datas);
    }

    // Count music
    let start = Instant::now();
    let relies_on_stroke = query
        .music_types
        .iter()
        .any(|ty| ty.strokes != StrokeSet::Both);
    let start_strokes = get_start_strokes(&chunks, &links, query);
    if start_strokes.is_none() && relies_on_stroke {
        return Err(BuildError::InconsistentStroke);
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
        links.map.len(),
    );

    // Compute start/end links
    use LinkSide::*;
    let mut starts = Vec::new();
    let mut ends = Vec::new();
    for (link_id, link) in &links.map {
        match (&link.from, &link.to) {
            (StartOrEnd, Chunk(chunk_id)) => starts.push((*link_id, chunk_id.clone())),
            (Chunk(chunk_id), StartOrEnd) => ends.push((*link_id, chunk_id.clone())),
            (Chunk(_), Chunk(_)) => {} // Internal link
            (StartOrEnd, StartOrEnd) => unreachable!("0-length start->end link found"),
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

/// Check a [`Query`] for obvious errors before starting to build the [`Graph`]
fn check_query(query: &Query) -> Result<(), BuildError> {
    // Different start/end rows aren't well defined for multi-parts
    if query.is_multipart() && query.start_row != query.end_row {
        return Err(BuildError::DifferentStartEndRowInMultipart);
    }

    // Two methods using the same shorthand
    for (i1, m1) in query.methods.iter_enumerated() {
        for m2 in &query.methods[..i1] {
            if m1.shorthand == m2.shorthand {
                return Err(BuildError::DuplicateShorthand {
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
            return Err(BuildError::WrongCallingPositionsLength {
                call_name: call.debug_symbol.clone(),
                calling_position_len: call.calling_positions.len(),
                stage: query.stage,
            });
        }
    }

    // Calls referring to non-existent lead locations
    let mut used_lead_locations = HashSet::<&String>::new();
    for m in &query.methods {
        for annot in m.plain_course().annots() {
            used_lead_locations.extend(annot.labels);
        }
    }
    for call in &query.calls {
        for lead_label in [&call.lead_location_from, &call.lead_location_to] {
            if !used_lead_locations.contains(lead_label) {
                return Err(BuildError::UndefinedLeadLocation {
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
                    return Err(BuildError::NoCourseHeadInPart {
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
    let total_length = per_part_length.0 * query.num_parts();
    Chunk {
        label: String::new(),

        per_part_length,
        total_length: TotalLength(total_length),
        // All the length goes to the method rung in this chunk
        method_counts: Counts::single_count(total_length, id.method.index(), query.methods.len()),

        // Filled in separate graph build passes
        predecessors: Vec::new(),
        successors: Vec::new(),
        false_chunks: Vec::new(),
        music: Breakdown::zero(0),
        duffer: false,

        // Used by optimisation passes
        lb_distance_from_non_duffer: 0,
        lb_distance_to_non_duffer: 0,
        required: false,
        lb_distance_from_rounds: 0,
        lb_distance_to_rounds: 0,
    }
}

/// Set the falseness links for some [`Chunk`]s.
fn compute_falseness(
    chunks: &mut HashMap<ChunkId, Chunk>,
    chunk_equiv_map: &mut ChunkEquivalenceMap,
    query: &Query,
    method_datas: &index_vec::IndexVec<MethodIdx, MethodData>,
) {
    let start = Instant::now();
    let chunk_ids_and_lengths = chunks
        .iter()
        .map(|(id, chunk)| (id.clone(), chunk.per_part_length))
        .collect::<HashSet<_>>();
    let falseness_table = FalsenessTable::new(&chunk_ids_and_lengths, query, method_datas);
    log::debug!("  Falseness table built in {:.2?}", start.elapsed());

    let start = Instant::now();
    chunks.retain(|id, chunk| {
        falseness_table
            .set_falseness_links(
                id,
                chunk.per_part_length,
                &mut chunk.false_chunks,
                chunk_equiv_map,
                &chunk_ids_and_lengths,
            )
            .is_true() // Remove any chunks which are self-false
    });
    log::debug!("  Falseness links set in {:.2?}", start.elapsed());
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
    for link in links.map.values() {
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
                    let stroke_after_chunk = new_stroke.offset(chunk.per_part_length.0);
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
    method_datas: &index_vec::IndexVec<MethodIdx, MethodData>,
    start_strokes: &Option<HashMap<ChunkId, Stroke>>,
    query: &Query,
) {
    // Always set music to `0`s, even if the chunk is unreachable.  If we don't, then an
    // optimisation pass could see this chunk and run `zip_eq` on the `MusicType`s, thus causing a
    // panic.
    chunk.music = Breakdown::zero(query.music_types.len());

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
        let row_iter = (0..chunk.per_part_length.0).map(|offset| {
            let index = (id.sub_lead_idx + offset) % plain_course.len();
            plain_course.get_row(index).unwrap()
        });
        // Count weight from music
        chunk.music += &Breakdown::from_rows(
            row_iter,
            &lead_head_in_part,
            query.music_types.as_raw_slice(),
            start_stroke,
        );
        // Count weight from CH masks
        for (mask, weight) in &query.ch_weights {
            if mask.matches(&lead_head_in_part) {
                // Weight applies to each row
                chunk.music.score += *weight * chunk.per_part_length.0 as f32;
            }
        }
    }
}

//////////////////
// CHUNK LAYOUT //
//////////////////

fn generate_chunk_lengths<'q>(
    query: &'q Query,
    method_datas: &MethodVec<MethodData>,
    config: &Config,
) -> Result<
    (
        ChunkEquivalenceMap<'q>,
        HashMap<ChunkId, PerPartLength>,
        LinkSet,
    ),
    BuildError,
> {
    let mut chunk_lengths = HashMap::new();
    let mut links = LinkSet::new();

    let mut frontier = BinaryHeap::<Reverse<FrontierItem<ChunkId>>>::new();
    let mut chunk_equiv_map = ChunkEquivalenceMap::new(&query.part_head_group);
    let chunk_factory = ChunkFactory::new(query, method_datas);

    // Populate the frontier with start chunks, and add start links to `links`
    for start_id in find_locations_of_row(&query.start_row, method_datas, Boundary::Start) {
        let (start_id, ph_rotation) = chunk_equiv_map.normalise(&start_id);
        links.add(Link {
            from: LinkSide::StartOrEnd,
            to: LinkSide::Chunk(start_id.clone()),
            call: None,
            ph_rotation,
            ph_rotation_back: !ph_rotation,
        });
        frontier.push(Reverse(FrontierItem::new(start_id, 0)));
    }

    // Repeatedly expand `ChunkId`s in increasing order of distance (i.e. we're exploring the graph
    // using Dijkstra's algorithm).
    while let Some(Reverse(frontier_item)) = frontier.pop() {
        let FrontierItem {
            item: chunk_id,
            distance: min_distance_from_start,
        } = frontier_item;

        if chunk_lengths.contains_key(&chunk_id) {
            continue; // Don't expand the same chunk multiple times
        }

        if chunk_lengths.len() > config.graph_size_limit {
            return Err(BuildError::SizeLimit(config.graph_size_limit));
        }

        // Build the chunk
        let (per_part_length, successors) = chunk_factory.build_chunk(chunk_id.clone());
        let min_distance_after_chunk =
            min_distance_from_start + per_part_length.0 * query.num_parts();

        // Stop expanding if the shortest path from rounds to the end of the chunk takes longer
        // than the max comp length
        if min_distance_after_chunk >= query.len_range.end {
            continue;
        }

        chunk_lengths.insert(chunk_id.clone(), per_part_length);
        // Create the successor links and add the corresponding `ChunkId`s to the frontier
        let mut links_so_far = HashSet::<(LinkSide<ChunkId>, PhRotation)>::new();
        for (id_to, call, is_end) in successors {
            // Add link to the graph
            let (id_to, ph_rotation) = chunk_equiv_map.normalise(&id_to);
            let link_side_to = match is_end {
                true => LinkSide::StartOrEnd,
                false => LinkSide::Chunk(id_to.clone()),
            };
            // Only store one link between every pair of `LinkSide`s
            // TODO: Always preserve the links with the *highest* score
            if !links_so_far.insert((link_side_to.clone(), ph_rotation)) {
                continue; // Skip any link which already exists
            }

            // Add the link
            links.add(Link {
                from: LinkSide::Chunk(chunk_id.clone()),
                to: link_side_to,
                call,
                ph_rotation,
                ph_rotation_back: !ph_rotation,
            });
            // If this isn't an end, add the new chunk to the frontier so it becomes part of the
            // graph
            if !is_end {
                frontier.push(Reverse(FrontierItem::new(id_to, min_distance_after_chunk)));
            }
        }
    }

    Ok((chunk_equiv_map, chunk_lengths, links))
}

/// Persistent state for building [`Chunk`]s
struct ChunkFactory<'a> {
    method_datas: &'a MethodVec<MethodData<'a>>,
    end_lookup_table: EndLookupTable,
    link_lookup_table: LinkLookupTable,
}

impl<'a> ChunkFactory<'a> {
    fn new(query: &'a Query, method_datas: &'a MethodVec<MethodData<'a>>) -> Self {
        Self {
            method_datas,
            end_lookup_table: EndLookupTable::new(query, method_datas),
            link_lookup_table: LinkLookupTable::new(query, method_datas),
        }
    }

    /// Given a [`ChunkId`] referring to the first row of a [`Chunk`], determine the length of that
    /// [`Chunk`] and initialise it.  This returns the new [`Chunk`] along with a list describing
    /// the successor [`Link`]s of that [`Chunk`].
    fn build_chunk(
        &self,
        chunk_id: ChunkId,
    ) -> (
        PerPartLength,
        Vec<(ChunkIdInFirstPart, Option<CallIdx>, bool)>,
    ) {
        let method_idx = chunk_id.method;
        let method_data = &self.method_datas[method_idx];
        let course_len = method_data.plain_course.len();

        // Determine the length of the `Chunk` by continually attempting to shorten it with calls
        // or ends
        let mut shortest_per_part_length = course_len;
        let mut links_at_shortest_length = Vec::new();
        // Closure to add a new link, shortening the chunk if needed
        let mut add_link = |per_part_length: usize,
                            id: ChunkIdInFirstPart,
                            call: Option<CallIdx>,
                            is_end: bool| {
            // If this link is strictly further away than some other link, then it'll never be
            // reached
            if per_part_length > shortest_per_part_length {
                return;
            }
            // If this new link makes the chunk strictly shorter, then all previous links
            // become irrelevant
            if per_part_length < shortest_per_part_length {
                shortest_per_part_length = per_part_length;
                links_at_shortest_length.clear();
            }
            // Now that the chunk is exactly the same length as this link, we can add it as an end
            links_at_shortest_length.push((id, call, is_end));
        };

        // Add links for ends and calls
        self.end_lookup_table
            .add_links(&chunk_id, self.method_datas, &mut add_link);
        self.link_lookup_table
            .add_links(&chunk_id, self.method_datas, &mut add_link);
        // Take the final length/links as those that will be used for the chunk
        assert!(shortest_per_part_length > 0);
        let chunk_length = PerPartLength(shortest_per_part_length);
        let mut links = links_at_shortest_length;
        // Replace any links which go directly to a part-head with an end (for example calling a
        // home in coursing order `54326` will cause the composition to instantly come round rather
        // than leading to `ChunkId(12345678:#,0)` where `#` is any method)
        for (id_to, _call, is_end) in &mut links {
            *is_end |= self.end_lookup_table.is_end(id_to);
        }

        (chunk_length, links)
    }
}

/////////////
// LOOKUPS //
/////////////

/// Lookup table for determining if a [`ChunkId`] contains `query.end_row`.
struct EndLookupTable {
    /// For each lead head in each [`Method`], how many rows away is the nearest instance of
    /// `query.end_row` (and the [`ChunkId`] referring to that end's location).
    end_lookup: HashMap<(RowBuf, MethodIdx), Vec<(PerPartLength, ChunkIdInFirstPart)>>,
}

impl EndLookupTable {
    fn new(query: &Query, method_datas: &MethodVec<MethodData>) -> Self {
        // Create lookup table for ends
        let mut end_lookup = HashMap::new();
        for part_head in query.part_head_group.rows() {
            let end_row = part_head * &query.end_row;
            for end_location in find_locations_of_row(&end_row, method_datas, Boundary::End) {
                let method_idx = end_location.method;
                let method = &method_datas[method_idx].method;
                // For each end location, any other lead in the same course can also contain an end
                // at this location.  In each iteration `leads_to_ring` stores the number of leads
                // required to go from `start_row` to the lead containing the current `end_loc`.
                for (leads_to_ring, plain_lead_head) in
                    method.lead_head().closure().into_iter().rev().enumerate()
                {
                    let lead_head = &end_location.lead_head * &plain_lead_head;
                    let distance_to_end =
                        leads_to_ring * method.lead_len() + end_location.sub_lead_idx;
                    end_lookup
                        .entry((lead_head, method_idx))
                        .or_insert_with(Vec::new)
                        .push((PerPartLength(distance_to_end), end_location.to_owned()));
                }
            }
        }
        Self { end_lookup }
    }

    fn is_end(&self, chunk_id: &ChunkIdInFirstPart) -> bool {
        match self
            .end_lookup
            .get(&(chunk_id.lead_head.clone(), chunk_id.row_idx.method))
        {
            Some(entries) => entries
                .iter()
                .any(|(len, _id)| len.0 == chunk_id.sub_lead_idx),
            None => false,
        }
    }

    fn add_links(
        &self,
        chunk_id: &ChunkId,
        method_datas: &MethodVec<MethodData>,
        add_link: &mut impl FnMut(usize, ChunkIdInFirstPart, Option<CallIdx>, bool),
    ) {
        let method_idx = chunk_id.method;
        let method_data = &method_datas[method_idx];
        let course_len = method_data.plain_course.len();
        // Attempt to shorten the chunk by ending the composition
        for (end_length_from_lead_head, end_id) in self
            .end_lookup
            .get(&(chunk_id.lead_head.deref().to_owned(), method_idx))
            .unwrap_or(&Vec::new())
        {
            // We have to take the result modulo `course_len` in case the end happens in the same
            // lead but before this chunk starts.  We also add `course_len` before subtracting to
            // avoid integer underflow.
            let mut length =
                (end_length_from_lead_head.0 + course_len - chunk_id.sub_lead_idx) % course_len;
            // 0-length ends (e.g. calling a `H` at the end of a composition to make it instantly
            // come round) are checked when links are being generated.  Therefore, if we _do_
            // generate a chunk that end immediately, it must be also be a start and we should
            // ring the entire course
            if length == 0 {
                length += course_len;
            }
            add_link(length, end_id.to_owned(), None, true);
        }
    }
}

/// Lookup table for where links can be placed
#[derive(Debug)]
struct LinkLookupTable {
    /// For each [`Method`] and each given lead head [`Mask`], where's the nearest place that links
    /// can be placed.  The [`LinkLookupEntry`]s have the property that, for every position within
    /// that lead, the first available set of links is in the [`Vec`] of [`LinkLookupEntry`]s.
    #[allow(clippy::type_complexity)]
    link_lookup: MethodVec<Vec<(Mask, Vec<(usize, Vec<LinkLookupEntry>)>)>>,
}

#[derive(Debug)]
struct LinkLookupEntry {
    call: Option<CallIdx>,
    lead_head_transposition: RowBuf,
    row_idx_to: RowIdx,
}

impl LinkLookupTable {
    fn new(query: &Query, method_datas: &MethodVec<MethodData>) -> Self {
        // Treat non-spliced comps as having `SpliceStyle::Calls`
        let splice_style = if query.is_spliced() {
            query.splice_style
        } else {
            SpliceStyle::Calls
        };

        // Maps lead labels to a list of:
        // ```
        // (
        //     RowIdx which that link leads to,
        //     post-transposition from that row to the corresponding lead head,
        // )
        // ```
        let mut link_ends_by_label = HashMap::<&str, Vec<(RowIdx, RowBuf)>>::new();
        for (method_idx, method_data) in method_datas.iter_enumerated() {
            for (sub_lead_idx, annot_row) in
                method_data.method.first_lead().annot_rows().enumerate()
            {
                for label in annot_row.annot() {
                    link_ends_by_label
                        .entry(label)
                        .or_default()
                        .push((RowIdx::new(method_idx, sub_lead_idx), annot_row.row().inv()));
                }
            }
        }

        // Create lookup table for links
        let mut link_lookup = MethodVec::new();
        for (method_idx, method_data) in method_datas.iter_enumerated() {
            let lead_len = method_data.method.lead_len();

            // `link_positions[pos]` is exactly the links which can be placed `pos` rows after a
            // lead head (for every `pos` within the plain course).
            let mut link_positions = HashMap::<Mask, HashMap<usize, Vec<LinkLookupEntry>>>::new();

            // for every labelled row in the plain course ...
            for (mut dist_from_lead_head, annot_row) in
                method_data.plain_course.annot_rows().enumerate()
            {
                // There's no sense in having a link at the 0th row (which would potentially
                // produce a 0-length chunk), so we move those rows to the end of the course
                if dist_from_lead_head == 0 {
                    dist_from_lead_head = method_data.plain_course.len();
                }
                for label in annot_row.annot().labels {
                    // Add links for calls
                    //
                    // ... for every call that can be placed there ...
                    for (call_idx, call) in query.calls.iter_enumerated() {
                        if &call.lead_location_from == label {
                            let row_before_call = method_data
                                .plain_course
                                .get_row(dist_from_lead_head - 1)
                                .unwrap();
                            let row_after_call = row_before_call * call.place_not.transposition();

                            create_links(
                                dist_from_lead_head,
                                Some(call_idx),
                                &row_after_call,
                                &call.lead_location_to,
                                method_data,
                                &link_ends_by_label,
                                method_datas,
                                &mut link_positions,
                            );
                        }
                    }

                    // Add links for plain splices (at every possible position)
                    if splice_style == SpliceStyle::LeadLabels {
                        let row_after_plain = method_data
                            .plain_course
                            .get_row(dist_from_lead_head)
                            .unwrap();
                        // Add plain links from every instance of a lead location to every other
                        // instance of that lead location
                        create_links(
                            dist_from_lead_head,
                            None,
                            row_after_plain,
                            label,
                            method_data,
                            &link_ends_by_label,
                            method_datas,
                            &mut link_positions,
                        );
                    }
                }
            }

            // Add links for plain leads
            if splice_style == SpliceStyle::Calls {
                // Add non-splice plain links to every position where there is already a call
                for link_positions in link_positions.values_mut() {
                    for (dist_from_lead_head, links) in link_positions {
                        assert!(!links.is_empty());
                        let num_leads = dist_from_lead_head / lead_len;
                        let sub_lead_idx = dist_from_lead_head % lead_len;
                        links.push(LinkLookupEntry {
                            call: None,
                            // The LH transposition just represents the number of leads we've
                            // rung (this always links to the same method, so the sub-lead
                            // transpositions cancel out)
                            lead_head_transposition: method_data
                                .method
                                .lead_head()
                                .pow_u(num_leads),
                            // Only add plain links to the current method
                            row_idx_to: RowIdx::new(method_idx, sub_lead_idx),
                        });
                    }
                }
            }

            // Turn `HashMap< Mask, HashMap<(usize, Vec<CallLookupEntry>)> >`
            // then `Vec    <(Mask, Vec    <(usize, Vec<CallLookupEntry>)>)>`
            link_lookup.push(
                link_positions
                    .into_iter()
                    .map(|(mask, link_positions)| {
                        // Since we're only accessing `links` from **within** lead who's head
                        // satisfies `mask`, we know we're not going to take any link after the
                        // first link after the lead finishes.  For example, if the leads are 32
                        // rows long and we have `links` like:
                        // ```
                        // links = [
                        //     ( 16, [...]), <- Could be accessed within the lead
                        //     ( 48, [...]), <- First links after lead, can still be taken
                        //     ( 80, [...]), <- Can't be taken because the links at 48 will always
                        //                      be chosen instead
                        //     (112, [...]), <- also can't be chosen
                        //        ...
                        // ]
                        // ```
                        // Note that this is simply an optimisation; it makes the tables smaller
                        // and faster but doesn't actually change the results
                        let mut sorted_link_positions = link_positions
                            .into_iter()
                            .sorted_by_key(|(len, _links)| *len)
                            .collect_vec();
                        let idx_of_lead_end = sorted_link_positions
                            .binary_search_by_key(&lead_len, |(len, _links)| *len)
                            .map_or_else(identity, identity);
                        sorted_link_positions.drain(idx_of_lead_end + 1..);

                        (mask, sorted_link_positions)
                    })
                    .collect_vec(),
            );
        }

        Self { link_lookup }
    }

    fn add_links(
        &self,
        chunk_id: &ChunkId,
        method_datas: &MethodVec<MethodData>,
        add_link: &mut impl FnMut(usize, ChunkIdInFirstPart, Option<CallIdx>, bool),
    ) {
        for (mask, link_positions) in &self.link_lookup[chunk_id.method] {
            if mask.matches(&chunk_id.lead_head) {
                // Binary search returns the index of the smallest value with `len > sub_lead_idx`
                let mut next_links_index = link_positions
                    .binary_search_by_key(&(chunk_id.sub_lead_idx + 1), |(len, _links)| *len)
                    .map_or_else(identity, identity);
                // If there are **no** possible links after this position in the lead, then wrap
                // round the end of the course and take the first links in this lead anyway
                if next_links_index >= link_positions.len() {
                    next_links_index = 0;
                }
                if let Some((len_from_lead_head, link_entries)) =
                    link_positions.get(next_links_index)
                {
                    let mut len = *len_from_lead_head - chunk_id.sub_lead_idx;
                    if len == 0 {
                        len = method_datas[chunk_id.method].plain_course.len();
                    }
                    for link_entry in link_entries {
                        let next_chunk_id = ChunkIdInFirstPart {
                            lead_head: chunk_id.lead_head.as_ref()
                                * &link_entry.lead_head_transposition,
                            row_idx: link_entry.row_idx_to,
                        };
                        add_link(len, next_chunk_id, link_entry.call, false);
                    }
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn create_links(
    dist_from_lead_head: usize,
    call: Option<CallIdx>,
    row_after_link: &Row,
    label_to: &str,
    method_data_from: &MethodData,

    link_ends_by_label: &HashMap<&str, Vec<(RowIdx, RowBuf)>>,
    method_datas: &index_vec::IndexVec<MethodIdx, MethodData>,
    link_lookup_for_method: &mut HashMap<Mask, HashMap<usize, Vec<LinkLookupEntry>>>,
) {
    let link_ends = link_ends_by_label
        .get(label_to)
        .expect("Undefined lead locations should be checked before graph expansion");
    // ... for every place within a lead this could go to ...
    for (row_idx_to, transposition_to_lead_head) in link_ends {
        // post-transposition
        //    from: the head of the lead the link is coming _from_
        //    to  : the head of the lead the link is coming _to_
        let lead_head_transposition = row_after_link * transposition_to_lead_head;

        // ... for every lead head mask of the method we're going to ...
        for lead_head_mask_to in &method_datas[row_idx_to.method].lead_head_masks {
            //     `lh_from * lh_transposition` satisfies `lh_mask_to`
            // iff `lh_from` satisfies `lh_mask_to * lh_transposition.inv()`
            let lead_head_mask_from = lead_head_mask_to * lead_head_transposition.inv();
            // Check if `lead_head_mask_from` can actually be reached (i.e. is there some LH mask
            // which is compatible with it?).  This doesn't change the results, but has a massive
            // performance benefit since chunk expansion is linear in the size of
            // `link_lookup_for_method`.  For example, this simple pruning causes a ~4x speedup for
            // tenors-together comps.
            let is_mask_reachable = method_data_from
                .lead_head_masks
                .iter()
                .any(|lh_mask| lh_mask.is_compatible_with(&lead_head_mask_from));
            if !is_mask_reachable {
                continue;
            }
            // Add mapping from `(lead_head_mask_from, dist_from_lead_head)` to `link_lookup`
            link_lookup_for_method
                .entry(lead_head_mask_from)
                .or_insert_with(HashMap::new)
                .entry(dist_from_lead_head)
                .or_insert_with(Vec::new)
                .push(LinkLookupEntry {
                    call,
                    lead_head_transposition: lead_head_transposition.clone(),
                    row_idx_to: *row_idx_to,
                });
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
pub(super) struct ChunkIdInFirstPart {
    pub(super) lead_head: RowBuf,
    pub(super) row_idx: RowIdx,
}

impl Deref for ChunkIdInFirstPart {
    type Target = RowIdx;

    fn deref(&self) -> &Self::Target {
        &self.row_idx
    }
}

#[derive(Debug)]
pub(super) struct ChunkEquivalenceMap<'query> {
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

    pub(super) fn normalise(&mut self, id: &ChunkIdInFirstPart) -> (ChunkId, PhRotation) {
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
pub(super) struct MethodData<'source> {
    pub(super) method: &'source Method,
    pub(super) plain_course: Block<bellframe::method::RowAnnot<'source>>,
    pub(super) lead_head_masks: Vec<Mask>,
}

impl<'source> MethodData<'source> {
    fn new(method: &'source Method, fixed_bells: &[(Bell, usize)]) -> Self {
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

        Self {
            method,
            plain_course: method.plain_course(),
            lead_head_masks: filtered_lead_head_masks,
        }
    }

    /// Checks if `row` is a valid lead head in this method (according to the CH masks provided).
    fn is_lead_head(&self, lead_head: &Row) -> bool {
        self.lead_head_masks.iter().any(|m| m.matches(lead_head))
    }
}

/// Finds all the possible locations of a given [`Row`] within the course head masks for each
/// [`Method`].
fn find_locations_of_row(
    row: &Row,
    method_datas: &MethodVec<MethodData>,
    boundary: Boundary,
) -> Vec<ChunkIdInFirstPart> {
    // Generate the method starts
    let mut starts = Vec::new();
    for (method_idx, method_data) in method_datas.iter_enumerated() {
        let m = &method_data.method;
        let allowed_indices = match boundary {
            Boundary::Start => m.start_indices.clone(),
            Boundary::End => match &m.end_indices {
                Some(idxs) => idxs.clone(),
                // If `end_indices` isn't specified, then every index within the lead is possible
                None => (0..m.lead_len() as isize).collect_vec(),
            },
        };
        for sub_lead_idx in allowed_indices {
            let lead_len_i = m.lead_len() as isize;
            let sub_lead_idx = (sub_lead_idx % lead_len_i + lead_len_i) as usize % m.lead_len();
            let lead_head = Row::solve_xa_equals_b(m.row_in_plain_lead(sub_lead_idx), row).unwrap();
            // This start is valid if it matches at least one of this method's lead head masks
            if method_data.is_lead_head(&lead_head) {
                starts.push(ChunkIdInFirstPart {
                    lead_head,
                    row_idx: RowIdx::new(method_idx, sub_lead_idx),
                });
            }
        }
    }
    starts
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
fn fixed_bells_of_method(method: &crate::Method, calls: &CallVec<crate::Call>) -> HashSet<Bell> {
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
    call: &crate::Call,
    set: &mut HashSet<Bell>,
) {
    // Note that all calls are required to only substitute one piece of place notation.
    for sub_lead_idx_after_call in method.label_indices(&call.lead_location_from) {
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

////////////////////
// ERROR MESSAGES //
////////////////////

impl Display for BuildError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::SizeLimit(limit) => write!(
                f,
                "Graph size limit of {} chunks reached.  You can set it \
higher with `--graph-size-limit <n>`.",
                limit
            ),
            BuildError::DifferentStartEndRowInMultipart => {
                write!(f, "Start/end rows must be the same for multipart comps")
            }
            BuildError::InconsistentStroke => write!(
                f,
                "The same chunk of ringing can be at multiple strokes, probably \
because you're using a method with odd-length leads"
            ),
            BuildError::NoMethods => write!(f, "Can't have a composition with no methods"),
            BuildError::WrongCallingPositionsLength {
                call_name,
                calling_position_len,
                stage,
            } => write!(
                f,
                "Call {:?} only specifies {} calling positions, but the stage has {} bells",
                call_name,
                calling_position_len,
                stage.num_bells()
            ),
            BuildError::DuplicateShorthand {
                shorthand,
                title1,
                title2,
            } => write!(
                f,
                "Methods {:?} and {:?} share a shorthand ({})",
                title1, title2, shorthand
            ),
            BuildError::UndefinedLeadLocation { call_name, label } => write!(
                f,
                "Call {:?} refers to a lead location {:?}, which doesn't exist",
                call_name, label
            ), // TODO: Suggest one that does exist
            BuildError::NoCourseHeadInPart {
                mask_in_first_part,
                part_head,
                mask_in_other_part,
            } => {
                writeln!(
                    f,
                    "course head `{}` becomes `{}` in the part starting `{}`, which isn't in `course_heads`.",
                    mask_in_first_part, mask_in_other_part, part_head
                )?;
                write!(
                    f,
                    "   help: consider adding `{}` to `course_heads`",
                    mask_in_other_part
                )
            }
        }
    }
}

impl std::error::Error for BuildError {}
