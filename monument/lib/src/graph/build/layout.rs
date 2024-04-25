use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
    ops::Deref,
};

use bellframe::{Mask, Row, RowBuf};
use itertools::Itertools;

use crate::{
    graph::{CallSeqIdx, ChunkId, Link, LinkSet, LinkSide, RowIdx},
    group::PhRotation,
    parameters::{CallIdx, MethodIdx, MethodVec, Parameters, SpliceStyle},
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary, FrontierItem,
    },
    Config,
};

use super::{ChunkEquivalenceMap, ChunkIdInFirstPart};

/// Compute the layout (ids, lengths and connections) of the graph representing a [`Parameters`].
///
/// This is the only item exported by this module; all the other code should be considered
/// implementation detail of this function.
pub(super) fn chunk_lengths<'q>(
    params: &'q Parameters,
    config: &Config,
) -> crate::Result<(
    ChunkEquivalenceMap<'q>,
    HashMap<ChunkId, PerPartLength>,
    LinkSet,
)> {
    // NOTE: Here we're always tracking the `CallSeqIdx`, even if the user hasn't provided a custom
    // calling.  If the user hasn't given a calling, the `CallSeqIdx` will always be 0.

    // Values to output
    let mut chunk_lengths = HashMap::new();
    let mut links = LinkSet::new();
    let mut chunk_equiv_map = ChunkEquivalenceMap::new(&params.part_head_group);

    // Working variables
    let call_sequence = params.parsed_call_string()?;
    let chunk_factory = ChunkFactory::new(params);
    let mut chunks_expanded = HashSet::<(ChunkId, CallSeqIdx)>::new();
    let mut links_generated = HashSet::<(LinkSide<ChunkId>, LinkSide<ChunkId>, PhRotation)>::new();
    let mut frontier =
        BinaryHeap::<Reverse<FrontierItem<(ChunkId, CallSeqIdx), TotalLength>>>::new();

    // Populate the frontier with start chunks, and add start links to `links`
    for start_id in boundary_locations(&params.start_row, Boundary::Start, params) {
        let (start_id, ph_rotation) = chunk_equiv_map.normalise(&start_id);
        links.add(Link {
            call: None,
            from: LinkSide::StartOrEnd,
            to: LinkSide::Chunk(start_id.clone()),
            ph_rotation,
            ph_rotation_back: !ph_rotation,
            sequence_idx: None,
        });
        frontier.push(Reverse(FrontierItem::new(
            (start_id, CallSeqIdx::new(0)),
            TotalLength::ZERO,
        )));
    }

    // Repeatedly expand `ChunkId`s in increasing order of distance (i.e. we're exploring the graph
    // using Dijkstra's algorithm).
    while let Some(Reverse(frontier_item)) = frontier.pop() {
        let FrontierItem {
            item: (chunk_id, call_sequence_idx),
            distance: min_distance_from_start,
        } = frontier_item;

        if chunks_expanded.contains(&(chunk_id.clone(), call_sequence_idx)) {
            continue; // Don't expand the same chunk multiple times
        }

        if chunk_lengths.len() > config.graph_size_limit {
            return Err(crate::Error::SizeLimit(config.graph_size_limit));
        }

        // Compute long this chunk is, and what its successors are
        let (per_part_length, successors) = chunk_factory.build_chunk(chunk_id.clone(), params);
        let min_distance_after_chunk =
            min_distance_from_start + per_part_length.as_total(&params.part_head_group);

        // Stop expanding if the shortest path from rounds to the end of the chunk takes longer
        // than the max comp length
        if min_distance_after_chunk > params.max_length() {
            continue;
        }

        // Actually add the chunk to the graph
        chunk_lengths.insert(chunk_id.clone(), per_part_length);
        chunks_expanded.insert((chunk_id.clone(), call_sequence_idx));

        // Create the successor links and add the corresponding `ChunkId`s to the frontier
        let mut links_from_this_chunk = HashSet::<(LinkSide<ChunkId>, PhRotation)>::new();
        for (unnormalised_id_to, call, is_end) in successors {
            // Determine where this link leads
            let (id_to, ph_rotation) = chunk_equiv_map.normalise(&unnormalised_id_to);
            let link_side_to = match is_end {
                true => LinkSide::StartOrEnd,
                false => LinkSide::Chunk(id_to.clone()),
            };
            // Only store one link between every pair of `LinkSide`s
            // TODO: Always preserve the links with the *highest* score
            if !links_from_this_chunk.insert((link_side_to.clone(), ph_rotation)) {
                continue; // An equivalent link has already been added
            }

            // If user forces a given call sequence, remove any links which don't correspond to the
            // correct next call.
            // TODO: Handle compositions which visit courses multiple times
            let mut link_sequence_idx = None;
            let mut next_call_sequence_idx = call_sequence_idx;
            if let Some(sequence) = &call_sequence {
                if let Some(call) = call {
                    let Some(&(expected_next_call, expected_place)) =
                        sequence.get(call_sequence_idx)
                    else {
                        continue; // Skip a call which is past the end of the composition
                    };

                    // Skip use of the wrong call
                    if expected_next_call != call {
                        continue;
                    }

                    // Skip use of the correct call but at the wrong calling position
                    let calling_bell_place =
                        unnormalised_id_to.lead_head.place_of(params.calling_bell);
                    if calling_bell_place != expected_place {
                        continue;
                    }

                    // If we haven't rejected it, allow the call
                    link_sequence_idx = Some(call_sequence_idx);
                    next_call_sequence_idx += 1;
                }
            }

            // Add the link, if it hasn't been added already
            let link_side_from = LinkSide::Chunk(chunk_id.clone());
            let is_link_new =
                links_generated.insert((link_side_from.clone(), link_side_to.clone(), ph_rotation));
            if is_link_new {
                links.add(Link {
                    call,
                    from: link_side_from,
                    to: link_side_to,
                    ph_rotation,
                    ph_rotation_back: !ph_rotation,
                    sequence_idx: link_sequence_idx,
                });
            }
            // If this isn't an end, add the new chunk to the frontier so it becomes part of the
            // graph
            if !is_end {
                frontier.push(Reverse(FrontierItem::new(
                    (id_to, next_call_sequence_idx),
                    min_distance_after_chunk,
                )));
            }
        }
    }

    Ok((chunk_equiv_map, chunk_lengths, links))
}

/// Persistent state for building [`Chunk`]s
struct ChunkFactory {
    end_lookup_table: EndLookupTable,
    link_lookup_table: LinkLookupTable,
}

impl ChunkFactory {
    fn new(params: &Parameters) -> Self {
        Self {
            end_lookup_table: EndLookupTable::new(params),
            link_lookup_table: LinkLookupTable::new(params),
        }
    }

    /// Given a [`ChunkId`] referring to the first row of a [`Chunk`], determine the length of that
    /// [`Chunk`] and initialise it.  This returns the new [`Chunk`] along with a list describing
    /// the successor [`Link`]s of that [`Chunk`].
    fn build_chunk(
        &self,
        chunk_id: ChunkId,
        params: &Parameters,
    ) -> (
        PerPartLength,
        Vec<(ChunkIdInFirstPart, Option<CallIdx>, bool)>,
    ) {
        let course_len = params.methods[chunk_id.method].course_len();
        // Determine the length of the `Chunk` by continually attempting to shorten it with calls
        // or ends
        let mut shortest_len = PerPartLength::new(course_len);
        let mut links_at_shortest_len = Vec::new();
        // Closure to add a new link, shortening the chunk if needed
        let mut add_link =
            |len: PerPartLength, id: ChunkIdInFirstPart, call: Option<CallIdx>, is_end: bool| {
                // If this link is strictly further away than some other link, then it'll never be
                // reached
                if len > shortest_len {
                    return;
                }
                // If this new link makes the chunk strictly shorter, then all previous links
                // become irrelevant
                if len < shortest_len {
                    shortest_len = len;
                    links_at_shortest_len.clear();
                }
                // Now that the chunk is exactly the same length as this link, we can add it as an
                // end
                links_at_shortest_len.push((id, call, is_end));
            };

        // Add links for ends and calls
        self.end_lookup_table
            .add_links(&chunk_id, params, &mut add_link);
        self.link_lookup_table
            .add_links(&chunk_id, params, &mut add_link);
        // Take the final length/links as those that will be used for the chunk
        assert!(shortest_len > PerPartLength::ZERO);
        let chunk_length = shortest_len;
        let mut links = links_at_shortest_len;
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

/// Lookup table for determining if a [`ChunkId`] contains `params.end_row`.
struct EndLookupTable {
    /// For each lead head in each [`Method`], how many rows away is the nearest instance of
    /// `params.end_row` (and the [`ChunkId`] referring to that end's location).
    end_lookup: HashMap<(RowBuf, MethodIdx), Vec<(PerPartLength, ChunkIdInFirstPart)>>,
}

impl EndLookupTable {
    fn new(params: &Parameters) -> Self {
        // Create lookup table for ends
        let mut end_lookup = HashMap::new();
        for part_head in params.part_head_group.rows() {
            let end_row = part_head * &params.end_row;
            for end_location in boundary_locations(&end_row, Boundary::End, params) {
                let method_idx = end_location.method;
                let method = &params.methods[method_idx];
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
                        .push((PerPartLength::new(distance_to_end), end_location.to_owned()));
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
                .any(|(len, _id)| len.as_usize() == chunk_id.sub_lead_idx),
            None => false,
        }
    }

    fn add_links(
        &self,
        chunk_id: &ChunkId,
        params: &Parameters,
        add_link: &mut impl FnMut(PerPartLength, ChunkIdInFirstPart, Option<CallIdx>, bool),
    ) {
        let method_idx = chunk_id.method;
        let method = &params.methods[method_idx];
        let course_len = method.course_len();
        // Attempt to shorten the chunk by ending the composition
        for (end_length_from_lead_head, end_id) in self
            .end_lookup
            .get(&(chunk_id.lead_head.deref().to_owned(), method_idx))
            .unwrap_or(&Vec::new())
        {
            // We have to take the result modulo `course_len` in case the end happens in the same
            // lead but before this chunk starts.  We also add `course_len` before subtracting to
            // avoid integer underflow.
            let mut length = (end_length_from_lead_head.as_usize() + course_len
                - chunk_id.sub_lead_idx)
                % course_len;
            // 0-length ends (e.g. calling a `H` at the end of a composition to make it instantly
            // come round) are checked when links are being generated.  Therefore, if we _do_
            // generate a chunk that end immediately, it must be also be a start and we should
            // ring the entire course
            if length == 0 {
                length += course_len;
            }
            add_link(PerPartLength::new(length), end_id.to_owned(), None, true);
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
    fn new(params: &Parameters) -> Self {
        // Treat non-spliced comps as having `SpliceStyle::Calls`
        let splice_style = if params.is_spliced() {
            params.splice_style
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
        for (method_idx, method_data) in params.methods.iter_enumerated() {
            for (sub_lead_idx, (annot, row)) in method_data.first_lead().annot_rows().enumerate() {
                for label in annot {
                    link_ends_by_label
                        .entry(label)
                        .or_default()
                        .push((RowIdx::new(method_idx, sub_lead_idx), row.inv()));
                }
            }
        }

        // Create lookup for allowed lead heads
        let allowed_lead_heads: MethodVec<Vec<Mask>> = params
            .methods
            .iter()
            .map(|m| m.allowed_lead_head_masks(params))
            .collect();

        // Create lookup table for links
        let mut link_lookup = MethodVec::new();
        for (method_idx, method) in params.methods.iter_enumerated() {
            let lead_len = method.lead_len();
            let plain_course = method.plain_course();

            // `link_positions[pos]` is exactly the links which can be placed `pos` rows after a
            // lead head (for every `pos` within the plain course).
            let mut link_positions = HashMap::<Mask, HashMap<usize, Vec<LinkLookupEntry>>>::new();

            // for every labelled row in the plain course ...
            for (mut dist_from_lead_head, annot) in plain_course.annots().enumerate() {
                // There's no sense in having a link at the 0th row (which would potentially
                // produce a 0-length chunk), so we move those rows to the end of the course
                if dist_from_lead_head == 0 {
                    dist_from_lead_head = plain_course.len();
                }

                for label in annot.labels {
                    // Add links for calls
                    //
                    // ... for every call that can be placed there ...
                    for (call_idx, call) in params.calls.iter_enumerated() {
                        if &call.label_from == label {
                            let row_before_call =
                                plain_course.get_row(dist_from_lead_head - 1).unwrap();
                            let row_after_call =
                                row_before_call * call.place_notation.transposition();

                            create_links(
                                dist_from_lead_head,
                                Some(call_idx),
                                &row_after_call,
                                &call.label_to,
                                method_idx,
                                &allowed_lead_heads,
                                &link_ends_by_label,
                                &mut link_positions,
                            );
                        }
                    }

                    // Add links for plain splices (at every possible position)
                    if splice_style == SpliceStyle::LeadLabels {
                        let row_after_plain = plain_course.get_row(dist_from_lead_head).unwrap();
                        // Add plain links from every instance of a label to every other instance
                        // of that label
                        create_links(
                            dist_from_lead_head,
                            None,
                            row_after_plain,
                            label,
                            method_idx,
                            &allowed_lead_heads,
                            &link_ends_by_label,
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
                            lead_head_transposition: method.lead_head().pow_u(num_leads),
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
                            .unwrap_or_else(|x| x);
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
        params: &Parameters,
        add_link: &mut impl FnMut(PerPartLength, ChunkIdInFirstPart, Option<CallIdx>, bool),
    ) {
        for (mask, link_positions) in &self.link_lookup[chunk_id.method] {
            if mask.matches(&chunk_id.lead_head) {
                // Binary search returns the index of the smallest value with `len > sub_lead_idx`
                let mut next_links_index = link_positions
                    .binary_search_by_key(&(chunk_id.sub_lead_idx + 1), |(len, _links)| *len)
                    .unwrap_or_else(|x| x);
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
                        len = params.methods[chunk_id.method].course_len();
                    }
                    for link_entry in link_entries {
                        let next_chunk_id = ChunkIdInFirstPart {
                            lead_head: chunk_id.lead_head.as_ref()
                                * &link_entry.lead_head_transposition,
                            row_idx: link_entry.row_idx_to,
                        };
                        add_link(
                            PerPartLength::new(len),
                            next_chunk_id,
                            link_entry.call,
                            false,
                        );
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
    method_idx_from: MethodIdx,

    allowed_lead_masks: &MethodVec<Vec<Mask>>,
    link_ends_by_label: &HashMap<&str, Vec<(RowIdx, RowBuf)>>,
    link_lookup_for_method: &mut HashMap<Mask, HashMap<usize, Vec<LinkLookupEntry>>>,
) {
    let link_ends = link_ends_by_label
        .get(label_to)
        .expect("Undefined labels should be checked before graph expansion");
    // ... for every place within a lead this could go to ...
    for (row_idx_to, transposition_to_lead_head) in link_ends {
        // post-transposition
        //    from: the head of the lead the link is coming _from_
        //    to  : the head of the lead the link is coming _to_
        let lead_head_transposition = row_after_link * transposition_to_lead_head;

        // ... for every lead head mask of the method we're going to ...
        for lead_head_mask_to in &allowed_lead_masks[row_idx_to.method] {
            //     `lh_from * lh_transposition` satisfies `lh_mask_to`
            // iff `lh_from`                    satisfies `lh_mask_to * lh_transposition.inv()`
            let lead_head_mask_from = lead_head_mask_to * lead_head_transposition.inv();
            // Check if `lead_head_mask_from` can actually be reached (i.e. is there some LH mask
            // which is compatible with it?).  This doesn't change the results, but has a massive
            // performance benefit since chunk expansion is linear in the size of
            // `link_lookup_for_method`.  This simple pruning often causes a ~4x speedup for
            // tenors-together comps.
            let is_mask_reachable = allowed_lead_masks[method_idx_from]
                .iter()
                .any(|lh_mask| lh_mask.is_compatible_with(&lead_head_mask_from));
            if !is_mask_reachable {
                continue;
            }
            // Add mapping from `(lead_head_mask_from, dist_from_lead_head)` to `link_lookup`
            link_lookup_for_method
                .entry(lead_head_mask_from)
                .or_default()
                .entry(dist_from_lead_head)
                .or_default()
                .push(LinkLookupEntry {
                    call,
                    lead_head_transposition: lead_head_transposition.clone(),
                    row_idx_to: *row_idx_to,
                });
        }
    }
}

/// Finds all the possible locations of a given [`Row`] within the course head masks for each
/// [`Method`].
fn boundary_locations(
    row: &Row,
    boundary: Boundary,
    params: &Parameters,
) -> Vec<ChunkIdInFirstPart> {
    // Generate the method starts
    let mut locations = Vec::new();
    for (method_idx, method) in params.methods.iter_enumerated() {
        for (lead_head, sub_lead_idx) in method.boundary_locations(row, boundary, params) {
            locations.push(ChunkIdInFirstPart {
                lead_head,
                row_idx: RowIdx::new(method_idx, sub_lead_idx),
            });
        }
    }
    locations
}
