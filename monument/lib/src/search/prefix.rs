use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    ops::Deref,
};

use bellframe::Row;
use bit_vec::BitVec;
use datasize::DataSize;
use ordered_float::OrderedFloat;

use crate::{
    composition::{Composition, PathElem},
    graph::LinkSide,
    group::PartHead,
    parameters::SpliceStyle,
    utils::{counts::Counts, div_rounding_up, lengths::TotalLength},
};

use super::{
    atw::AtwBitmap,
    graph::ChunkIdx,
    path::{PathId, Paths},
    Search,
};

/// The prefix of a composition.  These are ordered by average score per row.
#[derive(Debug, Clone)]
pub(super) struct CompPrefix {
    /// Total score generated so far
    score: f32,
    /// Length refers to the **end** of the current chunk.  We use `u32` because [`Score`] is also
    /// 32 bits long, making `CompPrefix` pack into 128 bits
    length: TotalLength,
    /// Data for this prefix which isn't accessed as much as `avg_score` or `length`.  We store it
    /// in a [`Box`] because the frontier spends a lot of time swapping elements, and copying a
    /// 128-bit struct is much much faster than copying an inlined [`PrefixInner`].  `avg_score`
    /// and `length` are accessed so often that they are left unboxed.
    inner: Box<PrefixInner>,
}

#[derive(Debug, Clone)]
pub(super) struct PrefixInner {
    /// The last node in the path taken so far
    path: PathId,

    /// The next [`LinkSide`] after chunk selection.  All other fields refer to the prefix up to
    /// **but not including** `next_link_side`.
    next_link_side: LinkSide<ChunkIdx>,
    /// For every [`ChunkIdx`], this contains `1` if that chunk is unringable (i.e. false against
    /// something in the prefix so far) and `0` otherwise
    unringable_chunks: BitVec,

    /// The [`group::Element`] representing the current part head.  For internal chunks, this value
    /// is completely arbitrary, but once the composition ends this is guaranteed to hold the part
    /// head we reached.
    // TODO: Compute this after search
    part_head: PartHead,

    /// Method counts refers to the **end** of the current chunk
    method_counts: Counts,
    /// Bitmap storing the parts of methods rung by each bell so far in the composition
    atw_bitmap: AtwBitmap,
}

impl CompPrefix {
    /// Given a index-based [`Graph`], return [`CompPrefix`]es representing each of the possible
    /// start links.
    pub fn starts(search: &Search, paths: &mut Paths) -> BinaryHeap<Self> {
        // `BitVec` that marks every `Chunk` as ringable
        let all_chunks_ringable = BitVec::from_elem(search.graph.chunks.len(), false);

        search
            .graph
            .starts
            .iter_enumerated()
            .map(|(start_idx, &(chunk_idx, _link_id, part_head))| {
                let chunk = &search.graph.chunks[chunk_idx];
                Self {
                    score: 0.0, // Start links can't have any score
                    length: TotalLength::ZERO,
                    inner: Box::new(PrefixInner {
                        path: paths.add_start(start_idx),
                        next_link_side: LinkSide::Chunk(chunk_idx),
                        unringable_chunks: all_chunks_ringable.clone(),
                        part_head,
                        method_counts: Counts::zeros(chunk.method_counts.len()),
                        atw_bitmap: search.atw_table.empty_bitmap(),
                    }),
                }
            })
            .collect()
    }

    /// Returns the number of bytes of memory occupied by `self`
    pub fn size(&self) -> usize {
        std::mem::size_of::<Self>()
            + std::mem::size_of::<PrefixInner>()
            + div_rounding_up(self.inner.unringable_chunks.len(), 8)
            + self.inner.method_counts.estimate_heap_size()
            + self.inner.atw_bitmap.estimate_heap_size()
    }

    pub fn avg_score(&self) -> OrderedFloat<f32> {
        OrderedFloat(self.score / self.length.as_usize() as f32)
    }

    pub fn path_head(&self) -> PathId {
        self.path
    }

    pub fn length(&self) -> TotalLength {
        self.length
    }
}

impl PartialEq for CompPrefix {
    fn eq(&self, other: &Self) -> bool {
        self.avg_score() == other.avg_score()
    }
}

impl Eq for CompPrefix {}

impl PartialOrd for CompPrefix {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CompPrefix {
    fn cmp(&self, other: &Self) -> Ordering {
        self.avg_score().cmp(&other.avg_score())
    }
}

impl Deref for CompPrefix {
    type Target = PrefixInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

//////////////////////
// PREFIX EXPANSION //
//////////////////////

impl CompPrefix {
    /// Expand this [`CompPrefix`], adding every 1-chunk-longer prefix to the `frontier`
    #[allow(clippy::let_unit_value)]
    pub(super) fn expand(
        self,
        search: &Search,
        paths: &mut Paths,
        frontier: &mut BinaryHeap<Self>,
    ) -> Option<Composition> {
        // Determine the chunk being expanded (or if it's an end, complete the composition)
        let chunk_idx = match self.next_link_side {
            LinkSide::Chunk(chunk_idx) => chunk_idx,
            LinkSide::StartOrEnd => return self.check_comp(search, paths),
        };
        let chunk = &search.graph.chunks[chunk_idx];

        /* From now on, we know we're expanding a chunk, not finishing a comp */

        let CompPrefix {
            inner,
            mut length,
            mut score,
        } = self;
        let PrefixInner {
            path,
            next_link_side: _,
            mut unringable_chunks,
            mut method_counts,
            part_head, // Don't make this `mut` because it would get updated in every loop iteration
            mut atw_bitmap,
        } = *inner;

        // Compute the values for after `chunk`
        length += chunk.total_length;
        score += chunk.score;
        method_counts += &chunk.method_counts;
        unringable_chunks.or(&chunk.falseness);
        // Factor in the change in atw score by subtracting and adding the scores on either side
        // of the change
        score -= search.atw_table.atw_score(&atw_bitmap);
        atw_bitmap.union_with(&chunk.atw_bitmap);
        score += search.atw_table.atw_score(&atw_bitmap);

        let succ_iter = chunk.succs.iter_enumerated();
        #[allow(unused_variables)]
        let chunk = (); // Prevent the loop from accessing `chunk` by accident

        let max_length = *search.refined_ranges.length.end();
        for (succ_idx, link) in succ_iter {
            let part_head = part_head * link.ph_rotation;
            let score = score + link.score;

            // If this `link` would add a new `Chunk`, check if that `Chunk` would make the comps
            // obviously impossible to complete
            if let LinkSide::Chunk(succ_idx) = link.next {
                let succ_chunk = &search.graph.chunks[succ_idx];
                let length_after_succ = length + succ_chunk.total_length;
                let method_counts_after_chunk = &method_counts + &succ_chunk.method_counts;

                if length_after_succ + succ_chunk.min_len_to_rounds > max_length {
                    continue; // Chunk would make comp too long
                }
                if unringable_chunks.get(succ_idx.index()).unwrap() {
                    continue; // Something already in the comp has made this unringable (i.e. false)
                }
                if !method_counts_after_chunk.is_feasible(
                    (max_length - length_after_succ).as_usize(),
                    search.refined_ranges.method_counts.as_raw_slice(),
                ) {
                    continue; // Can't recover the method balance before running out of rows
                }
            }

            frontier.push(CompPrefix {
                inner: Box::new(PrefixInner {
                    path: paths.add(path, succ_idx),
                    next_link_side: link.next,
                    unringable_chunks: unringable_chunks.clone(),
                    part_head,
                    method_counts: method_counts.clone(),
                    atw_bitmap: atw_bitmap.clone(),
                }),
                score,
                length,
            });
        }

        None
    }
}

///////////////////
// COMP CHECKING //
///////////////////

impl CompPrefix {
    /// Assuming that the [`CompPrefix`] has just finished the composition, check if the resulting
    /// composition satisfies the user's requirements.
    fn check_comp(&self, search: &Search, paths: &Paths) -> Option<Composition> {
        let params = &search.params;
        assert!(self.next_link_side.is_start_or_end());

        if !search.refined_ranges.length.contains(&self.length) {
            return None; // Comp is either too long or too short
        }
        // We have to re-check feasibility of `method_counts` even though a feasibility
        // check is performed when expanding, because the check on expansion checks
        // (conservatively) if the range is feasible within the _maximum possible_ length
        // range.  However, the composition is likely to be _shorter_ than this range and
        // removing those extra rows could make the method count infeasible.
        if !self
            .method_counts
            .is_feasible(0, search.refined_ranges.method_counts.as_raw_slice())
        {
            return None; // Comp doesn't have the required method balance
        }
        if !params.part_head_group.is_generator(self.part_head) {
            return None; // The part head reached wouldn't generate all the parts
        }
        if params.require_atw && search.atw_table.atw_factor(&self.atw_bitmap) < 0.99999 {
            return None; // The composition is not atw, but we were required to make it atw
        }

        /* At this point, all checks on the composition have passed and we know it satisfies the
         * user's parameters */

        let path = self.flattened_path(search, paths);
        let first_elem = path.first().expect("Must have at least one chunk");
        let last_elem = path.last().expect("Must have at least one chunk");

        // Handle splices over the part head
        let mut score = self.score;
        let is_splice = first_elem.method_id != last_elem.method_id
            || first_elem.start_sub_lead_idx != last_elem.end_sub_lead_idx(params);
        let splice_over_part_head = params.is_multipart() && is_splice;
        if splice_over_part_head {
            // Check if this splice is actually allowed under the composition (i.e. there must be a
            // common label between the start and end of the composition for a splice to be
            // allowed)
            let start_labels = search
                .params
                .get_method_by_id(first_elem.method_id)
                .first_lead()
                .get_annot(first_elem.start_sub_lead_idx)
                .unwrap();
            let end_labels = search
                .params
                .get_method_by_id(last_elem.method_id)
                .first_lead()
                .get_annot(last_elem.end_sub_lead_idx(&search.params))
                .unwrap();
            let is_valid_splice = start_labels.iter().any(|label| end_labels.contains(label));
            if !is_valid_splice {
                return None;
            }
            // Don't generate comp if it would violate the splice style over the part head
            if params.splice_style == SpliceStyle::Calls && last_elem.ends_with_plain() {
                return None;
            }
            // Add/subtract weights from the splices over the part head
            score += params.splice_weight * (params.num_parts() - 1) as f32;
        }

        // Now we know the composition is valid, construct it and return
        let comp = Composition {
            stage: params.stage,
            start_stroke: params.start_stroke,
            path,
            part_head: params.part_head_group.get_row(self.part_head).to_owned(),
            length: self.length,

            total_score: score,
        };
        // Sanity check that the composition is true
        if params.require_truth {
            let mut rows_so_far = HashSet::<&Row>::with_capacity(comp.length());
            for row in comp.rows(params).rows() {
                if !rows_so_far.insert(row) {
                    panic!("Generated false composition ({})", comp.call_string(params));
                }
            }
        }
        // Finally, return the comp
        Some(comp)
    }

    /// Create a sequence of [`ChunkId`]/[`LinkId`]s by traversing the [`Graph`] following the
    /// reversed-linked-list path.  Whilst traversing, this also totals up the music counts.
    fn flattened_path(&self, search: &Search, paths: &Paths) -> Vec<PathElem> {
        let params = &search.params;
        // Flatten the reversed-linked-list path into a flat `Vec` that we can iterate over
        let (start_idx, succ_idxs) = paths.flatten(self.path);

        let mut path = Vec::<PathElem>::new();

        // Traverse graph, following the flattened path, to enumerate the `ChunkId`/`LinkId`s.
        // Also compute music counts as we go.
        let (start_chunk_idx, _start_link, mut part_head_elem) = search.graph.starts[start_idx];
        let mut next_link_side = LinkSide::Chunk(start_chunk_idx);
        for succ_idx in succ_idxs {
            let next_chunk_idx = match next_link_side {
                LinkSide::Chunk(idx) => idx,
                LinkSide::StartOrEnd => unreachable!(),
            };
            // Load the chunk at the end of the previous link
            let chunk = &search.graph.chunks[next_chunk_idx];
            let succ_link = &chunk.succs[succ_idx];
            // Convert this chunk into a `PathElem`
            let method_idx = chunk.id.row_idx.method;
            let sub_lead_idx = chunk.id.row_idx.sub_lead_idx;
            let method = &search.params.methods[method_idx];
            path.push(PathElem {
                start_row: params.part_head_group.get_row(part_head_elem)
                    * chunk.id.lead_head.as_ref()
                    * method.row_in_plain_lead(sub_lead_idx),
                method_id: method.id,
                start_sub_lead_idx: sub_lead_idx,
                length: chunk.per_part_length,
                call_to_end: succ_link.call.map(|idx| search.params.calls[idx].id),
            });
            // Follow the link to the next chunk in the path
            next_link_side = succ_link.next;
            part_head_elem = part_head_elem * succ_link.ph_rotation;
        }
        assert!(next_link_side.is_start_or_end());
        path
    }
}
