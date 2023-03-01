use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    ops::Deref,
};

use bellframe::Row;
use bit_vec::BitVec;
use datasize::DataSize;
use itertools::Itertools;

use crate::{
    atw::AtwBitmap,
    builder::{MusicTypeId, SpliceStyle},
    composition::{Composition, PathElem},
    graph::LinkSide,
    group::PartHead,
    utils::{
        counts::Counts,
        div_rounding_up,
        lengths::{PerPartLength, TotalLength},
        Score,
    },
};

use super::{
    graph::ChunkIdx,
    path::{PathId, Paths},
    Search,
};

/// The prefix of a composition.  These are ordered by average score per row.
#[derive(Debug, Clone)]
pub(super) struct CompPrefix {
    /// Total score generated so far
    score: Score,
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

    /// Length of contiguous run of duffers up to the end of the prefix
    contiguous_duffer: PerPartLength,
    /// Total count of duffers used so far in the composition
    total_duffer: TotalLength,

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
                    score: Score::from(0.0), // Start links can't have any score
                    length: TotalLength::ZERO,
                    inner: Box::new(PrefixInner {
                        path: paths.add_start(start_idx),
                        next_link_side: LinkSide::Chunk(chunk_idx),
                        unringable_chunks: all_chunks_ringable.clone(),
                        part_head,
                        contiguous_duffer: PerPartLength::ZERO, // Start is considered a non-duffer
                        total_duffer: TotalLength::ZERO,
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

    pub fn avg_score(&self) -> Score {
        self.score / self.length.as_usize() as f32
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
            mut contiguous_duffer,
            mut total_duffer,
            mut atw_bitmap,
        } = *inner;

        // Compute the values for after `chunk`
        length += chunk.total_length;
        if chunk.duffer {
            contiguous_duffer += chunk.per_part_length;
            total_duffer += chunk.total_length;
        } else {
            contiguous_duffer = PerPartLength::ZERO;
        }
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

                // Contiguous run of duffers would be too long
                if let Some(duffer_limit) = search.query.max_contiguous_duffer {
                    if succ_chunk.duffer {
                        let min_contiguous_duffer = contiguous_duffer
                            + succ_chunk.per_part_length
                            + succ_chunk.min_dist_to_non_duffer;
                        if min_contiguous_duffer > duffer_limit {
                            continue; // Chunk would force there to be too much duffer
                        }
                    }
                }
                // Total duffers would be too much
                if let Some(max_total_duffer) = search.query.max_total_duffer {
                    let succ_duffer_len = match succ_chunk.duffer {
                        false => TotalLength::ZERO,
                        true => succ_chunk.total_length,
                    };
                    let total_duffer_including_succ = total_duffer
                        + succ_duffer_len
                        + succ_chunk
                            .min_dist_to_non_duffer
                            .as_total(&search.query.part_head_group);
                    if total_duffer_including_succ > max_total_duffer {
                        continue; // Chunk would force us to ring too much duffer
                    }
                }

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
                    contiguous_duffer,
                    total_duffer,
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
        if !search.query.part_head_group.is_generator(self.part_head) {
            return None; // The part head reached wouldn't generate all the parts
        }

        /* At this point, all checks on the composition have passed and we know it satisfies the
         * user's query */

        let (path, music_counts, contiguous_duffer_lengths) = self.flattened_path(search, paths);
        let first_elem = path.first().expect("Must have at least one chunk");
        let last_elem = path.last().expect("Must have at least one chunk");

        // Handle splices over the part head
        let mut score = self.score;
        let is_splice = first_elem.method != last_elem.method
            || first_elem.start_sub_lead_idx != last_elem.end_sub_lead_idx(&search.query);
        let splice_over_part_head = search.query.is_multipart() && is_splice;
        if splice_over_part_head {
            // Check if this splice is actually allowed under the composition (i.e. there must be a
            // common label between the start and end of the composition for a splice to be
            // allowed)
            let start_labels = search.query.methods[first_elem.method]
                .first_lead()
                .get_annot(first_elem.start_sub_lead_idx)
                .unwrap();
            let end_labels = search.query.methods[last_elem.method]
                .first_lead()
                .get_annot(last_elem.end_sub_lead_idx(&search.query))
                .unwrap();
            let is_valid_splice = start_labels.iter().any(|label| end_labels.contains(label));
            if !is_valid_splice {
                return None;
            }
            // Don't generate comp if it would violate the splice style over the part head
            if search.query.splice_style == SpliceStyle::Calls && last_elem.ends_with_plain() {
                return None;
            }
            // Add/subtract weights from the splices over the part head
            score += search.query.splice_weight * (search.query.num_parts() - 1) as f32;
        }

        // Now we know the composition is valid, construct it and return
        let comp = Composition {
            path,

            part_head: self.part_head,
            length: self.length,
            method_counts: self.method_counts.clone(),
            atw_bitmap: self.atw_bitmap.clone(),
            music_counts: search
                .query
                .music_types
                .iter_enumerated()
                .zip_eq(music_counts.iter())
                .map(|((index, _), count)| (MusicTypeId { index }, *count))
                .collect(),
            total_score: score,

            contiguous_duffer_lengths,
            total_duffer: self.total_duffer,

            query: search.query.clone(),
            atw_table: search.atw_table.clone(),
        };
        // Sanity check that the composition is true
        if search.query.require_truth {
            let mut rows_so_far = HashSet::<&Row>::with_capacity(comp.length());
            for row in comp.rows().rows() {
                if !rows_so_far.insert(row) {
                    panic!("Generated false composition ({})", comp.call_string());
                }
            }
        }
        // Finally, return the comp
        Some(comp)
    }

    /// Create a sequence of [`ChunkId`]/[`LinkId`]s by traversing the [`Graph`] following the
    /// reversed-linked-list path.  Whilst traversing, this also totals up the music counts.
    fn flattened_path(
        &self,
        search: &Search,
        paths: &Paths,
    ) -> (Vec<PathElem>, Counts, Vec<PerPartLength>) {
        // Flatten the reversed-linked-list path into a flat `Vec` that we can iterate over
        let (start_idx, succ_idxs) = paths.flatten(self.path);

        let mut path = Vec::<PathElem>::new();
        let mut music_counts = Counts::zeros(search.query.music_types.len());
        let mut duffer_lengths = Vec::<PerPartLength>::new();

        // Traverse graph, following the flattened path, to enumerate the `ChunkId`/`LinkId`s.
        // Also compute music counts as we go.
        let (start_chunk_idx, _start_link, mut part_head_elem) = search.graph.starts[start_idx];
        let mut next_link_side = LinkSide::Chunk(start_chunk_idx);
        let mut was_last_chunk_duffer = false; // No last chunk, but the start is non-duffer
        let mut consecutive_duffer = PerPartLength::ZERO;
        for succ_idx in succ_idxs {
            let next_chunk_idx = match next_link_side {
                LinkSide::Chunk(idx) => idx,
                LinkSide::StartOrEnd => unreachable!(),
            };
            // Load the chunk at the end of the previous link
            let chunk = &search.graph.chunks[next_chunk_idx];
            let succ_link = &chunk.succs[succ_idx];
            music_counts += &chunk.music_counts;
            // Check for duffer lengths
            match (was_last_chunk_duffer, chunk.duffer) {
                (false, true) => consecutive_duffer = chunk.per_part_length, // Starting duffers
                (true, true) => consecutive_duffer += chunk.per_part_length, // Continuing duffers
                (true, false) => duffer_lengths.push(consecutive_duffer),    // Finishing duffers
                (false, false) => {
                    // Calls which join two different non-duffer courses is a transition of zero
                    if succ_link.call.is_some() {
                        duffer_lengths.push(PerPartLength::ZERO);
                    }
                }
            }
            // Convert this chunk into a `PathElem`
            let method_idx = chunk.id.row_idx.method;
            let sub_lead_idx = chunk.id.row_idx.sub_lead_idx;
            path.push(PathElem {
                start_row: search.query.part_head_group.get_row(part_head_elem)
                    * chunk.id.lead_head.as_ref()
                    * search.query.methods[method_idx].row_in_plain_lead(sub_lead_idx),
                method: method_idx,
                start_sub_lead_idx: sub_lead_idx,
                length: chunk.per_part_length,
                call: succ_link.call,
            });
            // Follow the link to the next chunk in the path
            next_link_side = succ_link.next;
            was_last_chunk_duffer = chunk.duffer;
            part_head_elem = part_head_elem * succ_link.ph_rotation;
        }
        assert!(next_link_side.is_start_or_end());
        (path, music_counts, duffer_lengths)
    }
}
