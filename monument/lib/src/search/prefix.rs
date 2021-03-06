use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    ops::Deref,
    rc::Rc,
};

use bellframe::Row;
use bit_vec::BitVec;

use crate::{
    graph::LinkSide,
    music::Score,
    utils::{group::PartHead, Counts, TotalLength},
    Comp, PathElem, SpliceStyle,
};

use super::{
    graph::{ChunkIdx, StartIdx, SuccIdx},
    Graph, SearchData,
};

/// The prefix of a composition.  These are ordered by average score per row.
#[derive(Debug, Clone)]
pub(super) struct CompPrefix {
    /// Data for this prefix which isn't accessed as much as `avg_score` or `length`.  We store it
    /// in a [`Box`] because the frontier spends a lot of time swapping elements, and copying a
    /// 128-bit struct is much much faster than copying an inlined [`PrefixInner`].  `avg_score`
    /// and `length` are accessed so often that they are left unboxed.
    inner: Box<PrefixInner>,
    /// Score per row in the composition
    avg_score: Score,
    /// Length refers to the **end** of the current chunk.  We use `u32` because [`Score`] is also
    /// 32 bits long, making `CompPrefix` pack into 128 bits
    length: TotalLength,
}

#[derive(Debug, Clone)]
pub(super) struct PrefixInner {
    /// The path traced to this chunk
    path: CompPath,

    /// The next [`LinkSide`] after chunk selection.  All other fields refer to the prefix up to
    /// **but not including** `next_link_side`.
    next_link_side: LinkSide<ChunkIdx>,
    /// For every [`ChunkIdx`], this contains `1` if that chunk is unringable (i.e. false against
    /// something in the prefix so far) and `0` otherwise
    unringable_chunks: BitVec,

    /// The [`group::Element`] representing the current part head.  For internal chunks, this value
    /// is completely arbitrary, but once the composition ends this is guaranteed to hold the part
    /// head we reached.
    part_head: PartHead,

    /// Score refers to the **end** of the current chunk
    score: Score,
    /// Method counts refers to the **end** of the current chunk
    method_counts: Counts,
}

impl CompPrefix {
    /// Given a index-based [`Graph`], return [`CompPrefix`]es representing each of the possible
    /// start links.
    pub fn starts(graph: &Graph) -> BinaryHeap<Self> {
        // `BitVec` that marks every `Chunk` as ringable
        let all_chunks_ringable = BitVec::from_elem(graph.chunks.len(), false);

        graph
            .starts
            .iter_enumerated()
            .map(|(start_idx, &(chunk_idx, _link_id, rotation))| {
                let chunk = &graph.chunks[chunk_idx];
                Self::new(
                    CompPath::Start(start_idx),
                    LinkSide::Chunk(chunk_idx),
                    rotation,
                    all_chunks_ringable.clone(),
                    Score::from(0.0), // Start links can't have any score
                    TotalLength::ZERO,
                    Counts::zeros(chunk.method_counts.len()),
                )
            })
            .collect()
    }

    #[allow(clippy::too_many_arguments)]
    fn new(
        path: CompPath,
        next_link_side: LinkSide<ChunkIdx>,
        part_head: PartHead,
        unringable_chunks: BitVec,
        score: Score,
        length: TotalLength,
        method_counts: Counts,
    ) -> Self {
        Self {
            inner: Box::new(PrefixInner {
                path,
                next_link_side,
                unringable_chunks,
                part_head,
                score,
                method_counts,
            }),
            length,
            avg_score: score / length.as_usize() as f32,
        }
    }

    pub fn length(&self) -> TotalLength {
        self.length
    }
}

impl PartialEq for CompPrefix {
    fn eq(&self, other: &Self) -> bool {
        self.avg_score == other.avg_score
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
        self.avg_score.cmp(&other.avg_score)
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
    pub(super) fn expand(self, data: &SearchData, frontier: &mut BinaryHeap<Self>) -> Option<Comp> {
        // Determine the chunk being expanded (or if it's an end, complete the composition)
        let chunk_idx = match self.next_link_side {
            LinkSide::Chunk(chunk_idx) => chunk_idx,
            LinkSide::StartOrEnd => return self.check_comp(data),
        };
        let chunk = &data.graph.chunks[chunk_idx];

        /* From now on, we know we're expanding a chunk, not finishing a comp */

        let CompPrefix {
            inner,
            avg_score: _,
            mut length,
        } = self;
        let PrefixInner {
            path,
            next_link_side: _,
            mut unringable_chunks,
            mut score,
            mut method_counts,
            part_head, // Don't make this `mut` because it gets updated in every loop iteration
        } = *inner;

        // Compute the values for after `chunk`
        let path = Rc::new(path);
        length += chunk.total_length;
        score += chunk.score;
        method_counts += &chunk.method_counts;
        unringable_chunks.or(&chunk.falseness);

        let max_length = *data.ranges.length.end();
        for (succ_idx, link) in chunk.succs.iter_enumerated() {
            let rotation = part_head * link.ph_rotation;
            let score = score + link.score;

            // If this `link` would add a new `Chunk`, check if that `Chunk` would make the comps
            // obviously impossible to complete
            if let LinkSide::Chunk(succ_idx) = link.next {
                let succ_chunk = &data.graph.chunks[succ_idx];
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
                    data.ranges.method_counts.as_raw_slice(),
                ) {
                    continue; // Can't recover the method balance before running out of rows
                }
            }

            frontier.push(CompPrefix::new(
                CompPath::Cons(path.clone(), succ_idx),
                link.next,
                rotation,
                unringable_chunks.clone(), // PERF: Share these to save memory
                score,
                length,
                method_counts.clone(),
            ));
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
    fn check_comp(&self, data: &SearchData) -> Option<Comp> {
        assert!(self.next_link_side.is_start_or_end());

        if !data.ranges.length.contains(&self.length) {
            return None; // Comp is either too long or too short
        }
        // We have to re-check feasibility of `method_counts` even though a feasibility
        // check is performed when expanding, because the check on expansion checks
        // (conservatively) if the range is feasible within the _maximum possible_ length
        // range.  However, the composition is likely to be _shorter_ than this range and
        // removing those extra rows could make the method count infeasible.
        if !self
            .method_counts
            .is_feasible(0, data.ranges.method_counts.as_raw_slice())
        {
            return None; // Comp doesn't have the required method balance
        }
        if !data.query.part_head_group.is_generator(self.part_head) {
            return None; // The part head reached wouldn't generate all the parts
        }

        /* At this point, all checks on the composition have passed and we know it satisfies the
         * user's query */

        let (path, music_counts) = self.flattened_path(data);
        let first_elem = path.first().expect("Must have at least one chunk");
        let last_elem = path.last().expect("Must have at least one chunk");

        // Handle splices over the part head
        let mut score = self.score;
        let is_splice = first_elem.method != last_elem.method
            || first_elem.start_sub_lead_idx != last_elem.end_sub_lead_idx(data.query);
        let splice_over_part_head = data.query.is_multipart() && is_splice;
        if splice_over_part_head {
            // Check if this splice is actually allowed under the composition (i.e. there must be a
            // common lead_location between the start and end of the composition)
            let start_labels = data.query.methods[first_elem.method]
                .first_lead()
                .get_annot(first_elem.start_sub_lead_idx)
                .unwrap();
            let end_labels = data.query.methods[last_elem.method]
                .first_lead()
                .get_annot(last_elem.end_sub_lead_idx(data.query))
                .unwrap();
            let is_valid_splice = start_labels.iter().any(|label| end_labels.contains(label));
            if !is_valid_splice {
                return None;
            }
            // Don't generate comp if it would violate the splice style over the part head
            if data.query.splice_style == SpliceStyle::Calls && last_elem.ends_with_plain() {
                return None;
            }
            // Add/subtract weights from the splices over the part head
            score += (data.query.num_parts() - 1) as f32 * data.query.splice_weight;
        }

        // Now we know the composition is valid, construct it and return
        let comp = Comp {
            path,

            part_head: self.part_head,
            length: self.length,
            method_counts: self.method_counts.clone(),
            music_counts,
            total_score: score,
            avg_score: score / self.length.as_usize() as f32,
        };
        // Sanity check that the composition is true
        if !data.query.allow_false {
            let mut rows_so_far = HashSet::<&Row>::with_capacity(comp.length.as_usize());
            for row in comp.rows(data.query).rows() {
                if !rows_so_far.insert(row) {
                    panic!(
                        "Generated false composition ({})",
                        comp.call_string(data.query)
                    );
                }
            }
        }
        // Finally, return the comp
        Some(comp)
    }

    /// Create a sequence of [`ChunkId`]/[`LinkId`]s by traversing the [`Graph`] following the
    /// reversed-linked-list path.  Whilst traversing, this also totals up the music counts.
    fn flattened_path(&self, data: &SearchData) -> (Vec<PathElem>, Counts) {
        // Flatten the reversed-linked-list path into a flat `Vec` that we can iterate over
        let (start_idx, succ_idxs) = self.path.flatten();

        let mut music_counts = Counts::zeros(data.query.music_types.len());
        let mut path = Vec::<PathElem>::new();

        // Traverse graph, following the flattened path, to enumerate the `ChunkId`/`LinkId`s.
        // Also compute music counts as we go.
        let (start_chunk_idx, _start_link, mut part_head_elem) = data.graph.starts[start_idx];
        let mut next_link_side = LinkSide::Chunk(start_chunk_idx);
        for succ_idx in succ_idxs {
            let next_chunk_idx = match next_link_side {
                LinkSide::Chunk(idx) => idx,
                LinkSide::StartOrEnd => unreachable!(),
            };
            // Load the chunk at the end of the previous link
            let chunk = &data.graph.chunks[next_chunk_idx];
            let succ_link = &chunk.succs[succ_idx];
            music_counts += &chunk.music_counts;

            let method_idx = chunk.id.row_idx.method;
            let sub_lead_idx = chunk.id.row_idx.sub_lead_idx;
            path.push(PathElem {
                start_row: data.query.part_head_group.get_row(part_head_elem)
                    * chunk.id.lead_head.as_ref()
                    * data.query.methods[method_idx].row_in_plain_lead(sub_lead_idx),
                method: method_idx,
                start_sub_lead_idx: sub_lead_idx,
                length: chunk.per_part_length,
                call: succ_link.call,
            });
            // Follow the link to the next chunk in the path
            next_link_side = succ_link.next;
            part_head_elem = part_head_elem * succ_link.ph_rotation;
        }
        (path, music_counts)
    }
}

//////////////////////
// LINKED-LIST PATH //
//////////////////////

/// A route through the composition graph, stored as a 'reversed' linked list (i.e. one where each
/// chunk stores a reference to its _predecessor_ rather than its _successor_).  This allows for
/// multiple compositions with the same prefix to share the data for that prefix.
#[derive(Debug, Clone)]
enum CompPath {
    /// The start of a composition, along with the index within [`Graph::start_chunks`] of this
    /// specific start
    Start(StartIdx),
    /// The composition follows the path in the [`Rc`], then follows the given link to reach the
    /// given [`ChunkIdx`]
    Cons(Rc<Self>, SuccIdx),
}

impl CompPath {
    fn flatten(&self) -> (StartIdx, Vec<SuccIdx>) {
        let mut path = Vec::new();
        let start_idx = self.flatten_recursive(&mut path);
        (start_idx, path)
    }

    /// Flatten `self` into `path`, returning the [`StartIdx`] encoding how this comp started
    fn flatten_recursive(&self, path: &mut Vec<SuccIdx>) -> StartIdx {
        match self {
            Self::Start(start_link_id) => *start_link_id,
            Self::Cons(lhs, succ_idx) => {
                let start_link_id = lhs.flatten_recursive(path);
                path.push(*succ_idx);
                start_link_id
            }
        }
    }
}
