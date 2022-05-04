use std::{cmp::Ordering, collections::BinaryHeap, ops::Deref, rc::Rc};

use bit_vec::BitVec;

use crate::{
    layout::{BlockIdx, LinkIdx, StartIdx},
    music::Score,
    utils::{Counts, Rotation},
    Comp, CompInner, Query, SpliceStyle,
};

use super::{graph::ChunkIdx, Graph, SearchData};

/// The prefix of a composition.  These are ordered by average score per row.
#[derive(Debug, Clone)]
pub struct CompPrefix {
    /// Data for this prefix which isn't accessed as much as `avg_score` or `length`.  We store it
    /// in a [`Box`] because the frontier spends a lot of time swapping elements, and copying a
    /// 128-bit struct is much much faster than copying an inlined [`PrefixInner`].  `avg_score`
    /// and `length` are accessed so often that they are left unboxed.
    inner: Box<PrefixInner>,
    /// Score per row in the composition
    avg_score: Score,
    /// Length refers to the **end** of the current chunk.  We use `u32` because [`Score`] is also
    /// 32 bits long, making `CompPrefix` pack into 128 bits
    length: u32,
}

#[derive(Debug, Clone)]
// TODO: Not pub
pub struct PrefixInner {
    /// The path traced to this chunk
    path: CompPath,

    chunk_idx: ChunkIdx,
    /// For every [`ChunkIdx`], this contains `1` if that chunk is unringable (i.e. false against
    /// something in the prefix so far) and `0` otherwise
    unringable_chunks: BitVec,

    /// The number of part heads through which we have rotated.  This is kept in the range
    /// `0..graph.num_parts`
    rotation: Rotation,

    /// Score refers to the **end** of the current chunk
    score: Score,
    /// Method counts refers to the **end** of the current chunk
    method_counts: Counts,
}

impl CompPrefix {
    /// Given a index-based [`Graph`], return [`CompPrefix`]es representing each of the start
    /// chunks.
    pub fn starts(graph: &Graph) -> BinaryHeap<Self> {
        graph
            .starts
            .iter()
            .copied()
            .map(|(chunk_idx, start_idx, rotation)| {
                let chunk = &graph.chunks[chunk_idx];
                Self::new(
                    CompPath::Start(start_idx),
                    chunk_idx,
                    chunk.falseness.clone(),
                    rotation,
                    chunk.score,
                    chunk.length,
                    chunk.method_counts.clone(),
                )
            })
            .collect()
    }

    #[allow(clippy::too_many_arguments)]
    fn new(
        path: CompPath,
        chunk_idx: ChunkIdx,
        unringable_chunks: BitVec,
        rotation: Rotation,
        score: Score,
        length: u32,
        method_counts: Counts,
    ) -> Self {
        Self {
            inner: Box::new(PrefixInner {
                path,
                chunk_idx,
                unringable_chunks,
                rotation,
                score,
                method_counts,
            }),
            length,
            avg_score: score / length as f32,
        }
    }

    #[must_use]
    pub fn length(&self) -> u32 {
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

///////////////////
// COMP CHECKING //
///////////////////

pub enum CompCheckResult {
    /// This prefix isn't a complete composition, and should be expanded
    Incomplete,
    /// This prefix is a composition which satisfies the requirements
    Valid(Comp),
    /// This prefix is a composition, which doesn't satisfy the requirements.  Therefore, this
    /// branch should be pruned
    Invalid,
}

impl CompPrefix {
    pub(super) fn check_for_comp(&self, data: &SearchData) -> CompCheckResult {
        let chunk = &data.graph.chunks[self.chunk_idx];

        let end = match chunk.end {
            Some(end) => end,
            None => return CompCheckResult::Incomplete, // Prefix doesn't end in rounds
        };

        // At this point, we know the prefix is a valid composition, so check it's valid

        if !data.query.len_range.contains(&(self.length as usize)) {
            return CompCheckResult::Invalid; // Comp is either too long or too short
        }
        // We have to re-check feasibility of `method_counts` even though a feasibility
        // check is performed when expanding, because the check on expansion checks
        // (conservatively) if the range is feasible within the _maximum possible_ length
        // range.  However, the composition is likely to be _shorter_ than this range and
        // removing those extra rows could make the method count infeasible.
        if !self.method_counts.is_feasible(0, &data.method_count_ranges) {
            return CompCheckResult::Invalid; // Comp doesn't have the required method balance
        }
        if data.rotation_bitmap & (1 << self.rotation) == 0 {
            return CompCheckResult::Invalid; // The part head reached wouldn't generate all the parts
        }

        let FlattenOutput {
            start_idx,
            start_chunk_label,
            links,
            music_counts,
            splice_over_part_head,
        } = self.path.flatten(&data.graph, &data.query);

        // Handle splices over the part head
        let mut score = self.score;
        if splice_over_part_head {
            // Add/subtract weights from the splices over the part head
            score += (data.num_parts - 1) as f32 * data.query.splice_weight;

            let finishes_with_plain = if chunk.id.is_standard() {
                true // If we don't finish with a 0-length end then there can't be a call
            } else if let Some((link_idx, _s)) = links.last() {
                // If the last link is explicitly plain, then we finish with a plain
                data.query.layout.links[*link_idx].call_idx.is_none()
            } else {
                // If there were no links, then there can't be any calls either
                true
            };
            if data.query.splice_style == SpliceStyle::Calls && finishes_with_plain {
                return CompCheckResult::Invalid; // Don't generate comp if it would violate the splice style
            }
        }

        // Now we know the composition is valid, construct it and return
        let comp = Comp {
            query: data.query.clone(),

            inner: CompInner {
                start_idx,
                start_chunk_label,
                links,
                end,

                rotation: self.rotation,
                length: self.length as usize,
                method_counts: self.method_counts.clone(),
                music_counts,
                total_score: score,
                avg_score: score / self.length as f32,
            },
        };
        CompCheckResult::Valid(comp)
    }
}

//////////////////////
// PREFIX EXPANSION //
//////////////////////

impl CompPrefix {
    /// Expand this [`CompPrefix`], adding every 1-chunk-longer prefix to the `frontier`
    pub(super) fn expand(&self, data: &SearchData, frontier: &mut BinaryHeap<Self>) {
        let max_length = data.query.len_range.end;
        let chunk = &data.graph.chunks[self.chunk_idx];
        let path = Rc::new(self.path.clone());

        for link in &chunk.succs {
            let next_idx = link.next_chunk;
            let succ_chunk = &data.graph.chunks[next_idx];

            let rotation = (self.rotation + link.rot) % data.num_parts;
            let length = self.length + succ_chunk.length;
            let score = self.score + succ_chunk.score + link.score;
            let method_counts = &self.method_counts + &succ_chunk.method_counts;

            if length + succ_chunk.dist_to_rounds >= max_length as u32 {
                continue; // Chunk would make comp too long
            }
            if self.unringable_chunks.get(next_idx.index()).unwrap() {
                continue; // Chunk is false against something already in the comp
            }
            if !method_counts.is_feasible(max_length - length as usize, &data.method_count_ranges) {
                continue; // Can't recover the method balance before running out of rows
            }

            // Compute which chunks are unreachable after this chunk has been added
            let mut new_unringable_chunks = self.unringable_chunks.clone();
            new_unringable_chunks.or(&succ_chunk.falseness);

            frontier.push(CompPrefix::new(
                CompPath::Cons(path.clone(), link.source_idx, next_idx),
                next_idx,
                new_unringable_chunks,
                rotation,
                score,
                length,
                method_counts,
            ));
        }
    }
}

//////////////////////
// PATH LINKED-LIST //
//////////////////////

/// A route through the composition graph, stored as a 'reversed' linked list (i.e. one where each
/// node stores a reference to its _predecessor_ rather than its _successor_).  This allows for
/// multiple compositions with the same prefix to share the data for that prefix.
#[derive(Debug, Clone)]
enum CompPath {
    /// The start of a composition, along with the index within `Graph::start_chunks` of this
    /// specific start
    Start(StartIdx),
    /// The composition follows the path in the [`Rc`], then follows the given link to reach the
    /// given [`ChunkIdx`]
    Cons(Rc<Self>, LinkIdx, ChunkIdx),
}

struct FlattenOutput {
    start_idx: StartIdx,
    start_chunk_label: String,
    links: Vec<(LinkIdx, String)>,
    music_counts: Counts,
    /// `true` if a single part of the composition finishes on a different method to which it
    /// started (i.e. there would possibly be a splice over the part-head).
    splice_over_part_head: bool,
}

/// Data passed back up the call stack from `flatten_recursive`
struct RecursiveFlattenOutput {
    start_idx: StartIdx,
    start_chunk_label: String,
    first_block_idx: BlockIdx,
    last_block_idx: BlockIdx,
}

impl CompPath {
    fn flatten(&self, graph: &Graph, query: &Query) -> FlattenOutput {
        let mut links = Vec::new();
        let mut music_counts = Counts::zeros(query.music_types.len());
        let flatten_output = self.flatten_recursive(graph, query, &mut links, &mut music_counts);
        FlattenOutput {
            start_idx: flatten_output.start_idx,
            start_chunk_label: flatten_output.start_chunk_label,
            links,
            music_counts,
            splice_over_part_head: flatten_output.first_block_idx != flatten_output.last_block_idx,
        }
    }

    /// Recursively flatten `self`, returning
    /// ```text
    /// (
    ///     index of start chunk,
    ///     string of the start chunk,
    ///     BlockIdx of first chunk,
    ///     BlockIdx of last chunk,
    /// )
    /// ```
    fn flatten_recursive(
        &self,
        graph: &Graph,
        query: &Query,
        link_vec: &mut Vec<(LinkIdx, String)>,
        music_counts: &mut Counts,
    ) -> RecursiveFlattenOutput {
        match self {
            Self::Start(start_idx) => {
                let (start_chunk_idx, _, _) = graph
                    .starts
                    .iter()
                    .find(|(_, start_idx_2, _)| start_idx == start_idx_2)
                    .unwrap();
                let start_chunk = &graph.chunks[*start_chunk_idx];
                *music_counts += &start_chunk.music_counts;
                let start_block_idx = start_chunk
                    .id
                    .row_idx()
                    .expect("Can't start with a 0-length end chunk")
                    .block;
                RecursiveFlattenOutput {
                    start_idx: *start_idx,
                    start_chunk_label: start_chunk.label.clone(),
                    first_block_idx: start_block_idx,
                    last_block_idx: start_block_idx,
                }
            }
            Self::Cons(lhs, link, chunk_idx) => {
                // Recursively flatten all previous links in the list
                let mut output = lhs.flatten_recursive(graph, query, link_vec, music_counts);
                // Update music counts and list the links used
                let chunk = &graph.chunks[*chunk_idx];
                *music_counts += &chunk.music_counts;
                link_vec.push((*link, chunk.label.clone()));
                // Update the `last_block_idx` and propagate the output
                if let Some(row_idx) = chunk.id.row_idx() {
                    output.last_block_idx = row_idx.block;
                }
                output
            }
        }
    }
}
