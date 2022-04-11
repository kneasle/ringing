use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    fmt::Debug,
    rc::Rc,
    sync::{atomic::AtomicBool, mpsc::SyncSender, Arc},
};

use bit_vec::BitVec;
use itertools::Itertools;

use crate::{
    layout::{BlockIdx, LinkIdx, StartIdx},
    music::Score,
    utils::{coprime_bitmap, Counts, Rotation},
    Comp, CompInner, Progress, Query, QueryUpdate, SpliceStyle,
};

mod graph;

use graph::ChunkIdx;
pub use graph::Graph;

const ITERS_BETWEEN_ABORT_CHECKS: usize = 10_000;
const ITERS_BETWEEN_PROGRESS_UPDATES: usize = 100_000;

/// Searches a [`Graph`](m_gr::Graph) for compositions
pub(crate) fn search(
    graph: &crate::Graph,
    query: Arc<Query>,
    queue_limit: usize,
    mem_forget_search_data: bool,
    update_channel: SyncSender<QueryUpdate>,
    abort_flag: Arc<AtomicBool>,
) {
    // Initialise values for the search
    let max_duffer_rows = query.max_duffer_rows.map_or(u32::MAX, |m| m as u32);
    let len_range = (query.len_range.start as u32)..(query.len_range.end as u32);
    let num_parts = graph.num_parts() as Rotation;
    let rotation_bitmap = coprime_bitmap(num_parts);
    let method_count_ranges = query
        .layout
        .method_blocks
        .iter()
        .map(|b| b.count_range.clone())
        .collect_vec();

    // Lower the hash-based graph into a graph that's immutable but faster to traverse
    let graph = self::graph::Graph::new(graph, &query);

    // Initialise the frontier to just the start chunks
    let mut frontier = BinaryHeap::new();
    for (chunk_idx, start_idx, rotation) in graph.starts.iter() {
        let chunk = &graph.chunks[*chunk_idx];
        frontier.push(CompPrefix::new(
            CompPath::Start(*start_idx),
            *chunk_idx,
            chunk.falseness.clone(),
            *rotation,
            chunk.score,
            chunk.length,
            chunk.method_counts.clone(),
            if chunk.duffer {
                chunk.length // Rounds counts as a non-duffer
            } else {
                0
            },
        ));
    }

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).
    let mut iter_count = 0;
    let mut num_comps = 0;
    while let Some(prefix) = frontier.pop() {
        let CompPrefix {
            inner,
            avg_score: _, // We recompute this because splices over the part head might change `score`
            length,
        } = prefix;
        let PrefixInner {
            path,
            chunk_idx,
            unreachable_chunks,
            rotation,
            len_since_non_duffer,

            mut score,
            method_counts,
        } = *inner;
        let chunk = &graph.chunks[chunk_idx];

        // Check if the comp has come round
        if let Some(end) = chunk.end {
            if len_range.contains(&length)
                // We have to re-check feasibility of `method_counts` even though a feasibility
                // check is performed when expanding, because the check on expansion checks
                // (conservatively) if the range is feasible within the _maximum possible_ length
                // range.  However, the composition is likely to be _shorter_ than this range and
                // removing those extra rows could make the method count infeasible.
                && method_counts.is_feasible(0, &method_count_ranges)
                && rotation_bitmap & (1 << rotation) != 0
            {
                let std_out = std::io::stdout();
                let _lock = std_out.lock();
                let FlattenOutput {
                    start_idx,
                    start_chunk_label,
                    links,
                    music_counts,
                    splice_over_part_head,
                } = path.flatten(&graph, &query);
                if splice_over_part_head {
                    // Add/subtract weights from the splices over the part head
                    score += (query.num_parts() - 1) as f32 * query.splice_weight;

                    let finishes_with_plain = if chunk.id.is_standard() {
                        true // If we don't finish with a 0-length end then there can't be a call
                    } else if let Some((link_idx, _s)) = links.last() {
                        // If the last link is explicitly plain, then we finish with a plain
                        query.layout.links[*link_idx].call_idx.is_none()
                    } else {
                        // If there were no links, then there can't be any calls either
                        true
                    };
                    if query.splice_style == SpliceStyle::Calls && finishes_with_plain {
                        continue; // Don't generate comp if it would violate the splice style
                    }
                }
                let comp = Comp {
                    query: query.clone(),

                    inner: CompInner {
                        start_idx,
                        start_chunk_label,
                        links,
                        end,

                        rotation,
                        length: length as usize,
                        method_counts,
                        music_counts,
                        total_score: score,
                        avg_score: score / length as f32,
                    },
                };
                update_channel.send(QueryUpdate::Comp(comp)).unwrap();
                num_comps += 1;

                if num_comps == query.num_comps {
                    break; // Stop the search once we've got enough comps
                }
            }
            continue; // Don't expand comps after they've come round
        }

        // Expand this chunk
        let path = Rc::new(path);
        for link in &chunk.succs {
            let next_idx = link.next_chunk;
            let succ_chunk = &graph.chunks[next_idx];

            let rotation = (rotation + link.rot) % num_parts;
            let length = length + succ_chunk.length;
            let score = score + succ_chunk.score + link.score;
            let method_counts = &method_counts + &succ_chunk.method_counts;
            let len_since_non_duffer = if succ_chunk.duffer {
                len_since_non_duffer + succ_chunk.length
            } else {
                0 // Reset the counter whenever we encounter a non-duffer chunk
            };

            if length + succ_chunk.dist_to_rounds >= len_range.end {
                continue; // Chunk would make comp too long
            }
            if len_since_non_duffer + succ_chunk.dist_to_non_duffer >= max_duffer_rows {
                continue; // Can't get to a non-duffer fast enough
            }
            if unreachable_chunks.get(next_idx.index()).unwrap() {
                continue; // Chunk is false against something already in the comp
            }
            if !method_counts.is_feasible((len_range.end - length) as usize, &method_count_ranges) {
                continue; // Can't recover the method balance before running out of rows
            }

            // Compute which chunks are unreachable after this chunk has been added
            let mut new_unreachable_chunks = unreachable_chunks.clone();
            new_unreachable_chunks.or(&succ_chunk.falseness);

            frontier.push(CompPrefix::new(
                CompPath::Cons(path.clone(), link.source_idx, next_idx),
                next_idx,
                new_unreachable_chunks,
                rotation,
                score,
                length,
                method_counts,
                len_since_non_duffer,
            ));
        }

        // If the queue gets too long, then halve its size
        if frontier.len() >= queue_limit {
            update_channel.send(QueryUpdate::TruncatingQueue).unwrap();
            truncate_heap(&mut frontier, queue_limit / 2);
        }

        iter_count += 1;

        // Check for abort every so often
        if iter_count % ITERS_BETWEEN_ABORT_CHECKS == 0
            && abort_flag.load(std::sync::atomic::Ordering::Relaxed)
        {
            update_channel.send(QueryUpdate::Aborting).unwrap();
            break;
        }

        // Send stats every so often
        if iter_count % ITERS_BETWEEN_PROGRESS_UPDATES == 0 {
            let mut total_len = 0;
            let mut max_length = 0;
            frontier.iter().for_each(|n| {
                total_len += n.length as usize;
                max_length = max_length.max(n.length);
            });

            update_channel
                .send(QueryUpdate::Progress(Progress {
                    iter_count,
                    queue_len: frontier.len(),
                    avg_length: total_len as f32 / frontier.len() as f32,
                    max_length,
                }))
                .unwrap();
        }
    }

    // If we're running the CLI, then `mem::forget` the frontier to avoid tons of drop calls.  We
    // don't care about leaking because the Monument process is about to terminate and the OS will
    // clean up the memory anyway.
    if mem_forget_search_data {
        std::mem::forget(graph);
        std::mem::forget(frontier);
    }
}

fn truncate_heap<T: Ord>(heap_ref: &mut BinaryHeap<T>, len: usize) {
    let heap = std::mem::take(heap_ref);
    let mut chunks = heap.into_vec();
    chunks.sort_by(|a, b| b.cmp(a)); // Sort highest score first
    if len < chunks.len() {
        chunks.drain(len..);
    }
    *heap_ref = BinaryHeap::from(chunks);
}

///////////////////
// COMP PREFIXES //
///////////////////

#[derive(Debug, Clone)]
struct CompPrefix {
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

/// The prefix of a composition.  These are ordered by score.
#[derive(Debug, Clone)]
struct PrefixInner {
    /// The path traced to this chunk
    path: CompPath,

    chunk_idx: ChunkIdx,
    unreachable_chunks: BitVec,

    len_since_non_duffer: u32,

    /// The number of part heads through which we have rotated.  This is kept in the range
    /// `0..graph.num_parts`
    rotation: Rotation,

    /// Score refers to the **end** of the current chunk
    score: Score,
    /// Method counts refers to the **end** of the current chunk
    method_counts: Counts,
}

impl CompPrefix {
    #[allow(clippy::too_many_arguments)]
    fn new(
        path: CompPath,
        chunk_idx: ChunkIdx,
        unreachable_chunks: BitVec,
        rotation: Rotation,
        score: Score,
        length: u32,
        method_counts: Counts,
        len_since_non_duffer: u32,
    ) -> Self {
        Self {
            inner: Box::new(PrefixInner {
                path,
                chunk_idx,
                unreachable_chunks,
                rotation,
                score,
                method_counts,
                len_since_non_duffer,
            }),
            length,
            avg_score: score / length as f32,
        }
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

/// A route through the composition graph, stored as a reverse linked list.  This allows for
/// multiple compositions with the same prefix to share the data for that prefix.
#[derive(Debug, Clone)]
enum CompPath {
    /// The start of a composition, along with the index within `Graph::start_chunks` of this
    /// specific start
    Start(StartIdx),
    /// The composition follows the sequence in the [`Rc`], followed by taking the `n`th successor
    /// to that chunk.
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
