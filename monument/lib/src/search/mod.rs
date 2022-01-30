use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    fmt::Debug,
    rc::Rc,
    sync::{atomic::AtomicBool, mpsc::SyncSender, Arc},
};

use bit_vec::BitVec;

use crate::{
    layout::{LinkIdx, StartIdx},
    music::Score,
    utils::{coprime_bitmap, Rotation, RowCounts},
    Comp, CompInner, Progress, Query, QueryUpdate,
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
    update_channel: SyncSender<QueryUpdate>,
    abort_flag: Arc<AtomicBool>,
) {
    // Initialise values for the search
    let max_duffer_rows = query.max_duffer_rows.map_or(u32::MAX, |m| m as u32);
    let len_range = (query.len_range.start as u32)..(query.len_range.end as u32);
    let num_parts = graph.num_parts() as Rotation;
    let rotation_bitmap = coprime_bitmap(num_parts);

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
            avg_score,
            length,
        } = prefix;
        let PrefixInner {
            path,
            chunk_idx,
            unreachable_chunks,
            rotation,
            len_since_non_duffer,

            score,
            method_counts,
        } = *inner;
        let chunk = &graph.chunks[chunk_idx];

        // Check if the comp has come round
        if let Some(end) = chunk.end {
            if len_range.contains(&length)
                && method_counts.is_feasible(0, query.method_count_range.clone())
                && rotation_bitmap & (1 << rotation) != 0
            {
                let (start_idx, start_chunk_label, links) = path.flatten(&graph, &query);
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
                        score,
                        avg_score,
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
            if !method_counts.is_feasible(
                (len_range.end - length) as usize,
                query.method_count_range.clone(),
            ) {
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
            log::info!("Aborting");
            return;
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
    method_counts: RowCounts,
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
        method_counts: RowCounts,
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

impl CompPath {
    fn flatten(&self, graph: &Graph, query: &Query) -> (StartIdx, String, Vec<(LinkIdx, String)>) {
        let mut links = Vec::new();
        let (start_idx, start_chunk_label) = self.flatten_recursive(graph, query, &mut links);
        (start_idx, start_chunk_label, links)
    }

    /// Recursively flatten `self`, returning the start idx
    fn flatten_recursive(
        &self,
        graph: &Graph,
        query: &Query,
        out: &mut Vec<(LinkIdx, String)>,
    ) -> (StartIdx, String) {
        match self {
            Self::Start(start_idx) => {
                let (start_chunk_idx, _, _) = graph
                    .starts
                    .iter()
                    .find(|(_, start_idx_2, _)| start_idx == start_idx_2)
                    .unwrap();
                let label = graph.chunk_label(*start_chunk_idx);
                (*start_idx, label)
            }
            Self::Cons(lhs, link, chunk_idx) => {
                let start = lhs.flatten_recursive(graph, query, out);
                out.push((*link, graph.chunk_label(*chunk_idx)));
                start
            }
        }
    }
}
