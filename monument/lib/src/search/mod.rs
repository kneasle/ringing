use std::{
    collections::BinaryHeap,
    ops::Range,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::SyncSender,
        Arc,
    },
};

use itertools::Itertools;

use crate::{Progress, Query, QueryUpdate};

mod graph;
mod prefix;

pub use graph::Graph;

use self::prefix::CompPrefix;

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
    let search_data = SearchData::new(graph, query);

    // Initialise the frontier to just the start chunks
    let mut frontier: BinaryHeap<CompPrefix> = CompPrefix::starts(&search_data.graph);

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).
    let mut iter_count = 0;
    let mut num_comps = 0;
    while let Some(prefix) = frontier.pop() {
        let maybe_comp = prefix.expand(&search_data, &mut frontier);

        // Submit new compositions when they're generated
        if let Some(comp) = maybe_comp {
            update_channel.send(QueryUpdate::Comp(comp)).unwrap();
            num_comps += 1;

            if num_comps == search_data.query.num_comps {
                break; // Stop the search once we've got enough comps
            }
        }

        // If the queue gets too long, then halve its size
        if frontier.len() >= queue_limit {
            update_channel.send(QueryUpdate::TruncatingQueue).unwrap();
            truncate_heap(&mut frontier, queue_limit / 2);
        }

        iter_count += 1;

        // Check for abort every so often
        if iter_count % ITERS_BETWEEN_ABORT_CHECKS == 0 && abort_flag.load(Ordering::Relaxed) {
            update_channel.send(QueryUpdate::Aborting).unwrap();
            break;
        }

        // Send stats every so often
        if iter_count % ITERS_BETWEEN_PROGRESS_UPDATES == 0 {
            send_progress_update(&frontier, &update_channel, iter_count, num_comps);
        }
    }

    // Always send a final update before finishing
    send_progress_update(&frontier, &update_channel, iter_count, num_comps);

    // If we're running the CLI, then `mem::forget` the frontier to avoid tons of drop calls.  We
    // don't care about leaking because the Monument process is about to terminate and the OS will
    // clean up the memory anyway.
    if mem_forget_search_data {
        std::mem::forget(search_data);
        std::mem::forget(frontier);
    }
}

fn send_progress_update(
    frontier: &BinaryHeap<CompPrefix>,
    update_channel: &SyncSender<QueryUpdate>,
    iter_count: usize,
    num_comps: usize,
) {
    let mut total_len = 0;
    let mut max_length = 0;
    frontier.iter().for_each(|n| {
        total_len += n.length() as usize;
        max_length = max_length.max(n.length());
    });
    update_channel
        .send(QueryUpdate::Progress(Progress {
            iter_count,
            num_comps,
            queue_len: frontier.len(),
            avg_length: total_len as f32 / frontier.len() as f32,
            max_length,
        }))
        .unwrap();
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

/// Static, immutable data used for prefix expansion
struct SearchData {
    graph: self::graph::Graph,
    query: Arc<Query>,
    method_count_ranges: Vec<Range<usize>>,
}

impl SearchData {
    fn new(graph: &crate::Graph, query: Arc<Query>) -> Self {
        Self {
            graph: self::graph::Graph::new(graph, &query),
            method_count_ranges: query
                .methods
                .iter()
                .map(|m| m.count_range.clone())
                .collect_vec(),
            query,
        }
    }
}
