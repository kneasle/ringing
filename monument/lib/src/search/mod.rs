use std::{
    collections::BinaryHeap,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{
    prove_length::RefinedRanges, utils::TotalLength, Config, Progress, Query, QueryUpdate,
};

mod graph;
mod prefix;

pub use graph::Graph;

use self::prefix::CompPrefix;

const ITERS_BETWEEN_ABORT_CHECKS: usize = 10_000;
const ITERS_BETWEEN_PROGRESS_UPDATES: usize = 100_000;

/// Searches a [`Graph`](m_gr::Graph) for compositions
pub(crate) fn search(
    graph: &crate::Graph,
    query: &Query,
    config: &Config,
    refined_ranges: RefinedRanges,
    mut update_fn: impl FnMut(QueryUpdate),
    abort_flag: &AtomicBool,
) {
    // Build the graph and populate the `SearchData`
    let search_data = SearchData {
        graph: self::graph::Graph::new(graph, query),
        ranges: refined_ranges,
        query,
    };

    // Initialise the frontier to just the start chunks
    let mut frontier: BinaryHeap<CompPrefix> = CompPrefix::starts(&search_data.graph);

    let mut iter_count = 0;
    let mut num_comps = 0;

    macro_rules! send_progress_update {
        (truncating_queue = $truncating_queue: expr) => {
            send_progress_update(
                &frontier,
                &mut update_fn,
                iter_count,
                num_comps,
                $truncating_queue,
            );
        };
    }

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).  This is best-first search (and can be A* depending on the cost function used).
    while let Some(prefix) = frontier.pop() {
        let maybe_comp = prefix.expand(&search_data, &mut frontier);

        // Submit new compositions when they're generated
        if let Some(comp) = maybe_comp {
            update_fn(QueryUpdate::Comp(comp));
            num_comps += 1;

            if num_comps == search_data.query.num_comps {
                break; // Stop the search once we've got enough comps
            }
        }

        // If the queue gets too long, then halve its size
        if frontier.len() >= config.queue_limit {
            send_progress_update!(truncating_queue = true);
            truncate_heap(&mut frontier, config.queue_limit / 2);
            send_progress_update!(truncating_queue = false);
        }

        iter_count += 1;

        // Check for abort every so often
        if iter_count % ITERS_BETWEEN_ABORT_CHECKS == 0 && abort_flag.load(Ordering::Relaxed) {
            update_fn(QueryUpdate::Aborting);
            break;
        }

        // Send stats every so often
        if iter_count % ITERS_BETWEEN_PROGRESS_UPDATES == 0 {
            send_progress_update!(truncating_queue = false);
        }
    }

    // Always send a final update before finishing
    send_progress_update!(truncating_queue = false);

    // If we're running the CLI, then `mem::forget` the frontier to avoid tons of drop calls.  We
    // don't care about leaking because the Monument process is about to terminate and the OS will
    // clean up the memory anyway.
    if config.leak_search_memory {
        std::mem::forget(search_data);
        std::mem::forget(frontier);
    }
}

fn send_progress_update(
    frontier: &BinaryHeap<CompPrefix>,
    update_fn: &mut impl FnMut(QueryUpdate),
    iter_count: usize,
    num_comps: usize,
    truncating_queue: bool,
) {
    let mut total_len = 0u64; // NOTE: We have use `u64` here to avoid overflow
    let mut max_length = TotalLength::ZERO;
    frontier.iter().for_each(|n| {
        total_len += n.length().as_u32() as u64;
        max_length = max_length.max(n.length());
    });
    update_fn(QueryUpdate::Progress(Progress {
        iter_count,
        num_comps,

        queue_len: frontier.len(),
        avg_length: if frontier.is_empty() {
            0.0 // Avoid returning `NaN` if the frontier is empty
        } else {
            total_len as f32 / frontier.len() as f32
        },
        max_length,

        truncating_queue,
    }));
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
struct SearchData<'query> {
    graph: self::graph::Graph,
    query: &'query Query,
    ranges: RefinedRanges,
}
