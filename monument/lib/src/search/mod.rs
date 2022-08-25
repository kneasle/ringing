//! Monument's search routines.  Some parts are made public for users who want more control over
//! searches.

use std::{
    ops::RangeInclusive,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use crate::{
    prove_length::{prove_lengths, RefinedRanges},
    query::Query,
    Composition,
};

mod best_first;
mod graph;
mod prefix;

/// Immutable data required for a [`Query`] to run.
///
/// This is useful if you want to use the data before starting the full search (e.g. to access
/// computed method count ranges).
#[derive(Debug)]
pub struct SearchData<'a> {
    query: &'a Query,
    config: &'a Config,
    // TODO: Reintroduce this to make the fast graph sparser
    // source_graph: crate::graph::Graph,
    refined_ranges: RefinedRanges,
    graph: self::graph::Graph,
}

impl<'a> SearchData<'a> {
    pub fn new(query: &'a Query, config: &'a Config) -> crate::Result<Self> {
        // Build and optimise the graph
        let mut source_graph = crate::graph::Graph::unoptimised(query, config)?;
        source_graph.optimise(query);
        // Prove which lengths are impossible, and use that to refine the length and method count
        // ranges
        let refined_ranges = prove_lengths(&source_graph, query)?;
        // Create a fast-to-traverse copy of the graph
        let graph = self::graph::Graph::new(&source_graph, query);

        Ok(Self {
            query,
            config,
            refined_ranges,
            graph,
        })
    }

    pub fn search(&self, abort_flag: Arc<AtomicBool>, update_fn: impl FnMut(SearchUpdate)) {
        // Make sure that `abort_flag` starts as false (so the search doesn't abort immediately).
        // We want this to be sequentially consistent to make sure that the worker threads don't
        // see the previous value (which could be 'true').
        abort_flag.store(false, Ordering::SeqCst);
        best_first::search(self, update_fn, &abort_flag);
    }

    /// Returns an iterator which yields the refined method count ranges for each
    /// [`Method`](crate::query::Method) in the [`Query`].
    pub fn method_count_ranges(&self) -> impl Iterator<Item = RangeInclusive<usize>> + '_ {
        self.refined_ranges
            .method_counts
            .iter()
            .map(|range| range.start().as_usize()..=range.end().as_usize())
    }
}

/// Status/progress update for a search
#[derive(Debug)]
pub enum SearchUpdate {
    /// A new composition has been found
    Comp(Composition),
    /// A thread is sending a status update
    Progress(Progress),
    /// The search is being aborted
    Aborting,
}

/// How much of a search has been completed so far
#[derive(Debug)]
pub struct Progress {
    /// How many chunks have been expanded so far
    pub iter_count: usize,
    /// How many comps have been generated so far
    pub num_comps: usize,

    /// The current length of the A* queue
    pub queue_len: usize,
    /// The average length of a composition prefix in the queue
    pub avg_length: f32,
    /// The length of the longest composition prefix in the queue
    pub max_length: usize,

    /// `true` if the search is currently truncating the queue to save memory
    pub truncating_queue: bool,
}

impl Progress {
    /// The [`Progress`] made by a search which hasn't started yet
    pub const START: Self = Self {
        iter_count: 0,
        num_comps: 0,

        queue_len: 0,
        avg_length: 0.0,
        max_length: 0,

        truncating_queue: false,
    };
}

/// Configuration parameters for a [`Search`](SearchData) which **don't** change which compositions
/// are generated.
#[derive(Debug, Clone)]
pub struct Config {
    /* General */
    /// Number of threads used to generate compositions.  If `None`, this uses the number of
    /// **physical** CPU cores (i.e. ignoring hyper-threading).
    pub thread_limit: Option<usize>,

    /* Graph Generation */
    /// The maximum graph size, in chunks.  If a search would produce a graph bigger than this, it
    /// is aborted.
    pub graph_size_limit: usize,

    /* Search */
    /// The maximum number of [`Composition`] prefixes stored simultaneously whilst searching.
    pub queue_limit: usize,
    /// If `true`, the data structures used by searches will be leaked using [`std::mem::forget`].
    /// This massively improves the termination speed (because all individual allocations don't
    /// need to be freed), but only makes sense for the CLI, where Monument will do exactly one
    /// search run before terminating (thus returning the memory to the OS anyway).
    pub leak_search_memory: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            thread_limit: None,

            graph_size_limit: 100_000,

            queue_limit: 10_000_000,
            leak_search_memory: false,
        }
    }
}
