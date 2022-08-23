//! Core library for Monument, a fast and flexible composing engine.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

mod composition;
mod error;
mod graph;
mod prove_length;
pub mod query;
mod search;
pub mod utils;

pub use composition::Composition;
pub use error::{Error, Result};
pub use graph::Graph; // TODO: Not pub
pub use prove_length::RefinedRanges; // TODO: Not pub
pub use utils::OptRange; // TODO: Not pub

use std::sync::atomic::{AtomicBool, Ordering};

use query::{CallIdx, MethodIdx, MethodVec, Query};

/// The [`Score`] used to determine which [`Composition`]s are better than others.
// TODO: Not pub
pub type Score = ordered_float::OrderedFloat<f32>;

/// Configuration parameters for Monument which **don't** change which compositions are emitted.
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

////////////
// SEARCH //
////////////

impl Query {
    /// Prove which lengths/method counts are actually possible
    pub fn refine_ranges(&self, graph: &Graph) -> crate::Result<RefinedRanges> {
        prove_length::prove_lengths(graph, self)
    }

    /// Given a set of (optimised) graphs, run multi-threaded tree search to generate compositions.
    /// `update_fn` is run whenever a thread generates a [`QueryUpdate`].
    pub fn search(
        &self,
        graph: Graph,
        refined_ranges: RefinedRanges,
        config: &Config,
        update_fn: impl FnMut(QueryUpdate),
        abort_flag: &AtomicBool,
    ) {
        // Make sure that `abort_flag` starts as false (so the search doesn't abort immediately).
        // We want this to be sequentially consistent to make sure that the worker threads don't
        // see the previous value (which could be 'true').
        abort_flag.store(false, Ordering::SeqCst);
        search::search(&graph, self, config, refined_ranges, update_fn, abort_flag);
    }
}

/// Status/progress update for a search
#[derive(Debug)]
pub enum QueryUpdate {
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

impl Default for Progress {
    fn default() -> Self {
        Self::START
    }
}
