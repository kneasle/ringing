//! Monument's search routines, along with the code for interacting with in-flight [`Search`]es.

mod best_first;
mod graph;
mod prefix;

use std::{
    ops::RangeInclusive,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

use bellframe::Stage;

use crate::{
    prove_length::{prove_lengths, RefinedRanges},
    query::{MethodId, MusicTypeIdx, Query},
    Composition,
};

#[allow(unused_imports)] // Only used for doc comments
use crate::SearchBuilder;

/// Handle to a search being run by Monument.
///
/// This is used if you want to keep control over searches as they are running, for example
/// [to abort them](Self::signal_abort) or receive [`Update`]s on their [`Progress`].  If you just
/// want to run a (hopefully quick) search, use [`SearchBuilder::run`] or
/// [`SearchBuilder::run_with_config`].  Both of those will deal with handling the [`Search`] for
/// you.
#[derive(Debug)]
pub struct Search {
    /* Data */
    query: Arc<Query>,
    config: Config,
    // TODO: Reintroduce this to make the search graph smaller
    // source_graph: crate::graph::Graph,
    refined_ranges: RefinedRanges,
    graph: self::graph::Graph,
    /* Concurrency control */
    abort_flag: AtomicBool,
}

impl Search {
    /// Create a new `Search` which generates [`Composition`]s according to the given [`Query`].
    /// This also verifies that the [`Query`] makes sense; if not, an [`Error`](crate::Error)
    /// describing the problem is returned.
    ///
    /// **The returned `Search` won't start until you explicitly call
    /// [`search.run(...)`](Self::run)**.
    pub(crate) fn new(query: Query, config: Config) -> crate::Result<Self> {
        // Build and optimise the graph
        let mut source_graph = crate::graph::Graph::unoptimised(&query, &config)?;
        source_graph.optimise(&query);
        // Prove which lengths are impossible, and use that to refine the length and method count
        // ranges
        let refined_ranges = prove_lengths(&source_graph, &query)?;
        // Create a fast-to-traverse copy of the graph
        let graph = self::graph::Graph::new(&source_graph, &query);

        Ok(Search {
            query: Arc::new(query),
            config,
            refined_ranges,
            graph,

            abort_flag: AtomicBool::new(false),
        })
    }

    /// Runs the search, **blocking the current thread** until either the search is completed or an
    /// [abort is signalled](Self::signal_abort).
    pub fn run(&self, update_fn: impl FnMut(Update)) {
        // Make sure that `abort_flag` starts as false (so the search doesn't abort immediately).
        // We want this to be sequentially consistent to make sure that the worker threads don't
        // see the previous value (which could be 'true').
        self.abort_flag.store(false, Ordering::SeqCst);
        best_first::search(self, update_fn, &self.abort_flag);
    }

    /// Signal that the search should be aborted as soon as possible.  `Search` is [`Sync`] and
    /// uses interior mutability, so `signal_abort` can be called from a different thread to the
    /// one blocking on [`Search::run`].
    pub fn signal_abort(&self) {
        self.abort_flag.store(true, Ordering::Relaxed);
    }

    /// Returns `true` if the last attempt at this `Search` [was aborted](Self::signal_abort).
    pub fn was_aborted(&self) -> bool {
        self.abort_flag.load(Ordering::SeqCst)
    }
}

impl Search {
    pub fn length_range(&self) -> RangeInclusive<usize> {
        self.query.length_range_usize()
    }

    pub fn get_method(&self, id: &MethodId) -> &bellframe::Method {
        &self.query.methods[id.index]
    }

    pub fn get_method_shorthand(&self, id: &MethodId) -> &str {
        &self.query.methods[id.index].shorthand
    }

    /// Gets the range of counts required of the given [`MethodId`].
    pub fn method_count_range(&self, id: &MethodId) -> RangeInclusive<usize> {
        let range = &self.refined_ranges.method_counts[id.index];
        range.start().as_usize()..=range.end().as_usize()
    }

    pub fn methods(&self) -> impl Iterator<Item = (MethodId, &bellframe::Method, &str)> {
        self.query
            .methods
            .iter_enumerated()
            .map(|(index, method)| (MethodId { index }, &method.inner, method.shorthand.as_str()))
    }

    pub fn music_type_ids(&self) -> impl Iterator<Item = MusicTypeIdx> + '_ {
        self.query.music_types.iter_enumerated().map(|(id, _)| id)
    }

    pub fn max_music_count(&self, id: &MusicTypeIdx) -> usize {
        self.query.music_types[*id]
            .max_count()
            .unwrap_or(usize::MAX)
    }

    pub fn is_spliced(&self) -> bool {
        self.query.is_spliced()
    }

    pub fn num_parts(&self) -> usize {
        self.query.num_parts()
    }

    /// Does this `Query` generate [`Composition`](crate::Composition)s with more than one part?
    pub fn is_multipart(&self) -> bool {
        self.query.is_multipart()
    }

    /// Gets the [`effective_stage`](bellframe::Row::effective_stage) of the part heads used in
    /// this `Query`.  The short form of every possible part head will be exactly this length.
    pub fn effective_part_head_stage(&self) -> Stage {
        self.query.part_head_group.effective_stage()
    }
}

/// Update message from an in-flight [`Search`].
#[derive(Debug)]
pub enum Update {
    /// A new composition has been found
    Comp(Composition),
    /// A thread is sending a status update
    Progress(Progress),
    /// The search is being aborted
    Aborting,
}

/// How much of a [`Search`] has been completed so far.
#[derive(Debug)]
pub struct Progress {
    /// How many times the core composing loop has been run so far.
    pub iter_count: usize,
    /// How many [`Composition`]s have been generated so far.
    pub num_comps: usize,

    /// The current length of the queue of [`Composition`] prefixes waiting to be expanded.
    pub queue_len: usize,
    /// The average length of [`Composition`] prefixes in the queue.
    pub avg_length: f32,
    /// The length of the longest [`Composition`] prefix in the queue.
    pub max_length: usize,

    /// `true` if the search routine is currently shortening the prefix queue to save memory.
    pub truncating_queue: bool,
}

impl Progress {
    /// [`Progress`] made by a [`Search`] which hasn't started yet.
    pub const START: Self = Self {
        iter_count: 0,
        num_comps: 0,

        queue_len: 0,
        avg_length: 0.0,
        max_length: 0,

        truncating_queue: false,
    };
}

/// Configuration options for a [`Search`].
///
/// Unlike the options set by [`SearchBuilder`], `Config` options
/// **don't** change which [`Composition`]s are generated.
#[derive(Debug, Clone)]
pub struct Config {
    /* General */
    /// Number of threads used to generate compositions.  If `None`, this uses the number of
    /// **physical** CPU cores (i.e. ignoring hyper-threading).
    pub thread_limit: Option<usize>,

    /* Graph Generation */
    /// The maximum number of chunks in the composition graph.  If a search would produce a graph
    /// bigger than this, it is aborted.  If there was no limit, it would be very easy to cause an
    /// out-of-memory crash by requesting a hugely open query such as split-tenors Maximus.
    pub graph_size_limit: usize,

    /* Search */
    /// The maximum number of [`Composition`] prefixes stored simultaneously whilst searching.
    /// Hopefully this will soon be deprecated in favour of an explicit memory limit.
    pub queue_limit: usize,
    /// If `true`, the data structures used by searches will be leaked using [`std::mem::forget`].
    /// This massively improves the termination speed (because the search creates tons of small
    /// allocations which we now don't need to explicitly free) but only makes sense for the CLI,
    /// where the process will do exactly one search run before terminating (thus returning the memory
    /// to the OS anyway).
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
