//! Core library for Monument, a fast and flexible composing engine.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

mod composition;
mod error;
mod graph;
mod prove_length;
pub mod query;
pub mod search;
pub mod utils;

pub use composition::Composition;
pub use error::{Error, Result};
use search::{SearchData, SearchUpdate};
pub use utils::OptRange; // TODO: Not pub

use std::sync::{atomic::AtomicBool, Arc};

use query::{CallIdx, MethodIdx, MethodVec, Query};

/// Configuration parameters for Monument which **don't** change which compositions are emitted.
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

/// Run a [`Query`], blocking the current thread until the search finishes.
// TODO: Make this a method on `QueryBuilder`
pub fn run(query: &Query, config: &Config) -> crate::Result<Vec<Composition>> {
    let mut comps = Vec::<Composition>::new();
    let update_fn = |update| {
        if let SearchUpdate::Comp(comp) = update {
            comps.push(comp);
        }
    };
    let abort_flag = Arc::new(AtomicBool::new(true));
    SearchData::new(query, config)?.search(abort_flag, update_fn);
    Ok(comps)
}
