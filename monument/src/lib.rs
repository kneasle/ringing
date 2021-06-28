use std::{ops::Range, sync::Arc, thread};

use bellframe::{Bell, Method};
use graph::ProtoGraph;
use itertools::Itertools;

use compose::EngineWorker;
use layout::Layout;
use single_method::{single_method_layout, CallSpec, SingleMethodError};

mod compose;
mod graph;
mod layout;
pub mod mask;
mod music;
pub mod single_method;

// Imports used solely by doctests
#[allow(unused_imports)]
use bellframe::Row;

// Top level re-exports for convenience
pub use music::MusicType;

/// The static data required for composition generation.  This data will not be modified and
/// therefore can be shared between threads (once multi-threading is implemented).
#[derive(Debug, Clone)]
pub struct Engine {
    config: Config,
    len_range: Range<usize>,
    layout: Layout,
    music_types: Vec<MusicType>,
    prototype_graph: ProtoGraph,
}

impl Engine {
    /// Creates an `Engine` from a course [`Layout`].
    pub fn from_layout(
        config: Config,
        len_range: Range<usize>,
        layout: Layout,
        music_types: Vec<MusicType>,
    ) -> Self {
        let prototype_graph = ProtoGraph::from_layout(&layout, &music_types, len_range.end);

        Self {
            config,
            len_range,
            layout,
            music_types,
            prototype_graph,
        }
    }

    /// Creates an `Engine` to generate compositions of a single method.
    pub fn single_method(
        // General params
        config: Config,
        len_range: Range<usize>,
        // Method or course structure
        method: &Method,
        calls: &[CallSpec],
        non_fixed_bells: &[Bell],
        plain_lead_positions: Option<Vec<String>>,
        // Music
        music: Vec<MusicType>,
    ) -> Result<Self, SingleMethodError> {
        let layout = single_method_layout(method, plain_lead_positions, calls, non_fixed_bells)?;
        Ok(Self::from_layout(config, len_range, layout, music))
    }

    /// Generate the compositions
    pub fn compose(self) {
        let arc_self = Arc::from(self);

        let threads = (0usize..1)
            .map(|i| {
                let thread_arc = arc_self.clone();
                thread::Builder::new()
                    .name(format!("Worker{}", i))
                    .spawn(move || {
                        let mut worker = EngineWorker::from_engine(&thread_arc, i);
                        worker.compose();
                    })
                    .unwrap()
            })
            .collect_vec();

        // Stop the main thread until all workers have returned
        for t in threads {
            t.join().unwrap();
        }
    }
}

/// General configuration parameters for an [`Engine`].  These do not form parts of the composition
/// specification, instead changing how the [`Engine`] operates.
#[derive(Debug, Clone)]
pub struct Config {
    /// The number of compositions that the [`Engine`] will generate before terminating.  These
    /// `num_comps` compositions are guaranteed to be optimal.
    pub num_comps: usize,
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    pub num_threads: Option<usize>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_comps: 1,
            num_threads: None,
        }
    }
}
