#![allow(rustdoc::private_intra_doc_links)]

use std::{ops::Range, sync::Arc};

use bellframe::{Bell, Method};
use graph::ProtoGraph;

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
pub use layout::*;
pub use music::MusicType;

/// The static data required for composition generation.  Excluding the stack of [`CompPrefix`]es
/// (which is under a [`Mutex`]), this data will not be modified and therefore can be safely shared
/// between threads.
#[derive(Debug)]
pub struct Engine {
    config: Config,
    len_range: Range<usize>,
    /// The number of compositions that the [`Engine`] will generate before terminating.  These
    /// `num_comps` compositions are guaranteed to be optimal.
    num_comps: usize,
    layout: Layout,
    music_types: Vec<MusicType>,
    prototype_graph: ProtoGraph,
}

impl Engine {
    /// Creates an `Engine` from a course [`Layout`].
    pub fn from_layout(
        config: Config,
        len_range: Range<usize>,
        num_comps: usize,
        layout: Layout,
        music_types: Vec<MusicType>,
    ) -> Self {
        Engine {
            prototype_graph: ProtoGraph::from_layout(&layout, &music_types, len_range.end, &config),
            config,
            len_range,
            num_comps,
            layout,
            music_types,
        }
    }

    /// Creates an `Engine` to generate compositions of a single method.
    pub fn single_method(
        // General params
        config: Config,
        len_range: Range<usize>,
        num_comps: usize,
        // Method or course structure
        method: &Method,
        calls: &[CallSpec],
        non_fixed_bells: &[Bell],
        plain_lead_positions: Option<Vec<String>>,
        // Music
        music: Vec<MusicType>,
    ) -> Result<Self, SingleMethodError> {
        let layout = single_method_layout(method, plain_lead_positions, calls, non_fixed_bells)?;
        Ok(Self::from_layout(
            config, len_range, num_comps, layout, music,
        ))
    }

    /// Generate the compositions
    pub fn compose(self) {
        // Decide how many threads to use (defaulting to the number of CPU cores)
        let num_threads = self.config.num_threads.unwrap_or_else(num_cpus::get);
        if num_threads == 1 {
            println!("Using 1 thread.");
        } else {
            println!("Using {} threads.", num_threads);
        }

        // Delegate to the `compose` to run the composition search
        let arc_self = Arc::new(self);
        let comps = compose::compose(num_threads, &arc_self);

        // Print out the best comps from this global shortlist
        for c in comps {
            println!("{}", c.to_string(&arc_self.layout));
        }
    }
}

/// General configuration parameters for an [`Engine`].  These do not form parts of the composition
/// specification, instead changing how the [`Engine`] operates.
#[derive(Debug, Clone)]
pub struct Config {
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    pub num_threads: Option<usize>,
    /// Sort succession links so that likely musical nodes are explored first
    pub sort_successor_links: bool,
    /// Normalise music scores by their length (i.e. rank by average music output)
    pub normalise_music: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_threads: None,
            sort_successor_links: true,
            normalise_music: true,
        }
    }
}
