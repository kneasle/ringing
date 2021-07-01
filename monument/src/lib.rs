#![allow(rustdoc::private_intra_doc_links)]

use std::{ops::Range, sync::Arc};

use graph::ProtoGraph;

mod compose;
mod graph;
mod layout;
pub mod mask;
mod music;
pub mod single_method;

// Imports used solely by doctests
#[allow(unused_imports)]
use bellframe::Row;

// Top level re-exports for the convenience of people using this as a library (i.e. me)
pub use compose::{compose, Comp};
pub use layout::*;
pub use music::MusicType;

/// The `Spec`ification for a set of generated compositions.  This contains all the data which
/// directly impacts the composition list -- all other options are stored in [`Config`].
#[derive(Debug, Clone)]
pub struct Spec {
    /* COURSE LAYOUT */
    /// The course [`Layout`] describing the compositions' structure
    layout: Layout,
    /// The prototype graph, derived from the [`Layout`]
    prototype_graph: ProtoGraph,

    /* COMPOSITION ATTRIBUTES */
    /// The range of composition lengths that are permitted
    len_range: Range<usize>,
    /// The length of the output list of compositions.  Monument will not terminate until it
    /// produces the `num_comps` best compositions according to this `Spec`
    num_comps: usize,

    /* COMPOSITION RANKING */
    /// The types of music that `Monument` should use to rank compositions
    music_types: Vec<MusicType>,
    /// Normalise music scores by their length (i.e. rank by average music output)
    normalise_music: bool,
}

impl Spec {
    /// Creates a new composition specification
    pub fn new(
        layout: Layout,
        len_range: Range<usize>,
        num_comps: usize,
        music_types: Vec<MusicType>,
        normalise_music: bool,
        config: &Config,
    ) -> Arc<Self> {
        let prototype_graph = ProtoGraph::from_layout(&layout, &music_types, len_range.end, config);

        Arc::new(Self {
            layout,
            prototype_graph,
            len_range,
            num_comps,
            normalise_music,
            music_types,
        })
    }
}

/// General configuration parameters for Monument.  These will not change the resulting
/// compositions, instead changing how Monument operates (and therefore how quickly and in what
/// order the compositions are searched).
#[derive(Debug, Clone)]
pub struct Config {
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    pub num_threads: Option<usize>,
    /// Sort succession links so that likely musical nodes are explored first
    pub sort_successor_links: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_threads: None,
            sort_successor_links: true,
        }
    }
}
