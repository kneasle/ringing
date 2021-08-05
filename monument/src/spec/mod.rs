//! Module containing code that allows the user of the library to [`Spec`]ify composition queries

use std::{ops::Range, sync::Arc};

use log::LevelFilter;

use crate::{graph2::ProtoGraph, MusicType};

pub use layout::Layout;

pub mod layout;
pub mod single_method; // Helper method to generate a `layout` for a single method composition

/// The `Spec`ification for a set of generated compositions.  This contains all the data which
/// directly impacts the composition list -- all other options are stored in [`Config`].
#[derive(Debug, Clone)]
pub struct Spec {
    /* COURSE LAYOUT */
    /// The course [`Layout`] describing the compositions' structure
    pub(crate) layout: Layout,
    /// The prototype graph, derived from the [`Layout`]
    pub(crate) prototype_graph: ProtoGraph,

    /* COMPOSITION ATTRIBUTES */
    /// The range of composition lengths that are permitted
    pub(crate) len_range: Range<usize>,
    /// The length of the output list of compositions.  Monument will not terminate until it
    /// produces the `num_comps` best compositions according to this `Spec`
    pub(crate) num_comps: usize,

    /* COMPOSITION RANKING */
    /// The types of music that `Monument` should use to rank compositions
    pub(crate) music_types: Vec<MusicType>,
    /// Normalise music scores by their length (i.e. rank by average music output)
    pub(crate) normalise_music: bool,
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
            music_types,
            normalise_music,
        })
    }

    /// Gets the [`Layout`] of this `Spec`
    #[inline]
    pub fn layout(&self) -> &Layout {
        &self.layout
    }
}

/// How to combine the costs of each of the paths out of a given node (when sorting successor
/// links).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum SuccSortStrat {
    Average,
    Max,
}

/// General configuration parameters for Monument.  These will not change the resulting
/// compositions, instead changing how Monument operates (and therefore how quickly and in what
/// order the compositions are searched).
#[derive(Debug, Clone)]
pub struct Config {
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    ///
    /// Defaults to `None`.
    pub num_threads: Option<usize>,
    /// What logging level Monument should use
    pub log_level: LevelFilter,

    /// How many nodes' lookahead Monument will use to sort the successor of each node.  Sorting
    /// these links has no impact on the DFS search speed but makes Monument explore potentially
    /// good branches first.  If set to `0`, then the links will not be sorted.  There is a balance
    /// here - if this is too short then then Monument will ignore slightly slow linking courses,
    /// but too long and the results will be too noisy to be useful.
    ///
    /// Defaults to `2`.
    ///
    /// The purpose of this is to make Monument prefer sticking to the 'hot' music paths, getting
    /// some good compositions quickly before moving on to less obviously good parts of the search
    /// space.  If this is paired with music-prediction pruning, then getting good comps quickly
    /// corresponds directly to better pruning and therefore faster termination.
    pub successor_link_sort_depth: usize,
    /// What strategy Monument will use to combine the path scores when sorting successor links.
    ///
    /// Defaults to [`SuccSortStrat::Max`].
    pub successor_link_sort_strategy: SuccSortStrat,

    /// The maximum number of compositions which can be stored in the channel over which
    /// compositions are sent from the worker threads to the main thread
    pub comp_buffer_length: usize,
    /// The maximum number of stats updates which can be stored in the channel
    pub stats_buffer_length: usize,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_threads: None,
            log_level: LevelFilter::Info,
            successor_link_sort_depth: 2,
            successor_link_sort_strategy: SuccSortStrat::Max,
            comp_buffer_length: 1000,
            stats_buffer_length: 1000,
        }
    }
}
