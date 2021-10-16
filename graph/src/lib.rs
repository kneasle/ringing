//! Creation and manipulation of composition graphs.  This implements routines for creating and
//! optimising such graphs, in preparation for performing tree search.

mod falseness;
mod graph;
pub mod layout; // High-level description of the 'shape' of a [`Graph`]
pub mod music;
pub mod optimise;

use std::ops::Range;

pub use graph::{Graph, Node};
pub use layout::NodeId;

/// Data about a composition external to the node graph.  This can be used for lookup during
/// composing, and to inform optimisation decisions.
// TODO: Put this in its own file?
#[derive(Debug, Clone)]
pub struct Data {
    pub layout: layout::Layout,
    pub music_types: Vec<music::MusicType>,
    pub len_range: Range<usize>,
}

impl Data {
    pub fn unoptimised_graph(&self) -> Graph {
        // `- 1` on `len_range.end` makes sure that the length limit is an **inclusive** bound
        Graph::from_layout(&self.layout, &self.music_types, self.len_range.end - 1)
    }
}
