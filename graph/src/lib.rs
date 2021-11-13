//! Creation and manipulation of composition graphs.  This implements routines for creating and
//! optimising such graphs, in preparation for performing tree search.

mod falseness;
mod graph;
pub mod music;
pub mod optimise;

use std::ops::Range;

use bellframe::RowBuf;
pub use graph::{Graph, Node, Rotation};
pub use monument_layout::NodeId;

/// Data about a composition external to the node graph.  This can be used for lookup during
/// composing, and to inform optimisation decisions.
// TODO: Put this in its own file?
#[derive(Debug, Clone)]
pub struct Data {
    pub layout: monument_layout::Layout,
    pub music_types: Vec<music::MusicType>,
    pub part_head: RowBuf,
    pub len_range: Range<usize>,
    pub method_count_range: Range<usize>,
    pub num_comps: usize,
}

impl Data {
    pub fn unoptimised_graph(&self) -> Graph {
        // `- 1` on `len_range.end` makes sure that the length limit is an **inclusive** bound
        Graph::from_layout(&self.layout, &self.music_types, self.len_range.end - 1)
    }
}
