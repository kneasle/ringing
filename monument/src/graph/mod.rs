//! Creation and manipulation of composition graphs.  This implements routines for creating and
//! optimising such graphs, in preparation for performing tree search.

mod falseness;
mod graph;
pub mod optimise;

pub use graph::{Graph, Node};
