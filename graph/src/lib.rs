//! Creation and manipulation of composition graphs.

mod falseness;
mod graph;
pub mod layout; // High-level description of the 'shape' of a [`Graph`]
pub mod music;
pub mod pass;

pub use graph::{Graph, Node};
pub use layout::NodeId;
pub use pass::passes;
