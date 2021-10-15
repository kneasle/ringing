//! Creation and manipulation of composition graphs.

mod falseness;
mod graph;
pub mod layout; // High-level description of the 'shape' of a [`Graph`]
pub mod music;

pub use graph::{Graph, Node};
