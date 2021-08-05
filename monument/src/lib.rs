#![allow(rustdoc::private_intra_doc_links)]

mod compose;
mod graph;
pub mod mask;
mod music;
mod score;
pub mod spec; // Code to allow inputting of composition queries
mod stats;

// Imports used solely by doctests
#[allow(unused_imports)]
use bellframe::Row;

// Top level re-exports for the convenience of people using this as a library (i.e. me)
pub use compose::{compose, Comp, SearchResults};
pub use music::MusicType;
pub use score::Score;
