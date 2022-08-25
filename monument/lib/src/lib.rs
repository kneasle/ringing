//! Core library for Monument, a fast and flexible composing engine.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

mod composition;
mod error;
mod graph;
mod group;
mod prove_length;
pub mod query;
mod search;
mod utils;

pub use composition::Composition;
pub use error::{Error, Result};
pub use search::{Config, Progress, SearchData, SearchUpdate};
