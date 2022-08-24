//! Monument is a fast and embeddable composing engine for
//! [change ringing](https://en.wikipedia.org/wiki/Change_ringing).
//!
//! This crate aims to provide a convenient API for embedding Monument's core engine into larger
//! pieces of software.  Monument also has a command-line interface, provided by a separate crate
//! `monument_cli` which reads queries from TOML files, passes them to this library and prints the
//! resulting compositions to the console.
//!
//! # Description
//!
//! Given a [`Query`](query::Query) describing constraints, Monument will run a [`Search`] to find
//! [`Composition`]s which satisfy the constraints of the query.  The query also describes what
//! features make a composition 'good' and Monument will attempt to maximise that.
//!
//! Unlike other composition generators such as SMC, Monument does not attempt to exhaustively
//! search the space of possible compositions.  Instead, it aims to generate very good compositions
//! as quickly as possible, but making no guarantee that they will be optimal.  This trade-off
//! works well: for common searches, Monument is **orders of magnitude** faster than SMC at
//! producing compositions that you might want to ring.  Anyway, most interesting search spaces
//! can't be exhausted in the time left in the universe, so waiting a few minutes for very good
//! (but maybe not optimal) compositions seems like a very good deal.
//!
//! In fact, composing in general is so hard (NP-hard, in fact) that it's impossible to have an
//! engine that is both consistently fast and guarantees optimality.  Thus, any promise of speed is
//! a best-effort not a guarantee, and there will always be searches which are too complex for
//! Monument to handle.  Such must be true of any engine.
//!
//! **NOTE:** Monument's API is still very much work-in-progress.  Parts like the
//! [`QueryBuilder`](query::QueryBuilder) API need work, but to make progress I need more of an
//! idea of how those APIs will be used.
// TODO: Add example

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
pub use search::{Config, Progress, Search, Update};
