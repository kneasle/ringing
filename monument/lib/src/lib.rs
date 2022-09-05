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
//! Monument is a **composing engine**.  This means it is designed to run
//! [`Search`](search::Search)es to find [`Composition`]s which satisfy some set of constraints
//! (length, methods/calls used, etc.).  One can also describe what features make a [`Composition`]
//! 'good' and Monument will attempt to maximise them.
//!
//! Unlike other composition generators such as SMC, Monument does not attempt to exhaustively
//! search the space of possible compositions.  Instead, it aims to generate very good compositions
//! as quickly as possible, but making no guarantee that they will be optimal.  This trade-off
//! works well: for common searches, Monument is **orders of magnitude** faster than SMC at
//! producing compositions that you might want to ring.  Anyway, most interesting search spaces
//! can't be exhausted in the time left in the universe, so waiting a few minutes for very good
//! (but maybe not optimal) compositions seems like a very good deal.
//!
//! Composing in general is so hard ([NP-hard](https://en.wikipedia.org/wiki/NP-hardness), in fact)
//! that it's impossible to have an engine that is both consistently fast and guarantees
//! optimality.  Thus, any promise of speed is a best-effort not a guarantee, and there will always
//! be searches which are too complex for Monument to handle.  Such must be true of any engine.
//!
//! # Status
//!
//! This library is roughly in **alpha** stage of readiness.  Most pieces are working, but there
//! are a few major points that need addressing before Monument can be embedded in other projects
//! without major pain:
//!
//! 1. The API is very much work-in-progress.  The critical issue here is that the CLI is the only
//!    consumer of Monument's API, so I need to try embedding it so I can get a proper feel for
//!    what the library feels like to use.  The API will likely become increasingly stable until
//!    v1.0.0 is reached, at which point backward compatibility will be guaranteed.
//! 2. Currently, Monument's search routine uses potentially unbounded amounts of memory.  It is
//!    possible to control the usage using the [`Config::queue_limit`](search::Config::queue_limit)
//!    parameter, but this is at best a proxy for memory usage.  Soon™, `queue_limit` will be
//!    replaced with a proper memory limit: the search routine will set `queue_limit` internally to
//!    make sure the memory usage is bounded.
// TODO: Add example

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links, rustdoc::private_intra_doc_links)]

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
