//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

pub mod calls;
pub mod spec;

use std::{
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::{Duration, Instant},
};

use bellframe::{
    mask::RegexToMaskError,
    place_not::{self, PnBlockParseError},
    InvalidRowError,
};
use log::{log, LogLevelFilter};
use monument::{Comp, Config};
use spec::Spec;

pub fn init_logging(log_level: LogLevelFilter) {
    pretty_logger::init(
        pretty_logger::Destination::Stderr,
        log_level,
        pretty_logger::Theme::default(),
    )
    .unwrap();
}

pub fn run(
    input_file: &Path,
    debug_print: Option<DebugPrint>,
    queue_limit: usize,
) -> Result<Option<QueryResult>, Error> {
    let start_time = Instant::now();

    /// If the user specifies a [`DebugPrint`] flag with e.g. `-d layout`, then debug print the
    /// corresponding value and exit.
    macro_rules! debug_print {
        ($variant: ident, $val: expr) => {
            if debug_print == Some(DebugPrint::$variant) {
                dbg!($val);
                return Ok(None);
            }
        };
    }

    // Generate & debug print the TOML file specifying the search
    let spec =
        Spec::read_from_file(input_file).map_err(|e| Error::SpecFile(input_file.to_owned(), e))?;
    debug_print!(Spec, spec);

    // Convert the `Spec` into a `Layout` and other data required for running a search
    log::info!("Generating `Layout`");
    let query = Arc::new(spec.lower(input_file)?);
    debug_print!(Query, query);
    debug_print!(Layout, &query.layout);

    // Generate config
    let mut config = Config {
        queue_limit,
        num_threads: Some(1),
        ..Config::default()
    };

    // Run query and handle its debug output
    let graph = query.unoptimised_graph();
    debug_print!(Graph, graph);
    let optimised_graphs = query.optimise_graph(graph, &mut config);
    if debug_print == Some(DebugPrint::StopBeforeSearch) {
        return Ok(None);
    }

    let comps = monument::search(query.clone(), optimised_graphs, &mut config);
    Ok(Some(QueryResult {
        comps,
        duration: Instant::now() - start_time,
    }))
}

#[derive(Debug, Clone)]
pub struct QueryResult {
    pub comps: Vec<Comp>,
    pub duration: Duration,
}

impl QueryResult {
    pub fn print(&self) {
        println!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", c.long_string());
        }
        println!("Search completed in {:?}", self.duration);
    }
}

/// The possible ways that a run of Monument could fail
#[derive(Debug)]
pub enum Error {
    NoMethods,
    CcLibNotFound,
    PartHeadParse(InvalidRowError),
    ChMaskParse(String, RegexToMaskError),
    SpecFile(PathBuf, spec::TomlReadError),
    MusicFile(PathBuf, spec::TomlReadError),
    MethodNotFound { suggestions: Vec<String> },
    CallPnParse(String, place_not::ParseError),
    MethodPnParse(PnBlockParseError),
    LeadLocationIndex(String, ParseIntError),
    LayoutGen(monument::layout::new::Error),
}

/// What item should be debug printed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugPrint {
    Spec,
    Query,
    Layout,
    Graph,
    /// Stop just before the search starts, to let the user see what's been printed out without
    /// scrolling
    StopBeforeSearch,
}

impl FromStr for DebugPrint {
    type Err = String;

    fn from_str(v: &str) -> Result<Self, String> {
        Ok(match v.to_lowercase().as_str() {
            "spec" => Self::Spec,
            "query" => Self::Query,
            "layout" => Self::Layout,
            "graph" => Self::Graph,
            "search" => Self::StopBeforeSearch,
            _ => {
                return Err(format!(
                    "Unknown value {:?}. Expected `spec`, `query`, `layout`, `graph` or `search`.",
                    v
                ))
            }
        })
    }
}
