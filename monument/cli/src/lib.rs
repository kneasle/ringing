//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

#![deny(rustdoc::broken_intra_doc_links, rustdoc::private_intra_doc_links)]

pub mod args;
pub mod calls;
pub mod logging;
pub mod music;
pub mod spec;
pub mod utils;

use std::{
    path::Path,
    str::FromStr,
    sync::Arc,
    time::{Duration, Instant},
};

use log::LevelFilter;
use monument::{Composition, Search};
use ordered_float::OrderedFloat;
use ringing_utils::PrettyDuration;
use simple_logger::SimpleLogger;
use spec::Spec;

pub fn init_logging(filter: LevelFilter) {
    SimpleLogger::new()
        .without_timestamps()
        .with_colors(true)
        .with_level(filter)
        .init()
        .unwrap();
}

pub fn run(
    toml_path: &Path,
    options: &args::Options,
    env: Environment,
) -> anyhow::Result<Option<QueryResult>> {
    /// If the user specifies a [`DebugPrint`] flag with e.g. `-D layout`, then debug print the
    /// corresponding value and exit.
    macro_rules! debug_print {
        ($variant: ident, $val: expr) => {
            if options.debug_option == Some(DebugOption::$variant) {
                dbg!($val);
                return Ok(None);
            }
        };
    }

    let start_time = Instant::now();

    // Generate & debug print the TOML file specifying the search
    let spec = Spec::new(toml_path)?;
    debug_print!(Spec, spec);
    // If running in CLI mode, don't `drop` any of the search data structures, since Monument will
    // exit shortly after the search terminates.  With the `Arc`-based data structures, this is
    // seriously beneficial - it shaves many seconds off Monument's total running time.
    let leak_search_memory = env == Environment::Cli;
    // Convert the `Spec` into a `Layout` and other data required for running a search
    let (search, music_displays) = spec.lower(toml_path, options, leak_search_memory)?;
    debug_print!(Query, search);

    // Build all the data structures for the search
    let comp_printer = self::logging::CompositionPrinter::new(
        music_displays,
        search.clone(),
        spec.duffers_specified(),
    );
    let mut update_logger = self::logging::SingleLineProgressLogger::new(comp_printer.clone());

    if options.debug_option == Some(DebugOption::StopBeforeSearch) {
        return Ok(None);
    }

    // In CLI mode, attach `ctrl-C` to the abort flag
    if env == Environment::Cli {
        let search = Arc::clone(&search);
        if let Err(e) = ctrlc::set_handler(move || search.signal_abort()) {
            log::warn!("Error setting ctrl-C handler: {}", e);
        }
    }

    // Run the search, collecting the compositions as the search runs
    let mut comps = Vec::<Composition>::new();
    search.run(|update| {
        if let Some(comp) = update_logger.log(update) {
            comps.push(comp);
        }
    });

    // Once the search has completed, sort the compositions and return
    fn rounded_float(f: f32) -> OrderedFloat<f32> {
        const FACTOR: f32 = 1e-6;
        let rounded = (f / FACTOR).round() * FACTOR;
        OrderedFloat(rounded)
    }
    comps.sort_by_key(|comp| {
        (
            rounded_float(comp.music_score()),
            rounded_float(comp.average_score()),
            comp.call_string(),
        )
    });
    Ok(Some(QueryResult {
        comps,
        comp_printer,
        duration: start_time.elapsed(),
        aborted: search.was_aborted(),

        search,
    }))
}

/// How this instance of Monument is being run
#[derive(Debug, PartialEq, Eq)]
pub enum Environment {
    /// Being run by the test harness as a test case
    TestHarness,
    /// Being run by the CLI
    Cli,
}

#[derive(Debug, Clone)]
pub struct QueryResult {
    pub comps: Vec<Composition>,
    pub search: Arc<Search>,
    pub duration: Duration,
    pub aborted: bool,

    comp_printer: self::logging::CompositionPrinter,
}

impl QueryResult {
    pub fn print(&mut self) {
        eprintln!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", self.comp_printer.comp_string_with_headers(c));
        }
        println!("{}", self.comp_printer.footer_lines());
        eprintln!(
            "{} compositions generated{} {}",
            self.comps.len(),
            match self.aborted {
                true => ", aborted after",
                false => " in",
            },
            PrettyDuration(self.duration)
        );
    }
}

/// What item should be debug printed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugOption {
    Spec,
    Query,
    Graph,
    /// Stop just before the search starts, to let the user see what's been printed out without
    /// scrolling
    StopBeforeSearch,
}

impl FromStr for DebugOption {
    type Err = String;

    fn from_str(v: &str) -> Result<Self, String> {
        Ok(match v.to_lowercase().as_str() {
            "spec" => Self::Spec,
            "query" => Self::Query,
            "graph" => Self::Graph,
            "no-search" => Self::StopBeforeSearch,
            #[rustfmt::skip] // See https://github.com/rust-lang/rustfmt/issues/5204
            _ => return Err(format!(
                "Unknown value {:?}. Expected `spec`, `query`, `layout`, `graph` or `no-search`.",
                v
            )),
        })
    }
}
