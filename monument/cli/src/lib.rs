//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

#![deny(rustdoc::broken_intra_doc_links, rustdoc::private_intra_doc_links)]

pub mod args;
pub mod calls;
pub mod logging;
pub mod music;
pub mod toml_file;
pub mod utils;

use std::{
    path::Path,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use log::LevelFilter;
use monument::{Composition, Search};
use ordered_float::OrderedFloat;
use ringing_utils::PrettyDuration;
use simple_logger::SimpleLogger;
use toml_file::TomlFile;

use crate::logging::{CompositionPrinter, SingleLineProgressLogger};

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
) -> anyhow::Result<Option<SearchResult>> {
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
    let toml_file = TomlFile::new(toml_path)?;
    debug_print!(Toml, toml_file);
    // If running in CLI mode, don't `drop` any of the search data structures, since Monument will
    // exit shortly after the search terminates.  With the `Arc`-based data structures, this is
    // seriously beneficial - it shaves many seconds off Monument's total running time.
    let leak_search_memory = env == Environment::Cli;
    // Convert the `TomlFile` into a `Layout` and other data required for running a search
    let (params, music_displays) = toml_file.to_params(toml_path)?;
    debug_print!(Params, params);
    // Build the search
    let search = Arc::new(Search::new(
        params,
        toml_file.config(options, leak_search_memory),
    )?);
    debug_print!(Search, search);

    // Build all the data structures for the search
    let comp_printer = CompositionPrinter::new(
        music_displays,
        search.clone(),
        toml_file.should_print_atw(),
        !options.dont_display_comp_numbers,
    );
    let mut update_logger = SingleLineProgressLogger::new(match options.only_display_update_line {
        true => None,
        false => Some(comp_printer.clone()),
    });

    if options.debug_option == Some(DebugOption::StopBeforeSearch) {
        return Ok(None);
    }

    // In CLI mode, attach `ctrl-C` to the abort flag
    let abort_flag = Arc::new(AtomicBool::new(false));
    if env == Environment::Cli {
        let abort_flag = Arc::clone(&abort_flag);
        if let Err(e) = ctrlc::set_handler(move || abort_flag.store(true, Ordering::SeqCst)) {
            log::warn!("Error setting ctrl-C handler: {}", e);
        }
    }

    // Run the search, collecting the compositions as the search runs
    let mut comps = Vec::<Composition>::new();
    search.run(
        |update| {
            if let Some(comp) = update_logger.log(update) {
                comps.push(comp);
            }
        },
        &abort_flag,
    );

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
    Ok(Some(SearchResult {
        comps,
        comp_printer,
        duration: start_time.elapsed(),
        aborted: abort_flag.load(Ordering::SeqCst),

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
pub struct SearchResult {
    pub comps: Vec<Composition>,
    pub search: Arc<Search>,
    pub duration: Duration,
    pub aborted: bool,

    comp_printer: self::logging::CompositionPrinter,
}

impl SearchResult {
    pub fn print(&mut self) {
        eprintln!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", self.comp_printer.comp_string_with_possible_headers(c));
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
    Toml,
    Params,
    Search,
    Graph,
    /// Stop just before the search starts, to let the user see what's been printed out without
    /// scrolling
    StopBeforeSearch,
}

impl FromStr for DebugOption {
    type Err = String;

    fn from_str(v: &str) -> Result<Self, String> {
        Ok(match v.to_lowercase().as_str() {
            "toml" => Self::Toml,
            "params" => Self::Params,
            "search" => Self::Search,
            "graph" => Self::Graph,
            "no-search" => Self::StopBeforeSearch,
            #[rustfmt::skip] // See https://github.com/rust-lang/rustfmt/issues/5204
            _ => return Err(format!(
                "Unknown value {:?}. Expected `toml`, `params`, `search`, `graph` or `no-search`.",
                v
            )),
        })
    }
}
