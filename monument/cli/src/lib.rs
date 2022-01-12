//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod calls;
pub mod spec;

use std::{
    fmt::Write,
    io::Write as IoWrite,
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{atomic::AtomicBool, Arc, Mutex},
    time::{Duration, Instant},
};

use bellframe::{
    place_not::{self, PnBlockParseError},
    InvalidRowError,
};
use log::{log_enabled, LevelFilter};
use monument::{Comp, Config, Progress, Query, QueryUpdate};
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
    input_file: &Path,
    debug_print: Option<DebugOption>,
    queue_limit: usize,
) -> Result<Option<QueryResult>, Error> {
    let start_time = Instant::now();

    /// If the user specifies a [`DebugPrint`] flag with e.g. `-d layout`, then debug print the
    /// corresponding value and exit.
    macro_rules! debug_print {
        ($variant: ident, $val: expr) => {
            if debug_print == Some(DebugOption::$variant) {
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
    log::debug!("Generating query");
    let query = spec.lower(input_file)?;
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
    if debug_print == Some(DebugOption::StopBeforeSearch) {
        return Ok(None);
    }

    // Print comps as they are generated
    let comps = Arc::new(Mutex::new(Vec::<Comp>::new()));
    let comps_for_closure = comps.clone();
    let mut update_logger = UpdateLogger::new();
    Query::search(
        Arc::new(query),
        optimised_graphs,
        &config,
        move |update| {
            if let Some(comp) = update_logger.log(update) {
                comps_for_closure.lock().unwrap().push(comp);
            }
        },
        // User can abort with ctrl-C, so we don't need to use the abort flag
        Arc::new(AtomicBool::new(false)),
    );

    let mut comps = comps.lock().unwrap().to_vec();
    comps.sort_by_key(|comp| comp.avg_score);
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
    SpecFile(PathBuf, spec::TomlReadError),
    MusicFile(PathBuf, spec::TomlReadError),

    PartHeadParse(InvalidRowError),
    ChMaskParse(String, bellframe::mask::ParseError),
    ChPatternParse(String, bellframe::mask::ParseError),

    NoMethods,
    CcLibNotFound,
    MethodNotFound { suggestions: Vec<String> },
    MethodPnParse(PnBlockParseError),

    CallPnParse(String, place_not::ParseError),
    LeadLocationIndex(String, ParseIntError),

    LayoutGen(monument::layout::new::Error),
}

/// What item should be debug printed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugOption {
    Spec,
    Query,
    Layout,
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

/// Struct which stores the state required to maintain a nice logging interface
struct UpdateLogger {
    last_progress: Progress,
    is_truncating_queue: bool,
    /// The number of characters in the last line we printed.  `UpdateLogger` will use this add
    /// enough spaces to the end of the next message to completely overwrite the last one
    last_line_length: usize,
}

impl UpdateLogger {
    fn new() -> Self {
        Self {
            last_progress: Progress::START,
            is_truncating_queue: false,
            last_line_length: 0,
        }
    }

    fn log(&mut self, update: QueryUpdate) -> Option<Comp> {
        // Early return if we can't log anything, making sure to still keep the composition
        if !log_enabled!(log::Level::Info) {
            return match update {
                QueryUpdate::Comp(c) => Some(c),
                _ => None,
            };
        }

        let comp = self.update_progress(update);

        // Decide what string we're going to print.  This may have multiple lines (if a comp was
        // generated).
        let mut update_string = String::new();
        if let Some(c) = &comp {
            update_string.push_str(&c.long_string());
            update_string.push('\n');
        }
        self.append_progress_string(&mut update_string);
        let update_string = self.extend_string(&update_string);

        let std_out = std::io::stdout();
        let mut std_out = std_out.lock();
        write!(std_out, "{}\r", update_string).unwrap();
        std_out.flush().unwrap(); // `std_out` won't flush by default without a newline

        comp
    }

    /// Given a new update, update `self` and return the [`Comp`] (if one has just been generated)
    fn update_progress(&mut self, update: QueryUpdate) -> Option<Comp> {
        match update {
            QueryUpdate::Comp(comp) => return Some(comp),
            QueryUpdate::Progress(progress) => {
                self.last_progress = progress;
                self.is_truncating_queue = false;
            }
            QueryUpdate::TruncatingQueue => self.is_truncating_queue = true,
        }
        None
    }

    /// Append a progress summary to some [`String`] buffer
    fn append_progress_string(&self, buf: &mut String) {
        let p = &self.last_progress;
        write!(
            buf,
            "    {} iters, {} items in queue, avg/max len {:.0}/{}",
            p.iter_count, p.queue_len, p.avg_length, p.max_length
        )
        .unwrap();
        if self.is_truncating_queue {
            buf.push_str(".  Truncating queue...");
        }
    }

    /// Add whitespace to the end of a string to make sure it will cover the last thing we printed.
    /// This updates `self.last_update_length` for the next update.
    fn extend_string(&mut self, s: &str) -> String {
        let (first_line, other_lines) = match s.split_once('\n') {
            Some((f, o)) => (f, Some(o)),
            None => (s, None),
        };
        let num_spaces = self.last_line_length.saturating_sub(first_line.len());

        let mut output = String::new();
        output.push_str(first_line);
        output.extend(std::iter::repeat(' ').take(num_spaces)); // Add spaces to the first line
        if let Some(o) = other_lines {
            output.push('\n');
            output.push_str(o);
        }

        // Set `self.last_update_length` for next time.  `self.last_update_length` is the length of
        // the last line of `output` (the `+ 1` gives us the index **after** the '\n')
        self.last_line_length = output.len() - output.rfind('\n').map_or(0, |n| n + 1);

        output
    }
}
