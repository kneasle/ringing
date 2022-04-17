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
    path::Path,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    time::{Duration, Instant},
};

use log::{log_enabled, LevelFilter};
use monument::{Comp, Config, Progress, Query, QueryUpdate};
use ordered_float::OrderedFloat;
use ringing_utils::{BigNumInt, PrettyDuration};
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
    source: Source,
    debug_option: Option<DebugOption>,
    config: &Config,
    ctrl_c_behaviour: CtrlCBehaviour,
) -> anyhow::Result<Option<QueryResult>> {
    /// If the user specifies a [`DebugPrint`] flag with e.g. `-D layout`, then debug print the
    /// corresponding value and exit.
    macro_rules! debug_print {
        ($variant: ident, $val: expr) => {
            if debug_option == Some(DebugOption::$variant) {
                dbg!($val);
                return Ok(None);
            }
        };
    }

    let start_time = Instant::now();

    // Generate & debug print the TOML file specifying the search
    let spec = Spec::from_source(source)?;
    debug_print!(Spec, spec);

    // Convert the `Spec` into a `Layout` and other data required for running a search
    log::debug!("Generating query");
    let query = spec.lower(source)?;
    debug_print!(Query, query);
    debug_print!(Layout, &query.layout);

    // Optimise the graph(s)
    let graph = query
        .unoptimised_graph(config)
        .map_err(|e| anyhow::Error::msg(graph_build_err_msg(e)))?;
    debug_print!(Graph, graph);
    let optimised_graphs = query.optimise_graph(graph, config);
    if debug_option == Some(DebugOption::StopBeforeSearch) {
        return Ok(None);
    }

    // Thread-safe data for the query engine
    let abort_flag = Arc::new(AtomicBool::new(false));
    let comps = Arc::new(Mutex::new(Vec::<Comp>::new()));
    let mut update_logger = SingleLineProgressLogger::new();

    // Run the search
    if ctrl_c_behaviour == CtrlCBehaviour::RecoverComps {
        let abort_flag = abort_flag.clone();
        let handler = move || abort_flag.store(true, Ordering::Relaxed);
        if let Err(e) = ctrlc::set_handler(handler) {
            log::warn!("Error capturing ctrl-C: {}", e);
        }
    }
    let on_find_comp = {
        let comps = comps.clone();
        move |update| {
            if let Some(comp) = update_logger.log(update) {
                comps.lock().unwrap().push(comp);
            }
        }
    };
    Query::search(
        Arc::new(query),
        optimised_graphs,
        config,
        on_find_comp,
        abort_flag.clone(),
    );

    // Recover and sort the compositions, then return the query
    let mut comps = comps.lock().unwrap().to_vec();
    comps.sort_by_key(|comp| {
        (
            OrderedFloat(comp.music_score()),
            OrderedFloat(comp.avg_score),
        )
    });
    Ok(Some(QueryResult {
        comps,
        duration: start_time.elapsed(),
        aborted: abort_flag.load(Ordering::SeqCst),
    }))
}

/// The `Source` of the TOML that Monument should read.  In nearly all cases, this will be loaded
/// from a [`Path`].  For the test runner it's useful to be able to run Monument on strings that
/// aren't loaded from a specific file, so for this we have the [`Str`](Self::Str) variant.
#[derive(Debug, Clone, Copy)]
pub enum Source<'a> {
    /// The TOML 'spec' file should be loaded from this path
    Path(&'a Path),
    /// The TOML should be read directly from a string
    Str {
        spec: &'a str,
        music_file: Option<&'a str>,
    },
}

/// How this query run should handle `Ctrl-C`.  This is usually
/// [`RecoverComps`](Self::RecoverComps) when running as a stand-alone command and
/// [`TerminateProcess`](Self::TerminateProcess) when running in the unit tests.
#[derive(Debug, PartialEq, Eq)]
pub enum CtrlCBehaviour {
    /// Terminate the process instantly, without attempting to recover the compositions
    TerminateProcess,
    /// Capture the `Ctrl-C`, abort the search, and print the comps
    RecoverComps,
}

#[derive(Debug, Clone)]
pub struct QueryResult {
    pub comps: Vec<Comp>,
    pub duration: Duration,
    pub aborted: bool,
}

impl QueryResult {
    pub fn print(&self) {
        println!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", c);
        }
        println!(
            "{} compositions generated.  Search {} {}",
            self.comps.len(),
            match self.aborted {
                true => "aborted after",
                false => "completed in",
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
            "no-search" => Self::StopBeforeSearch,
            #[rustfmt::skip] // See https://github.com/rust-lang/rustfmt/issues/5204
            _ => return Err(format!(
                "Unknown value {:?}. Expected `spec`, `query`, `layout`, `graph` or `no-search`.",
                v
            )),
        })
    }
}

////////////////////
// ERROR MESSAGES //
////////////////////

fn graph_build_err_msg(e: monument::graph::BuildError) -> String {
    use monument::graph::BuildError as BE;
    match e {
        BE::SizeLimit(limit) => format!(
            "Graph size limit of {} nodes reached.  You can set it higher with `--graph-size-limit <n>`.",
            limit
        )
    }
}

/////////////////////////
// SINGLE LINE LOGGING //
/////////////////////////

/// Struct which handles logging updates, keeping the updates to a single line which updates as the
/// search progresses.
struct SingleLineProgressLogger {
    last_progress: Progress,
    is_truncating_queue: bool,
    is_aborting: bool,
    /// The number of characters in the last line we printed.  `UpdateLogger` will use this add
    /// enough spaces to the end of the next message to completely overwrite the last one
    last_line_length: usize,
}

impl SingleLineProgressLogger {
    fn new() -> Self {
        Self {
            last_progress: Progress::START,
            is_truncating_queue: false,
            is_aborting: false,
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
            update_string.push_str(&c.to_string());
            update_string.push('\n');
        }
        self.append_progress_string(&mut update_string);
        let update_string = self.extend_string(&update_string);

        let std_out = std::io::stdout();
        let mut std_out = std_out.lock();
        // We precede with a carriage return to make sure that we overwrite anything the user
        // types (e.g. `^C`).  We don't do anything in the case that the user's input is longer
        // than what we're writing - we'll just assume that no-one would be able to type that much
        // in between updates (which happen many many times per second).
        write!(std_out, "\r{}\r", update_string).unwrap();
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
            QueryUpdate::Aborting => {
                self.is_truncating_queue = false;
                self.is_aborting = true;
            }
        }
        None
    }

    /// Append a progress summary to some [`String`] buffer
    fn append_progress_string(&self, buf: &mut String) {
        let p = &self.last_progress;
        write!(
            buf,
            "    {} iters, {} items in queue, avg/max len {:.0}/{}",
            BigNumInt(p.iter_count),
            BigNumInt(p.queue_len),
            p.avg_length,
            p.max_length
        )
        .unwrap();
        buf.push_str(match (self.is_aborting, self.is_truncating_queue) {
            (false, false) => "",
            (true, false) => ".  Aborting...",
            (false, true) => ".  Truncating queue...",
            (true, true) => unreachable!("Must either be aborting or truncating queue"),
        });
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
