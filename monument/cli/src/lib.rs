//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod args;
pub mod calls;
pub mod music;
pub mod spec;
pub mod utils;

use std::{
    fmt::Write,
    io::Write as IoWrite,
    ops::RangeInclusive,
    path::PathBuf,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use bellframe::row::ShortRow;
use itertools::Itertools;
use log::{log_enabled, LevelFilter};
use monument::{
    query::Query,
    search::{Progress, SearchData, SearchUpdate},
    Composition,
};
use music::MusicDisplay;
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
    let spec = Spec::from_source(&source)?;
    debug_print!(Spec, spec);
    // Convert the `Spec` into a `Layout` and other data required for running a search
    let (query, music_displays) = spec.lower(&source)?;
    debug_print!(Query, query);

    let mut config = spec.config(options);
    // If running in CLI mode, don't `drop` any of the search data structures, since Monument will
    // exit shortly after the search terminates.  With the `Arc`-based data structures, this is
    // seriously beneficial - it shaves many seconds off Monument's total running time.
    config.leak_search_memory = env == Environment::Cli;

    // Build all the data structures for the search
    let search = SearchData::new(&query, &config)?;
    let comp_printer =
        CompPrinter::new(query.clone(), music_displays, search.method_count_ranges());
    let mut update_logger = SingleLineProgressLogger::new(comp_printer.clone());

    if options.debug_option == Some(DebugOption::StopBeforeSearch) {
        return Ok(None);
    }

    // In CLI mode, attach `ctrl-C` to the abort flag
    let abort_flag = Arc::new(AtomicBool::new(false));
    if env == Environment::Cli {
        let abort_flag = abort_flag.clone();
        let handler = move || abort_flag.store(true, Ordering::Relaxed);
        if let Err(e) = ctrlc::set_handler(handler) {
            log::warn!("Error setting ctrl-C handler: {}", e);
        }
    }
    // Set up a callback to log then store the compositions
    let mut comps = Vec::<Composition>::new();
    let update_fn = |update| {
        if let Some(comp) = update_logger.log(update) {
            comps.push(comp);
        }
    };
    // Run the search
    search.search(abort_flag.clone(), update_fn);
    use ordered_float::OrderedFloat as OF;
    comps.sort_by_key(|comp| (OF(comp.music_score(&query)), OF(comp.average_score())));
    Ok(Some(QueryResult {
        comps,
        query,
        comp_printer,
        duration: start_time.elapsed(),
        aborted: abort_flag.load(Ordering::SeqCst),
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

/// The `Source` of the TOML that Monument should read.  In nearly all cases, this will be loaded
/// from the file a given [`Path`](std::path::Path).  For the test runner it's useful to be able to
/// run Monument on strings that aren't loaded from a specific file, so for this we have the
/// [`Str`](Self::Str) variant.
#[derive(Debug, Clone)]
pub enum Source<'a> {
    /// The TOML 'spec' file should be loaded from this path
    Path(PathBuf),
    /// The TOML should be read directly from a string
    Str {
        spec: &'a str,
        music_file: Option<&'a str>,
    },
}

#[derive(Debug, Clone)]
pub struct QueryResult {
    pub comps: Vec<Composition>,
    pub query: Arc<Query>,
    pub duration: Duration,
    pub aborted: bool,

    comp_printer: CompPrinter,
}

impl QueryResult {
    pub fn print(&self) {
        println!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", self.comp_printer.comp_string(c));
        }
        println!("{}", self.comp_printer.ruleoff());
        println!("{}", self.comp_printer.header());
        println!(
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

/////////////////////////
// SINGLE LINE LOGGING //
/////////////////////////

/// Struct which handles logging updates, keeping the updates to a single line which updates as the
/// search progresses.
struct SingleLineProgressLogger {
    comp_printer: CompPrinter,
    is_first_comp: bool,

    last_progress: Progress,
    is_aborting: bool,
    /// The number of characters in the last line we printed.  `UpdateLogger` will use this add
    /// enough spaces to the end of the next message to completely overwrite the last one
    last_line_length: usize,
}

impl SingleLineProgressLogger {
    fn new(comp_printer: CompPrinter) -> Self {
        Self {
            comp_printer,
            is_first_comp: true,

            last_progress: Progress::START,
            is_aborting: false,
            last_line_length: 0,
        }
    }

    fn log(&mut self, update: SearchUpdate) -> Option<Composition> {
        // Early return if we can't log anything, making sure to still keep the composition
        if !log_enabled!(log::Level::Info) {
            return match update {
                SearchUpdate::Comp(c) => Some(c),
                _ => None,
            };
        }

        let comp = self.update_progress(update);

        // Decide what string we're going to print.  This may have multiple lines (if a comp was
        // generated).
        let mut update_string = String::new();
        if let Some(c) = &comp {
            if self.is_first_comp {
                update_string.push_str(&self.comp_printer.header());
                update_string.push('\n');
                update_string.push_str(&self.comp_printer.ruleoff());
                update_string.push('\n');
                self.is_first_comp = false;
            }
            update_string.push_str(&self.comp_printer.comp_string(c));
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

    /// Given a new update, update `self` and return the [`Composition`] (if one has just been
    /// generated)
    fn update_progress(&mut self, update: SearchUpdate) -> Option<Composition> {
        match update {
            SearchUpdate::Comp(comp) => return Some(comp),
            SearchUpdate::Progress(progress) => self.last_progress = progress,
            SearchUpdate::Aborting => self.is_aborting = true,
        }
        None
    }

    /// Append a progress summary to some [`String`] buffer
    fn append_progress_string(&self, buf: &mut String) {
        let p = &self.last_progress;
        write!(
            buf,
            "    {} iters, {} comps :: {} items in queue, avg/max len {:.0}/{}",
            BigNumInt(p.iter_count),
            BigNumInt(p.num_comps),
            BigNumInt(p.queue_len),
            p.avg_length,
            p.max_length
        )
        .unwrap();
        buf.push_str(
            match (self.is_aborting, self.last_progress.truncating_queue) {
                (false, false) => "",
                (true, false) => ".  Aborting...",
                (false, true) => ".  Truncating queue...",
                (true, true) => unreachable!("Must either be aborting or truncating queue"),
            },
        );
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

#[derive(Debug, Clone)]
struct CompPrinter {
    query: Arc<Query>,
    music_displays: Vec<MusicDisplay>,

    /// The maximum width of a composition's (total) length
    length_width: usize,
    /// For each method in the composition:
    /// ```text
    /// (
    ///     maximum width of row count,
    ///     shorthand
    /// )
    /// ```
    method_counts: Vec<(usize, String)>,
    /// If a part head should be displayed, then what's its width
    part_head_width: Option<usize>,
    /// The column widths of every `MusicDisplay` in the output
    music_widths: Vec<usize>,
}

impl CompPrinter {
    fn new(
        query: Arc<Query>,
        music_displays: Vec<MusicDisplay>,
        method_count_ranges: impl Iterator<Item = RangeInclusive<usize>>,
    ) -> Self {
        Self {
            length_width: query.length_range.end().to_string().len(),
            method_counts: query
                .methods
                .iter()
                .zip_eq(method_count_ranges)
                .map(|(method, count_range)| {
                    // TODO: Once integer logarithms become stable, use `.log10() + 1`
                    let max_count_width = count_range.end().to_string().len();
                    let max_width = max_count_width.max(method.shorthand.len());
                    (max_width, method.shorthand.clone())
                })
                .collect_vec(),
            part_head_width: (query.num_parts() > 2)
                .then(|| query.part_head_group.effective_stage().num_bells()),
            music_widths: music_displays
                .iter()
                .map(|d| d.col_width(&query.music_types))
                .collect_vec(),

            query,
            music_displays,
        }
    }

    fn ruleoff(&self) -> String {
        // Ruleoff is the same as header, but with every non-'|' char replaced with '-'
        let mut ruleoff = self
            .header()
            .chars()
            .map(|c| if c == '|' { '|' } else { '-' })
            .collect::<String>();
        ruleoff.push_str("---"); // Add a couple of extra `-`s to make the ruleoff a bit longer
        ruleoff
    }

    fn header(&self) -> String {
        let mut s = String::new();
        // Length
        write_centered_text(&mut s, "len", self.length_width);
        s.push(' ');
        // Method shorthands (for counts)
        if self.method_counts.len() > 1 {
            s.push_str("  ");
            for (width, shorthand) in &self.method_counts {
                write_centered_text(&mut s, shorthand, *width);
                s.push(' ');
            }
        }
        s.push('|');
        // Part head
        if let Some(w) = self.part_head_width {
            // Add 2 to the width to get one char of extra padding on either side
            write_centered_text(&mut s, "PH", w + 2);
            s.push('|');
        }
        // Music
        s.push_str("  music  ");
        if !self.music_displays.is_empty() {
            s.push(' ');
        }
        for (music_display, col_width) in self.music_displays.iter().zip_eq(&self.music_widths) {
            s.push_str("  ");
            write_centered_text(&mut s, &music_display.name, *col_width);
            s.push(' ');
        }
        // Everything else
        s.push_str("| avg score | calling");
        s
    }

    fn comp_string(&self, comp: &Composition) -> String {
        let query = &self.query;

        // Length
        let mut s = format!("{:>width$} ", comp.length(), width = self.length_width);
        // Method counts (for spliced)
        if self.method_counts.len() > 1 {
            s.push_str(": ");
            for ((width, _), count) in self.method_counts.iter().zip_eq(comp.method_counts()) {
                write!(s, "{:>width$} ", count, width = *width).unwrap();
            }
        }
        s.push('|');
        // Part head (if >2 parts; up to 2-parts must always have the same part head)
        if self.part_head_width.is_some() {
            write!(s, " {} |", ShortRow(comp.part_head(query))).unwrap();
        }
        // Music
        write!(s, " {:>7.2} ", comp.music_score(query)).unwrap();
        if !self.music_displays.is_empty() {
            s.push(':');
        }
        for (music_display, col_width) in self.music_displays.iter().zip_eq(&self.music_widths) {
            s.push_str("  ");
            write_centered_text(
                &mut s,
                &music_display.display_counts(&query.music_types, comp.music_counts()),
                *col_width,
            );
            s.push(' ');
        }
        // avg score, call string
        write!(
            s,
            "| {:>9.6} | {}",
            comp.average_score(),
            comp.call_string(query)
        )
        .unwrap();

        s
    }
}

/// Write some `string` to `out`, centering it among `width` spaces (rounding to the right).
fn write_centered_text(out: &mut String, text: &str, width: usize) {
    let w = width.saturating_sub(text.len());
    push_multiple(' ', w - (w / 2), out);
    out.push_str(text);
    push_multiple(' ', w / 2, out);
}

/// Push `n` copies of `c` to the end of `out`
fn push_multiple(c: char, n: usize, out: &mut String) {
    out.extend(std::iter::repeat(c).take(n));
}
