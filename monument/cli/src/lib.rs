//! Crate for loading and running Monument's input files.  The CLI itself is a very thin wrapper
//! around `monument_toml`, parsing CLI args and immediately calling into this.  This crate is also
//! shared between the various integration test runners, making sure that the integration tests run
//! in exactly the same way as Monument itself.

#![deny(rustdoc::broken_intra_doc_links, rustdoc::private_intra_doc_links)]

pub mod args;
pub mod calls;
pub mod music;
pub mod spec;
pub mod utils;

use std::{
    fmt::Write,
    io::Write as IoWrite,
    path::Path,
    str::FromStr,
    sync::Arc,
    time::{Duration, Instant},
};

use bellframe::row::ShortRow;
use itertools::Itertools;
use log::{log_enabled, LevelFilter};
use monument::{Composition, Progress, Search, Update};
use music::MusicDisplay;
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
    let comp_printer = CompPrinter::new(music_displays, search.clone(), spec.duffers_specified());
    let mut update_logger = SingleLineProgressLogger::new(comp_printer.clone());

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

    comp_printer: CompPrinter,
}

impl QueryResult {
    pub fn print(&self) {
        eprintln!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
        for c in &self.comps {
            println!("{}", self.comp_printer.comp_string(c));
        }
        println!("{}", self.comp_printer.ruleoff());
        println!("{}", self.comp_printer.header());
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

    fn log(&mut self, update: Update) -> Option<Composition> {
        // Early return if we can't log anything, making sure to still keep the composition
        if !log_enabled!(log::Level::Info) {
            return match update {
                Update::Comp(c) => Some(c),
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

        let std_err = std::io::stderr();
        let mut std_err = std_err.lock();
        // We precede with a carriage return to make sure that we overwrite anything the user
        // types (e.g. `^C`).  We don't do anything in the case that the user's input is longer
        // than what we're writing - we'll just assume that no-one would be able to type that much
        // in between updates (which happen many many times per second).
        write!(std_err, "\r{}\r", update_string).unwrap();
        std_err.flush().unwrap(); // `std_out` won't flush by default without a newline

        comp
    }

    /// Given a new update, update `self` and return the [`Composition`] (if one has just been
    /// generated)
    fn update_progress(&mut self, update: Update) -> Option<Composition> {
        match update {
            Update::Comp(comp) => return Some(comp),
            Update::Progress(progress) => self.last_progress = progress,
            Update::Aborting => self.is_aborting = true,
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
    search: Arc<Search>,
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
    /// `true` if the user specified anything about duffer courses, otherwise 'false'
    print_duffers: bool,
    /// If a part head should be displayed, then what's its width
    part_head_width: Option<usize>,
    /// The column widths of every `MusicDisplay` in the output
    music_widths: Vec<usize>,
}

impl CompPrinter {
    fn new(music_displays: Vec<MusicDisplay>, search: Arc<Search>, print_duffers: bool) -> Self {
        Self {
            length_width: search.length_range().end().to_string().len(),
            method_counts: search
                .methods()
                .map(|(id, _method, shorthand)| {
                    let max_count_width = search.method_count_range(&id).end().to_string().len();
                    let max_width = max_count_width.max(shorthand.len());
                    (max_width, shorthand.to_owned())
                })
                .collect_vec(),
            print_duffers,
            part_head_width: (search.num_parts() > 2)
                .then(|| search.effective_part_head_stage().num_bells()),
            music_widths: music_displays
                .iter()
                .map(|d| d.col_width(&search))
                .collect_vec(),

            search,
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
        // Duffers
        if self.print_duffers {
            s.push_str(" avg/max/sum dufr |");
        }
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
        // Duffers
        if self.print_duffers {
            write!(
                s,
                " {:>6.2} {:>4} {:>4} |",
                comp.contiguous_duffer_lengths().sum::<usize>() as f32
                    / comp.contiguous_duffer_lengths().count() as f32,
                comp.contiguous_duffer_lengths().max().unwrap_or(0),
                comp.total_duffer()
            )
            .unwrap();
        }
        // Part head (if >2 parts; up to 2-parts must always have the same part head)
        if self.part_head_width.is_some() {
            write!(s, " {} |", ShortRow(comp.part_head())).unwrap();
        }
        // Music
        write!(s, " {:>7.2} ", comp.music_score()).unwrap();
        if !self.music_displays.is_empty() {
            s.push(':');
        }
        for (music_display, col_width) in self.music_displays.iter().zip_eq(&self.music_widths) {
            s.push_str("  ");
            write_centered_text(
                &mut s,
                &music_display.display_counts(&self.search, comp.music_counts()),
                *col_width,
            );
            s.push(' ');
        }
        // avg score, call string
        write!(
            s,
            "| {:>9.6} | {}",
            comp.average_score(),
            comp.call_string()
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
