//! Code for handling the logging of compositions or updates provided by Monument

use std::{fmt::Write, io::Write as IoWrite, sync::Arc};

use bellframe::row::ShortRow;
use colored::Colorize;
use itertools::Itertools;
use log::log_enabled;
use monument::{Composition, Progress, Search, Update};
use ringing_utils::BigNumInt;

use crate::music::MusicDisplay;

/// Struct which handles logging updates, keeping the updates to a single line which updates as the
/// search progresses.
pub struct SingleLineProgressLogger {
    // Set to `None` if the `--only-update-line` option is set
    comp_printer: Option<CompositionPrinter>,

    last_progress: Progress,
    /// The number of characters in the last line we printed.  `UpdateLogger` will use this add
    /// enough spaces to the end of the next message to completely overwrite the last one
    last_line_length: usize,
}

impl SingleLineProgressLogger {
    pub fn new(comp_printer: Option<CompositionPrinter>) -> Self {
        Self {
            comp_printer,

            last_progress: Progress::START,
            last_line_length: 0,
        }
    }

    pub fn log(&mut self, update: Update, comps_generated_so_far: usize) -> Option<Composition> {
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
        if let (Some(printer), Some(c)) = (&mut self.comp_printer, &comp) {
            update_string
                .push_str(&printer.comp_string_with_possible_headers(c, comps_generated_so_far));
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
            Update::Complete => {} // Nothing to do, we're just about to return anyway
        }
        None
    }

    /// Append a progress summary to some [`String`] buffer
    fn append_progress_string(&self, buf: &mut String) {
        let p = self.last_progress;
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
        buf.push_str(match (p.aborting, p.truncating_queue) {
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

#[derive(Debug, Clone)]
pub struct CompositionPrinter {
    search: Arc<Search>,
    music_displays: Vec<MusicDisplay>,
    /// Counter which records how many compositions have been printed so far
    comps_printed: usize,

    /* COLUMN WIDTH INFORMATION */
    /// The maximum width of a composition's number (i.e. the width of the total number of
    /// compositions).  If `None`, no composition numbers are printed.
    comp_count_width: Option<usize>,
    /// The maximum width of a composition's (total) length
    length_width: usize,
    /// For each method in the composition:
    /// ```text
    /// (
    ///     maximum width of row count,
    ///     shorthand
    /// )
    /// ```
    method_count_widths: Vec<(usize, String)>,
    /// `true` if the user gave some weight to atw
    print_atw: bool,
    /// If a part head should be displayed, then what's its width
    part_head_width: Option<usize>,
    /// The column widths of every `MusicDisplay` in the output
    music_widths: Vec<usize>,
}

impl CompositionPrinter {
    pub fn new(
        music_displays: Vec<MusicDisplay>,
        search: Arc<Search>,
        print_atw: bool,
        print_comp_widths: bool,
    ) -> Self {
        Self {
            comp_count_width: print_comp_widths
                .then_some(search.parameters().num_comps.to_string().len()),
            length_width: search.parameters().max_length().to_string().len().max(3),
            method_count_widths: search
                .methods()
                .map(|(method, shorthand)| {
                    let max_count_width =
                        search.method_count_range(method.id).end().to_string().len();
                    let max_width = max_count_width.max(shorthand.len());
                    (max_width, shorthand)
                })
                .collect_vec(),
            comps_printed: 0,

            print_atw,
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

    /// Create some lines which summarise the given [`Composition`].  This may include additional
    /// lines for headers or ruleoffs, depending on how many compositions have been printed so far.
    pub fn comp_string_with_possible_headers(
        &mut self,
        comp: &Composition,
        generation_index: usize,
    ) -> String {
        let mut update_string = String::new();

        // Add a header every 50 lines
        if self.comps_printed % 50 == 0 {
            if self.comps_printed > 0 {
                update_string.push_str(&self.ruleoff());
                update_string.push('\n');
            }
            update_string.push_str(&self.header());
            update_string.push('\n');
            update_string.push_str(&self.ruleoff());
            update_string.push('\n');
        }
        // Add the composition
        update_string.push_str(&self.comp_string(comp, generation_index));
        self.comps_printed += 1;

        update_string
    }

    /// Return some lines which end a composition list.  This includes one line for a ruleoff and
    /// one for the column headers, like:
    /// ```text
    /// --------------|---------|-----------|-----------
    /// len    Y   C  |  music  | avg score | calling
    /// ```
    pub fn footer_lines(&self) -> String {
        let mut s = String::new();
        s.push_str(&self.ruleoff());
        s.push('\n');
        s.push_str(&self.header());
        s
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
        // Comp index
        if let Some(c) = self.comp_count_width {
            write_centered_text(&mut s, "#", c);
            s.push_str(" | ");
        }
        // Length
        write_centered_text(&mut s, "len", self.length_width);
        s.push(' ');
        // Method shorthands (for counts)
        if self.method_count_widths.len() > 1 {
            s.push_str("  ");
            for (width, shorthand) in &self.method_count_widths {
                write_centered_text(&mut s, shorthand, *width);
                s.push(' ');
            }
        }
        s.push('|');
        // Atw
        if self.print_atw {
            s.push_str(" atw |");
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

    fn comp_string(&self, comp: &Composition, generation_index: usize) -> String {
        let params = self.search.parameters();

        let mut s = String::new();
        // Comp index
        if let Some(c) = self.comp_count_width {
            write!(s, "{:>width$} | ", generation_index + 1, width = c).unwrap();
        }
        // Length
        write!(s, "{:>width$} ", comp.length(), width = self.length_width).unwrap();
        // Method counts (for spliced)
        if self.method_count_widths.len() > 1 {
            s.push_str(": ");
            for ((width, _), count) in self
                .method_count_widths
                .iter()
                .zip_eq(comp.method_counts(params))
            {
                write!(s, "{:>width$} ", count, width = *width).unwrap();
            }
        }
        s.push('|');
        // Atw
        if self.print_atw {
            let factor = comp.atw_factor(params);
            if factor > 0.999999 {
                s.push_str(&format!(" {} |", "atw".color(colored::Color::BrightGreen)));
            } else {
                write!(s, " {:>2}% |", (factor * 100.0).floor() as usize).unwrap();
            }
        }
        // Part head (if >2 parts; up to 2-parts must always have the same part head)
        if self.part_head_width.is_some() {
            write!(s, " {} |", ShortRow(comp.part_head())).unwrap();
        }
        // Music
        write!(s, " {:>7.2} ", comp.music_score(params)).unwrap();
        if !self.music_displays.is_empty() {
            s.push(':');
        }
        for (music_display, col_width) in self.music_displays.iter().zip_eq(&self.music_widths) {
            s.push_str("  ");
            write_centered_text(
                &mut s,
                &music_display.display_counts(&self.search, &comp.music_counts(params)),
                *col_width,
            );
            s.push(' ');
        }
        // avg score, call string
        write!(
            s,
            "| {:>9.6} | {}",
            comp.average_score(),
            comp.call_string(params)
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
