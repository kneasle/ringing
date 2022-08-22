//! Core library for Monument, a fast and flexible composing engine.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod graph;
pub mod music;
mod prove_length;
mod search;
pub mod utils;

pub use prove_length::RefinedRanges;
pub use utils::OptRange;

use serde::Deserialize;

use itertools::Itertools;
use music::Score;
use utils::{
    group::{PartHead, PartHeadGroup},
    Counts, PerPartLength, TotalLength,
};

use std::{
    fmt::{Display, Formatter},
    hash::Hash,
    ops::RangeInclusive,
    sync::atomic::{AtomicBool, Ordering},
};

use bellframe::{Bell, Block, Mask, PlaceNot, Row, RowBuf, Stage, Stroke};
use graph::Graph;

/// Information provided to Monument which specifies what compositions are generated.
///
/// Compare this to [`Config`], which determines _how_ those compositions are generated (and
/// therefore determines how quickly the results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    // GENERAL
    pub len_range: RangeInclusive<TotalLength>,
    pub num_comps: usize,
    pub allow_false: bool,
    pub stage: Stage,

    // METHODS & CALLING
    pub methods: MethodVec<Method>,
    pub calls: CallVec<Call>,
    // TODO: Make this defined per-method?  Or per-stage?
    pub call_display_style: CallDisplayStyle,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32,

    // COURSES
    //
    // (CH masks are defined on each `Method`)
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head_group: PartHeadGroup,
    /// The `f32` is the weight given to every row in any course matching the given [`Mask`]
    pub ch_weights: Vec<(Mask, f32)>,

    // MUSIC
    pub music_types: MusicTypeVec<music::MusicType>,
    pub music_displays: Vec<music::MusicDisplay>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    pub start_stroke: Stroke,
}

impl Query {
    pub fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    pub fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }

    pub fn is_spliced(&self) -> bool {
        self.methods.len() > 1
    }

    pub fn positional_calls(&self) -> bool {
        self.call_display_style == CallDisplayStyle::Positional
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub inner: bellframe::Method,
    pub shorthand: String,

    /// The number of rows of this method must fit within this range
    pub count_range: OptRange,
    /// The indices in which we can start a composition during this `Method`.  `None` means any
    /// index is allowed (provided the CH masks are satisfied).  These are interpreted modulo the
    /// lead length of the method.
    pub start_indices: Vec<isize>,
    /// The indices in which we can end a composition during this `Method`.  `None` means any index
    /// is allowed (provided the CH masks are satisfied).  These are interpreted modulo the lead
    /// length of the method.
    pub end_indices: Option<Vec<isize>>,
    pub ch_masks: Vec<Mask>,
}

impl Method {
    pub(crate) fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }
}

impl std::ops::Deref for Method {
    type Target = bellframe::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone)]
pub struct Call {
    pub display_symbol: String,
    pub debug_symbol: String,
    pub calling_positions: Vec<String>,

    pub label_from: String,
    pub label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub place_not: PlaceNot,

    pub weight: f32,
}

/// How the calls in a given composition should be displayed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallDisplayStyle {
    /// Calls should be displayed as a count since the last course head
    Positional,
    /// Calls should be displayed based on the position of the provided 'observation' [`Bell`]
    CallingPositions(Bell),
}

/// The different styles of spliced that can be generated
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label
    #[serde(rename = "leads")]
    LeadLabels,
    /// Splices only happen at calls
    #[serde(rename = "calls")]
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

/// Configuration parameters for Monument which **don't** change which compositions are emitted.
pub struct Config {
    /* General */
    /// Number of threads used to generate compositions.  If `None`, this uses the number of
    /// **physical** CPU cores (i.e. ignoring hyper-threading).
    pub thread_limit: Option<usize>,

    /* Graph Generation */
    /// The maximum graph size, in chunks.  If a search would produce a graph bigger than this, it
    /// is aborted.
    pub graph_size_limit: usize,

    /* Search */
    pub queue_limit: usize,
    /// If `true`, the data structures used by searches will be leaked using [`std::mem::forget`].
    /// This massively improves the termination speed (because all individual allocations don't
    /// need to be freed), but only makes sense for the CLI, where Monument will do exactly one
    /// search run before terminating (thus returning the memory to the OS anyway).
    pub leak_search_memory: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            thread_limit: None,

            graph_size_limit: 100_000,

            queue_limit: 10_000_000,
            leak_search_memory: false,
        }
    }
}

////////////
// ERRORS //
////////////

/// The different ways that graph building can fail
#[derive(Debug)]
pub enum Error {
    /* QUERY VERIFICATION ERRORS */
    /// Different start/end rows were specified in a multi-part
    DifferentStartEndRowInMultipart,
    /// Some [`Call`](crate::Call) refers to a label that doesn't exist
    UndefinedLabel { call_name: String, label: String },
    /// [`Query`] didn't define any [`Method`]s
    NoMethods,
    /// Two [`Method`]s use the same shorthand
    DuplicateShorthand {
        shorthand: String,
        title1: String,
        title2: String,
    },
    NoCourseHeadInPart {
        mask_in_first_part: Mask,
        part_head: RowBuf,
        mask_in_other_part: Mask,
    },
    /// Some [`Call`](crate::Call) doesn't have enough calling positions to cover the [`Stage`]
    WrongCallingPositionsLength {
        call_name: String,
        calling_position_len: usize,
        stage: Stage,
    },

    /* GRAPH BUILD ERRORS */
    /// The given maximum graph size limit was reached
    SizeLimit(usize),
    /// The same [`Chunk`](graph::Chunk) could start at two different strokes, and some
    /// [`MusicType`](crate::music::MusicType) relies on that
    InconsistentStroke,

    /* LENGTH PROVING ERRORS */
    /// The requested length range isn't achievable
    UnachievableLength {
        requested_range: RangeInclusive<TotalLength>,
        next_shorter_len: Option<TotalLength>,
        next_longer_len: Option<TotalLength>,
    },
    /// Some method range isn't achievable
    UnachievableMethodCount {
        method_name: String,
        requested_range: OptRange,
        next_shorter_len: Option<TotalLength>,
        next_longer_len: Option<TotalLength>,
    },
    /// The total of the minimum method counts is longer than the composition
    TooMuchMethodCount {
        min_total_method_count: TotalLength,
        max_length: TotalLength,
    },
    TooLittleMethodCount {
        max_total_method_count: TotalLength,
        min_length: TotalLength,
    },
}

pub type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::DifferentStartEndRowInMultipart => {
                write!(f, "Start/end rows must be the same for multipart comps")
            }
            Error::NoMethods => write!(f, "Can't have a composition with no methods"),
            Error::WrongCallingPositionsLength {
                call_name,
                calling_position_len,
                stage,
            } => write!(
                f,
                "Call {:?} only specifies {} calling positions, but the stage has {} bells",
                call_name,
                calling_position_len,
                stage.num_bells()
            ),
            Error::DuplicateShorthand {
                shorthand,
                title1,
                title2,
            } => write!(
                f,
                "Methods {:?} and {:?} share a shorthand ({})",
                title1, title2, shorthand
            ),
            Error::UndefinedLabel { call_name, label } => write!(
                f,
                "Call {:?} refers to a label {:?}, which doesn't exist",
                call_name, label
            ), // TODO: Suggest one that does exist
            Error::NoCourseHeadInPart {
                mask_in_first_part,
                part_head,
                mask_in_other_part,
            } => {
                writeln!(
                    f,
                    "course head `{}` becomes `{}` in the part starting `{}`, which isn't in `course_heads`.",
                    mask_in_first_part, mask_in_other_part, part_head
                )?;
                write!(
                    f,
                    "   help: consider adding `{}` to `course_heads`",
                    mask_in_other_part
                )
            }

            /* GRAPH BUILD ERRORS */
            Error::SizeLimit(limit) => write!(
                f,
                "Graph size limit of {} chunks reached.  You can set it \
higher with `--graph-size-limit <n>`.",
                limit
            ),
            Error::InconsistentStroke => write!(
                f,
                "The same chunk of ringing can be at multiple strokes, probably \
because you're using a method with odd-length leads"
            ),

            /* LENGTH PROVING ERRORS */
            Error::UnachievableLength {
                requested_range,
                next_shorter_len,
                next_longer_len,
            } => {
                write!(f, "No compositions can fit the required length range (")?;
                write_range(
                    f,
                    "length",
                    Some(*requested_range.start()),
                    Some(*requested_range.end()),
                )?;
                write!(f, ").  ")?;
                // Describe the nearest composition length(s)
                match (next_shorter_len, next_longer_len) {
                    (Some(l1), Some(l2)) => write!(f, "The nearest lengths are {l1} and {l2}."),
                    (Some(l), None) | (None, Some(l)) => write!(f, "The nearest length is {l}."),
                    // TODO: Give this its own error?
                    (None, None) => write!(f, "No compositions are possible."),
                }
            }
            Error::UnachievableMethodCount {
                method_name,
                requested_range,
                next_shorter_len,
                next_longer_len,
            } => {
                assert_ne!((requested_range.min, requested_range.max), (None, None));

                write!(
                    f,
                    "No method counts for {:?} satisfy the requested range (",
                    method_name,
                )?;
                write_range(f, "count", requested_range.min, requested_range.max)?;
                write!(f, ").  ")?;
                // Describe the nearest method counts
                match (next_shorter_len, next_longer_len) {
                    (Some(l1), Some(l2)) => write!(f, "The nearest counts are {l1} and {l2}."),
                    (Some(l), None) | (None, Some(l)) => write!(f, "The nearest count is {l}."),
                    (None, None) => unreachable!(), // Method count of 0 is always possible
                }
            }
            Error::TooMuchMethodCount {
                min_total_method_count,
                max_length,
            } => {
                write!(f, "Too much method counts; the method counts need at least")?;
                write!(
                    f,
                    " {min_total_method_count} rows, but at most {max_length} rows are available."
                )
            }
            Error::TooLittleMethodCount {
                max_total_method_count,
                min_length,
            } => {
                write!(
                    f,
                    "Not enough method counts; the composition needs at least {min_length} rows"
                )?;
                write!(
                    f,
                    " but the methods can make at most {max_total_method_count}."
                )
            }
        }
    }
}

/// Prettily format a (possibly open) inclusive range as an inequality (e.g. `300 <= count <= 500`)
fn write_range<T: Ord + Display>(
    f: &mut impl std::fmt::Write,
    name: &str,
    min: Option<T>,
    max: Option<T>,
) -> std::fmt::Result {
    match (min, max) {
        // Write e.g. `224 <= count <= 224` as `count == 224`
        (Some(min), Some(max)) if min == max => write!(f, "{name} == {min}")?,
        // Otherwise write everything as an inequality
        (min, max) => {
            if let Some(min) = min {
                write!(f, "{min} <= ")?;
            }
            write!(f, "{name}")?;
            if let Some(max) = max {
                write!(f, " <= {max}")?;
            }
        }
    }
    Ok(())
}

impl std::error::Error for Error {}

//////////////////
// COMPOSITIONS //
//////////////////

/// A `Comp`osition generated by Monument.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comp {
    pub path: Vec<PathElem>,

    pub part_head: PartHead,
    pub length: TotalLength,
    /// The number of rows generated of each method
    pub method_counts: Counts,
    /// The number of counts generated of each [`MusicType`](music::MusicType)
    pub music_counts: Counts,
    /// The total [`Score`] of this composition, accumulated from music, calls, coursing patterns,
    /// etc.
    pub total_score: Score,
    /// Average [`Score`] generated by each row in the composition.   This is used to rank
    /// compositions to prevent the search algorithm being dominated by long compositions.
    pub avg_score: Score,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathElem {
    start_row: RowBuf,
    method: MethodIdx,
    start_sub_lead_idx: usize,
    length: PerPartLength,
    call: Option<CallIdx>,
}

impl PathElem {
    pub fn ends_with_plain(&self) -> bool {
        self.call.is_none()
    }

    pub fn end_sub_lead_idx(&self, query: &Query) -> usize {
        query.methods[self.method].add_sub_lead_idx(self.start_sub_lead_idx, self.length)
    }
}

impl Comp {
    pub fn call_string(&self, query: &Query) -> String {
        let needs_brackets = query.is_spliced() || query.positional_calls();
        let is_snap_start = self.path[0].start_sub_lead_idx > 0;
        let is_snap_finish = self.path.last().unwrap().end_sub_lead_idx(query) > 0;
        let part_head = self.part_head(query);

        let mut path_iter = self.path.iter().peekable();

        let mut s = String::new();
        if query.call_display_style == CallDisplayStyle::Positional {
            s.push('#');
        }
        s.push_str(if is_snap_start { "<" } else { "" });
        while let Some(path_elem) = path_iter.next() {
            // Method text
            if query.is_spliced() || query.call_display_style == CallDisplayStyle::Positional {
                // Add one shorthand for every lead *covered* (not number of lead heads reached)
                //
                // TODO: Deal with half-lead spliced
                let method = &query.methods[path_elem.method];
                let num_leads_covered = num_leads_covered(
                    method.lead_len(),
                    path_elem.start_sub_lead_idx,
                    path_elem.length,
                );
                for _ in 0..num_leads_covered {
                    s.push_str(&method.shorthand);
                }
            }
            // Call text
            if let Some(call_idx) = path_elem.call {
                let call = &query.calls[call_idx];

                s.push_str(if needs_brackets { "[" } else { "" });
                // Call position
                match query.call_display_style {
                    CallDisplayStyle::CallingPositions(calling_bell) => {
                        let row_after_call = path_iter
                            .peek()
                            .map_or(part_head, |path_elem| &path_elem.start_row);
                        let place_of_calling_bell = row_after_call.place_of(calling_bell).unwrap();
                        let calling_position = &call.calling_positions[place_of_calling_bell];
                        s.push_str(&call.display_symbol);
                        s.push_str(calling_position);
                    }
                    // TODO: Compute actual counts for positional calls
                    CallDisplayStyle::Positional => s.push_str(&call.debug_symbol),
                }
                s.push_str(if needs_brackets { "]" } else { "" });
            }
        }
        s.push_str(if is_snap_finish { ">" } else { "" });

        s
    }

    pub fn part_head<'q>(&self, query: &'q Query) -> &'q Row {
        query.part_head_group.get_row(self.part_head)
    }

    pub fn music_score(&self, query: &Query) -> f32 {
        self.music_counts
            .iter()
            .zip_eq(&query.music_types)
            .map(|(count, music_type)| f32::from(music_type.weight) * *count as f32)
            .sum::<f32>()
    }

    pub fn rows(&self, query: &Query) -> Block<(MethodIdx, usize)> {
        // Generate plain courses for each method
        let plain_courses = query
            .methods
            .iter_enumerated()
            .map(|(idx, m)| m.plain_course().map_annots(|a| (idx, a.sub_lead_idx)))
            .collect::<MethodVec<_>>();

        // Generate the first part
        let mut first_part = Block::with_leftover_row(query.start_row.clone());
        for elem in &self.path {
            assert_eq!(first_part.leftover_row(), elem.start_row.as_row());
            let plain_course = &plain_courses[elem.method];
            // Add this elem to the first part
            let start_idx = elem.start_sub_lead_idx;
            let end_idx = start_idx + elem.length.as_usize();
            if end_idx > plain_course.len() {
                // `elem` wraps over the course head, so copy it in two pieces
                first_part
                    .extend_range(plain_course, start_idx..)
                    .expect("All path elems should have the same stage");
                first_part
                    .extend_range(plain_course, ..end_idx - plain_course.len())
                    .expect("All path elems should have the same stage");
            } else {
                // `elem` doesn't wrap over the course head, so copy it in one piece
                first_part
                    .extend_range(plain_course, start_idx..end_idx)
                    .expect("All path elems should have the same stage");
            }
            // If this PathElem ends in a call, then change the `leftover_row` to suit
            if let Some(call_idx) = elem.call {
                let last_non_leftover_row = first_part.rows().next_back().unwrap();
                let new_leftover_row =
                    last_non_leftover_row * query.calls[call_idx].place_not.transposition();
                first_part
                    .leftover_row_mut()
                    .copy_from(&new_leftover_row)
                    .unwrap();
            }
        }

        // Generate the other parts from the first
        let part_len = first_part.len();
        let mut comp = first_part;
        for _ in 0..query.num_parts() - 1 {
            comp.extend_from_within(..part_len);
        }
        assert_eq!(comp.len(), self.length.as_usize());
        assert_eq!(comp.leftover_row(), query.end_row.as_row());
        comp
    }
}

/// Return the number of leads covered by some [`Chunk`]
fn num_leads_covered(lead_len: usize, start_sub_lead_idx: usize, length: PerPartLength) -> usize {
    assert_ne!(length, PerPartLength::ZERO); // 0-length chunks shouldn't exist
    let dist_to_end_of_first_lead = lead_len - start_sub_lead_idx;
    let rows_after_end_of_first_lead = length.as_usize().saturating_sub(dist_to_end_of_first_lead);
    // `+ 1` for the first lead
    utils::div_rounding_up(rows_after_end_of_first_lead, lead_len) + 1
}

/// A way to display a [`Comp`] by pairing it with a [`Query`]
#[derive(Debug, Clone, Copy)]
struct DisplayComp<'a>(pub &'a Comp, pub &'a Query);

impl std::fmt::Display for DisplayComp<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisplayComp(comp, query) = self;

        write!(f, "len: {}, ", comp.length)?;
        // Method counts for spliced
        if query.is_spliced() {
            write!(f, "ms: {:>3?}, ", comp.method_counts.as_slice())?;
        }
        // Part heads if multi-part with >2 parts (2-part compositions only have one possible part
        // head)
        if query.num_parts() > 2 {
            write!(f, "PH: {}, ", comp.part_head(query))?;
        }
        write!(
            f,
            "music: {:>6.2?}, avg score: {:.6}, str: {}",
            comp.music_score(query),
            comp.avg_score,
            comp.call_string(query)
        )
    }
}

////////////
// SEARCH //
////////////

impl Query {
    pub fn unoptimised_graph(&self, config: &Config) -> crate::Result<Graph> {
        graph::Graph::new(self, config)
    }

    /// Prove which lengths/method counts are actually possible
    pub fn refine_ranges(&self, graph: &Graph) -> crate::Result<RefinedRanges> {
        prove_length::prove_lengths(graph, self)
    }

    /// Given a set of (optimised) graphs, run multi-threaded tree search to generate compositions.
    /// `update_fn` is run whenever a thread generates a [`QueryUpdate`].
    pub fn search(
        &self,
        graph: Graph,
        refined_ranges: RefinedRanges,
        config: &Config,
        update_fn: impl FnMut(QueryUpdate),
        abort_flag: &AtomicBool,
    ) {
        // Make sure that `abort_flag` starts as false (so the search doesn't abort immediately).
        // We want this to be sequentially consistent to make sure that the worker threads don't
        // see the previous value (which could be 'true').
        abort_flag.store(false, Ordering::SeqCst);
        search::search(&graph, self, config, refined_ranges, update_fn, abort_flag);
    }
}

/// Instances of this are emitted by the search as it's running
#[derive(Debug)]
pub enum QueryUpdate {
    /// A new composition has been found
    Comp(Comp),
    /// A thread is sending a status update
    Progress(Progress),
    /// The search is being aborted
    Aborting,
}

#[derive(Debug)]
pub struct Progress {
    /// How many chunks have been expanded so far
    pub iter_count: usize,
    /// How many comps have been generated so far
    pub num_comps: usize,

    /// The current length of the A* queue
    pub queue_len: usize,
    /// The average length of a composition in the queue
    pub avg_length: f32,
    /// The length of the longest composition in the queue
    pub max_length: TotalLength,

    /// `true` if the search is currently truncating the queue to save memory
    pub truncating_queue: bool,
}

impl Progress {
    /// The [`Progress`] made by a search which hasn't started yet
    pub const START: Self = Self {
        iter_count: 0,
        num_comps: 0,

        queue_len: 0,
        avg_length: 0.0,
        max_length: TotalLength::ZERO,

        truncating_queue: false,
    };
}

impl Default for Progress {
    fn default() -> Self {
        Self::START
    }
}

index_vec::define_index_type! { pub struct MethodIdx = usize; }
index_vec::define_index_type! { pub struct CallIdx = usize; }
index_vec::define_index_type! { pub struct MusicTypeIdx = usize; }
pub type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;

#[cfg(test)]
mod tests {
    use crate::utils::PerPartLength;

    #[test]
    fn num_leads_covered() {
        assert_eq!(super::num_leads_covered(32, 0, PerPartLength::new(32)), 1);
        assert_eq!(super::num_leads_covered(32, 2, PerPartLength::new(32)), 2);
        assert_eq!(super::num_leads_covered(32, 2, PerPartLength::new(30)), 1);
        assert_eq!(super::num_leads_covered(32, 0, PerPartLength::new(2)), 1);
        assert_eq!(super::num_leads_covered(32, 16, PerPartLength::new(24)), 2);
    }
}
