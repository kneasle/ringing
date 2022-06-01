//! Core library for Monument, a fast and flexible composing engine.

#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod graph;
pub mod music;
mod search;
pub mod utils;

use serde::Deserialize;
pub use utils::OptRange;

use itertools::Itertools;
use music::Score;
use utils::{Counts, Rotation};

use std::{
    hash::Hash,
    ops::Range,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::sync_channel,
        Arc, Mutex,
    },
};

use bellframe::{Bell, Mask, PlaceNot, RowBuf, Stage, Stroke};
use graph::{optimise::Pass, Graph, PerPartLength};

/// Information provided to Monument which specifies what compositions are generated.
///
/// Compare this to [`Config`], which determines _how_ those compositions are generated (and
/// therefore determines how quickly the results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    // GENERAL
    pub len_range: Range<usize>,
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
    pub part_head: RowBuf,
    /// The `f32` is the weight given to every row in any course matching the given [`Mask`]
    pub ch_weights: Vec<(Mask, f32)>,

    // MUSIC
    pub music_types: MusicTypeVec<music::MusicType>,
    pub music_displays: Vec<music::MusicDisplay>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    pub start_stroke: Stroke,
    pub max_duffer_rows: Option<usize>,
}

impl Query {
    pub fn is_multipart(&self) -> bool {
        !self.part_head.is_rounds()
    }

    pub fn is_spliced(&self) -> bool {
        self.methods.len() > 1
    }

    pub fn positional_calls(&self) -> bool {
        self.call_display_style == CallDisplayStyle::Positional
    }

    pub fn num_parts(&self) -> usize {
        self.part_head.order()
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub inner: bellframe::Method,
    pub shorthand: String,

    /// The number of rows of this method must fit within this [`Range`]
    pub count_range: Range<usize>,
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

    pub lead_location_from: String,
    pub lead_location_to: String,
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
    pub optimisation_passes: Vec<Mutex<Pass>>,
    /// The maximum graph size, in chunks.  If a search would produce a graph bigger than this, it
    /// is aborted.
    pub graph_size_limit: usize,

    /* Search */
    pub queue_limit: usize,
    /// If `true`, the data structures used by searches will be leaked using [`std::mem::forget`].
    /// This massively improves the termination speed (because all individual allocations don't
    /// need to be freed), but only makes sense for the CLI, where Monument will do exactly one
    /// search run before terminating (thus returning the memory to the OS anyway).
    pub mem_forget_search_data: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            thread_limit: None,

            graph_size_limit: 100_000,
            optimisation_passes: graph::optimise::passes::default(),

            queue_limit: 10_000_000,
            mem_forget_search_data: false,
        }
    }
}

/// A `Comp`osition generated by Monument.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comp {
    pub path: Vec<PathElem>,

    pub rotation: Rotation,
    pub length: usize,
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
        (self.start_sub_lead_idx + self.length.0) % query.methods[self.method].lead_len()
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
            if query.is_spliced() {
                // Add one shorthand for every lead *covered* (not number of lead heads reached)
                //
                // TODO: Deal with half-lead spliced
                let method = &query.methods[path_elem.method];
                let num_leads_covered = num_leads_covered(
                    method.lead_len(),
                    path_elem.start_sub_lead_idx,
                    path_elem.length.0,
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
                            .map_or(&part_head, |path_elem| &path_elem.start_row);
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

    pub fn part_head(&self, query: &Query) -> RowBuf {
        query.part_head.pow_u(self.rotation as usize)
    }

    pub fn music_score(&self, query: &Query) -> f32 {
        self.music_counts
            .iter()
            .zip_eq(&query.music_types)
            .map(|(count, music_type)| f32::from(music_type.weight) * *count as f32)
            .sum::<f32>()
    }
}

/// Return the number of leads covered by some [`Chunk`]
fn num_leads_covered(lead_len: usize, start_sub_lead_idx: usize, length: usize) -> usize {
    assert_ne!(length, 0); // 0-length chunks shouldn't exist
    let dist_to_end_of_first_lead = lead_len - start_sub_lead_idx;
    let rows_after_end_of_first_lead = length.saturating_sub(dist_to_end_of_first_lead);
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
    pub fn unoptimised_graph(&self, config: &Config) -> Result<Graph, graph::BuildError> {
        graph::Graph::new(self, config)
    }

    /// Converts a single [`Graph`] into a set of [`Graph`]s which make tree search faster but
    /// generate the same overall set of compositions.
    pub fn optimise_graph(&self, mut graph: Graph, config: &Config) -> Vec<Graph> {
        graph.optimise(&config.optimisation_passes, self);
        vec![graph]
    }

    /// Given a set of (optimised) graphs, run multi-threaded tree search to generate compositions.
    /// `update_fn` is run whenever a thread generates a [`QueryUpdate`].
    pub fn search(
        arc_self: Arc<Self>,
        graphs: Vec<Graph>,
        config: &Config,
        mut update_fn: impl FnMut(QueryUpdate) + Send + 'static,
        abort_flag: Arc<AtomicBool>,
    ) {
        // Make sure that `abort_flag` starts as false (so the search doesn't abort immediately).
        // We want this to be sequentially consistent to make sure that the worker threads don't
        // see the previous value (which could be 'true').
        abort_flag.store(false, Ordering::SeqCst);
        // Create a new thread which will handle the query updates
        let (update_tx, update_rx) = sync_channel::<QueryUpdate>(1_000);
        let update_thread = std::thread::spawn(move || {
            while let Ok(update) = update_rx.recv() {
                update_fn(update);
            }
        });
        // Run the search
        let num_threads = graphs.len();
        let handles = graphs
            .into_iter()
            .map(|graph| {
                let query = arc_self.clone();
                let queue_limit = config.queue_limit;
                let mem_forget_search_data = config.mem_forget_search_data;
                let abort_flag = abort_flag.clone();
                let update_channel = update_tx.clone();
                std::thread::spawn(move || {
                    search::search(
                        &graph,
                        query.clone(),
                        queue_limit / num_threads,
                        mem_forget_search_data,
                        update_channel,
                        abort_flag,
                    );
                })
            })
            .collect_vec();
        // `update_thread` will only terminate once **all** copies of `update_tx` are dropped.
        // Each worker thread has its own copy, but if we don't explicitly drop this one then
        // `update_thread` will never terminate (causing the worker thread to hang).
        drop(update_tx);

        // Wait for all search threads to terminate
        for h in handles {
            h.join().unwrap();
        }
        // Wait for `update_thread` to terminate
        update_thread.join().unwrap();
    }
}

/// Instances of this are emitted by the search as it's running
#[derive(Debug)]
pub enum QueryUpdate {
    /// A new composition has been found
    Comp(Comp),
    /// A thread is sending a status update
    Progress(Progress),
    /// The queue of prefixes has got too large and is being shortened
    TruncatingQueue,
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
    pub max_length: u32,
}

impl Progress {
    /// The [`Progress`] made by a search which hasn't started yet
    pub const START: Self = Self {
        iter_count: 0,
        num_comps: 0,

        queue_len: 0,
        avg_length: 0.0,
        max_length: 0,
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
    #[test]
    fn num_leads_covered() {
        assert_eq!(super::num_leads_covered(32, 0, 32), 1);
        assert_eq!(super::num_leads_covered(32, 2, 32), 2);
        assert_eq!(super::num_leads_covered(32, 2, 30), 1);
        assert_eq!(super::num_leads_covered(32, 0, 2), 1);
        assert_eq!(super::num_leads_covered(32, 16, 24), 2);
    }
}
