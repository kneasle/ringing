//! Code for the composition search algorithm

use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt::Write,
    io::Write as IoWrite,
    iter,
    sync::{
        atomic,
        mpsc::{sync_channel, Receiver},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use atomic_float::AtomicF64;
use bellframe::RowBuf;
use itertools::Itertools;
use number_prefix::NumberPrefix;
use shortlist::Shortlist;

use crate::{
    graph::NodeId,
    score::{AtomicScore, Score},
    stats::Stats,
    Config, SegmentId, Spec,
};

use worker::{SharedData, Worker};

mod worker;

/// Type alias for the queue of so-far-unexpanded prefixes.
///
/// The float number here represents the percentage of the search space that each prefix
/// corresponds to.  This is calculated naively: if a `CompPrefix` has `n` successors, then its
/// percentage is split into `n` parts.
type PrefixQueue = Mutex<VecDeque<QueueElem>>;

/// Generate compositions specified by the [`Engine`].  The current thread is blocked until the
/// best compositions have been found and returned.
pub fn compose(spec: &Arc<Spec>, config: &Arc<Config>) -> SearchResults {
    let start_time = Instant::now();

    // Decide how many threads to use (defaulting to the number of CPU cores)
    let num_threads = config.num_threads.unwrap_or_else(num_cpus::get);
    if num_threads == 1 {
        println!("Using 1 thread.");
    } else {
        println!("Using {} threads.", num_threads);
    }

    /* CREATE COMMUNICATION PRIMITIVES */

    // A set of atomic floats used by threads to store what percentage (as a proportion of the
    // overall search) of their current `QueueElem` is yet to be explored.  This, plus the
    // percentage derived from the queue itself, gives the overall completion percentage
    let percentage_left_per_thread = iter::repeat_with(|| Arc::new(AtomicF64::new(0.0)))
        .take(num_threads)
        .collect_vec();
    // A channel down which compositions will be sent when discovered
    let (comp_channel_tx, comp_channel_rx) = sync_channel(config.comp_buffer_length);
    // A channel down which statistics will periodically be sent
    let (stats_channel_tx, stats_channel_rx) = sync_channel(config.stats_buffer_length);
    // We use a Mutex-locked global queue of unexplored prefixes to make sure that the workers
    // always have compositions to explore.
    let unexplored_prefix_queue = Arc::new(Mutex::new(
        spec.prototype_graph.generate_prefixes(num_threads * 2),
    ));
    // 'Immutable' data shared between all the threads
    let shared_data = SharedData::new(
        spec.clone(),
        config.clone(),
        comp_channel_tx,
        stats_channel_tx,
    );

    /* SPAWN THREADS */

    // Spawn a thread to track the progress of the other threads
    let stats_thread = spawn_stats_thread(
        percentage_left_per_thread.clone(),
        unexplored_prefix_queue.clone(),
        stats_channel_rx,
    )
    .unwrap();
    // Spawn a thread which will read the compositions output by the other threads
    let shortlist_thread = spawn_shortlist_thread(
        spec.num_comps,
        shared_data.shortlist_min_score.clone(),
        comp_channel_rx,
    )
    .unwrap();

    // Spawn all the worker threads
    let threads = (0usize..num_threads)
        .map(|i| {
            // Create thread-local copies of the shared data which will be captured by the closure
            // of the new thread
            let thread_engine = shared_data.clone();
            let thread_queue = unexplored_prefix_queue.clone();
            let thread_percentage_left_var = percentage_left_per_thread[i].clone();

            // Spawn a new worker thread, which will immediately start generating compositions
            thread::Builder::new()
                .name(format!("Worker{}", i))
                .spawn(move || {
                    Worker::compose(thread_engine, &thread_queue, i, thread_percentage_left_var)
                })
                .unwrap()
        })
        .collect_vec();

    /* COLLECT DATA FROM THREADS */

    // We have to drop `shared_data` here because it owns a sender for the composition channel.
    // The channel won't close until ALL the senders have been dropped, so not dropping
    // `shared_data` here causes a deadlock: the main thread waits for `shortlist_thread` to
    // terminate, thus keeping the channel alive and preventing `shortlist_thread` from
    // terminating.
    drop(shared_data);

    // Wait for the worker threads to finish
    for t in threads {
        t.join().unwrap();
    }

    // Stop the timer **before** waiting for the auxiliary threads to finish.  The stats/percentage
    // thread only checks the termination condition 5x a second, so waiting for it essentially
    // rounds all times to the nearest 5th of a second, which is not useful for short benchmarks
    let time_taken = Instant::now() - start_time;

    // Collect accumulated data from the stats & comp threads
    let comps = shortlist_thread.join().unwrap();
    let stats = stats_thread.join().unwrap();

    // Collect all the results and return
    SearchResults {
        comps,
        stats,
        time_taken,
    }
}

/// Spawn a thread which will handle collection and periodic printing of stats
fn spawn_stats_thread(
    percentage_left_per_thread: Vec<Arc<AtomicF64>>,
    unexplored_prefix_queue: Arc<PrefixQueue>,
    stats_channel_rx: Receiver<StatsUpdate>,
) -> Result<JoinHandle<Stats>, std::io::Error> {
    /// Format a big number using Giga, Mega, Kilo, etc.
    fn fmt_with_prefix(num: usize) -> String {
        match NumberPrefix::decimal(num as f64) {
            NumberPrefix::Standalone(n) => format!("{:>6.0}", n),
            NumberPrefix::Prefixed(prefix, n) => format!("{:>5.1}{}", n, prefix),
        }
    }

    thread::Builder::new()
        .name("Stats".to_owned())
        .spawn(move || {
            let mut stdout = std::io::stdout();

            let mut stats_accum = Stats::zero();
            while let Ok(stat_update) = stats_channel_rx.recv() {
                stats_accum += stat_update.stats_since_last_signal;

                // Compute the percentage
                let percentage_left_in_queue = unexplored_prefix_queue
                    .lock()
                    .unwrap()
                    .iter()
                    .map(|elem| elem.percentage)
                    .sum::<f64>();
                let percentage_left_in_threads = percentage_left_per_thread
                    .iter()
                    .map(|p| p.load(atomic::Ordering::Relaxed))
                    .sum::<f64>();
                let percentage = 100.0 - (percentage_left_in_queue + percentage_left_in_threads);

                // End the progress line with just a carriage return, so that the next progress
                // line gets drawn over the top of this one.
                print!(
                    "  {:>6.2}% done: {} nodes searched, {} comps found\r",
                    percentage,
                    fmt_with_prefix(stats_accum.nodes_considered),
                    fmt_with_prefix(stats_accum.comps_found)
                );
                // We have to flush stdout manually because we didn't print a newline
                stdout.flush().unwrap();
            }

            // Finish by printing `100%`, and forcing through a newline so that any future printing
            // doesn't jankily overwrite the status line
            println!(
                "  {:>6.2}% done: {} nodes searched, {} comps found",
                100.0,
                fmt_with_prefix(stats_accum.nodes_considered),
                fmt_with_prefix(stats_accum.comps_found)
            );

            // Once all the worker threads have terminated, all the senders on `stats_channel` will
            // be dropped and `recv()` will return an `Err`.  This will break the loop, allowing us
            // to return the final stats
            stats_accum
        })
}

/// Spawn a thread to handle the [`Shortlist`] of generated compositions
fn spawn_shortlist_thread(
    shortlist_size: usize,
    shortlist_min_score: Arc<AtomicScore>,
    comp_channel_rx: Receiver<Comp>,
) -> Result<JoinHandle<Vec<Comp>>, std::io::Error> {
    thread::Builder::new()
        .name("Shortlist".to_owned())
        .spawn(move || {
            let mut shortlist = Shortlist::new(shortlist_size);
            // Repeatedly receive compositions from the worker threads over the channel ...
            while let Ok(comp) = comp_channel_rx.recv() {
                shortlist.push(comp);
                // If the shortlist is full, then update the minimum score required for a composition
                // to make it into the shortlist.  This is several effects:
                // 1. It allows the threads to use this value (and an upper bound of music score) to
                //    prune branches which will never generate enough music
                // 2. It prevents this thread from being inundated with bad comps, because the worker
                //    channels compare potential comps against the min value before sending them down
                //    the channel
                if shortlist.len() == shortlist.capacity() {
                    shortlist_min_score.set(shortlist.peek_min().unwrap().ranking_score);
                }
            }
            // Once all the worker threads have closed, `comp_channel_rx.recv()` will return an error
            // and the `while` loop will exit.  At this point the search is over and no more
            // compositions can be found, so we pass the final comp list to the main thread which then
            // returns it from `compose()`
            shortlist.into_sorted_vec()
        })
}

/// The results of a single search run
#[derive(Debug, Clone)]
pub struct SearchResults {
    pub time_taken: Duration,
    pub comps: Vec<Comp>,
    pub stats: Stats,
}

/// A completed composition.  This requires access to the [`Layout`] from which it was generated in
/// order to provide human-friendly data (like the calling string).
#[derive(Debug, Clone)]
pub struct Comp {
    pub(crate) links_taken: Vec<(NodeId, usize)>,
    pub(crate) end_id: NodeId,
    pub length: usize,
    /// The absolute score of the composition
    pub score: Score,
    /// The score of the composition used for ranking.  When using relative scoring, the scores are
    /// normalised against length to avoid bias towards longer comps.
    pub ranking_score: Score,
}

impl Comp {
    /// Generates the call string for this `Comp`
    pub fn call_string(&self, spec: &Spec) -> String {
        let mut string = String::new();

        write!(&mut string, "{}:", usize::from(self.links_taken[0].0.seg)).unwrap();

        for (id, link_idx) in &self.links_taken {
            let segment = spec.layout.get_segment(id.seg);
            string.push_str(&segment.name);
            string.push_str(&segment.links[*link_idx].display_name);
        }
        string.push_str(&spec.layout.get_segment(self.end_id.seg).name);

        string
    }

    /// Generates a 1-line summary string of this `Comp`
    pub fn to_string(&self, spec: &Spec) -> String {
        format!(
            "(len: {}, score: {}) {}",
            self.length,
            self.score,
            self.call_string(spec)
        )
    }
}

/* Compare `Comp`s according to their scores */

impl PartialOrd for Comp {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for Comp {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.ranking_score.cmp(&other.ranking_score)
    }
}

impl PartialEq for Comp {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.ranking_score == other.ranking_score
    }
}

impl Eq for Comp {}

/// The type sent down the `stats_channel`
#[derive(Debug, Clone)]
struct StatsUpdate {
    /// Which thread sent this `StatsUpdate`
    thread_id: usize,
    /// The statistics accumulated by the thread **since the last `StatsUpdate` sent down the
    /// `stats_channel`**
    stats_since_last_signal: Stats,
    /// The prefix that is loaded by the thread
    current_prefix: CompPrefix,
}

/// An element stored in the [`PrefixQueue`]
#[derive(Debug, Clone)]
pub(crate) struct QueueElem {
    prefix: CompPrefix,
    percentage: f64,
}

impl QueueElem {
    pub(crate) fn new(prefix: CompPrefix, percentage: f64) -> Self {
        Self { prefix, percentage }
    }

    pub(crate) fn just_start_node(id: NodeId, num_start_nodes: usize) -> Self {
        QueueElem {
            prefix: CompPrefix::just_start_node(id),
            /// Split the percentage equally between each start node
            percentage: 100.0 / num_start_nodes as f64,
        }
    }

    pub(crate) fn push(&mut self, succ_idx: usize, num_successors: usize) {
        self.prefix.push(succ_idx);
        // Split the percentage among all the successors
        self.percentage /= num_successors as f64;
    }
}

/// The prefix of a composition, or a branch of the search tree.
#[derive(Debug, Clone)]
pub(crate) struct CompPrefix {
    /// Which of the possible starting nodes was used to start the currently exploring composition
    start_node: NodeId,
    /// Which links where chosen after each node.  These are indices into the `successors` array in
    /// each [`Node`].  This is really cheap to track during the composing loop, but means that
    /// recovering a human-friendly representation requires traversing the node graph and
    /// performing lots of lookups into the [`Layout`].  I think this is an acceptable trade-off.
    successor_idxs: Vec<usize>,
}

impl CompPrefix {
    /// Creates a new `CompPrefix` with an empty start [`NodeId`] and no links taken.  The empty
    /// [`NodeId`] must be overwritten before search commences otherwise the code will panic.
    pub fn empty() -> Self {
        Self::just_start_node(NodeId::new(SegmentId::from(0), RowBuf::empty()))
    }

    /// Creates a new `CompPrefix` with an empty start [`NodeId`] and no links taken.  The empty
    /// [`NodeId`] must be overwritten before search commences otherwise the code will panic.
    pub fn just_start_node(start_node: NodeId) -> Self {
        Self {
            start_node,
            successor_idxs: Vec::new(),
        }
    }

    /// Push a new successor index to the end of the prefix, thus making it one node longer
    pub fn push(&mut self, idx: usize) {
        self.successor_idxs.push(idx);
    }

    /// Pop the last successor index off the end of the prefix, thus making it one node shorter
    pub fn pop(&mut self) -> Option<usize> {
        self.successor_idxs.pop()
    }
}
