use std::{
    cell::Cell,
    cmp::Ordering,
    collections::VecDeque,
    fmt::Write,
    sync::{
        atomic::{self, AtomicBool},
        mpsc::{sync_channel, SyncSender},
        Arc, Mutex, MutexGuard,
    },
    thread,
    time::{Duration, Instant},
};

use atomic_float::AtomicF64;
use bellframe::RowBuf;
use itertools::Itertools;
use log::Level;
use shortlist::Shortlist;

use crate::{
    graph::{Graph, Node, NodeId, ProtoGraph},
    score::{AtomicScore, Score},
    stats::Stats,
    Config, SegmentId, Spec,
};

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

    /* COMMUNICATION BETWEEN THREADS */

    // A set of atomic floats used by threads to store what percentage (as a proportion of the
    // overall search) of their current `QueueElem` is yet to be explored.  This, plus the
    // percentage derived from the queue itself, gives the overall completion percentage
    let percentage_left_per_thread = std::iter::repeat_with(|| Arc::new(AtomicF64::new(0.0)))
        .take(num_threads)
        .collect_vec();
    // A channel down which compositions will be sent when discovered
    let (comp_channel_tx, comp_channel_rx) = sync_channel(config.comp_buffer_length);
    // We use a Mutex-locked global queue of unexplored prefixes to make sure that the workers
    // always have compositions to explore.
    let unexplored_prefix_queue = Arc::new(Mutex::new(
        spec.prototype_graph.generate_prefixes(num_threads * 2),
    ));

    let engine = Engine {
        spec: spec.clone(),
        config: config.clone(),

        shortlist_min_score: Arc::new(AtomicScore::new(Score::MIN)),
        comp_channel: comp_channel_tx,
    };

    // This flag gets set to `true` once all the workers have returned and the composition is
    // finished
    let has_finished = Arc::new(AtomicBool::new(false));
    // Spawn a thread to track the progress of the other threads
    let stats_thread = thread::Builder::new()
        .name("Stats".to_owned())
        .spawn({
            // Variables captured by the thread
            let has_finished_clone = has_finished.clone();
            let percentage_left_per_thread_clone = percentage_left_per_thread.clone();
            let queue_clone = unexplored_prefix_queue.clone();

            move || {
                loop {
                    // Sleep for a while before updating the stats.  We start by sleeping so that
                    // very short (<0.2s) searches don't print any statistics at all
                    thread::sleep(Duration::from_secs_f64(0.2));

                    // Check if the computation has finished
                    if has_finished_clone.load(atomic::Ordering::Relaxed) {
                        break;
                    }

                    let percentage_left_in_queue = queue_clone
                        .lock()
                        .unwrap()
                        .iter()
                        .map(|elem| elem.percentage)
                        .sum::<f64>();
                    let percentage_left_in_threads = percentage_left_per_thread_clone
                        .iter()
                        .map(|p| p.load(atomic::Ordering::Relaxed))
                        .sum::<f64>();
                    let percentage =
                        100.0 - (percentage_left_in_queue + percentage_left_in_threads);

                    println!("{:>6.2}%", percentage);
                }
            }
        })
        .unwrap();

    // Spawn a thread which will read the compositions output by the other threads
    let shortlist_thread = thread::spawn({
        // Values which will be moved into the thread
        let shortlist_size = spec.num_comps;
        let shortlist_min_score = engine.shortlist_min_score.clone();

        move || {
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
        }
    });

    // Spawn all the worker threads
    let threads = (0usize..num_threads)
        .map(|i| {
            // Create thread-local copies of the shared data which will be captured by the closure
            // of the new thread
            let thread_engine = engine.clone();
            let thread_queue = unexplored_prefix_queue.clone();
            let thread_percentage_left_var = percentage_left_per_thread[i].clone();

            // Spawn a new worker thread, which will immediately start generating compositions
            thread::Builder::new()
                .name(format!("Worker{}", i))
                .spawn(move || {
                    EngineWorker::compose(
                        thread_engine,
                        &thread_queue,
                        i,
                        thread_percentage_left_var,
                    )
                })
                .unwrap()
        })
        .collect_vec();

    // We have to drop `engine` here because it holds a sender for the composition channel.  The
    // channel won't close until ALL the senders have been dropped, so not dropping `Engine` here
    // causes a deadlock: the main thread waits for `shortlist_thread` to terminate, thus keeping
    // the channel alive and preventing `shortlist_thread` from terminating.
    drop(engine);

    // Wait for the worker threads to finish, then total up all their statistics
    let mut all_stats = Stats::zero();
    for t in threads {
        all_stats += t.join().unwrap();
    }

    // Stop the timer **before** waiting for the auxiliary threads to finish.  The stats/percentage
    // thread only checks the termination condition 5x a second, so waiting for it essentially
    // rounds all times to the nearest 5th of a second, which is not useful for short benchmarks
    let time_taken = Instant::now() - start_time;

    // Once all the workers have finished, wait for the shortlist thread to finish and get its
    // composition list
    let comps = shortlist_thread.join().unwrap();
    // Stop the stats thread
    has_finished.store(true, atomic::Ordering::Relaxed);
    stats_thread.join().unwrap();

    SearchResults {
        comps,
        stats: all_stats,
        time_taken,
    }
}

/// 'Immutable' data shared between all the workers.  This also includes types with interior
/// mutability (like channels and atomic integers), but the important thing is that they don't
/// require the [`EngineWorker`]s to hold mutable references to them.
#[derive(Debug, Clone)]
struct Engine {
    // Immutable lookup data
    spec: Arc<Spec>,
    config: Arc<Config>,
    // Shared state with interior mutability
    shortlist_min_score: Arc<AtomicScore>,
    comp_channel: SyncSender<Comp>,
}

/// The mutable data required to generate a composition.  Each worker thread will have their own
/// `EngineWorker` struct (but all share the same [`Engine`]).
#[derive(Debug)]
struct EngineWorker {
    thread_id: usize,
    engine: Engine,
    stats: Stats,
    percentage_left: f64,
    percentage_left_atomic: Arc<AtomicF64>,
    /// The [`CompPrefix`] currently loaded by this Worker
    comp_prefix: CompPrefix,
}

impl EngineWorker {
    /// Creates a `EngineWorker` and in-memory node [`Graph`] for the current thread, and start
    /// tree searching.  `EngineWorker::compose` is called once for each worker thread.
    fn compose(
        engine: Engine,
        unexplored_prefix_queue: &PrefixQueue,
        thread_id: usize,
        percentage_left_atomic: Arc<AtomicF64>,
    ) -> Stats {
        let worker = EngineWorker {
            thread_id,
            engine,
            percentage_left: 0.0,
            percentage_left_atomic,
            stats: Stats::zero(),
            comp_prefix: CompPrefix::empty(),
        };
        worker._compose(unexplored_prefix_queue)
    }

    /// Compose function which takes the worker as a `self` parameter for convenience
    fn _compose(mut self, unexplored_prefix_queue: &PrefixQueue) -> Stats {
        // Generate a compact **copy** of the node graph where links are represented as pointers.
        // It is very important that this is an exact copy, otherwise the composition callings will
        // not be recovered correctly.
        let prototype_graph = &self.engine.spec.prototype_graph;
        let graph = Graph::from_prototype(
            prototype_graph,
            |node_id| NodePayload::new(node_id, prototype_graph),
            |_node_id| ExtraPayload(),
        );

        loop {
            // If this thread has nothing to do, then see if the global queue has any more prefixes
            // to explore
            let mut queue = unexplored_prefix_queue.lock().unwrap();

            if self.engine.config.log_level >= Level::Debug {
                println!("[{:>2}] Queue len: {}", self.thread_id, queue.len());
            }

            match queue.pop_front() {
                // If the queue isn't empty yet, then pick a new prefix to explore
                Some(queue_head) => self.explore_prefix(&graph, queue_head, queue),
                // If the queue is empty, then the search is almost over so exit the loop and stop
                // the thread
                None => break,
            }
        }

        // If the shortlist has no values left, then the search must be nearly finished and there's
        // no point keeping this thread running any longer.  Therefore we return, stopping this
        // thread and letting the others finish the search.
        if self.engine.config.log_level >= Level::Debug {
            println!("[{:>2}] Terminating", self.thread_id);
        }

        // Return the worker's statistics
        self.stats
    }

    /// Load a composition prefix, explore until the branch splits into two, then explore one
    /// branch whilst pushing the other back to the queue.  Thus no branches are lost and the queue
    /// only starts getting shorter when there are only very short branches left.
    fn explore_prefix(
        &mut self,
        graph: &Graph<NodePayload, ExtraPayload>,
        queue_elem: QueueElem,
        mut queue: MutexGuard<VecDeque<QueueElem>>,
    ) {
        let QueueElem { prefix, percentage } = queue_elem;

        /* ===== MOVE TO START NODE ===== */

        // Update the internal state to point to the start node of the `prefix`
        let start_node = graph.get_start_node(&prefix.start_node).unwrap();
        self.comp_prefix.start_node = prefix.start_node;
        self.comp_prefix.successor_idxs.clear();

        // Declare variables to track the state of the nodes explored so far
        let mut length_after_node = 0usize;
        let mut score_after_node = Score::ZERO;
        let mut node = start_node;
        // This keeps track of which nodes have been loaded to the graph before tree search
        // commences.  These will then be unloaded in reverse order before returning, so it is
        // **VERY** important that every node that is loaded is pushed onto this stack.  All nodes
        // are loaded through `add_node!`, which takes care of this.
        let mut loaded_node_stack: Vec<&Node<NodePayload, ExtraPayload>> = Vec::new();

        /* ===== DEFINE A LOAD OF USEFUL MACROS ===== */

        // (these macros would be closures, but (a) then we'd need to make sure LLVM inlines them
        // properly and (b) the borrow checker would be overzealous and not compile our code.  I
        // think macros are possibly the simplest way to handle this not-very-simple task).

        /// Macro which generates code to unload all the nodes that have been loaded (in reverse
        /// order to the way they were loaded)
        macro_rules! unload_all {
            () => {{
                for n in loaded_node_stack.into_iter().rev() {
                    self.unload_node(n);
                }
            }};
        }

        /// Macro to reset the node graph and return from this function (dropping the mutex lock
        /// on the way).
        macro_rules! reset_and_return {
            () => {{
                // There's no point having other threads wait for us to reset the graph, so drop
                // the mutex **before** performing the reset.
                drop(queue);
                unload_all!();
                return;
            }};
        }

        /// Macro to add a new node to the prefix (and update the graph and all the necessary
        /// counters).
        macro_rules! add_node {
            () => {{
                // Increment stats for this node
                length_after_node += node.length();
                score_after_node += node.score();
                // Update the graph to include this node in the composition prefix
                let should_prune = self.load_node(node, length_after_node, score_after_node);
                if should_prune {
                    // If the branch including this prefix should be pruned, then there's no point
                    // exploring it further and we need a new prefix from the queue.  Therefore, we
                    // reset the graph and return
                    reset_and_return!();
                }
                // Push the old node to the stack of nodes to unload.  It is important that this
                // contains all nodes which are loaded
                loaded_node_stack.push(node);
            }};
        }

        /// Macro to add a node and then immediately move on to a given successor
        macro_rules! move_to_successor {
            ($succ_idx: expr) => {{
                add_node!();
                // If this branch was not pruned, then push the successor index and move to the next
                // node in the prefix
                let succ_idx = $succ_idx;
                self.comp_prefix.push(succ_idx);
                node = node.successors()[succ_idx];
            }};
        }

        /* ===== LOAD THE COMPOSITION PREFIX ===== */

        // Load the composition prefix.  If any part of the prefix ends up being false (which can
        // only happen to the initial set of prefixes generated by the main thread), then there's
        // no point performing any more tree search (and consequently we have no prefixes to push
        // back onto the stack).
        for &succ_idx in &prefix.successor_idxs {
            move_to_successor!(succ_idx);
        }

        /* ===== EXPLORE TREE UNTIL A BRANCH (NODE WITH >=2 SUCCESSORS) IS FOUND ===== */

        // Now that the prefix has been loaded, we need to find another prefix to put back into the
        // queue.  But we also need a prefix to explore, so we keep expanding the current prefix
        // until we reach a node with more than one successor (which hopefully will happen
        // immediately).
        loop {
            match node.successors().len() {
                // If this node has no successors then this branch is a dead end, so we reset the
                // graph and return to get a new prefix from the queue
                0 => reset_and_return!(),
                // If this node has one successor, then add that successor and continue looking for
                // a node with multiple successors
                1 => move_to_successor!(0),
                // If this node has multiple successors, then we can begin tree search beginning
                // at this node
                _ => {
                    add_node!();
                    break;
                }
            }
        }

        /* ===== SAVE ONE OF THE BRANCHES BACK TO THE QUEUE ===== */

        let successors = node.successors();
        // We only take the final branch into account here, because all the previous branches are
        // either part of the prefix (represented in `percentage`) or have only one successor (and
        // therefore pass the percentage straight to their children).
        let percentage_per_branch = percentage / successors.len() as f64;
        // These successors are sorted by 'goodness', and we always want to get to the good comps
        // as quickly as possible.  We'll assume that pushing prefixes to the queue is the slowest
        // way of expanding them, so therefore we push the last successor back to the queue
        let mut prefix_to_push = self.comp_prefix.clone();
        prefix_to_push.push(successors.len() - 1);

        queue.push_back(QueueElem::new(prefix_to_push, percentage_per_branch));
        // Drop the mutex lock on the queue as soon as possible to prevent unnecessary locking of
        // the other threads
        drop(queue);

        /* ===== RUN DFS ON THE BRANCHES THAT WEREN'T PUSHED BACK TO THE QUEUE ===== */

        // Initialise percentage_left counters before starting tree search
        self.percentage_left = percentage_per_branch * (successors.len() - 1) as f64;
        self.percentage_left_atomic
            .store(self.percentage_left, atomic::Ordering::Relaxed);

        for (succ_idx, succ_node) in successors[..successors.len() - 1].iter().enumerate() {
            self.comp_prefix.push(succ_idx);
            self.explore_node(
                succ_node,
                length_after_node,
                score_after_node,
                percentage_per_branch,
            );
            self.comp_prefix.pop();
        }

        /* ===== RESET THE NODE GRAPH ===== */

        // Unload all the nodes which were loaded in the pre-tree search prefix, thus (hopefully)
        // resetting the graph
        unload_all!();

        // Check that the graph is reset (but only in debug mode)
        Self::assert_graph_reset(graph);
    }

    /// Does nothing (in release builds)
    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn assert_graph_reset(_graph: &Graph<NodePayload, ExtraPayload>) {}

    /// Checks that the state of the graph is reset
    #[cfg(debug_assertions)]
    fn assert_graph_reset(graph: &Graph<NodePayload, ExtraPayload>) {
        // Assert that the graph is properly reset
        let bad_nodes = graph
            .all_nodes()
            .filter(|n| n.payload().falseness_count.get() != 0)
            .collect_vec();

        if !bad_nodes.is_empty() {
            println!(
                "Bad nodes: {:?}",
                bad_nodes.iter().map(|n| n.id()).collect_vec()
            );
            panic!();
        }
    }

    /// Test a node, and either expand it or prune.  All arguments apply to the composition
    /// explored up to the first row of `node`.
    fn explore_node(
        &mut self,
        node: &Node<NodePayload, ExtraPayload>,
        length: usize,
        score: Score,
        // The percentage which will be covered once this node finishes
        percentage_covered_by_this_node: f64,
    ) {
        let length_after_this_node = length + node.length();
        let score_after_this_node = score + node.score();

        // Load the node into the node graph, potentially pruning this tree branch (if, for
        // example, the new node is false against the composition).
        let should_prune = self.load_node(node, length_after_this_node, score_after_this_node);
        if should_prune {
            // If this node was pruned, then tree search has made progress so add this node's
            // percentage to the progress counter.  This is correct because we split the percentage
            // at each level, meaning that updating the counter at every level would result in
            // massive over-counting.
            self.percentage_left -= percentage_covered_by_this_node;
            return;
        }

        // Every so often, update the atomic percentage counters (not too often because we don't
        // want to be doing atomic writes all the time).
        if self.stats.nodes_loaded % 1_000_000 == 0 {
            self.percentage_left_atomic
                .store(self.percentage_left, atomic::Ordering::Relaxed);
        }

        // Split the percentage evenly between the successors
        let percentage_per_successor =
            percentage_covered_by_this_node / node.successors().len() as f64;

        // Expand successor nodes
        for (i, &succ) in node.successors().iter().enumerate() {
            self.comp_prefix.push(i);
            // Add the new link to the composition
            self.explore_node(
                succ,
                length_after_this_node,
                score_after_this_node,
                percentage_per_successor,
            );
            self.comp_prefix.pop();
        }

        // Remove this node from the composition
        self.unload_node(node);
    }

    /// Load a new [`Node`] into the composition prefix, updating the state of the [`Graph`]
    /// accordingly.  If the branch rooted at the new [`Node`] should be pruned, then `true` is
    /// returned and the [`Graph`] is left unchanged.
    #[inline(always)]
    #[must_use]
    fn load_node(
        &mut self,
        node: &Node<NodePayload, ExtraPayload>,
        length_after_this_node: usize,
        score_after_this_node: Score,
    ) -> bool {
        self.stats.on_node_consider();

        let payload = node.payload();

        // If the node is false against anything in the comp prefix, then prune
        if payload.falseness_count.get() != 0 {
            self.stats.nodes_proven_false += 1;
            return true;
        }
        // If the node can't reach rounds in time to make the comp come round, then prune
        if length_after_this_node + payload.dist_from_end_to_rounds as usize
            >= self.engine.spec.len_range.end
        {
            return true;
        }
        // If we've found an end node, then this must be the end of the composition
        if node.is_end() && self.engine.spec.len_range.contains(&length_after_this_node) {
            self.save_comp(length_after_this_node, score_after_this_node);
            return true;
        }

        /* If none of the checks pruned this branch, then add this node to the graph */

        self.stats.nodes_loaded += 1;

        // Since we are committing to ringing this node, we should register its falseness against
        // other nodes
        for &n in node.false_nodes() {
            // Increment the false node's falseness counter by one (because one more node in the
            // composition is false against it).
            let false_count_cell = &n.payload().falseness_count;
            false_count_cell.set(false_count_cell.get() + 1);
        }

        // If this node passed all the pruning checks, tell the caller to continue tree search
        false
    }

    /// Unload a node from the graph.  Provided that all the load/unload calls are well balanced,
    /// this will precisely undo the effect of [`Self::load_node`].
    #[inline(always)]
    fn unload_node(&mut self, node: &Node<NodePayload, ExtraPayload>) {
        // Decrement the falseness counters on all the nodes false against this one
        for &n in node.false_nodes() {
            let false_count_cell = &n.payload().falseness_count;
            false_count_cell.set(false_count_cell.get() - 1);
        }

        // Sanity check that the falseness count has been reset to 0
        debug_assert_eq!(node.payload().falseness_count.get(), 0);
    }

    /// Save the composition corresponding to the path currently being explored
    #[inline(never)]
    fn save_comp(&mut self, length: usize, score: Score) {
        self.stats.on_find_comp();
        // If enabled, normalise the music scores by length
        let ranking_score = if self.engine.spec.normalise_music {
            score / length
        } else {
            score
        };
        // If the composition wouldn't make it into the shortlist, then there's no point creating
        // the `Comp` struct (which is quite time intensive)
        if self.engine.shortlist_min_score.get() >= ranking_score {
            return;
        }

        // Traverse the prototype graph and generate the comp
        let (links_taken, end_id) = self.engine.spec.prototype_graph.generate_path(
            &self.comp_prefix.start_node,
            self.comp_prefix.successor_idxs.iter().cloned(),
        );
        let comp = Comp {
            links_taken,
            end_id,
            length,
            score,
            ranking_score,
        };

        if self.engine.config.log_level >= Level::Debug {
            println!(
                "[{:>2}] FOUND COMP! {}",
                self.thread_id,
                comp.to_string(&self.engine.spec)
            );
        }

        self.engine
            .comp_channel
            .send(comp)
            .expect("Shortlist thread terminated before workers");
    }
}

/// The payload stored in each [`Node`] in the [`Graph`]
#[derive(Debug, Clone)]
pub struct NodePayload {
    /// The number of nodes which are false against this one and are already in the composition.
    /// Nodes will only be expanded if this is 0
    falseness_count: Cell<u32>,
    /// Assuming no falseness, the minimum number of rows required to get from the first row
    /// **after** this node to rounds.  Note the word 'after': this is the opposite way to how they
    /// are calculated by [`ProtoGraph`] when reducing the graph size
    dist_from_end_to_rounds: u32,
}

impl NodePayload {
    fn new(node_id: &NodeId, prototype_graph: &ProtoGraph) -> Self {
        Self {
            dist_from_end_to_rounds: prototype_graph
                .get_min_dist_from_end_to_rounds(node_id)
                .unwrap() as u32,
            falseness_count: Cell::new(0),
        }
    }
}

/// Payload stored in each [`Node`] behind one extra level of indirection (and therefore should
/// only be used outside of the composing loop).
#[derive(Debug, Clone)]
pub struct ExtraPayload();

/// The results of a single search run
#[derive(Debug, Clone)]
pub struct SearchResults {
    pub time_taken: Duration,
    pub comps: Vec<Comp>,
    pub stats: Stats,
}

/// A completed composition
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
