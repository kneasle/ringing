//! The code for a single thread running tree search

use std::{
    cell::Cell,
    sync::{mpsc::SyncSender, Arc},
};

use crossbeam_deque::Steal;
// Itertools is used only in debug builds by the graph reset check, but causes a warning in release
// mode
#[allow(unused_imports)]
use itertools::Itertools;
use log::Level;

use crate::{
    graph,
    graph2::ProtoGraph,
    score::AtomicScore,
    spec::{layout::NodeId, Config, Spec},
    stats::Stats,
    Comp, Score,
};

use super::{CompPrefix, PrefixQueue, QueueElem, StatsPayload, StatsUpdate};

// Type aliases for the specific payload types that we use.  It makes no sense for us to fully
// specify the same type parameters every time we use `Graph` or `Node`
type Graph = graph::Graph<NodePayload, ExtraPayload>;
type Node = graph::Node<NodePayload, ExtraPayload>;

/// 'Immutable' data shared between all the workers.  This also includes types with interior
/// mutability (like channels and atomic integers), but the important thing is that they don't
/// require the [`Worker`]s to hold mutable references to them.
#[derive(Debug, Clone)]
pub(super) struct SharedData {
    // Immutable lookup data
    spec: Arc<Spec>,
    config: Arc<Config>,
    // Shared state with interior mutability
    pub shortlist_min_score: Arc<AtomicScore>,
    comp_channel_tx: SyncSender<Comp>,
    stats_channel_tx: SyncSender<StatsUpdate>,
}

impl SharedData {
    pub(super) fn new(
        spec: Arc<Spec>,
        config: Arc<Config>,
        comp_channel_tx: SyncSender<Comp>,
        stats_channel_tx: SyncSender<StatsUpdate>,
    ) -> Self {
        Self {
            spec,
            config,
            shortlist_min_score: Arc::new(AtomicScore::new(Score::MIN)),
            comp_channel_tx,
            stats_channel_tx,
        }
    }
}

/// A single worker thread which is generating compositions
#[derive(Debug)]
pub(super) struct Worker {
    thread_id: usize,
    /// The [`CompPrefix`] currently loaded by this Worker
    comp_prefix: CompPrefix,
    shared_data: SharedData,
    stats: Stats,
    /// The length of the prefix popped from the job queue that this `Worker` is currently
    /// exploring
    prefix_len_from_queue: usize,
}

impl Worker {
    /// Creates a `EngineWorker` and in-memory node [`Graph`] for the current thread, and pops
    /// elements off the queue for tree searching.  `EngineWorker::compose` is called once for each
    /// worker thread.
    pub(super) fn start(
        shared_data: SharedData,
        unexplored_prefix_queue: &PrefixQueue,
        thread_id: usize,
    ) {
        let worker = Worker {
            thread_id,
            shared_data,
            stats: Stats::zero(),
            comp_prefix: CompPrefix::empty(),
            prefix_len_from_queue: 0,
        };
        worker._start(unexplored_prefix_queue);
    }

    /// Version of `Self::start` which takes the worker as a `self` parameter for convenience
    fn _start(mut self, unexplored_prefix_queue: &PrefixQueue) {
        // Generate a compact **copy** of the node graph where links are represented as pointers.
        // It is very important that this is an exact copy, otherwise the composition callings will
        // not be recovered correctly.
        let prototype_graph = &self.shared_data.spec.prototype_graph;
        let graph = Graph::from_prototype(
            prototype_graph,
            |node_id| NodePayload::new(node_id, prototype_graph),
            |_node_id| ExtraPayload(),
        );

        'compose_loop: loop {
            // If this thread has nothing to do, then see if the global queue has any more prefixes
            // to explore
            if self.shared_data.config.log_level >= Level::Debug {
                println!(
                    "[{:>2}] Queue len: {}",
                    self.thread_id,
                    unexplored_prefix_queue.len()
                );
            }

            let queue_head = loop {
                match unexplored_prefix_queue.steal() {
                    // If the queue isn't empty yet, then pick a new prefix to explore
                    Steal::Success(queue_head) => break queue_head,
                    // If the queue is empty, then the search is almost over so exit the outer
                    // loop (thus allowing this thread to terminate)
                    Steal::Empty => break 'compose_loop,
                    // If the queue gave a spurious retry, then go round the loop again
                    Steal::Retry => {}
                }
            };

            self.explore_prefix(&graph, queue_head, unexplored_prefix_queue);
        }

        // If the shortlist has no values left, then the search must be nearly finished and there's
        // no point keeping this thread running any longer.  Therefore we return, stopping this
        // thread and letting the others finish the search.
        if self.shared_data.config.log_level >= Level::Debug {
            println!("[{:>2}] Terminating", self.thread_id);
        }
    }

    /// Load a composition prefix, explore until the branch splits into two, then explore one
    /// branch whilst pushing the other back to the queue.  Thus no branches are lost and the queue
    /// only starts getting shorter when there are only very short branches left.
    fn explore_prefix(&mut self, graph: &Graph, queue_elem: QueueElem, queue: &PrefixQueue) {
        let QueueElem { prefix, percentage } = queue_elem;

        /* ===== MOVE TO START NODE ===== */

        // Update the internal state to point to the start node of the `prefix`
        let start_node = graph.get_start_node(prefix.start_idx).unwrap();
        self.comp_prefix.start_idx = prefix.start_idx;
        self.comp_prefix.successor_idxs.clear();

        // Declare variables to track the state of the nodes explored so far
        let mut length_after_node = 0usize;
        let mut score_after_node = Score::ZERO;
        let mut node = start_node;
        // This keeps track of which nodes have been loaded to the graph before tree search
        // commences.  These will then be unloaded in reverse order before returning, so it is
        // **VERY** important that every node that is loaded is pushed onto this stack.  All nodes
        // are loaded through `add_node!`, which takes care of this.
        let mut loaded_node_stack: Vec<&Node> = Vec::new();

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
        queue.push(QueueElem::new(prefix_to_push, percentage_per_branch));

        // Drop the mutex lock on the queue as soon as possible to prevent unnecessary locking of
        // the other threads
        drop(queue);

        // Before starting tree search, store the length of the comp_prefix from the queue (which
        // will be used later in percentage calculations).
        self.prefix_len_from_queue = self.comp_prefix.successor_idxs.len();

        /* ===== RUN DFS ON THE BRANCHES THAT WEREN'T PUSHED BACK TO THE QUEUE ===== */

        for (succ_idx, succ_node) in successors[..successors.len() - 1].iter().enumerate() {
            self.comp_prefix.push(succ_idx);
            self.explore_node(succ_node, length_after_node, score_after_node);
            self.comp_prefix.pop();
        }

        /* ===== RESET THE NODE GRAPH ===== */

        // Unload all the nodes which were loaded in the pre-tree search prefix, thus (hopefully)
        // resetting the graph
        unload_all!();

        // Check that the graph is reset (but only in debug mode)
        Self::assert_graph_reset(graph);

        // Tell the stats thread that we've finished this prefix
        self.shared_data
            .stats_channel_tx
            .send(StatsUpdate {
                thread_id: self.thread_id,
                stats_since_last_signal: self.stats,
                payload: StatsPayload::FinishedPrefix {
                    // We've finished the percentage from our allocated prefix, except the portion
                    // that we pushed back to the queue
                    percentage_done: percentage - percentage_per_branch,
                },
            })
            .unwrap();
        self.stats.reset();
    }

    /// Does nothing (in release builds)
    #[cfg(not(debug_assertions))]
    fn assert_graph_reset(_graph: &Graph) {}

    /// Checks that the state of the graph is reset
    #[cfg(debug_assertions)]
    fn assert_graph_reset(graph: &Graph) {
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
    fn explore_node(&mut self, node: &Node, length: usize, score: Score) {
        let length_after_this_node = length + node.length();
        let score_after_this_node = score + node.score();

        // Load the node into the node graph, potentially pruning this tree branch (if, for
        // example, the new node is false against the composition).
        let should_prune = self.load_node(node, length_after_this_node, score_after_this_node);
        if should_prune {
            return;
        }

        // Every so often, send an update to the stats thread (not too often because we don't want
        // to spam the channel with loads of messages).
        if self.stats.nodes_loaded % (1 << 23) == 0 {
            self.send_stats_update();
        }

        // Expand successor nodes
        for (i, &succ) in node.successors().iter().enumerate() {
            self.comp_prefix.push(i);
            // Add the new link to the composition
            self.explore_node(succ, length_after_this_node, score_after_this_node);
            self.comp_prefix.pop();
        }

        // Remove this node from the composition
        self.unload_node(node);
    }

    /// Send a [`StatsUpdate`] down the stats_channel
    #[inline(never)]
    fn send_stats_update(&mut self) {
        let prototype_graph = &self.shared_data.spec.prototype_graph;

        self.shared_data
            .stats_channel_tx
            .send(StatsUpdate {
                thread_id: self.thread_id,
                stats_since_last_signal: self.stats,
                payload: StatsPayload::Update {
                    current_prefix: self.comp_prefix.clone(),
                    percentage_so_far: prototype_graph
                        .compute_percentage(&self.comp_prefix, self.prefix_len_from_queue),
                },
            })
            .unwrap();

        // We need to reset the stats counter to make sure that stats are only counted once
        self.stats.reset();
    }

    /// Load a new [`Node`] into the composition prefix, updating the state of the [`Graph`]
    /// accordingly.  If the branch rooted at the new [`Node`] should be pruned, then `true` is
    /// returned and the [`Graph`] is left unchanged.
    #[inline(always)]
    #[must_use]
    fn load_node(
        &mut self,
        node: &Node,
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
            >= self.shared_data.spec.len_range.end
        {
            return true;
        }
        // If we've found an end node, then this must be the end of the composition
        if node.is_end()
            && self
                .shared_data
                .spec
                .len_range
                .contains(&length_after_this_node)
        {
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
    fn unload_node(&mut self, node: &Node) {
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
        let ranking_score = if self.shared_data.spec.normalise_music {
            score / length
        } else {
            score
        };
        // If the composition wouldn't make it into the shortlist, then there's no point creating
        // the `Comp` struct (which is quite time intensive)
        if self.shared_data.shortlist_min_score.get() >= ranking_score {
            return;
        }

        // Traverse the prototype graph and generate the comp
        let (start_idx, links_taken, end_idx) =
            self.shared_data.spec.prototype_graph.generate_path(
                self.comp_prefix.start_idx,
                self.comp_prefix.successor_idxs.iter().cloned(),
                &self.shared_data.spec.layout,
            );
        let comp = Comp {
            start_idx,
            links_taken,
            end_idx,
            length,
            score,
            ranking_score,
        };

        if self.shared_data.config.log_level >= Level::Debug {
            println!(
                "[{:>2}] FOUND COMP! {}",
                self.thread_id,
                comp.to_string(&self.shared_data.spec.layout)
            );
        }

        self.shared_data
            .comp_channel_tx
            .send(comp)
            .expect("Shortlist thread terminated before workers");
    }
}

/// The payload stored in each [`Node`] in the [`Graph`]
#[derive(Debug, Clone)]
pub struct NodePayload {
    /// The number of nodes which are false against this one and are already in the composition
    /// (including itself).
    /// Nodes will only be expanded if this is 0
    falseness_count: Cell<u32>,
    /// Assuming no falseness, the minimum number of rows required to get from the first row
    /// **after** this node to rounds.  Note the word 'after': this is the opposite way to how they
    /// are defined when reducing the graph size in [`ProtoGraph`]
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
