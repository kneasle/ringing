use std::{
    cell::Cell,
    cmp::Ordering,
    collections::VecDeque,
    fmt::Write,
    ops::Range,
    sync::{Arc, Mutex, MutexGuard},
    thread,
};

use bellframe::RowBuf;
use itertools::Itertools;
use shortlist::Shortlist;

use crate::{
    graph::{Graph, Node, NodeId},
    layout::Layout,
    Engine, SegmentId,
};

/// Generate compositions specified by the [`Engine`].  The current thread is blocked until the
/// best compositions have been found and returned.
pub fn compose(num_threads: usize, engine: &Arc<Engine>) -> Vec<Comp> {
    // We use a Mutex-locked global queue of unexplored prefixes to make sure that the workers
    // always have compositions to explore.
    let unexplored_prefixes = Arc::new(Mutex::new(
        engine.prototype_graph.generate_prefixes(num_threads * 2),
    ));

    // Spawn all the worker threads
    let threads = (0usize..num_threads)
        .map(|i| {
            // Create thread-local copies of the shared data which will be captured by the closure
            // of the new thread
            let thread_arc = engine.clone();
            let thread_queue = unexplored_prefixes.clone();

            // Spawn a new worker thread, which will immediately start generating compositions
            thread::Builder::new()
                .name(format!("Worker{}", i))
                .spawn(move || EngineWorker::compose(&thread_arc, &thread_queue, i))
                .unwrap()
        })
        .collect_vec();

    // Wait for the worker threads to return, and merge all the individual shortlists into one
    // shortlist for the overall best comps
    let mut main_shortlist: Shortlist<Comp> = Shortlist::new(engine.num_comps);
    for t in threads {
        let comps = t.join().unwrap();
        main_shortlist.append(comps);
    }

    main_shortlist.into_sorted_vec()
}

/// The mutable data required to generate a composition.  Each worker thread will have their own
/// `EngineWorker` struct (but all share the same [`Engine`]).
#[derive(Debug)]
struct EngineWorker<'e> {
    thread_id: usize,
    len_range: Range<usize>,
    engine: &'e Engine,
    /// A `Shortlist` of discovered compositions
    shortlist: Shortlist<Comp>,
    /// The [`CompPrefix`] currently loaded by this Worker
    comp_prefix: CompPrefix,
}

impl<'e> EngineWorker<'e> {
    /// Creates a `EngineWorker` and in-memory node [`Graph`] for the current thread, and start
    /// tree searching.  This function is called once for each worker thread.
    fn compose(
        engine: &'e Engine,
        unexplored_prefixes: &Mutex<VecDeque<CompPrefix>>,
        thread_id: usize,
    ) -> Vec<Comp> {
        let mut worker = EngineWorker {
            thread_id,
            engine,
            len_range: engine.len_range.clone(),
            shortlist: Shortlist::new(engine.num_comps),
            comp_prefix: CompPrefix::empty(),
        };

        // Generate a compact **copy** of the node graph where links are represented as pointers.
        // It is very important that this is an exact copy, otherwise the composition callings will
        // not be recovered correctly.
        let graph = Graph::from_prototype(
            &engine.prototype_graph,
            |node_id| NodePayload::new(node_id, &engine),
            |_node_id| ExtraPayload(),
        );

        loop {
            // If this thread has nothing to do, then see if the global queue has any more prefixes
            // to explore
            let mut queue = unexplored_prefixes.lock().unwrap();
            println!("[{:>2}] Queue len: {}", worker.thread_id, queue.len());
            match queue.pop_front() {
                // If the queue isn't empty yet, then pick a new prefix to explore
                Some(new_prefix) => worker.explore_prefix(&graph, new_prefix, queue),
                // If the queue is empty, then the search is almost over so exit the loop and stop
                // the thread
                None => break,
            }
        }

        // Once the search has finished, read the best comps from the shortlist and return them
        worker.shortlist.drain().collect_vec()
    }

    /// Load a composition prefix, explore until the branch splits into two, then explore one
    /// branch whilst pushing the other back to the queue.  Thus no branches are lost and the queue
    /// only starts getting shorter when there are only very short branches left.
    fn explore_prefix(
        &mut self,
        graph: &Graph<NodePayload, ExtraPayload>,
        prefix: CompPrefix,
        mut queue: MutexGuard<VecDeque<CompPrefix>>,
    ) {
        // Update the internal state to point to the start node of the `prefix`
        let start_node = graph.get_start_node(&prefix.start_node).unwrap();
        self.comp_prefix.start_node = prefix.start_node;
        self.comp_prefix.successor_idxs.clear();

        println!("Loading {:?}", prefix.successor_idxs);

        /// A macro to reset the node graph and return from this function (dropping the mutex lock
        /// on the way).
        macro_rules! reset_and_return {
            () => {{
                // There's no point having other threads wait for us to reset the graph, so drop
                // the mutex **before** performing the reset.
                drop(queue);
                self.unload_all(start_node);
                return;
            }};
        }

        /* ===== LOAD THE COMPOSITION PREFIX ===== */

        // Load the composition prefix.  If any part of the prefix ends up being false (which can
        // only happen to the initial set of prefixes generated by the main thread), then there's
        // no point performing any more tree search (and consequently we have no prefixes to push
        // back onto the stack).
        let mut length_after_node = 0usize;
        let mut score_after_node = 0f32;
        let mut node = start_node;

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
        // Because these successors are sorted by 'goodness', we always want to get to the good
        // comps as quickly as possible.  We'll assume that pushing prefixes to the queue is the
        // slowest way of expanding them, so therefore we push the last successor to the branch
        let mut prefix_to_push = self.comp_prefix.clone();
        prefix_to_push.push(successors.len() - 1);
        queue.push_back(prefix_to_push);
        // Drop the mutex lock on the queue as soon as possible to prevent unnecessary locking of
        // the other threads
        drop(queue);

        /* ===== SEARCH THE OTHER BRANCHES ===== */

        for (succ_idx, succ_node) in successors[..successors.len() - 1].iter().enumerate() {
            self.comp_prefix.push(succ_idx);
            self.expand_node(succ_node, length_after_node, score_after_node);
            self.comp_prefix.pop();
        }

        /* ===== RESET THE NODE GRAPH ===== */

        // Reset the node graph so that the next prefix can be explored
        self.unload_all(start_node);
    }

    /// Unload all the nodes that are currently loaded.  This has the effect of fully resetting the
    /// graph to the 'empty' state, ready for a new search to be started.  This unloads the nodes
    /// in reverse order, as though the stack was being unwound.
    fn unload_all(&mut self, start_node_ref: &Node<NodePayload, ExtraPayload>) {
        // List for the nodes we've explored (which starts with just the start node)
        let mut nodes: Vec<&Node<NodePayload, ExtraPayload>> = vec![start_node_ref];

        // Walk through the graph, storing a reference of each node as we go
        let mut current_node = start_node_ref;
        for &succ_idx in &self.comp_prefix.successor_idxs {
            current_node = current_node.successors()[succ_idx];
            nodes.push(current_node);
        }

        // Now unload all these nodes in reverse order (the order they would be unloaded in tree
        // search)
        for node in nodes.into_iter().rev() {
            self.unload_node(node);
        }
    }

    /// Test a node, and either expand it or prune.  All arguments apply to the composition
    /// explored up to the first row of `node`.
    fn expand_node(&mut self, node: &Node<NodePayload, ExtraPayload>, length: usize, score: f32) {
        let length_after_this_node = length + node.length();
        let score_after_this_node = score + node.score();

        // Load the node into the node graph, potentially pruning this tree branch (if, for
        // example, the new node is false against the composition).
        let should_prune = self.load_node(node, length_after_this_node, score_after_this_node);
        if should_prune {
            return;
        }

        // Expand successor nodes
        for (i, &succ) in node.successors().iter().enumerate() {
            self.comp_prefix.push(i);
            // Add the new link to the composition
            self.expand_node(succ, length_after_this_node, score_after_this_node);
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
        score_after_this_node: f32,
    ) -> bool {
        let payload = node.payload();

        // If the node is false against anything in the comp prefix, then prune
        if payload.falseness_count.get() != 0 {
            return true;
        }

        // If the node would make the comp too long, then prune
        if length_after_this_node >= self.len_range.end {
            return true;
        }

        // If we've found an end node, then this must be the end of the composition
        if node.is_end() && self.len_range.contains(&length_after_this_node) {
            self.save_comp(length_after_this_node, score_after_this_node);
            return true;
        }

        /* If none of the checks pruned this branch, then add this node to the graph */

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

    /// Unload a node from the composition.  Provided that all the load/unload calls are well
    /// balanced, this will precisely undo the effect of [`Self::load_node`].
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
    fn save_comp(&mut self, length: usize, score: f32) {
        // If enabled, normalise the music scores by length
        let ranking_score = if self.engine.config.normalise_music {
            score / length as f32
        } else {
            score
        };
        // If the composition wouldn't make it into the shortlist, then there's no point creating
        // the `Comp` struct (which is quite time intensive)
        if self.shortlist.len() == self.shortlist.capacity()
            && self.shortlist.peek_min().unwrap().ranking_score >= ranking_score
        {
            return;
        }

        // Traverse the prototype graph and generate the comp
        let (links_taken, end_id) = self.engine.prototype_graph.generate_path(
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

        println!(
            "[{:>2}] FOUND COMP! {}",
            self.thread_id,
            comp.to_string(&self.engine.layout)
        );

        self.shortlist.push(comp);
    }
}

/// The payload stored in each [`Node`] in the [`Graph`]
#[derive(Debug, Clone)]
pub struct NodePayload {
    /// The number of nodes which are false against this one and are already in the composition.
    /// Nodes will only be expanded if this is 0
    falseness_count: Cell<u32>,
}

impl NodePayload {
    fn new(_node_id: &NodeId, _engine: &Engine) -> Self {
        Self {
            falseness_count: Cell::new(0),
        }
    }
}

/// Payload stored in each [`Node`] behind one extra level of indirection (and therefore should
/// only be used outside of the composing loop).
#[derive(Debug, Clone)]
pub struct ExtraPayload();

/// A completed composition
#[derive(Debug, Clone)]
pub struct Comp {
    pub(crate) links_taken: Vec<(NodeId, usize)>,
    pub(crate) end_id: NodeId,
    pub length: usize,
    /// The absolute score of the composition
    pub score: f32,
    /// The score of the composition used for ranking.  When using relative scoring, this is
    /// normalised by length to avoid biasing towards longer comps.
    ranking_score: f32,
}

impl Comp {
    pub fn to_string(&self, layout: &Layout) -> String {
        let mut string = format!("(len: {}, score: {}) ", self.length, self.score);

        write!(&mut string, "{}:", usize::from(self.links_taken[0].0.seg)).unwrap();

        for (id, link_idx) in &self.links_taken {
            let segment = layout.get_segment(id.seg);
            string.push_str(&segment.name);
            string.push_str(&segment.links[*link_idx].display_name);
        }
        string.push_str(&layout.get_segment(self.end_id.seg).name);

        string
    }
}

/* Compare `Comp`s according to their scores */

impl PartialOrd for Comp {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.ranking_score.partial_cmp(&other.ranking_score)
    }
}

impl Ord for Comp {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl PartialEq for Comp {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.ranking_score == other.ranking_score
    }
}

impl Eq for Comp {}

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
