use std::{
    cell::Cell, cmp::Ordering, collections::VecDeque, fmt::Write, iter::Fuse, ops::Range,
    sync::MutexGuard,
};

use bellframe::RowBuf;
use itertools::Itertools;
use shortlist::Shortlist;

use crate::{
    graph::{Graph, Node, NodeId},
    layout::Layout,
    Engine, SegmentId,
};

/// The mutable data required to generate a composition.  Each worker thread will have their own
/// `EngineWorker` struct (but all share the same [`Engine`]).
#[derive(Debug)]
pub(crate) struct EngineWorker<'e> {
    thread_id: usize,
    len_range: Range<usize>,
    engine: &'e Engine,
    /// A `Shortlist` of discovered compositions
    shortlist: Shortlist<Comp>,
    /// The prefix of the currently loaded composition
    comp_prefix: CompPrefix,
}

impl<'e> EngineWorker<'e> {
    /// Creates a new `EngineWorker`
    pub fn from_engine(engine: &'e Engine, thread_id: usize) -> Self {
        EngineWorker {
            thread_id,
            engine,
            len_range: engine.len_range.clone(),
            shortlist: Shortlist::new(engine.num_comps),
            comp_prefix: CompPrefix::empty(),
        }
    }

    /// Run graph search over the node graph to find compositions.
    pub fn compose(&mut self) -> Vec<Comp> {
        let graph = Graph::from_prototype(
            &self.engine.prototype_graph,
            |node_id| NodePayload::new(node_id, &self.engine),
            |_node_id| ExtraPayload(),
        );

        loop {
            // If this thread has nothing to do, then see if the global queue has any more prefixes
            // to explore
            let mut queue = self.engine.unexplored_prefixes.lock().unwrap();
            println!("[{:>2}] Queue len: {}", self.thread_id, queue.len());
            if let Some(new_prefix) = queue.pop_front() {
                // If the queue isn't empty yet, then pick a new prefix to explore
                self.explore_prefix(&graph, new_prefix, queue);
            } else {
                // If the queue is empty, then there's no point keeping this thread running so
                // return
                break;
            }
        }

        // Once the search has finished, read the best comps from the shortlist and return them
        self.shortlist.drain().collect_vec()
    }

    /// Load a composition prefix, push a new prefix to the queue and then continue tree search
    fn explore_prefix(
        &mut self,
        graph: &Graph<NodePayload, ExtraPayload>,
        prefix: CompPrefix,
        queue: MutexGuard<VecDeque<CompPrefix>>,
    ) {
        // Update the internal state to point to the start node of the `prefix`
        let start_node = graph.get_start_node(&prefix.start_node).unwrap();
        self.comp_prefix.start_node = prefix.start_node;
        self.comp_prefix.successor_idxs.clear();

        self.expand_node(
            start_node,
            0,
            0.0,
            Some((prefix.successor_idxs.iter().cloned().fuse(), queue)),
        );
    }

    /// Test a node, and either expand it or prune.  All arguments apply to the composition
    /// explored up to the first row of `node`.
    fn expand_node<I: Iterator<Item = usize>>(
        &mut self,
        node: &Node<NodePayload, ExtraPayload>,
        length: usize,
        score: f32,
        mut prefix_to_expand: Option<(Fuse<I>, MutexGuard<VecDeque<CompPrefix>>)>,
    ) {
        let payload = node.payload();
        let length_after_this_node = length + node.length();
        let score_after_this_node = score + node.score();

        // Potentially prune the node
        if self.should_node_be_pruned(node, length_after_this_node, score_after_this_node) {
            return;
        }

        /* ===== ADD THE NODE TO THE COMPOSITION ===== */

        // Sanity check that adding this node wouldn't make the comp false
        debug_assert_eq!(payload.falseness_count.get(), 0);

        // Since we are committing to ringing this node, we should register its falseness against
        // other nodes
        for &n in node.false_nodes() {
            let false_count_cell = &n.payload().falseness_count;
            false_count_cell.set(false_count_cell.get() + 1);
        }

        /* ===== EXPAND CHILD NODES ===== */

        if let Some((succ_iter, queue)) = &mut prefix_to_expand {
            if let Some(succ_idx) = succ_iter.next() {
                // ... if we're part way through expanding a prefix then take the successors
                // dictated by that prefix
                let succ_node = node.successors()[succ_idx];
                // Add the new link to the composition
                self.comp_prefix.push(succ_idx);
                self.expand_node(
                    succ_node,
                    length_after_this_node,
                    score_after_this_node,
                    prefix_to_expand,
                );
                self.comp_prefix.pop();
            } else {
                // ... if we've finished expanding a prefix, then explore the first branch whilst
                // pushing the 2nd branch back onto the queue (then dropping the mutex lock to give
                // control back to other threads).

                // This node can't be an end node (because otherwise it would have been pruned),
                // and therefore must have at least one successor.  We want to push a prefix back
                // into the queue (to keep the queue full for as long as possible), so we want to
                // explore down the tree until we reach a node with more than one successor.  At
                // this point, this thread continues exploring the first (and likely best) branch,
                // pushing the 2nd one back to the queue.
                let mut succ_iter = node.successors().iter().enumerate();
                let (first_idx, first_succ) = succ_iter
                    .next()
                    .expect("Non-end nodes should have at least one successor.");

                if let Some((idx, _second_succ)) = succ_iter.next() {
                    // If this node does have at least two successors, then push its 2nd successor
                    // onto the queue for the future nodes to explore
                    debug_assert_eq!(idx, 1);
                    let mut second_prefix = self.comp_prefix.clone();
                    second_prefix.push(idx);
                    queue.push_back(second_prefix);
                    drop(prefix_to_expand);

                    // Now iterate over the remaining successors and perform tree search as usual
                    self.comp_prefix.push(first_idx);
                    self.expand_node::<I>(
                        first_succ,
                        length_after_this_node,
                        score_after_this_node,
                        None,
                    );
                    self.comp_prefix.pop();
                    for (i, &succ) in succ_iter {
                        // Add the new link to the composition
                        self.comp_prefix.push(i);
                        self.expand_node::<I>(
                            succ,
                            length_after_this_node,
                            score_after_this_node,
                            None,
                        );
                        self.comp_prefix.pop();
                    }
                } else {
                    // If this node only has one successor, then keep searching down the tree for a
                    // branch
                    debug_assert_eq!(first_idx, 0);
                    // Add the new link to the composition
                    self.comp_prefix.push(first_idx);
                    self.expand_node::<I>(
                        first_succ,
                        length_after_this_node,
                        score_after_this_node,
                        prefix_to_expand,
                    );
                    self.comp_prefix.pop();
                }
            }
        } else {
            // ... if we're not expanding a prefix, then do tree search as normal
            for (i, &succ) in node.successors().iter().enumerate() {
                self.comp_prefix.push(i);
                // Add the new link to the composition
                self.expand_node::<I>(succ, length_after_this_node, score_after_this_node, None);
                self.comp_prefix.pop();
            }
        }

        /* ===== REMOVE THIS NODE FROM THE COMPOSITION ===== */

        // Decrement the falseness counters on all the nodes false against this one
        for &n in node.false_nodes() {
            let false_count_cell = &n.payload().falseness_count;
            false_count_cell.set(false_count_cell.get() - 1);
        }

        // Sanity check that the falseness count has been reset to 0
        debug_assert_eq!(payload.falseness_count.get(), 0);
    }

    /// Determine whether or not a given node should be explored, given the currently loaded
    /// prefix.
    #[must_use]
    fn should_node_be_pruned(
        &mut self,
        node: &Node<NodePayload, ExtraPayload>,
        length_after_this_node: usize,
        score_after_this_node: f32,
    ) -> bool {
        {
            // If the node is false against anything in the comp prefix, then prune
            if node.payload().falseness_count.get() != 0 {
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
        }
        false
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
