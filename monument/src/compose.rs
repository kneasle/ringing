use std::{cell::Cell, cmp::Ordering, fmt::Write, ops::Range};

use shortlist::Shortlist;

use crate::{
    graph::{Graph, Node, NodeId},
    layout::Layout,
    Engine,
};

/// The mutable data required to generate a composition.  Each worker thread will have their own
/// `EngineWorker` struct (but all share the same [`Engine`]).
#[derive(Debug)]
pub(crate) struct EngineWorker<'e> {
    thread_id: usize,
    len_range: Range<usize>,
    engine: &'e Engine,
    /// The in-memory [`Graph`] of [`Node`]s
    graph: Graph<NodePayload, ExtraPayload>,
    /// A `Shortlist` of discovered compositions
    shortlist: Shortlist<Comp>,
    /* COMP DATA */
    /// Which of the possible starting nodes was used to start the currently exploring composition
    start_node_idx: usize,
    /// Which links where chosen after each node.  These are indices into the `successors` array in
    /// each [`Node`].  This is really cheap to track during the composing loop, but means that
    /// recovering a human-friendly representation requires traversing the node graph and
    /// performing lots of lookups into the [`Layout`].  I think this is an acceptable trade-off.
    comp_prefix: Vec<usize>,
}

impl<'e> EngineWorker<'e> {
    /// Creates a new `EngineWorker`
    pub fn from_engine(engine: &'e Engine, thread_id: usize) -> Self {
        EngineWorker {
            thread_id,
            engine,
            len_range: engine.len_range.clone(),
            graph: Graph::from_prototype(
                &engine.prototype_graph,
                |node_id| NodePayload::new(node_id, &engine),
                |_node_id| ExtraPayload(),
            ),
            shortlist: Shortlist::new(engine.config.num_comps),
            start_node_idx: 0,
            comp_prefix: Vec::new(),
        }
    }

    /// Run graph search over the node graph to find compositions.
    pub fn compose(&mut self) {
        for (idx, start_node) in self.graph.start_nodes.clone().into_iter().enumerate() {
            self.start_node_idx = idx;
            self.expand_node(unsafe { start_node.as_ref() }.unwrap(), 0);
        }
    }

    /// Test a node, and either expand it or prune
    fn expand_node(&mut self, node: &Node<NodePayload, ExtraPayload>, length: usize) {
        let payload = node.payload();
        let length_after_this_node = length + node.length();

        /* ===== POTENTIALLY PRUNE THE NODE ===== */

        // If the node is false against anything in the comp prefix, then prune
        if payload.falseness_count.get() != 0 {
            return;
        }

        // If we've found an end node, then this must be the end of the composition
        if node.is_end() && self.len_range.contains(&length) {
            self.save_comp(length_after_this_node, f32::NAN);
            return;
        }

        // If the node would make the comp too long, then prune
        if length_after_this_node >= self.len_range.end {
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

        for (i, &succ) in node.successors().iter().enumerate() {
            self.comp_prefix.push(i);
            // Add the new link to the composition
            self.expand_node(succ, length_after_this_node);
            self.comp_prefix.pop();
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

    /// Save the composition currently being explored
    #[inline(never)]
    fn save_comp(&mut self, length: usize, score: f32) {
        let comp = Comp {
            start_node_idx: self.start_node_idx,
            links_taken: self.comp_prefix.clone(),
            length,
            score,
        };

        println!(
            "FOUND COMP! {}",
            comp.to_string(&self.graph, &self.engine.layout)
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
    pub start_node_idx: usize,
    pub links_taken: Vec<usize>,
    pub length: usize,
    pub score: f32,
}

impl Comp {
    #[allow(dead_code)]
    fn to_string(&self, graph: &Graph<NodePayload, ExtraPayload>, layout: &Layout) -> String {
        let mut string = format!("(len: {}, score: {}) ", self.length, self.score);

        write!(&mut string, "{}:", self.start_node_idx).unwrap();

        // Traverse the node graph to reconstruct the calls
        let mut node = unsafe { graph.start_nodes[self.start_node_idx].as_ref() }.unwrap();
        for &succ_idx in &self.links_taken {
            string.push_str(&node.successor_link(succ_idx, layout).display_name);
            node = node.successors()[succ_idx];
        }
        assert!(node.is_end());

        string
    }
}

/* Compare `Comp`s according to their scores */

impl PartialOrd for Comp {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.score.partial_cmp(&other.score)
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
        self.score == other.score
    }
}

impl Eq for Comp {}
