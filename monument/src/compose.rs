use std::{cell::Cell, cmp::Ordering, fmt::Write, ops::Range};

use itertools::Itertools;
use shortlist::Shortlist;

use crate::{
    graph::{Graph, Node, NodeId, ProtoGraph},
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
    start_node: Option<NodeId>,
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
            start_node: None,
            comp_prefix: Vec::new(),
        }
    }

    /// Run graph search over the node graph to find compositions.
    pub fn compose(&mut self) -> Vec<Comp> {
        println!("{}", self.engine.layout.debug_string());
        for (start_id, start_node) in self.graph.start_nodes.clone() {
            println!("{}", start_id);
            self.start_node = Some(start_id);
            self.expand_node(unsafe { start_node.as_ref() }.unwrap(), 0, 0.0);
        }

        // Once the search has finished, read the best comps from the shortlist and return them
        self.shortlist.drain().collect_vec()
    }

    /// Test a node, and either expand it or prune.  All arguments apply to the composition
    /// explored up to the first row of `node`.
    fn expand_node(&mut self, node: &Node<NodePayload, ExtraPayload>, length: usize, score: f32) {
        let payload = node.payload();
        let length_after_this_node = length + node.length();
        let score_after_this_node = score + node.score();

        // println!("{} {:?}", length, self.comp_prefix);

        /* ===== POTENTIALLY PRUNE THE NODE ===== */

        // If the node is false against anything in the comp prefix, then prune
        if payload.falseness_count.get() != 0 {
            return;
        }

        // If we've found an end node, then this must be the end of the composition
        if node.is_end() && self.len_range.contains(&length) {
            self.save_comp(length_after_this_node, score_after_this_node);
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
            self.expand_node(succ, length_after_this_node, score_after_this_node);
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

    /// Save the composition corresponding to the path currently being explored
    #[inline(never)]
    fn save_comp(&mut self, length: usize, score: f32) {
        // If the composition wouldn't make it into the shortlist, then there's no point creating
        // the `Comp` struct
        if self.shortlist.len() == self.shortlist.capacity()
            && self.shortlist.peek_min().unwrap().score >= score
        {
            return;
        }

        let comp = Comp::new(
            &self.engine.prototype_graph,
            self.start_node.as_ref().unwrap(),
            &self.comp_prefix,
            length,
            score,
        );

        println!("FOUND COMP! {}", comp.to_string(&self.engine.layout));

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
    pub score: f32,
}

impl Comp {
    fn new(
        graph: &ProtoGraph,
        start_node: &NodeId,
        links_taken: &[usize],
        length: usize,
        score: f32,
    ) -> Self {
        let (links_taken, end_id) = graph.generate_path(start_node, links_taken.iter().cloned());
        Self {
            links_taken,
            end_id,
            length,
            score,
        }
    }

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
