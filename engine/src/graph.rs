//! Code for handling [`Graph`]s of [`Node`]s.  This is the only part of the code that knows how
//! the node graph is represented in memory, and provides a safe interface of the node graph to the
//! rest of the code.

use std::{
    alloc,
    cmp::{self, Ordering, Reverse},
    collections::{BinaryHeap, HashMap, HashSet},
    ops::Mul,
    pin::Pin,
};

use bellframe::RowBuf;

use crate::{layout::SegmentID, Engine};

/// An in-memory graph of [`Node`]s which is explored to find compositions.  Each [`Node`] carries
/// a custom payload (`P`), which can be used as annotations.
#[derive(Debug)]
pub(crate) struct Graph<P> {
    /// The set of nodes reachable in the graph.  For the rest of the code, these are entirely
    /// owned by the `Graph` (which acts like an arena for DSTs).
    nodes: HashMap<NodeId, Pin<Box<Node<P>>>>,
}

impl<P> Graph<P> {
    /// Create a new `Graph` containing enough nodes to generate all comps of the [`Engine`].
    pub fn from_engine(engine: &Engine, mut gen_payload: impl FnMut(&NodeId) -> P) -> Self
    where
        P: Unpin + std::fmt::Debug,
    {
        // Calculate which nodes are reachable within the length of the composition
        let reachable_node_ids = gen_reachable_node_ids(&engine);
        println!("{} reachable nodes", reachable_node_ids.len());

        // Generate blank nodes (i.e. nodes where all the succession/falseness pointers are
        // `null`, but everything else is initialised).
        let mut nodes: HashMap<NodeId, Pin<Box<Node<P>>>> = reachable_node_ids
            .iter()
            .map(|id| {
                let seg_table = engine.get_seg_table(id.seg_id);
                // Determine the number of successors that are reachable
                let num_successors = seg_table
                    .links
                    .iter()
                    .map(|seg_link| {
                        let succ_node = NodeId::new(
                            seg_link.end_segment,
                            id.row.as_row() * seg_link.transposition.as_row(),
                        );
                        reachable_node_ids.contains(&succ_node)
                    })
                    .count();
                // Determine the number of successors that are reachable
                let num_false_nodes = seg_table
                    .false_segments
                    .iter()
                    .map(|(false_course_head, seg_id)| {
                        let false_node =
                            NodeId::new(*seg_id, id.row.as_row() * false_course_head.as_row());
                        reachable_node_ids.contains(&false_node)
                    })
                    .count();

                (
                    id.clone(),
                    Node::blank(gen_payload(&id), num_successors, num_false_nodes),
                )
            })
            .collect();

        for (id, n) in &nodes {
            println!("{} of {}: {:?}", id.seg_id.v, id.row, n);
        }

        Self { nodes }
    }
}

/// A node in the in-memory graph.  This is a dynamically sized type, since it contains a
/// dynamically sized array of pointers to other nodes.
#[derive(Debug)]
// repr(C) guarantees that the pointer array is the last element (and thus can be extended whist
// allocating).
#[repr(C)]
pub(crate) struct Node<P> {
    /// The inner data for this `Node`.  The layout looks like:
    ///
    /// ```
    /// Node.inner:
    ///     length: usize // The total number of pointers contained within this node
    ///     header (NodeHeader):
    ///         num_successors: usize,
    ///         payload: I, // The custom payload of this node
    ///     slice ([*const Node; length]):
    ///         // The first `num_successors` elements point to the successor nodes
    ///         // The remaining elements point to false nodes
    /// ```
    payload: P,
    num_successors: usize,
    num_false_nodes: usize,
    /// An inline array of pointers to other nodes.  This array is extended whilst allocating, and
    /// has length `num_successors + num_false_nodes`.
    ///
    /// - The first `num_successors` pointers refer to successor nodes
    /// - The next `num_false_nodes` pointers refer to false nodes
    ptrs: [*const Node<P>; 1],
}

impl<P> Node<P> {
    /// Create a `Node` where all the successor pointers are [`null`](std::ptr::null).
    fn blank(payload: P, num_successors: usize, num_false_nodes: usize) -> Pin<Box<Self>>
    where
        P: Unpin,
    {
        let num_pointers = num_successors + num_false_nodes;
        // The size and alignment of a `Node` with one pointer
        let initial_size = std::mem::size_of::<Node<P>>();
        let align = std::mem::align_of::<Node<P>>();
        // The size of the node once the array is expanded (the `max` here ensures `Node.ptrs`
        // always has at least one element, so the unexpanded version is always safe to use).
        let extended_size = initial_size + (cmp::max(num_pointers, 1) - 1) * 8;

        // Allocate some uninitialised memory for the new node
        let extended_layout = alloc::Layout::from_size_align(extended_size, align).unwrap();
        let new_node = unsafe { alloc::alloc(extended_layout) as *mut Node<P> };
        // Check the alignment of the new memory
        assert!(new_node as usize % align == 0);

        // Initialise the new node
        unsafe {
            (*new_node).payload = payload;
            (*new_node).num_successors = num_successors;
            (*new_node).num_false_nodes = num_false_nodes;
            for i in 0..num_pointers {
                *((*new_node).ptrs.get_unchecked_mut(i) as *mut *const Node<P>) =
                    std::ptr::null_mut();
            }
        }

        // Wrap the new node in a pinned Box, and return
        Pin::new(unsafe { Box::from_raw(new_node) })
    }

    pub fn payload(&self) -> &P {
        &self.payload
    }

    pub fn successors(&self) -> &[*const Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe { std::slice::from_raw_parts(self.ptrs.as_ptr(), self.num_successors) }
    }

    pub fn false_nodes(&self) -> &[*const Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe {
            std::slice::from_raw_parts(
                self.ptrs.as_ptr().add(self.num_successors),
                self.num_false_nodes,
            )
        }
    }
}

/// The ID of a [`Node`] of the composition - this is a [`SectionID`] (usually some part of the
/// plain course), along with a [`Row`] describing the course head.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct NodeId {
    pub seg_id: SegmentID,
    pub row: RowBuf,
}

impl NodeId {
    pub fn new(seg_id: SegmentID, row: RowBuf) -> Self {
        NodeId { seg_id, row }
    }
}

/// Generate the set of all [`NodeId`]s reachable within the length requirements
fn gen_reachable_node_ids(engine: &Engine) -> HashSet<NodeId> {
    // The set of reachable nodes
    let mut expanded_nodes: HashSet<NodeId> = HashSet::new();

    /// An orderable type for storing nodes in the frontier
    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    struct FrontierNode(NodeId, usize);

    impl PartialOrd for FrontierNode {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for FrontierNode {
        fn cmp(&self, other: &Self) -> Ordering {
            self.1.cmp(&other.1)
        }
    }

    // Unexplored nodes, ordered by distance from rounds (i.e. the minimum number of rows required
    // to reach them from rounds)
    let mut frontier: BinaryHeap<Reverse<FrontierNode>> = BinaryHeap::new();

    /* Run Dijkstra's algorithm using the frontier */

    // Populate the frontier with all the possible start nodes, each with distance 0
    frontier.extend(
        engine
            .start_nodes
            .iter()
            .map(|node_id| Reverse(FrontierNode(node_id.clone(), 0))),
    );

    // Consume nodes from the frontier until the frontier is empty
    while let Some(Reverse(FrontierNode(node_id, distance))) = frontier.pop() {
        // Don't expand nodes multiple times (Dijkstra's algorithm makes sure that the first time
        // it is expanded will be have the shortest distance)
        if expanded_nodes.get(&node_id).is_some() {
            continue;
        }
        // If the node hasn't been expanded yet, then add its reachable nodes to the frontier
        let table = engine.get_seg_table(node_id.seg_id);
        for link in &table.links {
            let new_node = NodeId::new(link.end_segment, node_id.row.mul(&link.transposition));
            let new_dist = distance + table.length;
            // Add this new node to the frontier if it's distance is within the max composition
            // length
            if new_dist <= engine.len_range.end {
                frontier.push(Reverse(FrontierNode(new_node, new_dist)));
            }
        }
        // Mark this node as expanded
        expanded_nodes.insert(node_id);
    }

    // Once Dijkstra's completes, `expanded_nodes` contains every node reachable from rounds within
    // the length of the composition
    expanded_nodes
}
