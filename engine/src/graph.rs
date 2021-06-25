//! Code for handling [`Graph`]s of [`Node`]s.  This is the only part of the code that knows how
//! the node graph is represented in memory, and provides a safe interface of the node graph to the
//! rest of the code.

use std::{
    alloc,
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap, HashSet},
    ops::Mul,
    pin::Pin,
};

use bellframe::RowBuf;
use itertools::Itertools;

use crate::{layout::SegmentId, segment_table::SegmentTable, Engine};

/*
/// An in-memory graph of [`Node`]s which is explored to find compositions.  Each [`Node`] carries
/// a custom payload (`P`), which can be used as annotations.
#[derive(Debug)]
pub(crate) struct Graph<P> {
    /// The set of nodes reachable in the graph.  For the rest of the code, these are entirely
    /// owned by the `Graph` (which acts like an arena for dynamically sized [`Node`]s).
    nodes: HashMap<NodeId, Pin<Box<Node<P>>>>,
    /// The [`Node`]s which can start a composition
    pub start_nodes: Vec<*const Node<P>>,
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

        // TODO: Filter out nodes which can't reach rounds

        #[derive(Debug)]
        struct NodeInfo<P> {
            successors: Vec<(usize, NodeId)>,
            false_nodes: Vec<NodeId>,
            node: *mut Node<P>,
        }

        // For each reachable node ID, generate a blank node (i.e. a node where all the
        // succession/falseness pointers are `null`, but everything else is initialised) along with
        // additional information about which successors/false nodes are reachable
        let mut node_infos: HashMap<NodeId, NodeInfo<P>> = reachable_node_ids
            .iter()
            .map(|node_id| {
                let central_id = node_id.central_id(&engine.segment_table);
                let seg_table = engine.get_seg_table(central_id.seg_id);
                // Determine the number of reachable successors
                let successors = seg_table
                    .links
                    .iter()
                    .map(|seg_link| {
                        NodeId::new(
                            seg_link.end_segment,
                            node_id.row.as_row() * seg_link.transposition.as_row(),
                        )
                    })
                    .enumerate()
                    .filter(|(_i, succ_node)| reachable_node_ids.contains(succ_node))
                    .collect_vec();
                // Determine the number of reachable false nodes
                let false_nodes = seg_table
                    .false_segments
                    .iter()
                    // All nodes are false against themselves, which is encoded in `false_segments`
                    // but isn't needed for the graph traversal.  So we filter them out
                    .filter(|(fch, seg_id)| {
                        let is_same_node = *seg_id == node_id.seg_id && fch.is_rounds();
                        !is_same_node
                    })
                    // Compute the `NodeId`s of the false nodes by transposition
                    .map(|(false_course_head, seg_id)| {
                        NodeId::new(*seg_id, node_id.row.as_row() * false_course_head.as_row())
                    })
                    // Remove any nodes which are unreachable
                    .filter(|false_node| reachable_node_ids.contains(false_node))
                    .collect_vec();

                (
                    node_id.clone(),
                    NodeInfo {
                        node: Node::blank(
                            gen_payload(&node_id),
                            successors.len(),
                            false_nodes.len(),
                        ),
                        successors,
                        false_nodes,
                    },
                )
            })
            .collect();

        // Create a map from IDs to pointers to the nodes.  This creates multiple mutable
        // *pointers*, but Rust only enforces aliasing of *references* so this is fine (but doing
        // it in the loop trips the borrow checker).
        let node_ptrs: HashMap<NodeId, *mut Node<P>> = node_infos
            .iter()
            .map(|(id, v)| (id.clone(), v.node))
            .collect();

        // Now that all the nodes have been allocated, set the succession/falseness pointers
        for (_id, node_info) in node_infos.iter_mut() {
            let node = unsafe { node_info.node.as_mut() }.unwrap();

            let succ_ptrs = node.successors_mut();
            for ((_succ_ind, succ_id), succ_ptr) in
                node_info.successors.iter().zip_eq(succ_ptrs.iter_mut())
            {
                *succ_ptr = *node_ptrs.get(&succ_id).unwrap();
            }

            let false_ptrs = node.false_nodes_mut();
            for (false_id, false_ptr) in node_info.false_nodes.iter().zip_eq(false_ptrs.iter_mut())
            {
                *false_ptr = *node_ptrs.get(&false_id).unwrap();
            }
        }

        // Drop the blank node infos so that the raw pointers to the nodes are unique when they are
        // boxed
        drop(node_infos);

        // Get the pointers for the possible starting nodes
        let start_nodes = engine
            .segment_table
            .start_nodes
            .iter()
            .filter_map(|se_node| {
                node_ptrs
                    .get(&NodeId::Central(se_node.super_node))
                    .map(|ptr| *ptr as *const Node<P>)
            })
            .collect_vec();

        // Before returning the nodes, check that the `*const Node<P>` and `&Node<P>` can be safely
        // transmuted
        assert_eq!(
            std::alloc::Layout::new::<*const Node<P>>(),
            std::alloc::Layout::new::<&Node<P>>()
        );

        // Now that we've initialised all the nodes, we wrap the nodes into pinned boxes (so that
        // they're owned but can't be moved) and return
        Self {
            start_nodes,
            nodes: node_ptrs
                .into_iter()
                .map(|(id, ptr)| (id, Pin::new(unsafe { Box::from_raw(ptr) })))
                .collect(),
        }
    }
}

/// A node in the in-memory graph.  This is a dynamically sized type, since it contains a
/// dynamically sized array of pointers to other nodes.
#[derive(Debug)]
// repr(C) guarantees that the pointer array is the last element (and thus can be extended whist
// allocating).
#[repr(C)]
pub(crate) struct Node<P> {
    payload: P,
    num_successors: usize,
    num_false_nodes: usize,
    /// An inline array of pointers to other nodes.  This array is extended whilst allocating, and
    /// has length `num_successors + num_false_nodes`.
    ///
    /// - The first `num_successors` pointers refer to successor nodes
    /// - The next `num_false_nodes` pointers refer to false nodes
    ptrs: [*const Self; 1],
}

impl<P> Node<P> {
    /// Allocate a `Node` where all the successor pointers are [`null`](std::ptr::null).
    fn blank(payload: P, num_successors: usize, num_false_nodes: usize) -> *mut Self {
        let num_pointers = num_successors + num_false_nodes;
        // The size and alignment of a `Node` with one pointer
        let initial_size = std::mem::size_of::<Self>();
        let align = std::mem::align_of::<Self>();
        // The size of the node once the array is expanded (the `max` here ensures `Node.ptrs`
        // always has at least one element, so the unexpanded version is always safe to use).
        let extended_size =
            initial_size + num_pointers.saturating_sub(1) * std::mem::size_of::<*const Self>();

        // Allocate some uninitialised memory for the new node
        let extended_layout = alloc::Layout::from_size_align(extended_size, align).unwrap();
        let new_node = unsafe { alloc::alloc(extended_layout) as *mut Self };
        // Check the alignment of the new memory
        assert!(new_node as usize % align == 0);

        // Initialise the new node
        unsafe {
            (*new_node).payload = payload;
            (*new_node).num_successors = num_successors;
            (*new_node).num_false_nodes = num_false_nodes;
            for i in 0..num_pointers {
                *((*new_node).ptrs.get_unchecked_mut(i) as *mut *const Self) = std::ptr::null_mut();
            }
        }

        new_node
    }

    pub fn payload(&self) -> &P {
        &self.payload
    }

    pub fn successors(&self) -> &[&Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe {
            std::slice::from_raw_parts(self.ptrs.as_ptr() as *const &Self, self.num_successors)
        }
    }

    fn successors_mut(&mut self) -> &mut [*const Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe { std::slice::from_raw_parts_mut(self.ptrs.as_mut_ptr(), self.num_successors) }
    }

    pub fn false_nodes(&self) -> &[&Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe {
            std::slice::from_raw_parts(
                self.ptrs.as_ptr().add(self.num_successors) as *const &Self,
                self.num_false_nodes,
            )
        }
    }

    fn false_nodes_mut(&mut self) -> &mut [*const Self] {
        // This unsafety is OK because:
        // - we allocated the extended slice within a single allocation
        // - we initialised the entire contents of the extended slice
        unsafe {
            std::slice::from_raw_parts_mut(
                self.ptrs.as_mut_ptr().add(self.num_successors),
                self.num_false_nodes,
            )
        }
    }
}
*/

/// The ID of a [`Node`] that isn't at the start or end of a composition.  Start and end nodes will
/// be subsections of a 'central' node
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct CentralNodeId {
    pub row: RowBuf,
    pub seg_id: SegmentId,
}

impl CentralNodeId {
    pub fn new(seg_id: SegmentId, row: RowBuf) -> Self {
        CentralNodeId { seg_id, row }
    }
}

/// The ID of any [`Node`] of the composition, including start and end nodes - this is a
/// [`SectionID`] (usually some part of the plain course), along with a [`Row`] describing the
/// course head.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) enum NodeId {
    Central(CentralNodeId),
    Start(usize),
    End(usize),
}

impl NodeId {
    pub fn central_id<'s, 't: 's>(&'s self, seg_table: &'t SegmentTable) -> &'s CentralNodeId {
        match self {
            Self::Central(c_id) => c_id,
            &Self::Start(idx) => &seg_table.start_nodes[idx].super_node,
            &Self::End(idx) => &seg_table.end_nodes[idx].super_node,
        }
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

    dbg!(
        &engine.segment_table.start_nodes,
        &engine.segment_table.end_nodes
    );

    // Unexplored nodes, ordered by distance from rounds (i.e. the minimum number of rows required
    // to reach them from rounds)
    let mut frontier: BinaryHeap<Reverse<FrontierNode>> = BinaryHeap::new();

    /* Run Dijkstra's algorithm using comp length as edge weights */

    // Populate the frontier with all the possible start nodes, each with distance 0
    frontier.extend(
        (0..engine.segment_table.start_nodes.len())
            .map(|i| Reverse(FrontierNode(NodeId::Start(i), 0))),
    );

    // Consume nodes from the frontier until the frontier is empty
    while let Some(Reverse(FrontierNode(node_id, distance))) = frontier.pop() {
        // Don't expand nodes multiple times (Dijkstra's algorithm makes sure that the first time
        // it is expanded will be have the shortest distance)
        if expanded_nodes.get(&node_id).is_some() {
            continue;
        }
        // If the node hasn't been expanded yet, then add its reachable nodes to the frontier
        let central_id = node_id.central_id(&engine.segment_table);
        let table = engine.get_seg_table(central_id.seg_id);
        // If the shortest composition including this node is longer the length limit, then don't
        // include it in the node graph
        let new_dist = distance + table.length;
        if new_dist > engine.len_range.end {
            continue;
        }
        // Expand the node by adding its after-neighbours to the frontier
        for link in &table.links {
            // Find the Central ID of the segment reachable by this link
            let new_central_id =
                CentralNodeId::new(link.end_segment, central_id.row.mul(&link.transposition));
            // If the new segment contains rounds, truncate it to an end node
            let new_id = engine.segment_table.truncate(new_central_id);
            // Add the new node to the frontier
            frontier.push(Reverse(FrontierNode(new_id, new_dist)));
        }
        // Mark this node as expanded
        expanded_nodes.insert(node_id);
    }

    // Once Dijkstra's completes, `expanded_nodes` contains every node reachable from rounds within
    // the length of the composition
    expanded_nodes
}
