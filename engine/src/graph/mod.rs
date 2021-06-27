//! Code for handling [`Graph`]s of [`Node`]s.  This is the only part of the Monument that knows
//! the details of how the node graph is represented in memory, and provides a safe interface to
//! that graph to the rest of the code.

use std::{
    alloc,
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    pin::Pin,
};

use bellframe::RowBuf;
use itertools::Itertools;

use crate::{
    graph::prototype::ProtoNode,
    layout::{Position, SegmentId},
};

// Re-export `ProtoGraph` as an opaque type (the prototype only has to be computed once and then
// can be shared between all the threads)
pub use prototype::ProtoGraph;

mod falseness;
mod prototype;

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
    pub fn from_prototype(
        prototype_graph: &ProtoGraph,
        mut gen_payload: impl FnMut(&NodeId) -> P,
    ) -> Self
    where
        P: Unpin + std::fmt::Debug,
    {
        // For each reachable node ID, generate a blank node (i.e. a node where all the
        // succession/falseness pointers are `null`, but everything else is initialised) along with
        // additional information about which successors/false nodes are reachable
        let nodes: HashMap<&NodeId, (*mut Node<P>, &ProtoNode)> = prototype_graph
            .nodes
            .iter()
            .map(|(id, proto_node)| {
                (
                    id,
                    (
                        Node::blank(
                            gen_payload(&id),
                            proto_node.successors.len(),
                            proto_node.false_nodes.len(),
                        ),
                        proto_node,
                    ),
                )
            })
            .collect();

        // Now that all the nodes have been allocated, set the succession/falseness pointers
        for (_id, (node_ptr, proto_node)) in nodes.iter() {
            let node = unsafe { node_ptr.as_mut() }.unwrap();

            // Set the successor pointers
            let succ_ptrs = node.successors_mut();
            for ((_succ_ind, succ_id), succ_ptr) in
                proto_node.successors.iter().zip_eq(succ_ptrs.iter_mut())
            {
                *succ_ptr = nodes.get(&succ_id).unwrap().0;
            }

            // Set the falseness pointers
            let false_ptrs = node.false_nodes_mut();
            for (false_id, false_ptr) in proto_node.false_nodes.iter().zip_eq(false_ptrs.iter_mut())
            {
                *false_ptr = nodes.get(&false_id).unwrap().0;
            }
        }

        // Before returning the nodes, check that the `*const Node<P>` and `&Node<P>` can be safely
        // transmuted (if they can't then traversing the graph causes UB)
        assert_eq!(
            std::alloc::Layout::new::<*const Node<P>>(),
            std::alloc::Layout::new::<&Node<P>>()
        );

        // Get the pointers for the possible starting nodes
        let start_nodes = nodes
            .iter()
            .filter(|(_id, (_node, proto_node))| proto_node.position == Position::Start)
            .map(|(_id, (node, _proto_node))| *node as *const Node<P>)
            .collect_vec();

        // Now that we've initialised all the nodes, we wrap the nodes into pinned boxes (so that
        // they're owned but can't be moved) and return
        Self {
            start_nodes,
            nodes: nodes
                .into_iter()
                .map(|(id, (ptr, _))| (id.clone(), Pin::new(unsafe { Box::from_raw(ptr) })))
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

/// The ID of a [`Node`] that isn't at the start or end of a composition.  Start and end nodes will
/// be subsections of a 'central' node
// PERF: These get cloned a lot but are rarely get mutated, so we could replace `RowBuf` with
// `Rc<Row>` and avoid tons of allocations.
#[derive(Clone, Eq, PartialEq, Hash)]
pub(crate) struct NodeId {
    pub row: RowBuf,
    pub seg: SegmentId,
}

impl NodeId {
    pub fn new(seg: SegmentId, row: RowBuf) -> Self {
        NodeId { row, seg }
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeId({}, {})", self.row, self.seg.idx)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeId({}, {})", self.row, self.seg.idx)
    }
}
