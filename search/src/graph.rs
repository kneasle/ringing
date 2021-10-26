use std::collections::HashMap;

use bit_vec::BitVec;
use itertools::Itertools;
use monument_graph::{
    layout::{End, LinkIdx, StartIdx},
    music::Score,
    Data, NodeId, RowCounts,
};

/// An immutable version of [`monument_graph::Graph`] which can be traversed without hash table
/// lookups.
#[derive(Debug, Clone)]
pub struct Graph {
    pub starts: Vec<(NodeIdx, StartIdx)>,
    pub nodes: NodeVec<Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub score: Score,
    pub length: usize,
    pub method_counts: RowCounts,
    /// Minimum number of rows required to go from the end of `self` to rounds
    pub dist_to_rounds: usize,
    pub label: String,

    pub succs: Vec<(Score, LinkIdx, NodeIdx)>, // Indices must be aligned with those from the source graph
    // If this node is added to a composition, these bits denote the set of nodes will be marked as
    // unreachable.  This includes `Self`
    pub falseness: BitVec,

    pub end: Option<End>,
}

///////////////////////////////////////////
// CONVERSION FROM monument_graph::Graph //
///////////////////////////////////////////

impl Graph {
    pub fn new(source_graph: &monument_graph::Graph, data: &Data) -> Self {
        let num_nodes = source_graph.node_map().len();

        // Assign each node ID to a unique `NodeIdx`, and vice versa.  This way, we can now label
        // the set of nodes with numbers that can be used to index into a BitVec for falseness
        // computation.
        let mut index_to_id = NodeVec::<(NodeId, &monument_graph::Node)>::new();
        let mut id_to_index = HashMap::<NodeId, NodeIdx>::new();
        for (id, node) in source_graph.nodes() {
            let index = index_to_id.push((id.to_owned(), node));
            id_to_index.insert(id.to_owned(), index);
        }

        // Now convert nodes from `monument_graph::Node` to `self::Node`
        let nodes: NodeVec<_> = (0..num_nodes)
            .map(|index| {
                // Get the source node and its NodeId
                let index = NodeIdx::new(index);
                let (_id, source_node) = index_to_id[index].clone();

                // Generate a BitVec with a 1 for every node which is false against this node
                let mut falseness = BitVec::from_elem(num_nodes, false);
                for false_id in source_node.false_nodes() {
                    let false_node_idx = id_to_index[false_id];
                    falseness.set(false_node_idx.index(), true);
                }

                let succs = source_node
                    .successors()
                    .iter()
                    .map(|(link_idx, succ_id)| {
                        (
                            Score::from(data.layout.links[*link_idx].weight),
                            *link_idx,
                            id_to_index[succ_id],
                        )
                    })
                    .collect_vec();

                Node {
                    score: source_node.score(),
                    length: source_node.length(),
                    method_counts: source_node.method_counts().clone(),
                    dist_to_rounds: source_node.lb_distance_to_rounds,
                    label: source_node.label().to_owned(),
                    end: source_node.end(),
                    succs,
                    falseness,
                }
            })
            .collect();

        // Compute the list of start nodes and their labels
        let mut starts = Vec::new();
        for (start_id, start_idx) in source_graph.start_nodes() {
            if source_graph.get_node(start_id).is_some() {
                let node_idx = id_to_index[start_id];
                starts.push((node_idx, *start_idx));
            }
        }

        Graph { starts, nodes }
    }

    pub fn node_label(&self, idx: NodeIdx) -> String {
        self.nodes[idx].label.clone()
    }
}

index_vec::define_index_type! { pub struct NodeIdx = usize; }
type NodeVec<T> = index_vec::IndexVec<NodeIdx, T>;
