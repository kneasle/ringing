//! A mutable graph of nodes.  Compositions are represented as paths through this node graph.

use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap},
};

use itertools::Itertools;

use crate::{
    falseness::FalsenessTable,
    layout::{Layout, NodeId, Segment},
    music::{Breakdown, MusicType},
};

type Distance = usize;

/// A 'prototype' node graph that is (relatively) inefficient to traverse but easy to modify.  This
/// is usually used to build and optimise the node graph before being converted into an efficient
/// graph representation for use in tree search.
#[derive(Debug, Clone)]
pub struct Graph {
    // NOTE: References between nodes don't have to be valid (i.e. they can point to a
    // [`ProtoNode`] that isn't actually in the graph - in this case they will be ignored or
    // discarded during the optimisation process).
    nodes: HashMap<NodeId, Node>,
    start_nodes: Vec<NodeId>,
    end_nodes: Vec<NodeId>,
}

impl Graph {
    /// Generate a graph of all nodes which are reachable within a given length constraint.
    pub fn from_layout(layout: &Layout, music_types: &[MusicType], max_length: usize) -> Self {
        // The set of reachable nodes and whether or not they are a start node (each mapping to a
        // distance from rounds)
        let mut expanded_nodes: HashMap<NodeId, (Segment, Distance)> = HashMap::new();

        let mut end_nodes = Vec::new();

        // Unexplored nodes, ordered by distance from rounds (i.e. the minimum number of rows required
        // to reach them from rounds)
        let mut frontier: BinaryHeap<Reverse<FrontierNode>> = BinaryHeap::new();

        /* Run Dijkstra's algorithm using comp length as edge weights */

        // Populate the frontier with all the possible start nodes, each with distance 0
        let start_node_ids = layout
            .starts
            .iter()
            .map(|start| NodeId::new(start.course_head.to_owned(), start.row_idx, true))
            .collect_vec();
        frontier.extend(
            start_node_ids
                .iter()
                .cloned()
                .map(|id| FrontierNode(id, 0))
                .map(Reverse),
        );

        // Consume nodes from the frontier until the frontier is empty
        while let Some(Reverse(FrontierNode(node_id, distance))) = frontier.pop() {
            // Don't expand nodes multiple times (Dijkstra's algorithm makes sure that the first time
            // it is expanded will be have the shortest distance)
            if expanded_nodes.get(&node_id).is_some() {
                continue;
            }
            // If the node hasn't been expanded yet, then add its reachable nodes to the frontier
            let segment = layout
                .get_segment(&node_id)
                .expect("Infinite segment found");

            // If the shortest composition including this node is longer the length limit, then don't
            // include it in the node graph
            let new_dist = distance + segment.length;
            if new_dist > max_length {
                continue;
            }
            if segment.is_end() {
                end_nodes.push(node_id.clone());
            }
            // Expand the node by adding its successors to the frontier
            for (_link_idx, id_after_link) in &segment.links {
                // Add the new node to the frontier
                frontier.push(Reverse(FrontierNode(id_after_link.to_owned(), new_dist)));
            }
            // Mark this node as expanded
            expanded_nodes.insert(node_id, (segment, distance));
        }

        // Once Dijkstra's finishes, `expanded_nodes` contains every node reachable from rounds
        // within the length of the composition.  However, we're still not done because we have to
        // build a graph over these IDs (which requires computing falseness, music, connections,
        // etc.).
        let mut nodes: HashMap<NodeId, Node> = expanded_nodes
            .iter()
            .map(|(node_id, (segment, distance))| {
                let music = Breakdown::from_rows(
                    segment.untransposed_rows(layout),
                    &node_id.course_head,
                    music_types,
                );

                let new_node = Node {
                    length: segment.length,
                    music,

                    start_label: segment.end_label.to_owned(),
                    end_label: segment.end_label.to_owned(),

                    min_distance_from_rounds: *distance,
                    // Distances to rounds are computed during optimisation
                    min_distance_to_rounds: None,

                    successors: segment.links.to_owned(),
                    // These are populated in separate passes over the graph
                    false_nodes: Vec::new(),
                    predecessors: Vec::new(),
                };
                (node_id.clone(), new_node)
            })
            .collect();

        // We need to clone the `NodeId`s, because otherwise they would borrow from `nodes` whilst
        // the loop is modifying the contents (i.e. breaking reference aliasing)
        let node_ids_and_lengths = nodes
            .iter()
            .map(|(id, node)| (id.to_owned(), node.length))
            .collect_vec();

        // Compute falseness between the nodes
        let table = FalsenessTable::from_layout(layout, &node_ids_and_lengths);
        for (id, node) in nodes.iter_mut() {
            node.false_nodes = node_ids_and_lengths
                .iter()
                .filter(|(id2, length2)| table.are_false(id, node.length, id2, *length2))
                .map(|(id2, _)| id2.to_owned())
                .collect_vec();
        }

        // Add predecessor references
        for (id, _dist) in expanded_nodes {
            for (_, succ_id) in nodes.get(&id).unwrap().successors.clone() {
                if let Some(node) = nodes.get_mut(&succ_id) {
                    node.predecessors.push(id.clone());
                }
            }
        }

        Self {
            nodes,
            start_nodes: start_node_ids,
            end_nodes,
        }
    }
}

/// A node in a prototype graph
#[derive(Debug, Clone)]
pub struct Node {
    start_label: Option<String>,
    end_label: Option<String>,

    /// The number of rows in this node
    length: usize,
    /// The music generated by this node in the composition
    music: Breakdown,
    false_nodes: Vec<NodeId>,

    /// A lower bound on the number of rows required to go from any rounds to the first row of
    /// `self`
    min_distance_from_rounds: Distance,
    /// A lower bound on the number of rows required to go from the first row of `self` to rounds
    /// (or `None` if rounds is unreachable - a distance of infinity - or the distances haven't
    /// been computed yet).
    min_distance_to_rounds: Option<Distance>,

    /// The `usize` here denotes which link in the [`Layout`] has generated this succession.  This
    /// is required so that a human-friendly representation of the composition can be generated.
    successors: Vec<(usize, NodeId)>,
    predecessors: Vec<NodeId>,
}

/// An orderable type for storing nodes on Dijkstra's Algorithm's frontier
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
