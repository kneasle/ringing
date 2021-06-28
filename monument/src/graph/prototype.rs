//! A 'prototype' node graph that is inefficient to traverse but easy to modify

use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap, HashSet},
    ops::Mul,
};

use itertools::Itertools;

use crate::{
    layout::{Layout, Position, SegmentId},
    music::Score,
    MusicType,
};

use super::{falseness::FalsenessTable, NodeId};

/// A 'prototype' node graph that is inefficient to traverse but easy to modify.  This is used to
/// build and optimise the node graph before being converted into an efficient [`Graph`] structure
/// for use in tree search.
#[derive(Debug, Clone)]
pub struct ProtoGraph {
    /// Note: References between nodes don't have to be valid (i.e. they can point to a
    /// [`ProtoNode`] that isn't actually in the graph - in this case they will be ignored).
    pub(super) nodes: HashMap<NodeId, ProtoNode>,
}

impl ProtoGraph {
    /// Generate and optimise a graph from a [`Layout`]
    pub fn from_layout(layout: &Layout, music_types: &[MusicType], max_length: usize) -> Self {
        let mut graph = Self::reachable_graph(layout, music_types, max_length);

        let mut last_num_nodes = graph.nodes.len();

        // Repeatedly optimise the graph until it stops getting smaller
        loop {
            // Recompute distances and remove any nodes which can't be reached in a short enough
            // composition
            graph.recompute_distances_from_rounds();
            graph.recompute_distances_to_rounds();
            graph.remove_nodes_by_distance(max_length);

            // Other optimisation ideas:
            // - Remove nodes which are false against any required nodes
            // - Merge chunks of nodes which can only be rung together (i.e. each pair has
            //   precisely one successor and predecessor).
            // - Reorder successor links so that branches with easily reachable music are explored
            //   first

            // If the graph has not got smaller, then further optimisation is not useful so we can
            // stop optimising
            let num_nodes = graph.nodes.len();
            // println!("Graph reduction: {} -> {}", last_num_nodes, num_nodes);
            if num_nodes == last_num_nodes {
                break;
            }
            last_num_nodes = num_nodes;
        }

        // Prune all the references and return
        graph.prune_references();
        graph
    }

    /// Generate a graph of all nodes which are reachable within a given length constraint.
    fn reachable_graph(layout: &Layout, music_types: &[MusicType], max_length: usize) -> Self {
        // The set of reachable nodes (each mapping to a distance from rounds)
        let mut expanded_nodes: HashMap<NodeId, usize> = HashMap::new();

        // Unexplored nodes, ordered by distance from rounds (i.e. the minimum number of rows required
        // to reach them from rounds)
        let mut frontier: BinaryHeap<Reverse<FrontierNode>> = BinaryHeap::new();

        /* Run Dijkstra's algorithm using comp length as edge weights */

        // Populate the frontier with all the possible start nodes, each with distance 0
        frontier.extend(
            layout
                .segments
                .iter()
                .enumerate()
                .filter(|(_i, s)| s.position == Position::Start)
                .map(|(i, s)| {
                    FrontierNode(
                        s.as_node_id(SegmentId::from(i))
                            .expect("Start segments should only be able to exist in one course"),
                        0,
                    )
                })
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
            let table = layout.get_segment(node_id.seg);
            // If the shortest composition including this node is longer the length limit, then don't
            // include it in the node graph
            let new_dist = distance + table.row_range.1.length;
            if new_dist > max_length {
                continue;
            }
            // Expand the node by adding its successors to the frontier
            for link in &table.links {
                // Find the Central ID of the segment reachable by this link
                let new_central_id =
                    NodeId::new(link.end_segment, node_id.row.mul(&link.transposition));
                // If the new segment contains rounds, truncate it to an end node
                let new_id = layout.truncate(new_central_id);
                // Add the new node to the frontier
                frontier.push(Reverse(FrontierNode(new_id, new_dist)));
            }
            // Mark this node as expanded
            expanded_nodes.insert(node_id, distance);
        }

        // Once Dijkstra's finishes, `expanded_nodes` contains every node reachable from rounds
        // within the length of the composition.  However, we're still not done because we have to
        // build a graph over these IDs (which requires computing falseness, connections, etc.).
        let mut nodes: HashMap<NodeId, ProtoNode> = expanded_nodes
            .iter()
            .map(|(node_id, distance)| {
                let segment = layout.get_segment(node_id.seg);
                let score =
                    Score::from_rows(layout.segment_rows(node_id.seg), &node_id.row, music_types);

                let new_node = ProtoNode {
                    min_distance_from_rounds: *distance,
                    // Distances to rounds are computed during optimisation
                    min_distance_to_rounds: None,
                    length: segment.row_range.1.length,
                    score,
                    position: segment.position,
                    successors: segment
                        .links
                        .iter()
                        .enumerate()
                        .map(|(i, link)| {
                            (
                                i,
                                layout.truncate(NodeId::new(
                                    link.end_segment,
                                    node_id.row.mul(&link.transposition),
                                )),
                            )
                        })
                        .collect_vec(),
                    // These are populated in separate passes over the graph
                    false_nodes: Vec::new(),
                    predecessors: Vec::new(),
                };
                (node_id.clone(), new_node)
            })
            .collect();

        // Compute falseness between the nodes (by naively testing every pair of nodes in a lookup
        // table).
        let all_node_ids = nodes.keys().cloned().collect_vec();
        let falseness_table = FalsenessTable::from_layout(layout);
        for (id, node) in nodes.iter_mut() {
            node.false_nodes = all_node_ids
                .iter()
                .filter(|&id2| falseness_table.are_false(id, id2))
                .cloned()
                .collect_vec();
        }

        // Add predecessor references
        for (id, _dist) in expanded_nodes {
            // I wish there was a way to do this without cloning the node IDs, but alas the borrow
            // checker won't let me.  In future, we should allocate the node IDs into an arena (or
            // use RCs) to make the cloning cheaper
            for (_, succ_id) in nodes.get(&id).unwrap().successors.clone() {
                if let Some(node) = nodes.get_mut(&succ_id) {
                    node.predecessors.push(id.clone());
                }
            }
        }

        Self { nodes }
    }

    /// Run Dijkstra's algorithm to update `min_distance_from_rounds` on every node
    fn recompute_distances_from_rounds(&mut self) {
        // Initialise the frontier with all the starting nodes (with distance 0)
        let mut frontier: BinaryHeap<Reverse<FrontierNode>> = self
            .start_nodes()
            .map(|(id, _node)| Reverse(FrontierNode(id.clone(), 0)))
            .collect();

        while let Some(Reverse(FrontierNode(node_id, distance))) = frontier.pop() {
            let node = match self.nodes.get_mut(&node_id) {
                Some(n) => n,
                // Skip this node if it's already been removed from the graph
                None => continue,
            };

            // If this doesn't improve the existing min distance, then don't bother updating this
            // node's successors
            if distance >= node.min_distance_from_rounds {
                continue;
            }
            // Update the new (improved) min distance
            node.min_distance_from_rounds = distance;

            let dist_after_node = distance + node.length;
            // Expand the node by adding its successors to the frontier
            for (_, succ_id) in &node.successors {
                // Add the new node to the frontier
                frontier.push(Reverse(FrontierNode(succ_id.clone(), dist_after_node)));
            }
        }
    }

    /// Run Dijkstra's algorithm to update `min_distance_from_rounds` on every node
    fn recompute_distances_to_rounds(&mut self) {
        // Initialise the frontier with all the end nodes.  Note that these distances go from the
        // **END** of the nodes
        let mut frontier: BinaryHeap<Reverse<FrontierNode>> = self
            .end_nodes()
            .map(|(id, _node)| Reverse(FrontierNode(id.clone(), 0)))
            .collect();

        while let Some(Reverse(FrontierNode(node_id, distance_from_end))) = frontier.pop() {
            let node = match self.nodes.get_mut(&node_id) {
                Some(n) => n,
                // Skip this node if it's already been removed from the graph
                None => continue,
            };

            // If this doesn't improve the existing min distance, then don't bother updating this
            // node's successors
            let distance_from_start = distance_from_end + node.length;
            if node
                .min_distance_to_rounds
                .map_or(false, |existing_dist| distance_from_start >= existing_dist)
            {
                continue;
            }
            // Update the new (improved) min distance
            node.min_distance_to_rounds = Some(distance_from_start);

            // Expand the node by adding its predecessors to the frontier
            for pred_id in &node.predecessors {
                // Add the new node to the frontier
                frontier.push(Reverse(FrontierNode(pred_id.clone(), distance_from_start)));
            }
        }
    }

    /// Remove any nodes which can't be included in any round-block composition short enough to fit
    /// within the max length.
    ///
    /// # Panics
    ///
    /// This panics if `self.recompute_distances_to_rounds` hasn't been run before this
    fn remove_nodes_by_distance(&mut self, max_length: usize) {
        /* Remove unreachable nodes - unreachable nodes can't be in any compositions */

        let mut reachable_nodes: HashSet<NodeId> = HashSet::with_capacity(self.nodes.len());

        // Run DFA on the graph starting from rounds, adding the nodes to `reachable_nodes`
        for (id, _node) in self.start_nodes() {
            self.explore(id, &mut reachable_nodes);
        }

        // Remove any nodes in the graph which aren't in `reachable_nodes` (by adding them to
        // `nodes_to_remove` whilst initialising)
        let mut nodes_to_remove: HashSet<NodeId> = self
            .nodes
            .keys()
            .filter(|id| !reachable_nodes.contains(id))
            .cloned()
            .collect();
        nodes_to_remove.reserve(self.nodes.len());

        /* Remove nodes which can only exist in comps that are too long */

        for (id, node) in &self.nodes {
            match node.min_distance_to_rounds {
                // If the shortest composition going through this node is longer than the max
                // length, then this node can't be reachable in a short enough composition and we
                // can prune
                Some(dist_to_rounds) => {
                    let min_comp_length = node.min_distance_from_rounds + dist_to_rounds;
                    if min_comp_length > max_length {
                        nodes_to_remove.insert(id.clone());
                    }
                }
                // Either the node can't reach rounds (and can be removed from the graph), or this
                // hasn't been run after `recompute_distances_to_rounds` (which we can detect by
                // checking if end nodes are given a distance, and if not then panic).
                None => {
                    assert!(
                        node.position != Position::End,
                        "End node marked as being unable to reach rounds!"
                    );
                    nodes_to_remove.insert(id.clone());
                }
            }
        }

        self.remove_nodes(nodes_to_remove);
    }

    fn explore(&self, id: &NodeId, reachable_nodes: &mut HashSet<NodeId>) {
        // Only expand the node if it exists
        if let Some(node) = self.nodes.get(id) {
            // Check if this node has been reached before, and if so don't expand it (this makes
            // sure every node is visited at most once, guaranteeing termination).
            if reachable_nodes.contains(id) {
                return;
            }
            // If the node hasn't been reached yet, then mark it as reached and explore its
            // successors
            reachable_nodes.insert(id.clone());
            for (_, succ_id) in &node.successors {
                self.explore(succ_id, reachable_nodes);
            }
        }
    }

    /// Remove cross-node references (falseness, successor, predecessor, etc.) which don't point to
    /// existing nodes.
    pub(super) fn prune_references(&mut self) {
        // Lookup table for which node IDs actually correspond to nodes
        let node_ids: HashSet<NodeId> = self.nodes.keys().cloned().collect();

        for node in self.nodes.values_mut() {
            node.successors.retain(|(_, id)| node_ids.contains(id));
            node.predecessors.retain(|id| node_ids.contains(id));
            node.false_nodes.retain(|id| node_ids.contains(id));
        }
    }

    /* ===== HELPER FUNCTIONS ===== */

    pub(super) fn start_nodes(&self) -> impl Iterator<Item = (&NodeId, &ProtoNode)> {
        self.nodes
            .iter()
            .filter(|(_id, node)| node.position == Position::Start)
    }

    fn end_nodes(&self) -> impl Iterator<Item = (&NodeId, &ProtoNode)> {
        self.nodes
            .iter()
            .filter(|(_id, node)| node.position == Position::End)
    }

    /// Remove nodes from the graph by reference
    ///
    /// # Panics
    ///
    /// Panics if any ids point to non-existent nodes
    fn remove_nodes(&mut self, ids: impl IntoIterator<Item = NodeId>) {
        for id in ids {
            assert!(self.nodes.remove(&id).is_some());
        }
    }
}

/// A node in a prototype graph
#[derive(Debug, Clone)]
pub(super) struct ProtoNode {
    pub position: Position,
    /// The number of rows in this node
    pub length: usize,
    /// The music generated by this node in the composition
    pub score: Score,
    /// A lower bound on the number of rows required to go from any rounds to the first row of
    /// `self`
    min_distance_from_rounds: usize,
    /// A lower bound on the number of rows required to go from the first row of `self` to rounds
    /// (or `None` if rounds is unreachable - a distance of infinity - or the distances haven't
    /// been computed yet).
    min_distance_to_rounds: Option<usize>,
    pub false_nodes: Vec<NodeId>,
    /// The `usize` here denotes which link in the [`Layout`] has generated this succession.  This
    /// is required so that a human-friendly representation of the composition can be generated.
    pub successors: Vec<(usize, NodeId)>,
    pub predecessors: Vec<NodeId>,
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
