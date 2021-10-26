//! A mutable graph of nodes.  Compositions are represented as paths through this node graph.

use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap},
};

use bellframe::RowBuf;
use itertools::Itertools;
use monument_utils::FrontierItem;

use crate::{
    falseness::FalsenessTable,
    layout::{End, Layout, LinkIdx, NodeId, Segment, StartIdx},
    music::{Breakdown, MusicType, Score},
    optimise::Pass,
    row_counts::RowCounts,
    Data,
};

/// The number of rows required to get from a point in the graph to a start/end.
type Distance = usize;

/// A 'prototype' node graph that is (relatively) inefficient to traverse but easy to modify.  This
/// is usually used to build and optimise the node graph before being converted into an efficient
/// graph representation for use in tree search.
#[derive(Debug, Clone)]
pub struct Graph {
    // NOTE: References between nodes don't have to be valid (i.e. they can point to a [`Node`]
    // that isn't actually in the graph - in this case they will be ignored or discarded during the
    // optimisation process).
    nodes: HashMap<NodeId, Node>,
    /// **Invariant**: If `start_nodes` points to a node, it **must** be a start node (i.e. not
    /// have any predecessors, and have `start_label` set)
    start_nodes: Vec<(NodeId, StartIdx)>,
    /// **Invariant**: If `start_nodes` points to a node, it **must** be a end node (i.e. not have
    /// any successors, and have `end_nodes` set)
    end_nodes: Vec<(NodeId, End)>,
}

/// A `Node` in a node [`Graph`].  This is an indivisible chunk of ringing which cannot be split up
/// by calls or splices.
#[derive(Debug, Clone)]
pub struct Node {
    /// If this `Node` is a 'start' (i.e. it can be the first node in a composition), then this is
    /// `Some(label)` where `label` should be appended to the front of the human-friendly
    /// composition string.
    is_start: bool,
    /// If this `Node` is an 'end' (i.e. adding it will complete a composition), then this is
    /// `Some(label)` where `label` should be appended to the human-friendly composition string.
    end: Option<End>,
    /// The string that should be added when this node is generated
    label: String,

    successors: Vec<(LinkIdx, NodeId)>,
    predecessors: Vec<(LinkIdx, NodeId)>,

    /// The nodes which share rows with `self`, including `self` (because all nodes are false
    /// against themselves).  Optimisation passes should only be able to remove falseness
    /// references, never add them.
    false_nodes: Vec<NodeId>,

    /// The number of rows in this node.  Optimisation passes can't change this
    length: usize,
    /// The number of rows of each method generated by this node
    method_counts: RowCounts,
    /// The music generated by this node in the composition.  Optimisation passes can't change this
    music: Breakdown,

    /* MUTABLE STATE FOR OPTIMISATION PASSES */
    /// Does this node need to be included in every composition in this search?
    pub required: bool,
    /// A lower bound on the number of rows required to go from any rounds to the first row of
    /// `self`
    pub lb_distance_from_rounds: Distance,
    /// A lower bound on the number of rows required to go from the first row **after** `self` to
    /// rounds.
    pub lb_distance_to_rounds: Distance,
}

// ------------------------------------------------------------------------------------------

impl Graph {
    //! Optimisation

    /// Repeatedly apply a sequence of [`Pass`]es until the graph stops getting smaller, or 20
    /// iterations are made.  Use [`Graph::optimise_with_iter_limit`] to set a custom iteration limit.
    pub fn optimise(&mut self, passes: &mut [Pass], data: &Data) {
        self.optimise_with_iter_limit(passes, data, 20);
    }

    /// Repeatedly apply a sequence of [`Pass`]es until the graph either becomes static, or `limit`
    /// many iterations are performed.
    pub fn optimise_with_iter_limit(&mut self, passes: &mut [Pass], data: &Data, limit: usize) {
        let mut last_size = Size::from(&*self);

        for _ in 0..limit {
            self.run_passes(passes, data);

            let new_size = Size::from(&*self);
            // Stop optimising if the optimisations don't make the graph strictly smaller.  If
            // they make some parts smaller but others larger, then keep optimising.
            match new_size.partial_cmp(&last_size) {
                Some(Ordering::Equal | Ordering::Greater) => return,
                Some(Ordering::Less) | None => {}
            }
            last_size = new_size;
        }
    }

    /// Run a sequence of [`Pass`]es on `self`
    pub fn run_passes(&mut self, passes: &mut [Pass], data: &Data) {
        for p in &mut *passes {
            p.run(self, data);
        }
    }

    /// For each start node in `self`, creates a copy of `self` with _only_ that start node.  This
    /// partitions the set of generated compositions across these `Graph`s, but allows for better
    /// optimisations because more is known about each `Graph`.
    pub fn split_by_start_node(&self) -> Vec<Graph> {
        self.start_nodes
            .iter()
            .cloned()
            .map(|start_id| {
                let mut new_self = self.clone();
                new_self.start_nodes = vec![start_id];
                new_self
            })
            .collect_vec()
    }
}

// ------------------------------------------------------------------------------------------

impl Graph {
    //! Helpers for optimisation passes

    /// Removes all nodes for whom `pred` returns `false`
    pub fn retain_nodes(&mut self, pred: impl FnMut(&NodeId, &mut Node) -> bool) {
        self.nodes.retain(pred);
    }

    /// Remove elements from [`Self::start_nodes`] for which a predicate returns `false`.
    pub fn retain_start_nodes(&mut self, pred: impl FnMut(&(NodeId, StartIdx)) -> bool) {
        self.start_nodes.retain(pred);
    }

    /// Remove elements from [`Self::end_nodes`] for which a predicate returns `false`.
    pub fn retain_end_nodes(&mut self, pred: impl FnMut(&(NodeId, End)) -> bool) {
        self.end_nodes.retain(pred);
    }
}

impl Node {
    //! Helpers for optimisation passes

    /// A lower bound on the length of a composition which passes through this node.
    pub fn min_comp_length(&self) -> usize {
        self.lb_distance_from_rounds + self.length + self.lb_distance_to_rounds
    }
}

/// A measure of the `Size` of a [`Graph`].  Used to detect when further optimisations aren't
/// useful.
#[derive(Debug, PartialEq, Clone, Copy)]
struct Size {
    num_nodes: usize,
    num_links: usize,
    num_starts: usize,
    num_ends: usize,
}

impl From<&Graph> for Size {
    fn from(g: &Graph) -> Self {
        Self {
            num_nodes: g.nodes.len(),
            // This assumes that every successor link also corresponds to a predecessor link
            num_links: g.nodes().map(|(_id, node)| node.successors.len()).sum(),
            num_starts: g.start_nodes.len(),
            num_ends: g.end_nodes.len(),
        }
    }
}

impl PartialOrd for Size {
    // TODO: Make this into a macro?
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let cmp_nodes = self.num_nodes.cmp(&other.num_nodes);
        let cmp_links = self.num_links.cmp(&other.num_links);
        let cmp_starts = self.num_starts.cmp(&other.num_starts);
        let cmp_ends = self.num_ends.cmp(&other.num_ends);

        let all_comparisons = [cmp_nodes, cmp_links, cmp_starts, cmp_ends];

        let are_any_less = all_comparisons
            .iter()
            .any(|cmp| matches!(cmp, Ordering::Less));
        let are_any_greater = all_comparisons
            .iter()
            .any(|cmp| matches!(cmp, Ordering::Greater));

        match (are_any_less, are_any_greater) {
            (true, false) => Some(Ordering::Less), // If nothing got larger, then the size is smaller
            (false, true) => Some(Ordering::Greater), // If nothing got smaller, then the size is larger
            (false, false) => Some(Ordering::Equal),  // No < or > means all components are equal
            (true, true) => None, // If some are smaller & some are greater then these are incomparable
        }
    }
}

// ------------------------------------------------------------------------------------------

impl Graph {
    //! Getters & Iterators

    // Getters

    pub fn get_node<'graph>(&'graph self, id: &NodeId) -> Option<&'graph Node> {
        self.nodes.get(id)
    }

    pub fn get_node_mut<'graph>(&'graph mut self, id: &NodeId) -> Option<&'graph mut Node> {
        self.nodes.get_mut(id)
    }

    pub fn start_nodes(&self) -> &[(NodeId, StartIdx)] {
        &self.start_nodes
    }

    pub fn end_nodes(&self) -> &[(NodeId, End)] {
        &self.end_nodes
    }

    pub fn node_map(&self) -> &HashMap<NodeId, Node> {
        &self.nodes
    }

    pub fn get_start(&self, idx: usize) -> Option<(&Node, StartIdx)> {
        let (start_node_id, start_idx) = self.start_nodes.get(idx)?;
        let start_node = self.nodes.get(start_node_id)?;
        assert!(start_node.is_start);
        Some((start_node, *start_idx))
    }

    // Iterators

    /// An [`Iterator`] over the [`NodeId`] of every [`Node`] in this `Graph`
    pub fn ids(&self) -> impl Iterator<Item = &NodeId> {
        self.nodes.keys()
    }

    /// An [`Iterator`] over every [`Node`] in this `Graph` (including its [`NodeId`])
    pub fn nodes(&self) -> impl Iterator<Item = (&NodeId, &Node)> {
        self.nodes.iter()
    }

    /// An [`Iterator`] over every [`Node`] in this `Graph`, without its [`NodeId`].
    pub fn just_nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.values()
    }

    /// A mutable [`Iterator`] over the [`NodeId`] of every [`Node`] in this `Graph`
    pub fn nodes_mut(&mut self) -> impl Iterator<Item = (&NodeId, &mut Node)> {
        self.nodes.iter_mut()
    }
}

impl Node {
    //! Getters & Iterators

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn method_counts(&self) -> &RowCounts {
        &self.method_counts
    }

    pub fn score(&self) -> Score {
        self.music.total
    }

    pub fn label(&self) -> &str {
        self.label.as_str()
    }

    // STARTS/ENDS //

    pub fn is_start(&self) -> bool {
        self.is_start
    }

    pub fn end(&self) -> Option<End> {
        self.end
    }

    pub fn is_end(&self) -> bool {
        self.end.is_some()
    }

    // CROSS-NODE REFERENCES //

    pub fn successors(&self) -> &[(LinkIdx, NodeId)] {
        self.successors.as_slice()
    }

    pub fn successors_mut(&mut self) -> &mut Vec<(LinkIdx, NodeId)> {
        &mut self.successors
    }

    pub fn predecessors(&self) -> &[(LinkIdx, NodeId)] {
        self.predecessors.as_slice()
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<(LinkIdx, NodeId)> {
        &mut self.predecessors
    }

    pub fn false_nodes(&self) -> &[NodeId] {
        self.false_nodes.as_slice()
    }

    pub fn false_nodes_mut(&mut self) -> &mut Vec<NodeId> {
        &mut self.false_nodes
    }
}

////////////////////////////
// CONVERSION FROM LAYOUT //
////////////////////////////

impl Graph {
    /// Generate a graph of all nodes which are reachable within a given length constraint.
    pub fn from_layout(layout: &Layout, music_types: &[MusicType], max_length: usize) -> Self {
        // The set of reachable nodes and whether or not they are a start node (each mapping to a
        // distance from rounds)
        let mut expanded_nodes: HashMap<NodeId, (Segment, Distance)> = HashMap::new();

        let mut end_nodes = Vec::new();

        // Unexplored nodes, ordered by distance from rounds (i.e. the minimum number of rows required
        // to reach them from rounds)
        let mut frontier: BinaryHeap<Reverse<FrontierItem<NodeId>>> = BinaryHeap::new();

        /* Run Dijkstra's algorithm using comp length as edge weights */

        // Populate the frontier with all the possible start nodes, each with distance 0
        let start_nodes = layout
            .starts
            .iter()
            .enumerate()
            .map(|(idx, start)| {
                let id = NodeId::standard(start.course_head.to_arc(), start.row_idx, true);
                (id, StartIdx::new(idx))
            })
            .collect_vec();
        frontier.extend(
            start_nodes
                .iter()
                .cloned()
                .map(|(id, _)| FrontierItem::new(id))
                .map(Reverse),
        );

        while let Some(Reverse(FrontierItem {
            item: node_id,
            distance,
        })) = frontier.pop()
        {
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
            if let Some(end) = segment.end {
                end_nodes.push((node_id.clone(), end));
            }
            // Expand the node by adding its successors to the frontier
            for (_link_idx, id_after_link) in &segment.links {
                // Add the new node to the frontier
                frontier.push(Reverse(FrontierItem {
                    item: id_after_link.to_owned(),
                    distance: new_dist,
                }));
            }
            // Mark this node as expanded
            expanded_nodes.insert(node_id, (segment, distance));
        }

        // Once Dijkstra's finishes, `expanded_nodes` contains every node reachable from rounds
        // within the length of the composition.  However, we're still not done because we have to
        // build a graph over these IDs (which requires computing falseness, music, connections,
        // etc.).
        let rounds = RowBuf::rounds(layout.stage);
        let mut nodes: HashMap<NodeId, Node> = expanded_nodes
            .iter()
            .map(|(node_id, (segment, distance))| {
                assert_eq!(node_id, &segment.node_id);

                let music = Breakdown::from_rows(
                    segment.untransposed_rows(layout),
                    node_id.course_head().unwrap_or(&rounds),
                    music_types,
                );

                let new_node = Node {
                    length: segment.length,
                    method_counts: segment.method_counts.clone(),
                    music,

                    is_start: node_id.is_start(),
                    end: segment.end,
                    label: segment.label.clone(),

                    required: false,
                    lb_distance_from_rounds: *distance,
                    // Distances to rounds are computed later.  However, the distance is an lower
                    // bound, so we can set it to 0 without breaking any invariants.
                    lb_distance_to_rounds: 0,

                    successors: segment.links.to_owned(),

                    // These are populated in separate passes once all the `Node`s have been created
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
            for (link_idx, succ_id) in nodes.get(&id).unwrap().successors.clone() {
                if let Some(node) = nodes.get_mut(&succ_id) {
                    node.predecessors.push((link_idx, id.clone()));
                }
            }
        }

        Self {
            nodes,
            start_nodes,
            end_nodes,
        }
    }
}
