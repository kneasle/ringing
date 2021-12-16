//! A plugin-able system of passes made over the composition [`Graph`], in order to modify or
//! optimise the graph.

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
    ops::Not,
};

use crate::{layout::NodeId, utils::FrontierItem, Query};

use super::{Graph, Node};

use self::Direction::{Backward, Forward};

pub type SinglePass = Box<dyn FnMut(&mut Graph, &Query)>;
/// A [`Pass`] which can be run both [`Forward`] and [`Backward`] over a [`Graph`].  For example,
/// computing distances to/from rounds (removing unreachable nodes).
pub type DirectionalPass = Box<dyn FnMut(DirectionalView<'_>, &Query)>;

/// A pass which modifies a [`Graph`].  Passes are generally intended to perform optimisations -
/// they preserve the _semantic_ meaning of a [`Graph`] (i.e. the set of true compositions which it
/// generates), whilst modifying the [`Graph`] to make tree search faster.
pub enum Pass {
    /// Run a single non-directional pass
    Single(SinglePass),
    /// Run a `DirectionalPass` but only in one [`Direction`]
    OneDirection(DirectionalPass, Direction),
    /// Run a `DirectionalPass` twice, [`Forward`] first
    BothDirections(DirectionalPass),
    /// Run a `DirectionalPass` twice, [`Backward`] first
    BothDirectionsRev(DirectionalPass),
}

impl Pass {
    /// Apply the effect of this [`Pass`] to a [`Graph`]
    pub fn run(&mut self, graph: &mut Graph, query: &Query) {
        let mut run_in_direction = |direction: Direction, pass: &mut DirectionalPass| {
            pass(DirectionalView::new(graph, direction), query)
        };

        match self {
            Pass::Single(pass) => pass(graph, query),
            Pass::OneDirection(pass, direction) => run_in_direction(*direction, pass),
            Pass::BothDirections(pass) => {
                run_in_direction(Forward, pass);
                run_in_direction(Backward, pass);
            }
            Pass::BothDirectionsRev(pass) => {
                run_in_direction(Backward, pass);
                run_in_direction(Forward, pass);
            }
        }
    }
}

////////////////////////
// DIRECTIONAL PASSES //
////////////////////////

/// A `Direction` in which a [`DirectionalPass`] can be run
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Copy)]
pub enum Direction {
    /// The direction that a composition will be rung (i.e. start -> end)
    Forward,
    /// The reverse of the direction that a composition will be rung (i.e. end -> start)
    Backward,
}

impl Not for Direction {
    type Output = Direction;

    fn not(self) -> Self::Output {
        match self {
            Forward => Backward,
            Backward => Forward,
        }
    }
}

/// The view of a [`Graph`] where 'start'/'end' and 'successors'/'predecessors' are defined in a
/// given [`Direction`].  I.e. if the [`Direction`] is [`Backward`], then the graph's ordering is
/// reversed.
#[derive(Debug)]
pub struct DirectionalView<'graph> {
    graph: &'graph mut Graph,
    direction: Direction,
}

impl<'graph> DirectionalView<'graph> {
    pub fn new(graph: &'graph mut Graph, direction: Direction) -> Self {
        Self { graph, direction }
    }

    pub fn nodes(&self) -> impl Iterator<Item = (&NodeId, NodeView)> {
        self.graph
            .nodes()
            .map(|(id, node)| (id, NodeView::new(node, self.direction)))
    }

    /// Gets the IDs of the 'start' nodes of the [`Graph`] going in this [`Direction`]
    pub fn start_nodes(&self) -> Box<dyn Iterator<Item = &NodeId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.start_nodes().iter().map(|(id, _, _)| id)),
            Backward => Box::new(self.graph.end_nodes().iter().map(|(id, _)| id)),
        }
    }

    /// Gets the IDs of the 'start' nodes of the [`Graph`] going in this [`Direction`]
    pub fn end_nodes(&self) -> Box<dyn Iterator<Item = &NodeId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.end_nodes().iter().map(|(id, _)| id)),
            Backward => Box::new(self.graph.start_nodes().iter().map(|(id, _, _)| id)),
        }
    }

    pub fn get_node(&'graph self, id: &NodeId) -> Option<NodeView<'graph>> {
        self.graph
            .get_node(id)
            .map(|node| NodeView::new(node, self.direction))
    }

    pub fn get_node_mut(&'graph mut self, id: &NodeId) -> Option<NodeViewMut<'graph>> {
        let direction = self.direction;
        self.graph
            .get_node_mut(id)
            .map(|node| NodeViewMut::new(node, direction))
    }

    pub fn retain_nodes(&mut self, mut pred: impl FnMut(&NodeId, NodeViewMut) -> bool) {
        let direction = self.direction;
        self.graph
            .retain_nodes(|id, node| pred(id, NodeViewMut::new(node, direction)));
    }
}

/// Immutable view of a [`Node`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct NodeView<'graph> {
    pub node: &'graph Node,
    pub direction: Direction,
}

impl<'graph> NodeView<'graph> {
    pub fn new(node: &'graph Node, direction: Direction) -> Self {
        Self { node, direction }
    }

    pub fn successors(self) -> &'graph [super::Link] {
        match self.direction {
            Forward => self.node.successors(),
            Backward => self.node.predecessors(),
        }
    }

    pub fn predecessors(self) -> &'graph [super::Link] {
        match self.direction {
            Forward => self.node.predecessors(),
            Backward => self.node.successors(),
        }
    }
}

/// Mutable view of a [`Node`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug)]
#[non_exhaustive]
pub struct NodeViewMut<'graph> {
    pub node: &'graph mut Node,
    pub direction: Direction,
}

impl<'graph> NodeViewMut<'graph> {
    fn new(node: &'graph mut Node, direction: Direction) -> Self {
        Self { node, direction }
    }

    pub fn successors_mut(&mut self) -> &mut Vec<super::Link> {
        match self.direction {
            Forward => self.node.successors_mut(),
            Backward => self.node.predecessors_mut(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<super::Link> {
        match self.direction {
            Forward => self.node.predecessors_mut(),
            Backward => self.node.successors_mut(),
        }
    }

    /// Mutable reference to the distance from rounds **to** the start of this node
    pub fn distance_mut(&mut self) -> &mut usize {
        match self.direction {
            Forward => &mut self.node.lb_distance_from_rounds,
            Backward => &mut self.node.lb_distance_to_rounds,
        }
    }

    /// Mutable reference to the distance from rounds **to** the start of this node
    pub fn non_duffer_distance_mut(&mut self) -> &mut usize {
        match self.direction {
            Forward => &mut self.node.lb_distance_from_non_duffer,
            Backward => &mut self.node.lb_distance_to_non_duffer,
        }
    }
}

////////////////////
// BUILTIN PASSES //
////////////////////

mod music; // Proving nodes as required/unusable based on music requirements
mod strip_refs; // Strip references to non-existent nodes

pub mod passes {
    use std::collections::HashSet;

    use itertools::Itertools;

    use crate::{graph::Graph, layout::NodeId, Query};

    use super::{DirectionalView, Pass};

    /// A default sequence of built-in optimisation passes
    pub fn default() -> Vec<Pass> {
        vec![
            // Distance-related optimisation
            compute_distances(),
            strip_long_nodes(),
            strip_refs(),
            // Duffer-related optimisation
            compute_duffer_distances(),
            strip_duff_nodes(),
            // Music optimisation
            required_music(),
            remove_nodes_exceeding_max_count(),
            // Required node optimisation
            single_start_or_end_required(),
            remove_nodes_false_against_required(),
        ]
    }

    /* Simple passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every node,
    /// removing any which can't reach rounds in either direction.
    pub fn strip_refs() -> Pass {
        Pass::Single(Box::new(super::strip_refs::strip_refs))
    }

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every node,
    /// removing any which can't reach rounds in either direction.
    pub fn required_music() -> Pass {
        Pass::Single(Box::new(super::music::required_music_min))
    }

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every node,
    /// removing any which can't reach rounds in either direction.
    pub fn remove_nodes_exceeding_max_count() -> Pass {
        Pass::Single(Box::new(super::music::remove_nodes_exceeding_max_count))
    }

    /* Distance related passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every node,
    /// removing any which can't reach rounds in either direction.
    pub fn compute_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let expanded_node_distances = super::compute_distances(
                view.start_nodes().map(|id| (id, 0)),
                &view,
                query.len_range.end,
            );
            // Set the node distances and strip out unreachable nodes
            view.retain_nodes(|id, mut node_view| match expanded_node_distances.get(id) {
                // keep reachable nodes and update their distance lower bounds
                Some(&new_distance) => {
                    *node_view.distance_mut() = new_distance;
                    true
                }
                None => true, // Remove unreachable nodes
            });
        }))
    }

    /// A [`Pass`] which removes any nodes which can't be included in a short enough composition.
    pub fn strip_long_nodes() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            graph.retain_nodes(|_id, node| node.min_comp_length() < query.len_range.end);
        }))
    }

    /// Creates a [`Pass`] which recomputes how close nodes are to non-duffer nodes, removing any
    /// which can't reach a non-duffer node in either direction.
    pub fn compute_duffer_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let duffer_distances = super::compute_distances(
                // Give all non-duffer nodes a distance of 0, and assign all start/end nodes to
                // their own length (thus treating rounds as a 0-length non-duffer)
                view.graph
                    .nodes()
                    .filter(|&(_id, node)| !node.duffer)
                    .map(|(id, _node)| (id, 0))
                    .chain(view.start_nodes().filter_map(|id| {
                        let node = view.graph.get_node(id)?;
                        node.duffer.then(|| (id, node.length()))
                    })),
                &view,
                query.max_duffer_rows.unwrap_or(usize::MAX),
            );
            // Set the node distances and strip out unreachable nodes
            view.retain_nodes(|id, mut node_view| match duffer_distances.get(id) {
                // keep reachable nodes and update their distance lower bounds
                Some(&new_distance) => {
                    *node_view.non_duffer_distance_mut() = new_distance;
                    true
                }
                // Remove unreachable duffer nodes
                None => {
                    assert!(node_view.node.duffer);
                    false
                }
            });
        }))
    }

    /// A [`Pass`] which removes any nodes which can't connect two non-duffer nodes quickly enough
    pub fn strip_duff_nodes() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            if let Some(max_duffer_rows) = query.max_duffer_rows {
                graph.retain_nodes(|_id, node| node.min_duffer_length() <= max_duffer_rows);
            }
        }))
    }

    /* Passes related to required nodes */

    /// A [`Pass`] which checks for a single start/end node and marks that node as required
    /// (because all compositions must start or end at that node).
    pub fn single_start_or_end_required() -> Pass {
        Pass::BothDirections(Box::new(|view: DirectionalView, _| {
            let single_start_id = match view.start_nodes().exactly_one() {
                Ok(id) => id.clone(),
                Err(_) => return,
            };
            if let Some(node) = view.graph.get_node_mut(&single_start_id) {
                node.required = true;
            }
        }))
    }

    /// A [`Pass`] which removes any nodes which are false against a node marked as required
    pub fn remove_nodes_false_against_required() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, _| {
            let mut node_ids_to_remove: HashSet<NodeId> = HashSet::new();
            // For each required node ...
            for (id, node) in graph.nodes() {
                if node.required {
                    // ... mark all its false nodes (**except itself**) to be removed
                    let other_false_node_ids = node
                        .false_nodes()
                        .iter()
                        .map(|id| NodeId::Standard(id.clone()))
                        .filter(|false_id| false_id != id);
                    node_ids_to_remove.extend(other_false_node_ids);
                }
            }
            graph.retain_nodes(|id, _node| !node_ids_to_remove.contains(id));
        }))
    }
}

///////////
// UTILS //
///////////

/// Given a set of starting nodes (and their distances), compute the shortest distance to every
/// reachable node.
fn compute_distances<'a>(
    start_nodes: impl IntoIterator<Item = (&'a NodeId, usize)>,
    view: &DirectionalView<'a>,
    dist_limit: usize,
) -> HashMap<NodeId, usize> {
    // Set of nodes which are reachable within the range limit, mapped to their shortest distance
    // from a start node.  These are the nodes which will be kept in the graph.
    let mut expanded_node_distances: HashMap<NodeId, usize> = HashMap::new();

    // A priority queue of NodeIds, sorted by distance with the nearest nodes at the front of the
    // queue.  Initialise this with just the start nodes.
    let mut frontier: BinaryHeap<Reverse<FrontierItem<&NodeId>>> = start_nodes
        .into_iter()
        .map(|(id, dist)| FrontierItem::new(id, dist))
        .map(Reverse)
        .collect();

    // Run Dijkstra's algorithm on the nodes
    while let Some(Reverse(FrontierItem { item: id, distance })) = frontier.pop() {
        let node_view = match view.get_node(id) {
            Some(v) => v,
            None => continue, // Don't expand node links which don't lead anywhere
        };

        // Mark this node as expanded, and ignore it if we've already expanded it (because
        // Dijkstra's guarantees it must have been given a distance <= to `distance`)
        if let Some(&existing_dist) = expanded_node_distances.get(id) {
            assert!(existing_dist <= distance);
            continue;
        }

        expanded_node_distances.insert(id.to_owned(), distance);

        // Skip this node if any node succeeding it would take longer to reach than the length of
        // the composition
        let distance_after_node = distance + node_view.node.length();
        if distance_after_node > dist_limit {
            continue;
        }

        // Expand this node
        for succ_link in node_view.successors() {
            let succ_id = &succ_link.id;
            let new_frontier_item = FrontierItem {
                item: succ_id,
                distance: distance_after_node,
            };
            frontier.push(Reverse(new_frontier_item));
        }
    }

    expanded_node_distances
}
