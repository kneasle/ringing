//! A plugin-able system of passes made over the composition [`Graph`], in order to modify or
//! optimise the graph.

use std::{fmt::Debug, ops::Not};

use crate::{graph, Data, Graph, Node, NodeId};

use self::Direction::{Backward, Forward};

pub type SinglePass = Box<dyn FnMut(&mut Graph, &Data)>;
/// A [`Pass`] which can be run both [`Forward`] and [`Backward`] over a [`Graph`].  For example,
/// computing distances to/from rounds (removing unreachable nodes).
pub type DirectionalPass = Box<dyn FnMut(DirectionalView<'_>, &Data)>;

/// A pass which modifies a [`Graph`].  Passes are generally intended to perform optimisations -
/// they preserve the _semantic_ meaning of a [`Graph`] (i.e. the set of true compositions which it
/// generates), whilst modifying the [`Graph`] to make tree search faster.
pub enum Pass {
    /// Run a single non-directional pass
    Single(SinglePass),
    /// Apply a `DirectionalPass` but only in one [`Direction`]
    OneDirection(DirectionalPass, Direction),
    /// Apply a `DirectionalPass` twice, [`Forward`] first
    BothDirections(DirectionalPass),
    /// Apply a `DirectionalPass` twice, [`Backward`] first
    BothDirectionsRev(DirectionalPass),
}

impl Pass {
    /// Apply the effect of this [`Pass`] to a [`Graph`]
    pub fn run(&mut self, graph: &mut Graph, data: &Data) {
        match self {
            Pass::Single(pass) => pass(graph, data),
            Pass::OneDirection(pass, direction) => run_in_direction(*direction, pass, graph, data),
            Pass::BothDirections(pass) => {
                run_in_direction(Forward, pass, graph, data);
                run_in_direction(Backward, pass, graph, data);
            }
            Pass::BothDirectionsRev(pass) => {
                run_in_direction(Backward, pass, graph, data);
                run_in_direction(Forward, pass, graph, data);
            }
        }
    }
}

////////////////////////
// DIRECTIONAL PASSES //
////////////////////////

fn run_in_direction(
    direction: Direction,
    pass: &mut DirectionalPass,
    graph: &mut Graph,
    data: &Data,
) {
    pass(DirectionalView::new(graph, direction), data)
}

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

    /// Gets the IDs of the 'start' nodes of the [`Graph`] going in this [`Direction`]
    pub fn start_nodes(&self) -> Box<dyn Iterator<Item = &NodeId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.start_nodes().iter().map(|(id, _)| id)),
            Backward => Box::new(self.graph.end_nodes().iter().map(|(id, _)| id)),
        }
    }

    /// Gets the IDs of the 'start' nodes of the [`Graph`] going in this [`Direction`]
    pub fn end_nodes(&self) -> Box<dyn Iterator<Item = &NodeId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.end_nodes().iter().map(|(id, _)| id)),
            Backward => Box::new(self.graph.start_nodes().iter().map(|(id, _)| id)),
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

    pub fn successors(self) -> &'graph [graph::Link] {
        match self.direction {
            Forward => self.node.successors(),
            Backward => self.node.predecessors(),
        }
    }

    pub fn predecessors(self) -> &'graph [graph::Link] {
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

    pub fn successors_mut(&mut self) -> &mut Vec<graph::Link> {
        match self.direction {
            Forward => self.node.successors_mut(),
            Backward => self.node.predecessors_mut(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<graph::Link> {
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
}

////////////////////
// BUILTIN PASSES //
////////////////////

mod distance; // Compute node distances
mod strip_refs; // Strip references to non-existent nodes

pub mod passes {
    use std::collections::HashSet;

    use itertools::Itertools;

    use crate::{optimise::DirectionalView, Data, Graph, NodeId};

    use super::Pass;

    /// A default sequence of built-in optimisation passes
    pub fn default() -> Vec<Pass> {
        vec![
            strip_refs(),
            // Distance-related optimisation
            compute_distances(),
            strip_long_nodes(),
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

    /* Distance related passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every node,
    /// removing any which can't reach rounds in either direction.
    pub fn compute_distances() -> Pass {
        Pass::BothDirections(Box::new(super::distance::compute_distances))
    }

    /// A [`Pass`] which removes any nodes which can't be included in a short enough round block.
    pub fn strip_long_nodes() -> Pass {
        fn pass(graph: &mut Graph, data: &Data) {
            graph.retain_nodes(|_id, node| node.min_comp_length() < data.len_range.end);
        }
        Pass::Single(Box::new(pass))
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
