use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
};

use crate::utils::FrontierItem;

use super::{Data, DirectionalView, NodeId, NodeView};

/// Compute node distances to/from rounds
pub fn compute_distances(mut view: DirectionalView, data: &Data) {
    // TODO: Refactor out Dijkstra's algorithm into its own method?

    // Set of nodes which are reachable within the range limit, mapped to their shortest distance
    // from a start node.  These are the nodes which will be kept in the graph.
    let mut expanded_node_distances: HashMap<NodeId, usize> = HashMap::new();

    // A priority queue of NodeIds, sorted by distance with the nearest nodes at the front of the
    // queue.  Initialise this with just the start nodes.
    let mut frontier: BinaryHeap<Reverse<FrontierItem<(&NodeId, NodeView)>>> = view
        .start_nodes()
        .filter_map(|id| view.get_node(id).map(|node| (id, node))) // Collect IDs and nodes
        .map(FrontierItem::new)
        .map(Reverse)
        .collect();

    // Run Dijkstra's algorithm on the nodes
    while let Some(Reverse(FrontierItem {
        item: (id, node_view),
        distance,
    })) = frontier.pop()
    {
        // Mark this node as expanded, and ignore it if we've already expanded it (because
        // Dijkstra's guarantees it must have been given a distance <= to `distance`)
        if let Some(&existing_dist) = expanded_node_distances.get(id) {
            assert!(existing_dist <= distance);
            continue;
        }

        // Skip this node if any node succeeding it would take longer to reach than the length of
        // the composition
        let distance_after_node = distance + node_view.node.length();
        if distance_after_node > data.len_range.end {
            continue;
        }

        // Expand this node
        expanded_node_distances.insert(id.to_owned(), distance);
        for succ_link in node_view.successors() {
            let succ_id = &succ_link.id;
            if let Some(succ_node) = view.get_node(succ_id) {
                let new_frontier_item = FrontierItem {
                    item: (succ_id, succ_node),
                    distance: distance_after_node,
                };
                frontier.push(Reverse(new_frontier_item));
            }
        }
    }

    // Now that we've finished traversing the graph, set the node distances and strip out
    // unreachable nodes
    view.retain_nodes(|id, mut node_view| match expanded_node_distances.get(id) {
        // keep reachable nodes and update their distance lower bounds
        Some(&new_distance) => {
            *node_view.distance_mut() = new_distance;
            true
        }
        None => false, // Remove unreachable nodes
    });
}
