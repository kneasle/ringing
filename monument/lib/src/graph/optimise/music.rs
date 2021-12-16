use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use log::log;
use ordered_float::OrderedFloat;

use crate::{
    graph::{Graph, Node},
    layout::{NodeId, StandardNodeId},
    music::Breakdown,
    Query,
};

/// How many nodes will be searched to determine which node patterns generate the required music
const ITERATION_LIMIT: usize = 10_000;

pub(super) fn required_music_min(graph: &mut Graph, query: &Query) {
    // log::debug!("\n\n\n");

    // For each `MusicType`, maps its index to minimum count
    let min_music_counts = query
        .music_types
        .iter()
        .enumerate()
        .filter_map(|(i, ty)| ty.count_range().min.map(|min| (i, min)))
        .collect::<HashMap<_, _>>();

    if min_music_counts.is_empty() {
        return; // If there are no music bounds, then there's nothing to do
    }

    // The `(NodeId, Node)` pairs of nodes which contribute to the types of music that we care
    // about.
    let (required_nodes, non_required_nodes) = graph
        .nodes()
        .filter(|(_id, node)| {
            min_music_counts
                .iter()
                .any(|(ty_idx, _)| node.music().counts[*ty_idx] > 0)
        })
        .map(|(id, node)| (id.standard().unwrap(), node))
        .partition::<Vec<_>, _>(|(_id, node)| node.required);

    // Determine how much music is contributed by required nodes.
    let mut required_music_counts = vec![0; query.music_types.len()];
    for (idx, min) in &min_music_counts {
        required_music_counts[*idx] = *min;
    }
    let mut counts_needed_from_non_required_nodes = Breakdown {
        score: OrderedFloat(0.0),
        counts: required_music_counts,
    };
    for (_id, node) in required_nodes {
        // Non-required nodes aren't required to get music which the required nodes can already
        // achieve
        counts_needed_from_non_required_nodes.saturating_sub_assign(node.music());
    }

    // Do tree search over the non-required interesting nodes, determining which combinations of
    // the nodes satisfy the required music output.
    let node_combinations =
        search_node_combinations(&counts_needed_from_non_required_nodes, &non_required_nodes);
    for vs in &node_combinations {
        log::debug!("{:?}", vs.iter().sorted().collect_vec());
    }

    // Any nodes which are in every possible combination are required for the composition to
    // generate the required music.
    let new_required_nodes = non_required_nodes
        .iter()
        .filter(|(id, _node)| {
            node_combinations
                .iter()
                .all(|combination| combination.contains(id))
        })
        .map(|(id, _node)| (*id).clone())
        .collect_vec();
    let no_required_nodes = new_required_nodes.is_empty();
    log::debug!("required: {:?}", new_required_nodes);
    for required_id in new_required_nodes {
        graph
            .get_node_mut(&NodeId::Standard(required_id))
            .unwrap()
            .required = true;
    }

    // If there aren't any nodes to mark as required, then we can pick a node to condition on and
    // create two graphs to optimise: one where that node is required, and the other where that
    // node is removed.
    if no_required_nodes {
        log::warn!("No required nodes made");
        todo!();
    }
}

/// Remove any node which exceeds the max count for any music type.  Usually this max count will be
/// 0 (i.e. any nodes with that music should be removed).
pub(crate) fn remove_nodes_exceeding_max_count(graph: &mut Graph, query: &Query) {
    let mut counts_from_required_nodes = Breakdown::zero(query.music_types.len());
    for node in graph.nodes.values() {
        if node.required {
            counts_from_required_nodes += &node.music;
        }
    }

    for (music_ty_idx, count_from_required) in counts_from_required_nodes
        .counts
        .iter()
        .copied()
        .enumerate()
    {
        let music_type = &query.music_types[music_ty_idx];
        if let Some(count_limit) = music_type.count_range().max {
            let max_count_left_per_node = count_limit.checked_sub(count_from_required).expect(
                "Search can't be completed because the required nodes exceed a maximum music count.",
            );
            // Remove any nodes which exceed the count on their own
            graph
                .nodes
                .retain(|_id, node| node.music.counts[music_ty_idx] <= max_count_left_per_node);
        }
    }
}

/// Search every combination of the musical nodes, adding any working sets of nodes to
/// `node_patterns`.
// TODO: Why is this returning duplicates?
fn search_node_combinations<'gr>(
    counts_needed_from_non_required_nodes: &Breakdown,
    non_required_nodes: &[(&'gr StandardNodeId, &'gr Node)],
) -> Vec<HashSet<&'gr StandardNodeId>> {
    let mut node_patterns = Vec::<HashSet<&StandardNodeId>>::new();
    let mut nodes_used = HashSet::<&StandardNodeId>::new();
    let mut iter_count_down = ITERATION_LIMIT;
    search_nodes(
        non_required_nodes.iter(),
        counts_needed_from_non_required_nodes,
        non_required_nodes,
        &mut nodes_used,
        &mut node_patterns,
        &mut iter_count_down,
    );
    node_patterns
}

/// Recursively attempt to add any subset of [`NodeId`]s taken from `nodes`, adding any working
/// patterns to `node_patterns`.
fn search_nodes<'a, 'gr: 'a>(
    mut nodes: impl Iterator<Item = &'a (&'gr StandardNodeId, &'gr Node)> + Clone,

    counts_needed: &Breakdown,
    non_required_nodes: &[(&'gr StandardNodeId, &'gr Node)],

    nodes_used: &mut HashSet<&'gr StandardNodeId>,
    node_patterns: &mut Vec<HashSet<&'gr StandardNodeId>>,
    // Counter which is **decremented** every time this function is called, and the search is
    // terminated when this reaches 0.
    iters_left: &mut usize,
) {
    // Terminate once the iteration limit is reached
    match iters_left.checked_sub(1) {
        Some(v) => *iters_left = v,
        None => return,
    }
    // If the required count is reached without this node, then don't bother exploring further
    if counts_needed.counts.iter().all(|cnt| *cnt == 0) {
        node_patterns.push(nodes_used.clone());
        return;
    }

    let (id, node) = match nodes.next() {
        Some(v) => v,
        None => return, // No more nodes to test
    };

    // Test if other nodes can get the required score without including this one
    search_nodes(
        nodes.clone(),
        counts_needed,
        non_required_nodes,
        nodes_used,
        node_patterns,
        iters_left,
    );

    for false_id in node.false_nodes() {
        if nodes_used.contains(false_id) {
            return; // Don't add this node if it's false
        }
    }

    // Add the node
    nodes_used.insert(id);
    let counts_needed_with_this_node = counts_needed.saturating_sub(node.music());
    // Continue searching, assuming that this node is used
    search_nodes(
        nodes.clone(),
        &counts_needed_with_this_node,
        non_required_nodes,
        nodes_used,
        node_patterns,
        iters_left,
    );
    // Remove this node before returning
    assert!(nodes_used.remove(id));
}
