use std::collections::HashSet;

use crate::{
    graph::{Graph, LinkSide},
    query::Query,
};

/// Removes dangling references from the [`Graph`]
pub(super) fn remove_dangling_refs(graph: &mut Graph, _query: &Query) {
    // Strip dangling starts and ends
    for starts_or_ends in [&mut graph.starts, &mut graph.ends] {
        starts_or_ends.retain(|(_link_id, chunk_id)| graph.chunks.contains_key(chunk_id));
    }
    // Strip links with dangling from/to refs
    graph.links.retain(|_link_id, link| {
        if let LinkSide::Chunk(from_id) = &link.from {
            if !graph.chunks.contains_key(from_id) {
                return false;
            }
        }
        if let LinkSide::Chunk(to_id) = &link.to {
            if !graph.chunks.contains_key(to_id) {
                return false;
            }
        }
        true // Both from/to are non-dangling
    });
    // Strip dangling chunk refs (i.e. predecessor, successor or falseness)
    let chunk_ids = graph.chunks.keys().cloned().collect::<HashSet<_>>();
    for chunk in graph.chunks.values_mut() {
        chunk.successors.retain(|l| graph.links.contains(*l));
        chunk.predecessors.retain(|l| graph.links.contains(*l));
        chunk.false_chunks.retain(|id| chunk_ids.contains(id));
    }
}
