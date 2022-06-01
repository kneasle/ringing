use std::collections::HashSet;

use crate::{graph::Graph, Query};

/// Removes dangling references from the [`Graph`]
pub(super) fn remove_dangling_refs(graph: &mut Graph, _query: &Query) {
    // Cloned set of which `ChunkID`s are in the graph.  We need to clone these, because otherwise
    // we'd have to borrow the chunk map whilst mutably iterating over it.
    let chunk_ids = graph.ids().cloned().collect::<HashSet<_>>();

    graph.remove_dangling_starts();
    graph.remove_dangling_ends();
    graph.remove_dangling_links();
    let link_ids = graph.links.keys().collect::<HashSet<_>>();
    // Strip chunk refs (i.e. predecessor, successor or falseness)
    for (_id, chunk) in graph.chunks_mut() {
        chunk
            .successors_mut()
            .retain(|link| link_ids.contains(link));
        chunk
            .predecessors_mut()
            .retain(|link| link_ids.contains(link));
        chunk.false_chunks_mut().retain(|id| chunk_ids.contains(id));
    }
}
