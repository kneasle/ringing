use std::collections::HashSet;

use crate::{graph::Graph, layout::ChunkId, Query};

/// Removes chunk references which point to non-existent chunks.  This cannot fail.
pub(super) fn strip_refs(graph: &mut Graph, _query: &Query) {
    // Cloned set of which `ChunkID`s are in the graph.  We need to clone these, because otherwise
    // we'd have to borrow the chunk map whilst mutably iterating over it.
    let chunk_ids = graph.ids().cloned().collect::<HashSet<_>>();

    // Strip start/end chunks
    graph.retain_start_chunks(|(id, _start_idx, _rotation)| chunk_ids.contains(id));
    graph.retain_end_chunks(|(id, _end_idx)| chunk_ids.contains(id));
    // Strip chunk refs (i.e. predecessor, successor or falseness)
    for (_id, chunk) in graph.chunks_mut() {
        chunk
            .successors_mut()
            .retain(|link| chunk_ids.contains(&link.to));
        chunk
            .predecessors_mut()
            .retain(|link| chunk_ids.contains(&link.to));
        chunk
            .false_chunks_mut()
            .retain(|id| chunk_ids.contains(&ChunkId::Standard(id.clone())));
    }

    // TODO: Strip `successor` ptrs which don't have a matching `predecessor` (or vice versa)
}
