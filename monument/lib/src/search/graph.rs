use std::collections::HashMap;

use crate::{
    layout::{chunk_range::End, ChunkId, LinkIdx, StartIdx},
    music::Score,
    utils::{Counts, Rotation},
    Query,
};
use bit_vec::BitVec;
use itertools::Itertools;

/// An immutable version of [`monument_graph::Graph`] which can be traversed without hash table
/// lookups.
#[derive(Debug, Clone)]
pub struct Graph {
    pub starts: Vec<(ChunkIdx, StartIdx, Rotation)>,
    pub chunks: ChunkVec<Chunk>,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub score: Score,
    pub music_counts: Counts, // PERF: This is only used when reconstructing compositions

    pub length: u32,
    pub method_counts: Counts,
    /// Minimum number of rows required to go from the end of `self` to rounds
    pub dist_to_rounds: u32,
    pub label: String,

    pub duffer: bool,
    pub dist_to_non_duffer: u32,

    // Indices must be aligned with those from the source graph
    pub succs: Vec<Link>,
    // If this chunk is added to a composition, these bits denote the set of chunks will be marked as
    // unreachable.  This includes `Self`, because every chunk is guaranteed to be false against
    // itself.
    pub falseness: BitVec,

    pub end: Option<End>,
}

/// A link between a chunk and its successor
#[derive(Debug, Clone)]
pub struct Link {
    pub score: Score,
    pub source_idx: LinkIdx,
    pub next_chunk: ChunkIdx,
    pub rot: Rotation,
}

impl Link {
    pub fn new(score: f32, source_idx: LinkIdx, next_chunk: ChunkIdx, rot: Rotation) -> Self {
        Self {
            score: Score::from(score),
            source_idx,
            next_chunk,
            rot,
        }
    }
}

///////////////////////////////////////////
// CONVERSION FROM monument_graph::Graph //
///////////////////////////////////////////

impl Graph {
    pub fn new(source_graph: &crate::graph::Graph, query: &Query) -> Self {
        let num_chunks = source_graph.chunk_map().len();

        // Assign each chunk ID to a unique `ChunkIdx`, and vice versa.  This way, we can now label
        // the set of chunks with numbers that can be used to index into a BitVec for falseness
        // computation.
        let mut index_to_id = ChunkVec::<(ChunkId, &crate::graph::Chunk)>::new();
        let mut id_to_index = HashMap::<ChunkId, ChunkIdx>::new();
        for (id, chunk) in source_graph.chunks() {
            let index = index_to_id.push((id.to_owned(), chunk));
            id_to_index.insert(id.to_owned(), index);
        }

        // Now convert chunks from `monument_graph::Chunk` to `self::Chunk`
        let chunks: ChunkVec<_> = (0..num_chunks)
            .map(|index| {
                // Get the source chunk and its ChunkId
                let index = ChunkIdx::new(index);
                let (_id, source_chunk) = index_to_id[index].clone();

                // Generate a BitVec with a 1 for every chunk which is false against this chunk
                let mut falseness = BitVec::from_elem(num_chunks, false);
                for false_std_id in source_chunk.false_chunks() {
                    let false_id = ChunkId::Standard(false_std_id.clone());
                    let false_chunk_idx = id_to_index[&false_id];
                    falseness.set(false_chunk_idx.index(), true);
                }

                let succs = source_chunk
                    .successors()
                    .iter()
                    .filter_map(|link| {
                        let link_idx = link.source_idx;
                        let score =
                            query.layout.links[link_idx].weight * source_graph.num_parts() as f32;
                        let succ_idx = id_to_index.get(&link.id)?;
                        Some(Link::new(score, link_idx, *succ_idx, link.rotation))
                    })
                    .collect_vec();

                Chunk {
                    score: source_chunk.score(),
                    music_counts: source_chunk.music().counts.clone(),

                    length: source_chunk.length() as u32,
                    method_counts: source_chunk.method_counts().clone(),
                    dist_to_rounds: source_chunk.lb_distance_to_rounds as u32,
                    label: source_chunk.label().to_owned(),
                    end: source_chunk.end(),

                    duffer: source_chunk.duffer(),
                    dist_to_non_duffer: source_chunk.lb_distance_to_non_duffer as u32,

                    succs,
                    falseness,
                }
            })
            .collect();

        // Compute the list of start chunks and their labels
        let mut starts = Vec::new();
        for (start_id, start_idx, rotation) in source_graph.start_chunks() {
            if source_graph.get_chunk(start_id).is_some() {
                let chunk_idx = id_to_index[start_id];
                starts.push((chunk_idx, *start_idx, *rotation));
            }
        }

        Graph { starts, chunks }
    }
}

index_vec::define_index_type! { pub struct ChunkIdx = usize; }
type ChunkVec<T> = index_vec::IndexVec<ChunkIdx, T>;
