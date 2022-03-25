//! A plugin-able system of passes made over the composition [`Graph`], in order to modify or
//! optimise the graph.

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
    ops::Not,
};

use crate::{layout::ChunkId, utils::FrontierItem, Query};

use super::{Chunk, Graph};

use self::Direction::{Backward, Forward};

pub type SinglePass = Box<dyn FnMut(&mut Graph, &Query)>;
/// A [`Pass`] which can be run both [`Forward`] and [`Backward`] over a [`Graph`].  For example,
/// computing distances to/from rounds (removing unreachable chunks).
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

    pub fn chunks(&self) -> impl Iterator<Item = (&ChunkId, ChunkView)> {
        self.graph
            .chunks()
            .map(|(id, chunk)| (id, ChunkView::new(chunk, self.direction)))
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    pub fn start_chunks(&self) -> Box<dyn Iterator<Item = &ChunkId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.start_chunks().iter().map(|(id, _, _)| id)),
            Backward => Box::new(self.graph.end_chunks().iter().map(|(id, _)| id)),
        }
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    pub fn end_chunks(&self) -> Box<dyn Iterator<Item = &ChunkId> + '_> {
        match self.direction {
            Forward => Box::new(self.graph.end_chunks().iter().map(|(id, _)| id)),
            Backward => Box::new(self.graph.start_chunks().iter().map(|(id, _, _)| id)),
        }
    }

    pub fn get_chunk(&'graph self, id: &ChunkId) -> Option<ChunkView<'graph>> {
        self.graph
            .get_chunk(id)
            .map(|chunk| ChunkView::new(chunk, self.direction))
    }

    pub fn get_chunk_mut(&'graph mut self, id: &ChunkId) -> Option<ChunkViewMut<'graph>> {
        let direction = self.direction;
        self.graph
            .get_chunk_mut(id)
            .map(|chunk| ChunkViewMut::new(chunk, direction))
    }

    pub fn retain_chunks(&mut self, mut pred: impl FnMut(&ChunkId, ChunkViewMut) -> bool) {
        let direction = self.direction;
        self.graph
            .retain_chunks(|id, chunk| pred(id, ChunkViewMut::new(chunk, direction)));
    }
}

/// Immutable view of a [`Chunk`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct ChunkView<'graph> {
    pub chunk: &'graph Chunk,
    pub direction: Direction,
}

impl<'graph> ChunkView<'graph> {
    pub fn new(chunk: &'graph Chunk, direction: Direction) -> Self {
        Self { chunk, direction }
    }

    pub fn successors(self) -> &'graph [super::Link] {
        match self.direction {
            Forward => self.chunk.successors(),
            Backward => self.chunk.predecessors(),
        }
    }

    pub fn predecessors(self) -> &'graph [super::Link] {
        match self.direction {
            Forward => self.chunk.predecessors(),
            Backward => self.chunk.successors(),
        }
    }
}

/// Mutable view of a [`Chunk`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug)]
#[non_exhaustive]
pub struct ChunkViewMut<'graph> {
    pub chunk: &'graph mut Chunk,
    pub direction: Direction,
}

impl<'graph> ChunkViewMut<'graph> {
    fn new(chunk: &'graph mut Chunk, direction: Direction) -> Self {
        Self { chunk, direction }
    }

    pub fn successors_mut(&mut self) -> &mut Vec<super::Link> {
        match self.direction {
            Forward => self.chunk.successors_mut(),
            Backward => self.chunk.predecessors_mut(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<super::Link> {
        match self.direction {
            Forward => self.chunk.predecessors_mut(),
            Backward => self.chunk.successors_mut(),
        }
    }

    /// Mutable reference to the distance from rounds **to** the start of this chunk
    pub fn distance_mut(&mut self) -> &mut usize {
        match self.direction {
            Forward => &mut self.chunk.lb_distance_from_rounds,
            Backward => &mut self.chunk.lb_distance_to_rounds,
        }
    }

    /// Mutable reference to the distance from rounds **to** the start of this chunk
    pub fn non_duffer_distance_mut(&mut self) -> &mut usize {
        match self.direction {
            Forward => &mut self.chunk.lb_distance_from_non_duffer,
            Backward => &mut self.chunk.lb_distance_to_non_duffer,
        }
    }
}

////////////////////
// BUILTIN PASSES //
////////////////////

mod music; // Proving chunks as required/unusable based on music requirements
mod strip_refs; // Strip references to non-existent chunks

pub mod passes {
    use std::{collections::HashSet, sync::Mutex};

    use itertools::Itertools;

    use crate::{graph::Graph, layout::ChunkId, Query};

    use super::{DirectionalView, Pass};

    /// A default sequence of built-in optimisation passes.  Each is stored in a [`Mutex`] to
    /// enable concurrent access.
    pub fn default() -> Vec<Mutex<Pass>> {
        [
            // Distance-related optimisation
            compute_distances(),
            strip_long_chunks(),
            strip_refs(),
            // Duffer-related optimisation
            compute_duffer_distances(),
            strip_duff_chunks(),
            // Music optimisation
            required_music(),
            remove_chunks_exceeding_max_count(),
            // Required chunk optimisation
            single_start_or_end_required(),
            remove_chunks_false_against_required(),
        ]
        .into_iter()
        .map(Mutex::new)
        .collect_vec()
    }

    /* Simple passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    pub fn strip_refs() -> Pass {
        Pass::Single(Box::new(super::strip_refs::strip_refs))
    }

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    pub fn required_music() -> Pass {
        Pass::Single(Box::new(super::music::required_music_min))
    }

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    pub fn remove_chunks_exceeding_max_count() -> Pass {
        Pass::Single(Box::new(super::music::remove_chunks_exceeding_max_count))
    }

    /* Distance related passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    pub fn compute_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let expanded_chunk_distances = super::compute_distances(
                view.start_chunks().map(|id| (id, 0)),
                &view,
                query.len_range.end,
            );
            // Set the chunk distances and strip out unreachable chunks
            view.retain_chunks(
                |id, mut chunk_view| match expanded_chunk_distances.get(id) {
                    // keep reachable chunks and update their distance lower bounds
                    Some(&new_distance) => {
                        *chunk_view.distance_mut() = new_distance;
                        true
                    }
                    None => true, // Remove unreachable chunks
                },
            );
        }))
    }

    /// A [`Pass`] which removes any chunks which can't be included in a short enough composition.
    pub fn strip_long_chunks() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            graph.retain_chunks(|_id, chunk| chunk.min_comp_length() < query.len_range.end);
        }))
    }

    /// Creates a [`Pass`] which recomputes how close chunks are to non-duffer chunks, removing any
    /// which can't reach a non-duffer chunk in either direction.
    pub fn compute_duffer_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let duffer_distances = super::compute_distances(
                // Give all non-duffer chunks a distance of 0, and assign all start/end chunks to
                // their own length (thus treating rounds as a 0-length non-duffer)
                view.graph
                    .chunks()
                    .filter(|&(_id, chunk)| !chunk.duffer)
                    .map(|(id, _chunk)| (id, 0))
                    .chain(view.start_chunks().filter_map(|id| {
                        let chunk = view.graph.get_chunk(id)?;
                        chunk.duffer.then(|| (id, chunk.length()))
                    })),
                &view,
                query.max_duffer_rows.unwrap_or(usize::MAX),
            );
            // Set the chunk distances and strip out unreachable chunks
            view.retain_chunks(|id, mut chunk_view| match duffer_distances.get(id) {
                // keep reachable chunks and update their distance lower bounds
                Some(&new_distance) => {
                    *chunk_view.non_duffer_distance_mut() = new_distance;
                    true
                }
                // Remove unreachable duffer chunks
                None => {
                    assert!(chunk_view.chunk.duffer);
                    false
                }
            });
        }))
    }

    /// A [`Pass`] which removes any chunks which can't connect two non-duffer chunks quickly enough
    pub fn strip_duff_chunks() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            if let Some(max_duffer_rows) = query.max_duffer_rows {
                graph.retain_chunks(|_id, chunk| chunk.min_duffer_length() <= max_duffer_rows);
            }
        }))
    }

    /* Passes related to required chunks */

    /// A [`Pass`] which checks for a single start/end chunk and marks that chunk as required
    /// (because all compositions must start or end at that chunk).
    pub fn single_start_or_end_required() -> Pass {
        Pass::BothDirections(Box::new(|view: DirectionalView, _| {
            let single_start_id = match view.start_chunks().exactly_one() {
                Ok(id) => id.clone(),
                Err(_) => return,
            };
            if let Some(chunk) = view.graph.get_chunk_mut(&single_start_id) {
                chunk.required = true;
            }
        }))
    }

    /// A [`Pass`] which removes any chunks which are false against a chunk marked as required
    pub fn remove_chunks_false_against_required() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, _| {
            let mut chunk_ids_to_remove: HashSet<ChunkId> = HashSet::new();
            // For each required chunk ...
            for (id, chunk) in graph.chunks() {
                if chunk.required {
                    // ... mark all its false chunks (**except itself**) to be removed
                    let other_false_chunk_ids = chunk
                        .false_chunks()
                        .iter()
                        .map(|id| ChunkId::Standard(id.clone()))
                        .filter(|false_id| false_id != id);
                    chunk_ids_to_remove.extend(other_false_chunk_ids);
                }
            }
            graph.retain_chunks(|id, _chunk| !chunk_ids_to_remove.contains(id));
        }))
    }
}

///////////
// UTILS //
///////////

/// Given a set of starting chunks (and their distances), compute the shortest distance to every
/// reachable chunk.
fn compute_distances<'a>(
    start_chunks: impl IntoIterator<Item = (&'a ChunkId, usize)>,
    view: &DirectionalView<'a>,
    dist_limit: usize,
) -> HashMap<ChunkId, usize> {
    // Set of chunks which are reachable within the range limit, mapped to their shortest distance
    // from a start chunk.  These are the chunks which will be kept in the graph.
    let mut expanded_chunk_distances: HashMap<ChunkId, usize> = HashMap::new();

    // A priority queue of ChunkIds, sorted by distance with the nearest chunks at the front of the
    // queue.  Initialise this with just the start chunks.
    let mut frontier: BinaryHeap<Reverse<FrontierItem<&ChunkId>>> = start_chunks
        .into_iter()
        .map(|(id, dist)| FrontierItem::new(id, dist))
        .map(Reverse)
        .collect();

    // Run Dijkstra's algorithm on the chunks
    while let Some(Reverse(FrontierItem { item: id, distance })) = frontier.pop() {
        let chunk_view = match view.get_chunk(id) {
            Some(v) => v,
            None => continue, // Don't expand chunk links which don't lead anywhere
        };

        // Mark this chunk as expanded, and ignore it if we've already expanded it (because
        // Dijkstra's guarantees it must have been given a distance <= to `distance`)
        if let Some(&existing_dist) = expanded_chunk_distances.get(id) {
            assert!(existing_dist <= distance);
            continue;
        }

        expanded_chunk_distances.insert(id.to_owned(), distance);

        // Skip this chunk if any chunk succeeding it would take longer to reach than the length of
        // the composition
        let distance_after_chunk = distance + chunk_view.chunk.length();
        if distance_after_chunk > dist_limit {
            continue;
        }

        // Expand this chunk
        for succ_link in chunk_view.successors() {
            let succ_id = &succ_link.to;
            let new_frontier_item = FrontierItem {
                item: succ_id,
                distance: distance_after_chunk,
            };
            frontier.push(Reverse(new_frontier_item));
        }
    }

    expanded_chunk_distances
}
