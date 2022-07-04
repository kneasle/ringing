//! A plugin-able system of passes made over the composition [`Graph`], in order to modify or
//! optimise the graph.

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
    ops::Not,
};

use crate::{utils::FrontierItem, Query};

use super::{Chunk, ChunkId, Graph, Link, LinkId, LinkSide};

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
            .map(|(id, chunk)| (id, ChunkView::new(chunk, self.graph, self.direction)))
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    pub fn starts(&self) -> &[(LinkId, ChunkId)] {
        match self.direction {
            Forward => self.graph.starts(),
            Backward => self.graph.ends(),
        }
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    pub fn ends(&self) -> &[(LinkId, ChunkId)] {
        match self.direction {
            Forward => self.graph.ends(),
            Backward => self.graph.starts(),
        }
    }

    pub fn get_chunk(&'graph self, id: &ChunkId) -> Option<ChunkView<'graph>> {
        self.graph
            .get_chunk(id)
            .map(|chunk| ChunkView::new(chunk, self.graph, self.direction))
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
    pub graph: &'graph Graph,
    pub direction: Direction,
}

impl<'graph> ChunkView<'graph> {
    #[must_use]
    pub fn new(chunk: &'graph Chunk, graph: &'graph Graph, direction: Direction) -> Self {
        Self {
            chunk,
            graph,
            direction,
        }
    }

    pub fn successors(&'graph self) -> impl Iterator<Item = LinkView<'graph>> + 'graph {
        self.convert_links(match self.direction {
            Forward => self.chunk.successors(),
            Backward => self.chunk.predecessors(),
        })
    }

    pub fn predecessors(&'graph self) -> impl Iterator<Item = LinkView<'graph>> + 'graph {
        self.convert_links(match self.direction {
            Forward => self.chunk.predecessors(),
            Backward => self.chunk.successors(),
        })
    }

    fn convert_links(
        &'graph self,
        links: &'graph [LinkId],
    ) -> impl Iterator<Item = LinkView<'graph>> + 'graph {
        links.iter().filter_map(|link_id| {
            Some(LinkView {
                link: self.graph.get_link(*link_id)?,
                direction: self.direction,
            })
        })
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

    pub fn successors_mut(&mut self) -> &mut Vec<super::LinkId> {
        match self.direction {
            Forward => self.chunk.successors_mut(),
            Backward => self.chunk.predecessors_mut(),
        }
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<super::LinkId> {
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

/// A view of a [`Link`], facing in a given [`Direction`]
#[derive(Debug)]
#[non_exhaustive]
pub struct LinkView<'graph> {
    pub link: &'graph Link,
    pub direction: Direction,
}

impl<'graph> LinkView<'graph> {
    pub fn from(&self) -> &'graph LinkSide<ChunkId> {
        match self.direction {
            Direction::Forward => &self.link.from,
            Direction::Backward => &self.link.to,
        }
    }

    pub fn to(&self) -> &'graph LinkSide<ChunkId> {
        match self.direction {
            Direction::Forward => &self.link.to,
            Direction::Backward => &self.link.from,
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

    use crate::{
        graph::{ChunkId, Graph},
        Query,
    };

    use super::{DirectionalView, Pass};

    /// A default sequence of built-in optimisation passes.  Each is stored in a [`Mutex`] to
    /// enable concurrent access.
    pub fn default() -> Vec<Mutex<Pass>> {
        [
            // Misc optimisations
            remove_links_between_false_chunks(),
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
            mark_single_start_or_end_as_required(),
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
        Pass::Single(Box::new(super::strip_refs::remove_dangling_refs))
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

    /// Creates a [`Pass`] which removes any links between two chunks which are mutually false.
    pub fn remove_links_between_false_chunks() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, _query: &Query| {
            graph.retain_internal_links(|_link, _id_from, chunk_from, id_to, _chunk_to| {
                chunk_from.truth_against(id_to).is_true()
            })
        }))
    }

    /* Distance related passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    pub fn compute_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let expanded_chunk_distances = super::compute_distances(
                view.starts()
                    .iter()
                    .map(|(_link_id, chunk_id)| (chunk_id, 0)),
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
                    .chain(view.starts().iter().filter_map(|(_link_id, chunk_id)| {
                        let chunk = view.graph.get_chunk(chunk_id)?;
                        chunk.duffer.then(|| (chunk_id, chunk.total_length()))
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
    pub fn mark_single_start_or_end_as_required() -> Pass {
        Pass::BothDirections(Box::new(|view: DirectionalView, _| {
            let single_chunk_id = match view.starts().iter().exactly_one() {
                Ok((_link_id, chunk_id)) => chunk_id.clone(),
                Err(_) => return,
            };
            if let Some(chunk) = view.graph.get_chunk_mut(&single_chunk_id) {
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
                        .cloned()
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
    let mut frontier: BinaryHeap<Reverse<FrontierItem<ChunkId>>> = start_chunks
        .into_iter()
        .map(|(id, dist)| FrontierItem::new(id.clone(), dist))
        .map(Reverse)
        .collect();

    // Run Dijkstra's algorithm on the chunks
    while let Some(Reverse(FrontierItem { item: id, distance })) = frontier.pop() {
        let chunk_view = match view.get_chunk(&id) {
            Some(v) => v,
            None => continue, // Don't expand chunk links which don't lead anywhere
        };

        // Mark this chunk as expanded, and ignore it if we've already expanded it (because
        // Dijkstra's guarantees it must have been given a distance <= to `distance`)
        if let Some(&existing_dist) = expanded_chunk_distances.get(&id) {
            assert!(existing_dist <= distance);
            continue;
        }

        expanded_chunk_distances.insert(id.to_owned(), distance);

        // Skip this chunk if any chunk succeeding it would take longer to reach than the length of
        // the composition
        let distance_after_chunk = distance + chunk_view.chunk.total_length();
        if distance_after_chunk > dist_limit {
            continue;
        }

        // Expand this chunk
        for succ_link in chunk_view.successors() {
            let next_chunk_id = match succ_link.to() {
                LinkSide::Chunk(id) => id.clone(),
                LinkSide::StartOrEnd => continue,
            };
            let new_frontier_item = FrontierItem {
                item: next_chunk_id,
                distance: distance_after_chunk,
            };
            frontier.push(Reverse(new_frontier_item));
        }
    }

    expanded_chunk_distances
}
