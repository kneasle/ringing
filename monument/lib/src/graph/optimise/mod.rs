//! Code to optimise a composition [`Graph`].

use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
    ops::Not,
    time::Instant,
};

use crate::{
    query::Query,
    utils::{
        lengths::{PerPartLength, TotalLength},
        FrontierItem,
    },
};

use super::{Chunk, ChunkId, Graph, Link, LinkId, LinkSide};

use Direction::{Backward, Forward};

impl Graph {
    /// Repeatedly optimise the graph until the graph stops getting smaller, or 20 iterations are
    /// made.
    pub(crate) fn optimise(&mut self, query: &Query) {
        const ITERATION_LIMIT: usize = 20;

        let passes = self::passes::default();

        log::debug!("Optimising graph:");
        let mut last_size = self.size();
        log::debug!("  Initial size: {}", self.size_summary());
        let mut iter_count = 0;
        let mut passes_since_last_time_graph_got_smaller = 0;
        let start_time = Instant::now();
        'optimisation: loop {
            // Run every optimisation pass
            for p in &passes {
                // TODO: Find a better locking system, or remove the `FnMut` bound so that locking
                // is unnecessary.  I think that this system can deadlock if multiple threads are
                // optimising graphs in parallel using the same set of passes.
                p.lock().unwrap().run(self, query);

                // Check if this optimisation pass has made the graph smaller
                let new_size = self.size();
                match new_size.cmp(&last_size) {
                    Ordering::Less => passes_since_last_time_graph_got_smaller = 0,
                    Ordering::Equal => {}
                    Ordering::Greater => {
                        unreachable!("Optimisation should never increase graph size")
                    }
                }
                last_size = new_size;
                // If we've run every optimisation pass without the graph getting smaller, then no
                // more optimisation is possible
                if passes_since_last_time_graph_got_smaller >= passes.len() {
                    break 'optimisation;
                }
                passes_since_last_time_graph_got_smaller += 1;
            }
            log::debug!("  New     size: {}", self.size_summary());

            // Stop optimising if the limit has been reached
            if iter_count > ITERATION_LIMIT {
                log::warn!(
                    "Graph optimisation limit reached, but more progress could have been made."
                );
                break;
            }
            iter_count += 1;
        }
        log::debug!("  Final   size: {}", self.size_summary());
        log::debug!(
            "Finished optimisation in {:?} after {} iters of every pass",
            start_time.elapsed(),
            iter_count
        );
    }

    fn size_summary(&self) -> String {
        format!(
            "{} chunks ({} non-duffer, {} required); {} links, {} starts, {} ends",
            self.chunks.len(),
            self.chunks.values().filter(|chunk| !chunk.duffer).count(),
            self.chunks.values().filter(|chunk| chunk.required).count(),
            self.links.len(),
            self.starts.len(),
            self.ends.len()
        )
    }

    /// Return a value representing the 'size' of this graph.  Optimisation passes are
    /// **required** to never increase this quantity.  Graph size is compared on the following
    /// factors (in order of precedence, most important first):
    /// 1. Number of chunks (smaller is better)
    /// 2. Number of links (smaller is better)
    /// 3. Number of required chunks (more is better)
    fn size(&self) -> (usize, usize, Reverse<usize>) {
        let mut num_links = 0;
        let mut num_required_chunks = 0;
        for chunk in self.chunks.values() {
            num_links += chunk.successors.len();
            if chunk.required {
                num_required_chunks += 1;
            }
        }
        (self.chunks.len(), num_links, Reverse(num_required_chunks))
    }
}

type SinglePass = Box<dyn FnMut(&mut Graph, &Query)>;
/// A [`Pass`] which can be run both [`Forward`] and [`Backward`] over a [`Graph`].  Very useful
/// when some graph operation is agnostic to the directionality of the graph, e.g.  computing
/// distances to/from rounds.
type DirectionalPass = Box<dyn FnMut(DirectionalView<'_>, &Query)>;

/// A pass which modifies a [`Graph`].  Passes are generally intended to perform optimisations -
/// they preserve the _semantic_ meaning of a [`Graph`] (i.e. the set of true compositions which it
/// generates), whilst modifying the [`Graph`] to make tree search faster.
enum Pass {
    /// Run a single non-directional pass
    Single(SinglePass),
    /// Run a `DirectionalPass` twice, [`Forward`] first
    BothDirections(DirectionalPass),
}

impl Pass {
    /// Apply the effect of this [`Pass`] to a [`Graph`]
    fn run(&mut self, graph: &mut Graph, query: &Query) {
        match self {
            Pass::Single(pass) => pass(graph, query),
            Pass::BothDirections(pass) => {
                pass(DirectionalView::new(graph, Forward), query);
                pass(DirectionalView::new(graph, Backward), query);
            }
        }
    }
}

////////////////////////
// DIRECTIONAL PASSES //
////////////////////////

/// A `Direction` in which a [`DirectionalPass`] can be run
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Copy)]
enum Direction {
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
struct DirectionalView<'graph> {
    graph: &'graph mut Graph,
    direction: Direction,
}

impl<'graph> DirectionalView<'graph> {
    fn new(graph: &'graph mut Graph, direction: Direction) -> Self {
        Self { graph, direction }
    }

    fn chunks(&'graph self) -> impl Iterator<Item = (&'graph ChunkId, ChunkView<'graph>)> + 'graph {
        self.graph
            .chunks
            .iter()
            .map(|(id, chunk)| (id, ChunkView::new(chunk, &*self.graph, self.direction)))
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    fn starts(&self) -> &[(LinkId, ChunkId)] {
        match self.direction {
            Forward => &self.graph.starts,
            Backward => &self.graph.ends,
        }
    }

    /// Gets the IDs of the 'start' chunks of the [`Graph`] going in this [`Direction`]
    #[allow(dead_code)] // Don't want `starts` without `ends`
    fn ends(&self) -> &[(LinkId, ChunkId)] {
        match self.direction {
            Forward => &self.graph.ends,
            Backward => &self.graph.starts,
        }
    }

    fn get_chunk(&'graph self, id: &ChunkId) -> Option<ChunkView<'graph>> {
        self.graph
            .chunks
            .get(id)
            .map(|chunk| ChunkView::new(chunk, self.graph, self.direction))
    }

    fn retain_chunks(&mut self, mut pred: impl FnMut(&ChunkId, ChunkViewMut) -> bool) {
        let direction = self.direction;
        self.graph
            .chunks
            .retain(|id, chunk| pred(id, ChunkViewMut::new(chunk, direction)));
    }
}

/// Immutable view of a [`Chunk`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
struct ChunkView<'graph> {
    chunk: &'graph Chunk,
    graph: &'graph Graph,
    direction: Direction,
}

impl<'graph> ChunkView<'graph> {
    #[must_use]
    fn new(chunk: &'graph Chunk, graph: &'graph Graph, direction: Direction) -> Self {
        Self {
            chunk,
            graph,
            direction,
        }
    }

    fn successors(&'graph self) -> impl Iterator<Item = LinkView<'graph>> + 'graph {
        self.convert_links(match self.direction {
            Forward => &self.chunk.successors,
            Backward => &self.chunk.predecessors,
        })
    }

    fn convert_links(
        &'graph self,
        links: &'graph [LinkId],
    ) -> impl Iterator<Item = LinkView<'graph>> + 'graph {
        links.iter().filter_map(|link_id| {
            Some(LinkView {
                link: self.graph.links.get(*link_id)?,
                direction: self.direction,
            })
        })
    }
}

/// Mutable view of a [`Chunk`], facing in a given [`Direction`] (i.e. a [`Backward`] view will
/// swap the successors/predecessors).
#[derive(Debug)]
#[non_exhaustive]
struct ChunkViewMut<'graph> {
    chunk: &'graph mut Chunk,
    direction: Direction,
}

impl<'graph> ChunkViewMut<'graph> {
    fn new(chunk: &'graph mut Chunk, direction: Direction) -> Self {
        Self { chunk, direction }
    }

    /// Mutable reference to the distance from rounds **to** the start of this chunk
    fn distance_to_boundary_mut(&mut self) -> &mut TotalLength {
        match self.direction {
            Forward => &mut self.chunk.lb_distance_from_rounds,
            Backward => &mut self.chunk.lb_distance_to_rounds,
        }
    }

    /// Mutable reference to the distance from rounds **to** the start of this chunk
    fn distance_to_non_duffer_mut(&mut self) -> &mut PerPartLength {
        match self.direction {
            Forward => &mut self.chunk.lb_distance_from_non_duffer,
            Backward => &mut self.chunk.lb_distance_to_non_duffer,
        }
    }
}

/// A view of a [`Link`], facing in a given [`Direction`]
#[derive(Debug)]
#[non_exhaustive]
struct LinkView<'graph> {
    link: &'graph Link,
    direction: Direction,
}

impl<'graph> LinkView<'graph> {
    #[allow(dead_code)] // Don't want `to` without `from`
    fn from(&self) -> &'graph LinkSide<ChunkId> {
        match self.direction {
            Direction::Forward => &self.link.from,
            Direction::Backward => &self.link.to,
        }
    }

    fn to(&self) -> &'graph LinkSide<ChunkId> {
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

mod passes {
    use std::{collections::HashSet, sync::Mutex};

    use itertools::Itertools;

    use crate::{
        graph::{ChunkId, Graph, LinkSide},
        query::Query,
    };

    use super::{DirectionalView, Pass};

    /// A default sequence of built-in optimisation passes.  Each is stored in a [`Mutex`] to
    /// enable concurrent access.
    pub(super) fn default() -> Vec<Mutex<Pass>> {
        [
            // Distance-related optimisation
            compute_distances(),
            strip_long_chunks(),
            Pass::Single(Box::new(super::strip_refs::remove_dangling_refs)),
            // Music optimisation
            Pass::Single(Box::new(super::music::required_music_min)),
            Pass::Single(Box::new(super::music::remove_chunks_exceeding_max_count)),
            // Required chunk optimisation
            mark_single_start_or_end_as_required(),
            remove_chunks_false_against_required(),
            // Misc optimisations
            remove_links_between_false_chunks(),
            // Non-duffers
            compute_duffer_distances(),
            strip_long_duffers(),
        ]
        .into_iter()
        .map(Mutex::new)
        .collect_vec()
    }

    /* Simple passes */

    /// Creates a [`Pass`] which removes any links between two chunks which are mutually false.
    fn remove_links_between_false_chunks() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, _query: &Query| {
            graph.retain_internal_links(|_link, _id_from, chunk_from, id_to, _chunk_to| {
                !chunk_from.false_chunks.contains(id_to)
            })
        }))
    }

    /* Distance related passes */

    /// Creates a [`Pass`] which recomputes the distances to and from rounds for every chunk,
    /// removing any which can't reach rounds in either direction.
    fn compute_distances() -> Pass {
        Pass::BothDirections(Box::new(|mut view: DirectionalView, query: &Query| {
            let expanded_chunk_distances = super::compute_distances(
                view.starts().iter().map(|(_, chunk_id)| chunk_id),
                &view,
                Some(query.max_length()),
            );
            // Set the chunk distances and strip out unreachable chunks
            view.retain_chunks(
                |id, mut chunk_view| match expanded_chunk_distances.get(id) {
                    // keep reachable chunks and update their distance lower bounds
                    Some(&new_distance) => {
                        *chunk_view.distance_to_boundary_mut() = new_distance;
                        true
                    }
                    None => false, // Remove unreachable chunks
                },
            );
        }))
    }

    /// A [`Pass`] which removes any chunks which can't be included in a short enough composition.
    fn strip_long_chunks() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            graph.chunks.retain(|_id, chunk| {
                let min_comp_length_with_chunk = chunk.lb_distance_from_rounds
                    + chunk.total_length
                    + chunk.lb_distance_to_rounds;
                min_comp_length_with_chunk <= query.max_length()
            });
        }))
    }

    /* Passes related to required chunks */

    /// A [`Pass`] which checks for a single start/end chunk and marks that chunk as required
    /// (because all compositions must start or end at that chunk).
    fn mark_single_start_or_end_as_required() -> Pass {
        Pass::BothDirections(Box::new(|view: DirectionalView, _| {
            let single_chunk_id = match view.starts().iter().exactly_one() {
                Ok((_link_id, chunk_id)) => chunk_id.clone(),
                Err(_) => return,
            };
            if let Some(chunk) = view.graph.chunks.get_mut(&single_chunk_id) {
                chunk.required = true;
            }
        }))
    }

    /// A [`Pass`] which removes any chunks which are false against a chunk marked as required
    fn remove_chunks_false_against_required() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, _| {
            let mut chunk_ids_to_remove: HashSet<ChunkId> = HashSet::new();
            // For each required chunk ...
            for (id, chunk) in &graph.chunks {
                if chunk.required {
                    // ... mark all its false chunks (**except itself**) to be removed
                    let other_false_chunk_ids = chunk
                        .false_chunks
                        .iter()
                        .cloned()
                        .filter(|false_id| false_id != id);
                    chunk_ids_to_remove.extend(other_false_chunk_ids);
                }
            }
            graph
                .chunks
                .retain(|id, _chunk| !chunk_ids_to_remove.contains(id));
        }))
    }

    /* Non-duffer optimisations */

    fn compute_duffer_distances() -> Pass {
        Pass::BothDirections(Box::new(
            |mut graph_view: DirectionalView, query: &Query| {
                // Find which chunks follow directly from non-duffer chunks
                let mut successors_of_non_duffers = HashSet::<ChunkId>::new();
                for (_id, chunk_view) in graph_view.chunks().filter(|(_, v)| !v.chunk.duffer) {
                    for succ_link in chunk_view.successors() {
                        if let LinkSide::Chunk(succ_id) = succ_link.to() {
                            successors_of_non_duffers.insert(succ_id.clone());
                        }
                    }
                }
                // Starts are also considered a non-duffer for the purposes of distance calculations
                successors_of_non_duffers.extend(
                    graph_view
                        .starts()
                        .iter()
                        .map(|(_link, chunk_id)| chunk_id.clone()),
                );
                // Compute distances of every chunk, starting at those which follow directly from a
                // non-duffer chunk (i.e. are zero distance away from non-duffers)
                let distances_from_non_duffer = super::compute_distances(
                    successors_of_non_duffers.iter(),
                    &graph_view,
                    query
                        .max_contiguous_duffer
                        .map(|l| l.as_total(&query.part_head_group)),
                );
                // Set the distances (stripping any chunks which are too far from a duffer)
                graph_view.retain_chunks(|id, mut chunk_view| {
                    match distances_from_non_duffer.get(id) {
                        // keep reachable chunks and update their distance lower bounds
                        Some(&new_distance) => {
                            *chunk_view.distance_to_non_duffer_mut() =
                                new_distance.as_per_part(&query.part_head_group);
                            true
                        }
                        None => false, // Remove unreachable chunks
                    }
                });
            },
        ))
    }

    fn strip_long_duffers() -> Pass {
        Pass::Single(Box::new(|graph: &mut Graph, query: &Query| {
            if let Some(duffer_limit) = query.max_contiguous_duffer {
                graph.chunks.retain(|_id, chunk| {
                    // The length of the shortest chunk of duffer containing this chunk
                    let min_duffer_length = chunk.lb_distance_from_non_duffer
                        + chunk.per_part_length
                        + chunk.lb_distance_to_non_duffer;
                    !chunk.duffer || min_duffer_length <= duffer_limit
                });
            }
        }))
    }
}

///////////
// UTILS //
///////////

/// Given a set of starting chunks (and their distances), compute the shortest distance to every
/// reachable chunk.
fn compute_distances<'a>(
    start_chunks: impl IntoIterator<Item = &'a ChunkId>,
    view: &DirectionalView<'a>,
    inclusive_dist_limit: Option<TotalLength>,
) -> HashMap<ChunkId, TotalLength> {
    // Set of chunks which are reachable within the range limit, mapped to their shortest distance
    // from a start chunk.  These are the chunks which will be kept in the graph.
    let mut expanded_chunk_distances: HashMap<ChunkId, TotalLength> = HashMap::new();

    // A priority queue of ChunkIds, sorted by distance with the nearest chunks at the front of the
    // queue.  Initialise this with just the start chunks.
    let mut frontier: BinaryHeap<Reverse<FrontierItem<ChunkId, TotalLength>>> = start_chunks
        .into_iter()
        .map(|id| FrontierItem::new(id.clone(), TotalLength::ZERO))
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
        let distance_after_chunk = distance + chunk_view.chunk.total_length;
        if let Some(l) = inclusive_dist_limit {
            if distance_after_chunk > l {
                continue;
            }
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

/// # Helpers for optimisation passes
impl Graph {
    /// Removes all internal (i.e. [`Chunk`] to [`Chunk`]) links for whom `pred` returns `false`.
    fn retain_internal_links(
        &mut self,
        mut pred: impl FnMut(&Link, &ChunkId, &Chunk, &ChunkId, &Chunk) -> bool,
    ) {
        self.links.retain(|_link_id, link| {
            // Extract `ChunkId`s on either side of this link, or return `true` if the link is
            // external
            let (id_from, id_to) = match (&link.from, &link.to) {
                (LinkSide::Chunk(f), LinkSide::Chunk(t)) => (f, t),
                (LinkSide::StartOrEnd, LinkSide::StartOrEnd) => {
                    unreachable!("StartOrEnd -> StartOrEnd links aren't allowed");
                }
                _ => return true, // Link is external
            };

            match (self.chunks.get(id_from), self.chunks.get(id_to)) {
                (Some(from), Some(to)) => pred(link, id_from, from, id_to, to),
                _ => false, // If either side of the link is dangling, remove the link
            }
        })
    }
}
