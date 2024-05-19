//! Code to optimise a composition [`Graph`].

use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap, HashSet},
    fmt::Debug,
    ops::Not,
    time::Instant,
};

use crate::{
    parameters::Parameters,
    prove_length::RefinedRanges,
    utils::{lengths::TotalLength, FrontierItem},
};

use super::{Chunk, ChunkId, Graph, Link, LinkId, LinkSide};

use itertools::Itertools;
use Direction::{Backward, Forward};

impl Graph {
    /// Repeatedly optimise the graph until the graph stops getting smaller, or 20 iterations are
    /// made.
    pub(crate) fn optimise(&mut self, params: &Parameters, ranges: &RefinedRanges) {
        const ITERATION_LIMIT: usize = 20;

        log::debug!("Optimising graph:");
        let mut last_size = self.size();
        log::debug!("  Initial size: {}", self.size_summary());
        let mut iter_count = 0;
        let start_time = Instant::now();
        loop {
            // Distance-related optimisation
            self.run_bidirectional_pass(|view| passes::compute_distances(view, params));
            passes::strip_long_chunks(self, params);
            // Required chunk optimisation
            self.run_bidirectional_pass(passes::mark_start_or_ends_as_required);
            passes::remove_chunks_false_against_required(self);
            // Required calls
            self.run_bidirectional_pass(|view| {
                passes::remove_links_conflicting_with_calling(view, params)
            });
            // Misc optimisations
            passes::remove_links_between_false_chunks(self);
            passes::remove_chunks_with_long_method_counts(self, ranges);
            passes::remove_links_with_long_method_counts(self, ranges);
            passes::remove_chunks_which_exceed_music_limits(self, params);
            // Cleanup
            passes::remove_dangling_refs(self);

            // Check if this optimisation pass has made the graph smaller, stopping if no progress
            // is being made
            log::debug!("  New     size: {}", self.size_summary());
            let new_size = self.size();
            match new_size.cmp(&last_size) {
                Ordering::Less => {}      // Graph got smaller, still work to be done
                Ordering::Equal => break, // No progress has been made, so no more work to do
                Ordering::Greater => {
                    unreachable!("Optimisation should never increase graph size")
                }
            }
            last_size = new_size;
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

    fn run_bidirectional_pass(&mut self, mut pass: impl FnMut(DirectionalView)) {
        pass(DirectionalView::new(self, Direction::Forward));
        pass(DirectionalView::new(self, Direction::Backward));
    }

    fn size_summary(&self) -> String {
        format!(
            "{} chunks, {} required groups, {} links, {} starts, {} ends",
            self.chunks.len(),
            self.required_chunk_sets.len(),
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
        for chunk in self.chunks.values() {
            num_links += chunk.successors.len();
        }
        (
            self.chunks.len(),                       // Less is better
            num_links,                               // Less is better
            Reverse(self.required_chunk_sets.len()), // More is better
        )
    }

    /// Remove any [`ChunkId`] in `self.required_chunks` which doesn't point to a valid chunk.
    fn strip_dangling_required_chunks(&mut self) {
        // Make a new set of trimmed chunk sets
        let mut new_required_chunk_sets = HashSet::new();
        for mut set in std::mem::take(&mut self.required_chunk_sets) {
            set.retain(|id| self.chunks.contains_key(id));
            set.sort();
            set.dedup();
            new_required_chunk_sets.insert(set);
        }
        // Commit them back after we're done
        self.required_chunk_sets = new_required_chunk_sets;
    }
}

////////////
// PASSES //
////////////

mod passes {
    use crate::graph::CallSeqVec;

    use super::*;
    use bellframe::RowBuf;
    use itertools::Itertools;
    use std::collections::HashSet;

    /* Simple passes */

    /// Removes dangling references from the [`Graph`]
    pub(super) fn remove_dangling_refs(graph: &mut Graph) {
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
        // Strip dangling links in required chunks
        graph.strip_dangling_required_chunks();
    }

    /// Creates a [`Pass`] which removes any links between two chunks which are mutually false.
    pub(super) fn remove_links_between_false_chunks(graph: &mut Graph) {
        graph.retain_internal_links(|_link, _id_from, chunk_from, id_to, _chunk_to| {
            !chunk_from.false_chunks.contains(id_to)
        })
    }

    pub(super) fn remove_chunks_with_long_method_counts(graph: &mut Graph, ranges: &RefinedRanges) {
        graph
            .chunks
            .retain(|id, chunk| chunk.total_length <= *ranges.method_counts[id.method].end())
    }

    pub(super) fn remove_links_with_long_method_counts(graph: &mut Graph, ranges: &RefinedRanges) {
        graph.retain_internal_links(|_link, id_from, chunk_from, id_to, chunk_to| {
            if id_from.method == id_to.method {
                let max_method_count = *ranges.method_counts[id_from.method].end();
                chunk_from.total_length + chunk_to.total_length <= max_method_count
            } else {
                true
            }
        })
    }

    pub(super) fn remove_chunks_which_exceed_music_limits(graph: &mut Graph, params: &Parameters) {
        graph.chunks.retain(|_id, chunk| {
            // Determine whether this chunk's music counts exceed the limit specified in
            // the parameters
            for (idx, music_type) in params.music_types.iter().enumerate() {
                if let Some(limit) = music_type.count_range.max {
                    let counts = chunk.music_counts.as_slice()[idx];
                    if music_type.masked_total(counts) > limit {
                        return false; // Chunk contributes too much of this music
                    }
                }
            }
            true
        });
    }

    /* Distance related passes */

    /// Recomputes the distance to/from rounds for every chunk, and removes any chunks which can't
    /// reach rounds in either direction.
    pub(super) fn compute_distances(mut view: DirectionalView, params: &Parameters) {
        let expanded_chunk_distances = super::compute_distances(
            view.starts().iter().map(|(_, chunk_id)| chunk_id),
            &view,
            Some(params.max_length()),
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
    }

    /// Removes any chunks which can't be included in a short enough composition.
    pub(super) fn strip_long_chunks(graph: &mut Graph, params: &Parameters) {
        graph.chunks.retain(|_id, chunk| {
            let min_comp_length_with_chunk =
                chunk.lb_distance_from_rounds + chunk.total_length + chunk.lb_distance_to_rounds;
            min_comp_length_with_chunk <= params.max_length()
        });
    }

    /* Passes relating to required chunks */

    /// Checks for a single start/end chunk and marks that chunk as required
    /// (because all compositions must start or end at that chunk).
    pub(super) fn mark_start_or_ends_as_required(view: DirectionalView) {
        let start_chunks = view
            .starts()
            .iter()
            .map(|(_link_id, chunk_id)| chunk_id.to_owned())
            .collect_vec();
        view.graph.required_chunk_sets.insert(start_chunks);
    }

    /// A [`Pass`] which removes any chunks which are false against a chunk marked as required
    pub(super) fn remove_chunks_false_against_required(graph: &mut Graph) {
        // Make sure that all links in required_chunks lead to valid chunks
        graph.strip_dangling_required_chunks();

        let mut chunk_ids_to_remove: HashSet<ChunkId> = HashSet::new();
        for required_chunk_set in &graph.required_chunk_sets {
            if required_chunk_set.is_empty() {
                continue; // If a required chunk set ends up empty, no comps are possible
            }

            // Put the false chunks of each chunk into a set
            let mut false_chunks_per_chunk_in_set = Vec::<HashSet<&ChunkId>>::new();
            for required_id in required_chunk_set {
                let false_chunks = graph.chunks[required_id]
                    .false_chunks
                    .iter()
                    .filter(|false_id| *false_id != required_id)
                    .collect();
                false_chunks_per_chunk_in_set.push(false_chunks);
            }
            // Put the small sets first, to make us reject chunk IDs more quickly
            false_chunks_per_chunk_in_set.sort_by_key(|k| k.len());

            'chunk_loop: for id in graph.chunks.keys() {
                for false_set in &false_chunks_per_chunk_in_set {
                    if !false_set.contains(id) {
                        // This chunk isn't false against one of the chunks in the set, so don't
                        // remove it
                        continue 'chunk_loop;
                    }
                }
                // If this chunk is false against all chunks in this group, remove it
                chunk_ids_to_remove.insert(id.clone());
            }
        }

        // Remove any chunks we wanted to remove
        graph
            .chunks
            .retain(|id, _chunk| !chunk_ids_to_remove.contains(id));
    }

    /* Misc passes */

    pub(super) fn remove_links_conflicting_with_calling(
        view: DirectionalView,
        params: &Parameters,
    ) {
        if !(params.require_truth && params.calling.is_some() && !params.omit_round_blocks) {
            return;
        }

        // Collects which rows can go after each call in the sequences
        let mut rows_after_each_call: CallSeqVec<HashSet<LinkSide<RowBuf>>> =
            index_vec::index_vec![HashSet::new(); view.graph.call_sequence_length];
        for (_id, link_view) in view.links() {
            if let Some(call_idx) = link_view.link.call_sequence_idx {
                let row_after_call = link_view.to().map_ref(|id| params.chunk_head(id));
                rows_after_each_call[call_idx].insert(row_after_call);
            }
        }

        // TODO: Make a nice error message if two calls in the seq end on the same row

        let call_goes_to_only_one_row: CallSeqVec<bool> = rows_after_each_call
            .iter()
            .map(|set| set.len() == 1)
            .collect();

        // For each chunk which has some
        let mut links_to_remove = HashSet::<LinkId>::new();
        for (_chunk_id, chunk) in view.chunks() {
            let can_only_be_reached_through_call =
                chunk.predecessors().into_iter().any(|(_id, link_view)| {
                    let Some(call_idx) = link_view.link.call_sequence_idx else {
                        return false;
                    };
                    call_goes_to_only_one_row[call_idx]
                });

            if can_only_be_reached_through_call {
                // If this chunk can only be reached through a call, then any plain links into
                // this chunk just can't be rung because they would cause the row after that call
                // to be repeated
                for (pred_id, pred_view) in chunk.predecessors() {
                    if pred_view.link.call_sequence_idx.is_none() {
                        links_to_remove.insert(pred_id);
                    }
                }
            }
        }

        view.graph
            .links
            .retain(|link_id, _link| !links_to_remove.contains(&link_id));
    }
}

////////////////////////////
// DIRECTIONAL PASS UTILS //
////////////////////////////

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

// TODO: Do we need this?
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

    fn links(&'graph self) -> Vec<(LinkId, LinkView<'graph>)> {
        convert_links(self.graph, self.direction, self.graph.links.keys())
    }

    #[allow(dead_code)]
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

    fn successors(&'graph self) -> Vec<(LinkId, LinkView<'graph>)> {
        convert_links(
            self.graph,
            self.direction,
            match self.direction {
                Forward => &self.chunk.successors,
                Backward => &self.chunk.predecessors,
            },
        )
    }

    fn predecessors(&'graph self) -> Vec<(LinkId, LinkView<'graph>)> {
        convert_links(
            self.graph,
            self.direction,
            match self.direction {
                Forward => &self.chunk.predecessors,
                Backward => &self.chunk.successors,
            },
        )
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

fn convert_links<'graph>(
    graph: &'graph Graph,
    direction: Direction,
    links: impl IntoIterator<Item = &'graph LinkId>,
) -> Vec<(LinkId, LinkView<'graph>)> {
    links
        .into_iter()
        .filter_map(move |&link_id| {
            Some((
                link_id,
                LinkView {
                    link: graph.links.get(link_id)?,
                    direction,
                },
            ))
        })
        .collect_vec()
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
        for (_id, succ_link) in chunk_view.successors() {
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
