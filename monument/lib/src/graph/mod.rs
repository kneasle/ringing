//! Creation and manipulation of composition graphs.  This implements routines for creating and
//! optimising such graphs, in preparation for performing tree search.

mod build;
mod falseness;
pub mod optimise;

pub use build::BuildError;

use std::{
    cmp::{Ordering, Reverse},
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    ops::{Deref, Index},
    sync::{Arc, Mutex},
    time::Instant,
};

use bellframe::{Row, Truth};

use crate::{
    music::{Breakdown, Score},
    utils::{Boundary, Counts, Rotation},
    CallIdx, Config, MethodIdx, Query,
};

use self::optimise::Pass;

/// The number of rows required to get from a point in the graph to a start/end.
type Distance = usize;

/// A 'prototype' chunk graph that is (relatively) inefficient to traverse but easy to modify.  This
/// is usually used to build and optimise the chunk graph before being converted into an efficient
/// graph representation for use in tree search.
#[derive(Debug, Clone)]
pub struct Graph {
    // NOTE: References between chunks don't have to be valid (i.e. they can point to a [`Chunk`]
    // that isn't actually in the graph - in this case they will be ignored or discarded during the
    // optimisation process).
    chunks: HashMap<ChunkId, Chunk>,
    links: LinkSet,

    // TODO: Can we get away without having these?
    /// Lookup table for the [`Link`]s which can start a composition, along with the [`ChunkId`]s
    /// that they lead to
    ///
    /// **Invariant**: for each `(link_id, chunk_id)` in `self.starts`:
    /// - `self.links[link_id].from == LinkSide::StartOrEnd`
    /// - `self.links[link_id].to == LinkSide::Chunk(chunk_id)`
    starts: Vec<(LinkId, ChunkId)>,
    /// Lookup table for the [`Link`]s which can end a composition, along with the [`ChunkId`]s
    /// which lead to them
    ///
    /// **Invariant**: for each `(link_id, chunk_id)` in `self.ends`:
    /// - `self.links[link_id].from == LinkSide::Chunk(chunk_id)`
    /// - `self.links[link_id].to == LinkSide::StartOrEnd`
    ends: Vec<(LinkId, ChunkId)>,

    /// The number of different parts
    num_parts: Rotation,
}

/// A `Chunk` in a chunk [`Graph`].  This is an indivisible chunk of ringing which cannot be split
/// up by calls or splices.
#[derive(Debug, Clone)]
pub struct Chunk {
    /// The string that should be added when this chunk is generated
    label: String,

    predecessors: Vec<LinkId>,
    successors: Vec<LinkId>,

    /// The chunks which share rows with `self`, including `self` (because all chunks are false
    /// against themselves).  Optimisation passes probably shouldn't mess with falseness.
    false_chunks: Vec<ChunkId>,

    /// The number of rows in the range covered by this chunk (i.e. its length in one part of the
    /// composition)
    per_part_length: PerPartLength,
    /// The number of rows that this this chunk adds to the composition (its total length across all
    /// parts).  Optimisation passes can't change this
    total_length: TotalLength,
    /// The number of rows of each method generated by this chunk
    method_counts: Counts,
    /// The music generated by this chunk in the composition.  Optimisation passes can't change this
    music: Breakdown,

    /// `true` if this chunk doesn't contain music that's interesting to the composition
    duffer: bool,
    /// A lower bound on the number of rows required to go from any non-duffer chunk to the first
    /// row of `self`
    pub lb_distance_from_non_duffer: usize,
    /// A lower bound on the number of rows required to go from the first row **after** `self` to
    /// any non-duffer chunk.
    pub lb_distance_to_non_duffer: usize,

    /* MUTABLE STATE FOR OPTIMISATION PASSES */
    /// Does this chunk need to be included in every composition in this search?
    pub required: bool,
    /// A lower bound on the number of rows required to go from any rounds to the first row of
    /// `self`
    pub lb_distance_from_rounds: Distance,
    /// A lower bound on the number of rows required to go from the first row **after** `self` to
    /// rounds.
    pub lb_distance_to_rounds: Distance,
}

/// A link between two [`Chunk`]s in a [`Graph`]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Link {
    pub from: LinkSide<ChunkId>,
    pub to: LinkSide<ChunkId>,
    /// Indexes into [`Query::calls`]
    pub call: Option<CallIdx>,
    pub rotation: Rotation,
    pub rotation_back: Rotation,
}

/// What a `Link` points to.  This is either a [`StartOrEnd`](Self::StartOrEnd), or a specific
/// [`Chunk`](Self::Chunk).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkSide<Id> {
    StartOrEnd,
    Chunk(Id),
}

// ------------------------------------------------------------------------------------------

impl Graph {
    /// Generate a graph of all chunks which are reachable within a given length constraint.
    // TODO: Move this wholesale into the `build` submodule
    pub fn new(query: &Query, config: &Config) -> Result<Self, BuildError> {
        build::build(query, config)
    }
}

impl<Id> LinkSide<Id> {
    pub fn is_chunk(&self) -> bool {
        matches!(self, Self::Chunk(_))
    }

    pub fn is_start_or_end(&self) -> bool {
        matches!(self, Self::StartOrEnd)
    }

    pub fn chunk(&self) -> Option<&Id> {
        match self {
            Self::Chunk(c) => Some(c),
            Self::StartOrEnd => None,
        }
    }

    pub fn map<U, F: Fn(&Id) -> U>(&self, f: F) -> LinkSide<U> {
        match self {
            Self::StartOrEnd => LinkSide::StartOrEnd,
            Self::Chunk(t) => LinkSide::Chunk(f(t)),
        }
    }
}

// ------------------------------------------------------------------------------------------

/// # Optimisation
impl Graph {
    /// Repeatedly apply a sequence of [`Pass`]es until the graph stops getting smaller, or 20
    /// iterations are made.  Use [`Graph::optimise_with_iter_limit`] to set a custom iteration
    /// limit.
    pub fn optimise(&mut self, passes: &[Mutex<Pass>], query: &Query) {
        self.optimise_with_iter_limit(passes, query, 20);
    }

    /// Repeatedly apply a sequence of [`Pass`]es until the graph either becomes static, or `limit`
    /// many iterations are performed.
    pub fn optimise_with_iter_limit(
        &mut self,
        passes: &[Mutex<Pass>],
        query: &Query,
        limit: usize,
    ) {
        log::debug!("Optimising graph:");
        let mut last_size = self.size();
        log::debug!("  Initial graph size: {:?}", last_size);
        let mut iter_count = 0;
        let start_time = Instant::now();
        loop {
            // Run every optimisation pass
            for p in passes {
                // TODO: Find a better locking system, or remove the `FnMut` bound so that locking
                // is unnecessary.  I think that this system can deadlock if multiple threads are
                // optimising graphs in parallel using the same set of passes.
                p.lock().unwrap().run(self, query);
            }
            // Stop optimising if the limit has been reached
            if iter_count > limit {
                log::warn!(
                    "Graph optimisation limit reached, but more progress could have been made."
                );
                break;
            }
            iter_count += 1;
            // Stop optimising if the graph has stopped getting smaller
            let new_size = self.size();
            log::debug!("  New size: {:?}", new_size);
            match new_size.cmp(&last_size) {
                // If graph got smaller, then keep optimising in case more optimisation is possible
                Ordering::Less => {}
                // If the last optimisation pass couldn't make `self` smaller, then assume no
                // changes has been made and further optimisation is impossible
                Ordering::Equal => break,
                // Optimisation passes shouldn't increase the graph size
                Ordering::Greater => unreachable!("Optimisation should never increase graph size"),
            }
            last_size = new_size;
        }
        log::debug!(
            "Finished optimisation in {:?} after {} iters of every pass",
            start_time.elapsed(),
            iter_count
        );
        log::debug!(
            "Optimised graph has {} chunks, {} starts, {} ends",
            self.chunk_map().len(),
            self.starts().len(),
            self.starts().len()
        );
    }

    pub fn num_parts(&self) -> Rotation {
        self.num_parts
    }

    /// Return a value representing the 'size' of this graph.  Optimisation passes are
    /// **required** to never increase this quantity.  Graph size is compared on the following
    /// factors (in order of precedence, most important first):
    /// 1. Number of chunks (smaller is better)
    /// 2. Number of links (smaller is better)
    /// 3. Number of required chunks (more is better)
    pub fn size(&self) -> (usize, usize, Reverse<usize>) {
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

// ------------------------------------------------------------------------------------------

/// # Helpers for optimisation passes
impl Graph {
    /// Removes all chunks for whom `pred` returns `false`
    pub fn retain_chunks(&mut self, pred: impl FnMut(&ChunkId, &mut Chunk) -> bool) {
        self.chunks.retain(pred);
    }

    /// Removes all internal (i.e. [`Chunk`] to [`Chunk`]) links for whom `pred` returns `false`.
    pub fn retain_internal_links(
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

    pub fn remove_dangling_links(&mut self) {
        self.links.retain(|_link_id, link| {
            if let LinkSide::Chunk(from_id) = &link.from {
                if !self.chunks.contains_key(from_id) {
                    return false;
                }
            }
            if let LinkSide::Chunk(to_id) = &link.to {
                if !self.chunks.contains_key(to_id) {
                    return false;
                }
            }
            true // Both from/to are non-dangling
        });
    }

    pub fn remove_dangling_starts(&mut self) {
        self.remove_dangling_boundary_refs(Boundary::Start);
    }

    pub fn remove_dangling_ends(&mut self) {
        self.remove_dangling_boundary_refs(Boundary::End);
    }

    fn remove_dangling_boundary_refs(&mut self, boundary: Boundary) {
        let link_id_vec = match boundary {
            Boundary::Start => &mut self.starts,
            Boundary::End => &mut self.ends,
        };
        link_id_vec.retain(|(_link_id, chunk_id)| self.chunks.contains_key(chunk_id));
    }
}

impl Chunk {
    //! Helpers for optimisation passes

    /// Returns the mutual truth of `self` against the chunk with id of `other`
    pub fn truth_against(&self, other: &ChunkId) -> Truth {
        // Chunks are mutually *true* if `other` *isn't* included in `self.false_chunks`
        Truth::from(!self.false_chunks.contains(other))
    }

    /// A lower bound on the length of a composition which passes through this chunk.
    pub fn min_comp_length(&self) -> usize {
        self.lb_distance_from_rounds + self.total_length() + self.lb_distance_to_rounds
    }

    /// A lower bound on the length of the run of duffers which passes through this chunk.
    pub fn min_duffer_length(&self) -> usize {
        if self.duffer {
            0 // Make sure that non-duffers are never pruned
        } else {
            self.lb_distance_from_non_duffer + self.total_length() + self.lb_distance_to_non_duffer
        }
    }
}

// ------------------------------------------------------------------------------------------

impl Graph {
    //! Getters & Iterators

    // Getters

    pub fn get_link(&self, id: LinkId) -> Option<&Link> {
        self.links.get(id)
    }

    pub fn get_link_mut(&mut self, id: LinkId) -> Option<&mut Link> {
        self.links.get_mut(id)
    }

    pub fn get_chunk<'graph>(&'graph self, id: &ChunkId) -> Option<&'graph Chunk> {
        self.chunks.get(id)
    }

    pub fn get_chunk_mut<'graph>(&'graph mut self, id: &ChunkId) -> Option<&'graph mut Chunk> {
        self.chunks.get_mut(id)
    }

    pub fn starts(&self) -> &[(LinkId, ChunkId)] {
        &self.starts
    }

    pub fn ends(&self) -> &[(LinkId, ChunkId)] {
        &self.ends
    }

    pub fn chunk_map(&self) -> &HashMap<ChunkId, Chunk> {
        &self.chunks
    }

    // Iterators

    /// An [`Iterator`] over the [`ChunkId`] of every [`Chunk`] in this `Graph`
    pub fn ids(&self) -> impl Iterator<Item = &ChunkId> {
        self.chunks.keys()
    }

    /// An [`Iterator`] over every [`Chunk`] in this `Graph` (including its [`ChunkId`])
    pub fn chunks(&self) -> impl Iterator<Item = (&ChunkId, &Chunk)> {
        self.chunks.iter()
    }

    /// An [`Iterator`] over every [`Chunk`] in this `Graph`, without its [`ChunkId`].
    pub fn just_chunks(&self) -> impl Iterator<Item = &Chunk> {
        self.chunks.values()
    }

    /// A mutable [`Iterator`] over the [`ChunkId`] of every [`Chunk`] in this `Graph`
    pub fn chunks_mut(&mut self) -> impl Iterator<Item = (&ChunkId, &mut Chunk)> {
        self.chunks.iter_mut()
    }
}

impl Chunk {
    //! Getters & Iterators

    pub fn total_length(&self) -> usize {
        self.total_length.0
    }

    pub fn per_part_length(&self) -> usize {
        self.per_part_length.0
    }

    pub fn method_counts(&self) -> &Counts {
        &self.method_counts
    }

    pub fn score(&self) -> Score {
        self.music.score
    }

    pub fn label(&self) -> &str {
        self.label.as_str()
    }

    pub fn music(&self) -> &Breakdown {
        &self.music
    }

    pub fn duffer(&self) -> bool {
        self.duffer
    }

    // CROSS-CHUNK REFERENCES //

    pub fn successors(&self) -> &[LinkId] {
        self.successors.as_slice()
    }

    pub fn successors_mut(&mut self) -> &mut Vec<LinkId> {
        &mut self.successors
    }

    pub fn predecessors(&self) -> &[LinkId] {
        self.predecessors.as_slice()
    }

    pub fn predecessors_mut(&mut self) -> &mut Vec<LinkId> {
        &mut self.predecessors
    }

    pub fn false_chunks(&self) -> &[ChunkId] {
        self.false_chunks.as_slice()
    }

    pub fn false_chunks_mut(&mut self) -> &mut Vec<ChunkId> {
        &mut self.false_chunks
    }
}

////////////////////////
// UTILITY DATA TYPES //
////////////////////////

/// The unique identifier of a [`Chunk`] within a given [`Graph`].
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct ChunkId {
    pub lead_head: Arc<Row>, // `Arc` is used to make cloning cheaper
    pub row_idx: RowIdx,
}

impl ChunkId {
    pub fn new(lead_head: Arc<Row>, row_idx: RowIdx) -> Self {
        Self { lead_head, row_idx }
    }
}

impl Deref for ChunkId {
    type Target = RowIdx;

    fn deref(&self) -> &Self::Target {
        &self.row_idx
    }
}

impl Debug for ChunkId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ChunkId({})", self)
    }
}

impl Display for ChunkId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{:?}:{}",
            self.lead_head, self.method, self.sub_lead_idx,
        )?;
        Ok(())
    }
}

/// The unique index of a [`Row`] within a lead.
// TODO: Merge this into `ChunkId`?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowIdx {
    pub method: MethodIdx,
    pub sub_lead_idx: usize,
}

impl RowIdx {
    pub fn new(method_idx: MethodIdx, sub_lead_idx: usize) -> Self {
        Self {
            method: method_idx,
            sub_lead_idx,
        }
    }
}

/// The length of a [`Chunk`] **in one part**.  This and [`TotalLength`] allow the compiler to
/// disallow mixing up the different definitions of 'length'.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PerPartLength(pub usize);

/// The combined length of a [`Chunk`] **in all parts**.  This and [`PerPartLength`] allow the
/// compiler to disallow mixing up the different definitions of 'length'.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TotalLength(pub usize);

//////////////
// LINK SET //
//////////////

/// Unique identifier for a [`Link`] within a [`Graph`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LinkId(usize);

/// A [`HashMap`] containing a set of [`Link`]s, all addressed by unique [`LinkId`]s
#[derive(Debug, Clone, Default)]
pub struct LinkSet {
    next_id: usize,
    map: HashMap<LinkId, Link>,
}

impl LinkSet {
    /// Create a [`LinkSet`] containing no [`Link`]s
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new [`Link`] to this set, returning its [`LinkId`]
    pub fn add(&mut self, link: Link) -> LinkId {
        let id = self.next_id();
        self.map.insert(id, link);
        id
    }

    pub fn get(&self, id: LinkId) -> Option<&Link> {
        self.map.get(&id)
    }

    pub fn get_mut(&mut self, id: LinkId) -> Option<&mut Link> {
        self.map.get_mut(&id)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<LinkId, Link> {
        self.map.iter()
    }

    pub fn keys(&self) -> std::iter::Copied<std::collections::hash_map::Keys<LinkId, Link>> {
        self.map.keys().copied()
    }

    pub fn values(&self) -> std::collections::hash_map::Values<LinkId, Link> {
        self.map.values()
    }

    /// Remove any [`Link`]s from `self` which don't satisfy a given predicate
    pub fn retain(&mut self, mut pred: impl FnMut(LinkId, &mut Link) -> bool) {
        self.map.retain(|id, link| pred(*id, link))
    }

    /// Get a new [`LinkId`], unique from all the others returned from `self`
    fn next_id(&mut self) -> LinkId {
        let id = LinkId(self.next_id);
        self.next_id += 1;
        id
    }
}

impl Index<LinkId> for LinkSet {
    type Output = Link;

    #[track_caller]
    fn index(&self, index: LinkId) -> &Self::Output {
        &self.map[&index]
    }
}
