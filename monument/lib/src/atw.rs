use std::collections::{HashMap, HashSet};

use bellframe::Bell;
use datasize::DataSize;
use itertools::Itertools;

use crate::{
    graph::ChunkId,
    query::{MethodIdx, MethodVec, Query},
    utils::{div_rounding_up, lengths::PerPartLength, Score},
};

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct BitIndex(usize);
type UniqueRowCount = usize;
type Chunk = u16;

const FLAGS_PER_CHUNK: usize = Chunk::BITS as usize;

#[derive(Debug, Clone)]
pub(crate) struct AtwTable {
    atw_weight: Score,
    total_unique_row_positions: UniqueRowCount,

    /// One for every [`Chunk`] in the bitmaps representing the inclusion of [`AtwFlag`]s
    bitmap_chunk_multipliers: Vec<UniqueRowCount>,
    flag_per_bit: Vec<Option<AtwFlag>>,

    /// Maps `(bell, place bell, method)` triples onto a [`Vec`] of `(sub lead index, bit index)`.
    /// This [`Vec`] is always sorted in increasing order of sub-lead index.
    bell_place_to_bitmap_index: HashMap<(Bell, u8, MethodIdx), Vec<(usize, BitIndex)>>,
}

#[derive(Debug, Clone)]
struct AtwFlag {
    method_idx: MethodIdx,
    sub_lead_chunk_start: usize,
    sub_lead_chunk_len: PerPartLength,
    bell_place_bell_pairs: Vec<(Bell, u8)>,
}

impl AtwFlag {
    /// How many `(bell, place bell, method, sub-lead-index)` tuples are represented by this flag
    fn unique_row_positions(&self) -> usize {
        // Each of the (bell, place_bell) pairs will ring every sub-lead-index within the chunk
        self.sub_lead_chunk_len.as_usize() * self.bell_place_bell_pairs.len()
    }
}

impl AtwTable {
    pub fn new(query: &Query, chunk_lengths: &HashMap<ChunkId, PerPartLength>) -> Self {
        let working_bells = query
            .stage
            .bells()
            .filter(|b| !query.fixed_bells.iter().any(|(b1, _)| b1 == b)) // not in fixed_bells
            .collect_vec();
        // Which bells do we have to track, according to the part-head.  For example, for a
        // composition with part head `13425678`, 3 and 4 will not have their place bells
        // tracked because their positions are implied by that of the 2.  In this case, we
        // just track the 2 and multiply its score by 3.
        let part_head_cycles = query
            .part_head_group
            .bell_cycles()
            .into_iter()
            .filter(|g| g.len() > 1)
            .collect_vec();

        // Work out which sub-lead ranges are possible for each (bell, place bell, method)
        let place_bell_range_boundaries: HashMap<(Bell, u8, MethodIdx), Vec<usize>> =
            place_bell_range_boundaries(query, chunk_lengths);
        // Combine these sub-lead ranges into [`AtwFlag`]s, each of which corresponds to one bit
        // in the [`AtwBitmap`].
        let flags: Vec<AtwFlag> = range_boundaries_to_flags(
            &working_bells,
            &part_head_cycles,
            &query.methods,
            place_bell_range_boundaries,
        );

        let total_unique_row_positions =
            total_unique_row_positions(&working_bells, &query.methods, &flags);
        let (bitmap_chunk_multipliers, flag_per_bit) = split_flags_into_bitmap_chunks(flags);

        Self {
            atw_weight: query.atw_weight.unwrap_or(Score::from(0.0)),
            bell_place_to_bitmap_index: make_bell_place_to_bitmap_index(&flag_per_bit),
            total_unique_row_positions,
            flag_per_bit,
            bitmap_chunk_multipliers,
        }
    }

    /// Given the [`ChunkId`] and [`PerPartLength`] of a chunk, create an [`AtwBitmap`] which
    /// stores all of that chunk's ATW information.
    pub fn bitmap_for_chunk(
        &self,
        query: &Query,
        id: &ChunkId,
        chunk_len: PerPartLength,
    ) -> AtwBitmap {
        let mut bitmap = self.empty_bitmap();
        for (lead_head, sub_lead_range) in query.chunk_lead_regions(id, chunk_len) {
            for (place, bell) in lead_head.bell_iter().enumerate() {
                if let Some(bit_starts) =
                    self.bell_place_to_bitmap_index
                        .get(&(bell, place as u8, id.method))
                {
                    // Extract the segment of `bit_starts` which are covered by this range
                    let index_within_bit_starts = |sub_lead_idx: usize| -> usize {
                        bit_starts
                            .binary_search_by_key(&sub_lead_idx, |(sub_lead_idx, _)| *sub_lead_idx)
                            .unwrap_or_else(|x| x)
                    };
                    let start_idx = index_within_bit_starts(sub_lead_range.start);
                    let end_idx = index_within_bit_starts(sub_lead_range.end);
                    let bit_idxs = &bit_starts[start_idx..end_idx];
                    assert!(bit_idxs.len() >= 1);
                    assert_eq!(bit_idxs[0].0, sub_lead_range.start);

                    // Add each of the bits we've found to the bitmask for this chunk
                    for (_sub_lead_idx, bit_index) in bit_idxs {
                        bitmap.add_bit(*bit_index);
                    }
                }
            }
        }
        bitmap
    }

    /// Given an [`AtwBitmap`], recovers the `(method, sub-lead-range, bell, place bell)`
    /// quadruples which are marked as rung
    pub fn place_bells_rung(&self, bitmap: &AtwBitmap) -> Vec<PlaceBellRange> {
        let mut place_bell_ranges = Vec::new();
        for (bit_index, flag) in self.flag_per_bit.iter().enumerate() {
            if !bitmap.get_bit(BitIndex(bit_index)) {
                continue; // Skip any unset bits
            }

            // Get the [`PlaceBellRange`]s from the flag represented by this bit
            let flag = flag
                .as_ref()
                .expect("Every 1 in a bitmap should correspond to a flag");
            for &(bell, place_bell) in &flag.bell_place_bell_pairs {
                place_bell_ranges.push(PlaceBellRange {
                    method_idx: flag.method_idx,
                    sub_lead_idx_start: flag.sub_lead_chunk_start,
                    length: flag.sub_lead_chunk_len,
                    bell,
                    place_bell,
                })
            }
        }
        place_bell_ranges.sort();
        place_bell_ranges
    }

    pub fn atw_score(&self, bitmap: &AtwBitmap) -> Score {
        let factor = self.atw_factor(bitmap);
        self.atw_weight * factor * factor
    }

    /// Factor from `0.0..=1.0`, where `0.0` means no place bells are rung and `1.0` means the comp
    /// is ATW.
    pub fn atw_factor(&self, bitmap: &AtwBitmap) -> f32 {
        self.unique_place_bell_rows_rung(bitmap) as f32 / self.total_unique_row_positions as f32
    }

    pub fn unique_place_bell_rows_rung(&self, bitmap: &AtwBitmap) -> usize {
        self.bitmap_chunk_multipliers
            .iter()
            .zip_eq(&bitmap.chunks)
            .map(|(positions_per_bit, chunk)| *positions_per_bit * chunk.count_ones() as usize)
            .sum::<usize>()
    }

    pub fn empty_bitmap(&self) -> AtwBitmap {
        AtwBitmap {
            chunks: vec![0 as Chunk; self.bitmap_chunk_multipliers.len()],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlaceBellRange {
    pub bell: Bell,
    pub(crate) method_idx: MethodIdx, // TODO: Make pub
    pub place_bell: u8,
    pub sub_lead_idx_start: usize,
    pub(crate) length: PerPartLength, // TODO: Make pub
}

/// An opaque bitmap representing a some set of `(method, sub-lead-range, bell, place-bell)`
/// quadruples
#[derive(Debug, Clone, DataSize)]
pub(crate) struct AtwBitmap {
    chunks: Vec<Chunk>,
}

impl AtwBitmap {
    pub fn union_with(&mut self, other: &Self) {
        for (chunk, other_chunk) in self.chunks.iter_mut().zip_eq(&other.chunks) {
            *chunk |= *other_chunk;
        }
    }

    fn get_bit(&self, idx: BitIndex) -> bool {
        let (chunk_idx, mask) = Self::split_idx(idx);
        self.chunks[chunk_idx] & mask != 0
    }

    fn add_bit(&mut self, idx: BitIndex) {
        let (chunk_idx, mask) = Self::split_idx(idx);
        self.chunks[chunk_idx] |= mask;
    }

    /// Split a [`BitIndex`] into `(chunk_idx, mask_within_chunk)`
    fn split_idx(idx: BitIndex) -> (usize, Chunk) {
        let chunk_idx = idx.0 / (Chunk::BITS as usize);
        let sub_chunk_idx = idx.0 % (Chunk::BITS as usize);
        (chunk_idx, 1 << sub_chunk_idx)
    }
}

//////////////////////
// HELPER FUNCTIONS //
//////////////////////

/// Sort the flags into 16-bit chunk, where all the flags in each chunk have the same number of
/// `unique_row_positions`.  This way, the total number of positions can be cheaply computed using a
/// popcount and a multiplication.
fn split_flags_into_bitmap_chunks(flags: Vec<AtwFlag>) -> (Vec<usize>, Vec<Option<AtwFlag>>) {
    let mut chunk_multipliers = Vec::new();
    let mut flag_per_bit = Vec::new();
    let flag_groups = flags
        .into_iter()
        .into_group_map_by(AtwFlag::unique_row_positions);
    for (unique_row_positions, flags) in &flag_groups {
        // How many chunks in the bitmap are required to store all the flags with this many
        // unique row positions.
        let chunks_required = div_rounding_up(flags.len(), FLAGS_PER_CHUNK);
        // Set the mutlipliers for these chunks
        chunk_multipliers.extend(std::iter::repeat(*unique_row_positions).take(chunks_required));
        // Fill these chunk's bits with flags
        let mut flag_iter = flags.iter().fuse();
        for _ in 0..chunks_required * FLAGS_PER_CHUNK {
            flag_per_bit.push(flag_iter.next().cloned());
        }
    }
    assert_eq!(
        flag_per_bit.len(),
        chunk_multipliers.len() * FLAGS_PER_CHUNK
    );
    (chunk_multipliers, flag_per_bit)
}

fn make_bell_place_to_bitmap_index(
    flag_per_bit: &[Option<AtwFlag>],
) -> HashMap<(Bell, u8, MethodIdx), Vec<(usize, BitIndex)>> {
    let mut bell_place_to_index: HashMap<_, Vec<_>> = HashMap::new();
    for (bit_index, flag) in flag_per_bit.iter().enumerate() {
        if let Some(flag) = flag {
            for &(bell, place_bell) in &flag.bell_place_bell_pairs {
                bell_place_to_index
                    .entry((bell, place_bell, flag.method_idx))
                    .or_default()
                    .push((flag.sub_lead_chunk_start, BitIndex(bit_index)));
            }
        }
    }
    // Sort bit indices and return
    for bit_indices in bell_place_to_index.values_mut() {
        bit_indices.sort_unstable_by_key(|(sub_lead_idx, _bit_idx)| *sub_lead_idx);
    }
    bell_place_to_index
}

// Check that the right number of `(bell, place bell, method, sub-lead idx)` quadruples are
// accounted for
fn total_unique_row_positions(
    working_bells: &[Bell],
    methods: &MethodVec<crate::query::Method>,
    flags: &[AtwFlag],
) -> usize {
    let total_unique_row_positions = working_bells.len() // Working bells
        * working_bells.len() // Working place bells
        * methods.iter().map(|m| m.lead_len()).sum::<usize>();
    assert_eq!(
        flags
            .iter()
            .map(AtwFlag::unique_row_positions)
            .sum::<usize>(),
        total_unique_row_positions
    );
    total_unique_row_positions
}

/// Determine how the (bell, place bell, method, sub-lead idx) tuples can be combined into
/// individual bitflags.
fn place_bell_range_boundaries(
    query: &Query,
    chunk_lengths: &HashMap<ChunkId, PerPartLength>,
) -> HashMap<(Bell, u8, MethodIdx), Vec<usize>> {
    // For each (bell, place bell, method) triple, determine at which sub-lead indices the chunks
    // change.  Each region between these indices will be given a unique flag.
    let mut range_boundaries = HashMap::<(Bell, u8, MethodIdx), Vec<usize>>::new();
    for (chunk_id, length) in chunk_lengths {
        // Process each lead region separately, since a single chunk will often cover multiple
        // leads.  When a chunk does so, each bell could cover multiple place bells.
        for (lead_head, sub_lead_range) in query.chunk_lead_regions(chunk_id, *length) {
            for (place, bell) in lead_head.bell_iter().enumerate() {
                // Add both the start and end of chunks as possible boundaries
                range_boundaries
                    .entry((bell, place as u8, chunk_id.method))
                    .or_default()
                    .extend_from_slice(&[sub_lead_range.start, sub_lead_range.end]);
            }
        }
    }

    // Sort and deduplicate the flag boundaries
    for idxs in range_boundaries.values_mut() {
        idxs.sort_unstable();
        idxs.dedup();
    }
    range_boundaries
}

fn range_boundaries_to_flags(
    working_bells: &[Bell],
    part_head_cycles: &[Vec<Bell>],
    methods: &MethodVec<crate::query::Method>,
    range_boundaries: HashMap<(Bell, u8, MethodIdx), Vec<usize>>,
) -> Vec<AtwFlag> {
    let mut flags = Vec::new();
    for (method_idx, method) in methods.iter_enumerated() {
        let bell_place_sets = bell_place_sets(working_bells, &part_head_cycles, method);
        // Add one flag for every chunk and every (bell, place bell) pair
        for bell_place_set in &bell_place_sets {
            // It is possible for different (bell, place bell) pairs to have different lead
            // region boundaries.  To allow for all combinations, we say that if one
            // (bell, place bell) is split at some index, then *every* (bell, place bell) in this
            // set must also be split.  Thus, the set of flags is guaranteed to be able to handle
            // every possible lead range found in the graph (even if it splits that region into
            // more pieces than are strictly necessary).
            let mut range_boundaries_for_set = Vec::<usize>::new();
            for (bell, place_bell) in bell_place_set {
                let boundaries_for_this_pair = range_boundaries
                    .get(&(*bell, *place_bell, method_idx))
                    .map(Vec::as_slice)
                    .unwrap_or(&[] as &[_]);
                range_boundaries_for_set.extend_from_slice(boundaries_for_this_pair);
            }
            // Perform set union by sorting and deduping the combined list of integers
            range_boundaries_for_set.sort_unstable();
            range_boundaries_for_set.dedup();

            for (sub_lead_chunk_start, sub_lead_chunk_end) in
                range_boundaries_for_set.into_iter().tuple_windows()
            {
                flags.push(AtwFlag {
                    method_idx,
                    sub_lead_chunk_start,
                    sub_lead_chunk_len: PerPartLength::new(
                        sub_lead_chunk_end - sub_lead_chunk_start,
                    ),
                    bell_place_bell_pairs: bell_place_set.clone(),
                });
            }
        }
    }
    flags
}

/// Given a [`crate::query::Method`], determine which sets of `(bell, place bell)` pairs can be
/// combined and tracked with one flag.
fn bell_place_sets(
    working_bells: &[Bell],
    part_head_cycles: &[Vec<Bell>],
    method: &crate::query::Method,
) -> Vec<Vec<(Bell, u8)>> {
    let mut bells_left_to_track = working_bells.iter().copied().collect::<HashSet<_>>();
    let mut bell_place_sets = Vec::<Vec<(Bell, u8)>>::new();
    // Always track part head cycles.  I.e. if `a` and `b` are in the same cycle, then
    // `a` ringing a place bell implies that `b` must also ring that same place bell.
    for cycle in part_head_cycles {
        for place_bell in working_bells {
            bell_place_sets.push(
                cycle
                    .iter()
                    .map(|bell| (*bell, place_bell.index_u8()))
                    .collect_vec(),
            );
        }
        // Mark all these bells as tracked
        for bell in cycle {
            bells_left_to_track.remove(bell);
        }
    }
    // If only one course mask is specified (e.g. `1*7890...`), then all those bells can
    // tracked together.
    if method.allowed_course_masks.len() == 1 {
        // The lead head masks specify exactly which sets of place bells are visited by
        // these fixed tenors
        for lead_mask in &method.allowed_lead_masks {
            let mut bell_place_pairs = Vec::new();
            for (place, bell) in lead_mask.bells().enumerate() {
                if let Some(bell) = bell {
                    if bells_left_to_track.contains(&bell) {
                        bell_place_pairs.push((bell, place as u8));
                    }
                }
            }
            bell_place_sets.push(bell_place_pairs);
        }
        // Mark these bells as covered
        for bell in method.allowed_course_masks[0].bells().flatten() {
            bells_left_to_track.remove(&bell);
        }
    }
    // Any bells which aren't tracked as part of a group are tracked individually.
    for bell in bells_left_to_track {
        for place_bell in working_bells {
            bell_place_sets.push(vec![(bell, place_bell.index_u8())]);
        }
    }
    bell_place_sets
}
