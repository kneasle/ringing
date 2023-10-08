//! Efficiently compute falseness between [`Chunk`]s in a [`Graph`](crate::graph::Graph).

// This algorithm is fiddly, and I think that the somewhat verbose type hints are the best way to
// show how the code works (both to the reader and the compiler)
#![allow(clippy::type_complexity)]

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter, Write},
    time::Instant,
};

use bellframe::{Mask, Row, RowBuf, SameStageVec, Truth};
use itertools::Itertools;

use super::{ChunkEquivalenceMap, ChunkIdInFirstPart};
use crate::{
    graph::{Chunk, ChunkId, PerPartLength, RowIdx},
    parameters::Parameters,
};

/// Set the falseness links for some [`Chunk`]s, removing any which are false against themselves.
///
/// This is the *only* item exported by this module, so the entire module should be considered
/// implementation detail of this function.
pub(super) fn set_links(
    chunks: &mut HashMap<ChunkId, Chunk>,
    chunk_equiv_map: &mut ChunkEquivalenceMap,
    params: &Parameters,
) {
    let start = Instant::now();
    let chunk_ids_and_lengths = chunks
        .iter()
        .map(|(id, chunk)| (id.clone(), chunk.per_part_length))
        .collect::<HashSet<_>>();
    let falseness_table = FalsenessTable::new(&chunk_ids_and_lengths, params);
    log::debug!("  Falseness table built in {:.2?}", start.elapsed());

    let start = Instant::now();
    chunks.retain(|id, chunk| {
        falseness_table
            .set_falseness_links(
                id,
                chunk.per_part_length,
                &mut chunk.false_chunks,
                chunk_equiv_map,
                &chunk_ids_and_lengths,
            )
            .is_true() // Remove any chunks which are self-false
    });
    log::debug!("  Falseness links set in {:.2?}", start.elapsed());
}

/// A pre-computed table used to quickly determine the falseness in an entire
/// [`Graph`](crate::Graph).
///
/// Naively iterating through every pair of chunks is far too slow, so this instead stores a set of
/// false lead head transpositions between the different chunk types.  This way, once the table is
/// built, the computing the falseness of a chunk is one [`HashMap`] lookup and some row
/// transpositions.  Building the `FalsenessTable` is still quadratic, but it's quadratic in the
/// number of _unique chunk ranges_, which is often orders of magnitude smaller than the total
/// number of chunks.
#[derive(Debug, Clone)]
struct FalsenessTable {
    /// For each [`ChunkRange`], list the false [`ChunkRange`]s and the lead head transpositions
    /// that make them false.  Intuitively, these transposition tables are very similar to false
    /// course head tables for methods - they encode every possible falseness in an efficient way.
    ///
    /// More precisely, `falseness[r1]` contains `(r2, ch)` iff `r2` of course `ch` is false
    /// against `r1` of the plain course.
    falseness_entries: HashMap<ChunkRange, FalsenessEntry>,
}

#[derive(Debug, Clone)]
enum FalsenessEntry {
    /// Every instance of this [`ChunkRange`] is false against itself.  Any instance of this chunk
    /// range will be removed from the graph.
    SelfFalse,
    /// This [`ChunkRange`] isn't self-false, but this range of the plain course is false against
    /// `r` of course `ch` for every `(r, ch)` in the [`Vec`].
    // PERF: store transpositions in a single `SameStageVec`s for cache efficiency
    FalseCourseHeads(Vec<(Mask, Vec<(ChunkRange, RowBuf)>)>),
}

impl FalsenessTable {
    /// Creates a `FalsenessTable` capable of efficiently generating falseness between a given set
    /// of chunks.
    fn new(chunks: &HashSet<(ChunkId, PerPartLength)>, params: &Parameters) -> Self {
        // Determine which (lead head mask, range) pairs are **actually** used in the graph.  We
        // will produce a 'FCH' tables for every one of these, which will be used as lookups when
        // generating false links.
        let mut masks_used = HashSet::<(ChunkRange, Mask)>::new();
        for (method_idx, method_data) in params.methods.iter_enumerated() {
            for lead_head_mask in method_data.allowed_lead_masks(params) {
                for (id, len) in chunks {
                    if id.method == method_idx && lead_head_mask.matches(&id.lead_head) {
                        masks_used
                            .insert((ChunkRange::new(id.row_idx, *len), lead_head_mask.clone()));
                    }
                }
            }
        }

        // When computing falseness entries in multipart comps, we need to compute falseness
        // against the masks used **in every part** (not just the ones which are used in chunk
        // equivalence classes).  Therefore, we produce a new set of masks to be compared against
        // those in the graph.
        let mut masks_used_in_all_parts = masks_used
            .iter()
            .cartesian_product(params.part_head_group.rows())
            .map(|((range, mask), part_head)| (*range, part_head * mask))
            .collect::<HashSet<_>>();

        // For any ranges which have lots of masks, replace those masks with a single 'empty' one
        // (see doc comment of `reduce_masks` for more info)
        reduce_masks(&mut masks_used, &mut masks_used_in_all_parts, params);

        // Group rows and compute self-falseness
        let (self_false_ranges, row_groups) = group_rows(masks_used_in_all_parts, params);

        // Compute FCHs between every `(range, le_mask)` combination
        let false_chunk_transpositions =
            generate_false_chunk_transpositions(&masks_used, &row_groups);

        // Combine `self_false_ranges` and `false_chunk_transpositions` into the final
        // `FalsenessEntry`s
        let falseness_entries =
            generate_falseness_entries(self_false_ranges, false_chunk_transpositions);

        Self { falseness_entries }
    }

    /// Set the falseness links for a given [`Chunk`].  If the [`Chunk`] is false against itself in
    /// the same part (i.e. 'self-false'), then [`Truth::False`] is returned.
    // TODO: Decouple this from the `build::*` module
    fn set_falseness_links(
        &self,
        id: &ChunkId,
        length: PerPartLength,
        false_chunk_vec: &mut Vec<ChunkId>,
        chunk_equiv_map: &mut ChunkEquivalenceMap,
        chunk_ids_and_lengths: &HashSet<(ChunkId, PerPartLength)>,
    ) -> Truth {
        // Get the false chunk transpositions for this chunk's range, or return on self-falseness
        let fchs = match &self.falseness_entries[&ChunkRange::new(id.row_idx, length)] {
            FalsenessEntry::FalseCourseHeads(fchs) => fchs,
            FalsenessEntry::SelfFalse => return Truth::False,
        };

        false_chunk_vec.clear();
        // For each matching set of false ranges ...
        for (mask, false_ranges) in fchs {
            if mask.matches(&id.lead_head) {
                // ... for each chunk/LH transposition ...
                for (false_range, lead_head_transposition) in false_ranges {
                    let false_lead_head = id.lead_head.as_ref() * lead_head_transposition;
                    let false_id = ChunkIdInFirstPart {
                        lead_head: false_lead_head,
                        row_idx: false_range.start,
                    };
                    let (equiv_false_id, ph_rotation) = chunk_equiv_map.normalise(&false_id);

                    // We need to check `rotation != 0` because all chunks are trivially false
                    // against themselves (in that if a chunk is rung, then it cannot be rung again
                    // without incurring falseness).
                    if &equiv_false_id == id && !ph_rotation.is_identity() {
                        return Truth::False; // Remove chunk if it's false against itself in
                                             // another part
                    }
                    // If the chunk at `false_id` is in the graph, then it's false against `chunk`
                    let false_id_and_len = (equiv_false_id.clone(), false_range.len);
                    if chunk_ids_and_lengths.contains(&false_id_and_len) {
                        false_chunk_vec.push(equiv_false_id);
                    }
                }
            }
        }

        // If this chunk isn't false against itself in any part, it must be self-true
        Truth::True
    }
}

/// Combine the [set](HashSet) of self-false [`ChunkRange`]s with the computed
/// [`FalseTranspositions`] into a set of [`FalsenessEntry`]s which summarise all the falseness in
/// the [`Graph`].
fn generate_falseness_entries(
    self_false_ranges: HashSet<ChunkRange>,
    false_chunk_transpositions: FalseTranspositions,
) -> HashMap<ChunkRange, FalsenessEntry> {
    let mut falseness_entries = HashMap::<ChunkRange, FalsenessEntry>::new();
    for range in self_false_ranges {
        falseness_entries.insert(range, FalsenessEntry::SelfFalse);
    }
    let mut false_entries_by_range =
        HashMap::<ChunkRange, Vec<(Mask, Vec<(ChunkRange, RowBuf)>)>>::new();
    for ((range, mask), false_ranges) in false_chunk_transpositions {
        let mut false_chunks = Vec::<(ChunkRange, RowBuf)>::new();
        for ((false_range, _false_mask), false_transpositions) in false_ranges {
            for transposition in false_transpositions {
                false_chunks.push((*false_range, transposition));
            }
        }
        false_entries_by_range
            .entry(*range)
            .or_default()
            .push((mask.clone(), false_chunks));
    }
    for (range, entry) in false_entries_by_range {
        falseness_entries.insert(range, FalsenessEntry::FalseCourseHeads(entry));
    }
    falseness_entries
}

//////////////////////////////////////
// HELPER FUNCTIONS FOR TABLE BUILD //
//////////////////////////////////////

type RowGroups = HashMap<Mask, SameStageVec>;
type FalseTranspositions<'masks, 'groups> =
    HashMap<&'masks (ChunkRange, Mask), HashMap<&'groups (ChunkRange, Mask), HashSet<RowBuf>>>;

/// Optimisation: If some [`ChunkRange`]s have too many corresponding [`Mask`]s, then replace all
/// the masks with the [empty `Mask`](Mask::empty).  This often happens in e.g. cyclic spliced,
/// where all chunks are a lead long but often hundreds of lead head masks are generated.
///
/// This is worthwhile because falseness generation is quadratic in _two things_:
///  a) building the table is quadratic in the number of `(range, mask)`
///  b) setting the links is `O(|nodes| * |fch table entries|)`
///
/// Therefore if `(range, mask)`s is _really big_ but there are relatively few nodes, all of the
/// same length and `sub_lead_idx` (this often happens with e.g. cyclic Maximus), we end up with
/// the naive table generation being thousands of times slower than setting the links (I had one
/// search that took 70s for build but only 47ms for links; a difference factor of 1500x).  In
/// cases like these, making the table build faster at the cost of the link generation is a massive
/// win overall.
fn reduce_masks(
    masks_used: &mut HashSet<(ChunkRange, Mask)>,
    masks_used_in_all_parts: &mut HashSet<(ChunkRange, Mask)>,
    params: &Parameters,
) {
    let mut fixed_bell_mask = Mask::any(params.stage);
    for (bell, place) in params.fixed_bells() {
        fixed_bell_mask
            .set_bell(bell, place)
            .expect("Fixed bells shouldn't repeat");
    }

    // Determine which methods need reducing, and which `ChunkRange` they reduce to
    let masks_by_range = masks_used_in_all_parts
        .iter()
        .into_group_map_by(|(range, _masks)| *range);
    let reduced_ranges = masks_by_range
        .into_iter()
        .filter(|(_range, masks)| masks.len() > 50) // Reduce any range with too many masks
        .map(|(range, _mask)| range)
        .collect::<HashSet<_>>();

    // Do the reduction
    for mask_set in [masks_used_in_all_parts, masks_used] {
        // Remove existing masks for any `ChunkRange` which we reduced
        mask_set.retain(|(range, _mask)| !reduced_ranges.contains(range));
        // In their places, add a single mask containing only fixed bells (e.g. `1xxxxxxxxxxx` for
        // fixed-treble Maximus).
        mask_set.extend(
            reduced_ranges
                .iter()
                .map(|range| (*range, fixed_bell_mask.clone())),
        );
    }

    // Log what we've done
    if !reduced_ranges.is_empty() {
        let fmt_range = |range: &ChunkRange| -> String {
            let method = &params.methods[range.start.method];
            let mut s = method.shorthand();
            if range.start.sub_lead_idx == 0 && range.len.as_usize() == method.lead_len() {
                // Whole lead; don't add any extra annotation
            } else {
                // Not a whole lead; annotate it with the sub-lead range
                write!(
                    s,
                    "({:?}+{})",
                    range.start.sub_lead_idx,
                    range.len.as_usize()
                )
                .unwrap();
            }
            s
        };
        log::debug!(
            "Computing all false lead heads for {}",
            reduced_ranges.iter().map(fmt_range).join(", ")
        );
    }
}

/// For every `(range, mask)` pair, group the rows by the locations of the bells in the mask.  For
/// example, if the mask is `1xxxxx78` then the first few rows of Cornwall would end up something
/// like:
///  Group   ->    Row
/// 1xxxxx78 -> 12345678
/// x1xxxx87 -> 21436587
/// 1xxxxx78 -> 12346578
/// x1xxxx87 -> 21435687
///    ...         ...
///
/// These would be grouped by the LHS into:
/// 1xxxxx78 -> [12345678, 12346578]
/// x1xxxx87 -> [21436587, 21435687]
///
/// This is useful because falseness can exist between two rows **only** if their 'group' masks are
/// [compatible](Mask::is_compatible_with).  For example, any rows of the form `xx1xx8x7` can't be false
/// against a row of the form `x1xxx8x7` because the treble can't be in two places at once.  The
/// time needed to compute falseness tables is quadratic in the sizes of the rows we have to
/// cross-product together, so it is extremely worthwhile to split large groups of rows into many
/// smaller groups which can be computed independently.
///
/// In this loop, we also check for `ChunkRange`s which are 'self-false' (i.e. include some row
/// multiple times).
fn group_rows(
    masks_used_in_all_parts: HashSet<(ChunkRange, Mask)>,
    params: &Parameters,
) -> (HashSet<ChunkRange>, HashMap<(ChunkRange, Mask), RowGroups>) {
    let mut self_false_ranges = HashSet::<ChunkRange>::new();
    let mut row_groups = HashMap::<(ChunkRange, Mask), RowGroups>::new();
    'range_mask_loop: for (range, mask) in &masks_used_in_all_parts {
        let plain_course = params.methods[range.start.method].plain_course();

        // The chunks with the same `range` are either all self-false or all self-true
        if self_false_ranges.contains(range) {
            continue;
        }

        let mut rows_so_far = HashSet::<&Row>::new();
        let mut row_groups_for_this_range: RowGroups = HashMap::new();
        for offset in 0..range.len.as_usize() {
            let row_index = (range.start.sub_lead_idx + offset) % plain_course.len();
            let row = plain_course.get_row(row_index).unwrap();
            // Check for self-falseness.  I.e. if some row is repeated twice within a chunk,
            // then it's considered 'self-false' and should be removed from the graph
            if !rows_so_far.insert(row) {
                self_false_ranges.insert(*range);
                // Don't bother computing falseness against self-false chunks, because they
                // will not end up in the graph
                continue 'range_mask_loop;
            }
            // Group the new row
            let transposed_mask = mask * row;
            row_groups_for_this_range
                .entry(transposed_mask)
                .or_insert_with(|| SameStageVec::new(params.stage))
                .push(row);
        }

        row_groups.insert((*range, mask.clone()), row_groups_for_this_range);
    }

    // Sanity check that all self-false ranges don't appear in `row_groups_by_range` (there's no
    // point computing falseness for them because they can't actually appear in the graph).
    for (range, _) in row_groups.keys() {
        assert!(!self_false_ranges.contains(range));
    }

    (self_false_ranges, row_groups)
}

/// For each (range, mask) used as an equivalence mask in the composition, compute the false chunk
/// transpositions against every (range, mask) in **every part** of the composition.
///
/// Note that this is the section that causes the quadratic behaviour (created by the heavy use of
/// `cartesian_product`s).
fn generate_false_chunk_transpositions<'masks, 'groups>(
    masks_used: &'masks HashSet<(ChunkRange, Mask)>,
    row_groups: &'groups HashMap<(ChunkRange, Mask), RowGroups>,
) -> FalseTranspositions<'masks, 'groups> {
    let mut false_chunk_transpositions: FalseTranspositions = HashMap::new();
    // For every pair of `(range, mask)`s ...
    for (range_mask1, (range_mask2, row_groups2)) in masks_used.iter().cartesian_product(row_groups)
    {
        let row_groups1 = match row_groups.get(range_mask1) {
            Some(rg) => rg,
            None => continue, // Anything not in `row_groups` is self-false
        };

        let fch_entry = false_chunk_transpositions
            .entry(range_mask1)
            .or_default()
            .entry(range_mask2)
            .or_default();

        // ... for every pair of row groups within them ...
        for ((row_mask1, rows1), (row_mask2, rows2)) in
            row_groups1.iter().cartesian_product(row_groups2)
        {
            // ... if the masks are compatible ...
            if row_mask1.is_compatible_with(row_mask2) {
                // ... then falseness is possible and every pair of rows in `rows1 x rows2`
                // will generate a false course head between `i1` and `i2`
                for (row1, row2) in rows1.iter().cartesian_product(rows2) {
                    let false_course_head = Row::solve_xa_equals_b(row2, row1);
                    fch_entry.insert(false_course_head);
                }
            }
        }
    }
    false_chunk_transpositions
}

/// The range of rows covered by some [`Chunk`].
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ChunkRange {
    start: RowIdx,
    len: PerPartLength,
}

impl ChunkRange {
    fn new(start: RowIdx, len: PerPartLength) -> Self {
        Self { start, len }
    }
}

impl Debug for ChunkRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ChunkRange({:?},{}+{})",
            self.start.method, self.start.sub_lead_idx, self.len
        )
    }
}
