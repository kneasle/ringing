//! Utilities for computing falseness between graph chunks

// This algorithm is fiddly, and I think that the somewhat verbose type hints are the best way to
// show how the code works (both to the reader and the compiler)
#![allow(clippy::type_complexity)]

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bellframe::{Mask, Row, RowBuf, Truth};
use itertools::Itertools;

use crate::layout::{chunk_range::PerPartLength, ChunkId, Layout, RowIdx, RowRange};

/// A pre-computed table used to quickly determine the falseness in an entire
/// [`Graph`](crate::Graph).
///
/// Naively iterating through every pair of chunks is far too slow, so this instead stores a set of
/// false course heads between the different chunk types.  This way, computing the falseness of a
/// chunk is one [`HashMap`] lookup and some row transpositions.
#[derive(Debug, Clone)]
pub(super) struct FalsenessTable {
    /// For each `RowRange`, list the false `RowRange`s and their CH transpositions.  If
    /// `falseness[r1]` contains `(r2, ch)`, then `r2` of course `ch` is false against `r1` of the
    /// plain course.
    falseness: HashMap<RowRange, FalsenessEntry>,
}

#[derive(Debug, Clone)]
pub(super) enum FalsenessEntry {
    /// Every instance of this [`RowRange`] is false against itself.  Any instance of this chunk
    /// will be removed from the graph.
    SelfFalse,
    /// This [`RowRange`] isn't self-false, but this range of the plain course is false against
    /// `r` of course `ch` for every `(r, ch)` in the [`Vec`].
    FalseCourseHeads(Vec<(RowRange, RowBuf)>),
}

impl FalsenessTable {
    /// Creates a `FalsenessTable` capable of efficiently generating falseness between a given set
    /// of chunks.
    pub fn from_layout<'a>(
        layout: &Layout,
        chunks: impl IntoIterator<Item = &'a (ChunkId, PerPartLength)>,
    ) -> Self {
        // Extract (from the layout's links) which CH masks apply to each range start/end
        let (masks_by_range_start, masks_by_range_end) = ch_masks_from_links(layout);

        // For each non-self-false `RowRange`, determine which course heads are used.  We don't
        // need to compute falseness for self-false `RowRange`s, because the resulting chunks will
        // be removed from the chunk graph.
        let mut range_self_truth: HashMap<RowRange, Truth> = HashMap::new(); // Memoise self-falseness
        let mut chs_by_range: HashMap<RowRange, Vec<&Row>> = HashMap::new();
        for (id, length) in chunks {
            if let ChunkId::Standard(id) = id {
                let range = RowRange::new(id.row_idx, *length);
                let self_truth = *range_self_truth
                    .entry(range)
                    .or_insert_with(|| layout.self_truth(range));
                // If this chunk isn't self-false, then record that this CH has been used
                if self_truth == Truth::True {
                    chs_by_range
                        .entry(range)
                        .or_insert_with(Vec::new)
                        .push(&id.course_head);
                }
            }
        }

        let grouped_rows: Vec<(RowRange, Vec<(Mask, Vec<&Row>)>)> = chs_by_range
            .into_iter()
            .map(|(range, chs)| {
                // Determine which masks/range pairs are actually needed (pre-filtering the number
                // of masks is very worthwhile, because the overall time requirement for proving is
                // at least quadratic in the length of `mask_ranges`).
                let masks = get_masks_for_range(
                    range,
                    &chs,
                    &masks_by_range_start,
                    &masks_by_range_end,
                    layout,
                );
                // For every `(RowRange, CH mask)` pair, group the rows by the locations of the
                // bells in the CH mask.  For example, if the CH mask is `1xxxxx78` then the first
                // few rows of Cornwall would end up something like:
                // 1xxxxx78 -> 12345678
                // x1xxxx87 -> 21436587
                // 1xxxxx78 -> 12346578
                // x1xxxx87 -> 21435687
                //    ...         ...
                //
                // These would be grouped by the LHS into:
                // 1xxxxx78 -> [12345678, 12346578]
                // x1xxxx87 -> [21436587, 21435687]
                //
                // This is important because falseness can exist between two rows **only** if their
                // 'group' masks are compatible.  For example, any rows of the form `xx1xx8x7`
                // can't be false against a row of the form `x1xxx8x7` because the treble can't be
                // in two places at once.
                let mut row_groups = Vec::new();
                for mask in masks {
                    row_groups.extend(group_rows(layout.untransposed_rows(range), &mask));
                }

                (range, row_groups)
            })
            .collect_vec();

        // For the 'plain course' of every `RowRange`, which other `RowRange`s are false.  We use a
        // `HashSet` rather than a `Vec` to remove duplicate FCHs
        let mut fchs = HashMap::<RowRange, HashSet<(RowRange, RowBuf)>>::new();
        // For every pair of ranges ...
        for ((range1, groups1), (range2, groups2)) in
            grouped_rows.iter().cartesian_product(grouped_rows.iter())
        {
            // ... and for every pair of row groups within them ...
            for ((mask1, rows1), (mask2, rows2)) in groups1.iter().cartesian_product(groups2) {
                // ... if the masks are compatible ...
                if mask1.is_compatible_with(mask2) {
                    // ... then falseness is possible and every pair of rows in `rows1 x rows2`
                    // will generate a false course head between `i1` and `i2`
                    for (row1, row2) in rows1.iter().cartesian_product(rows2) {
                        let false_course_head = Row::solve_xa_equals_b(row2, row1).unwrap();
                        fchs.entry(*range1)
                            .or_insert_with(HashSet::new)
                            .insert((*range2, false_course_head));
                    }
                }
            }
        }
        // Convert `HashMap<RowRange,             HashSet          <(RowRange, RowBuf)>>`
        //      to `HashMap<RowRange, FalsenessEntry containing Vec<(RowRange, RowBuf)>>`
        let falseness = range_self_truth
            .into_iter()
            .map(|(range, self_truth)| {
                let entry = match self_truth {
                    Truth::True => {
                        let fch_set = fchs.remove(&range).unwrap();
                        FalsenessEntry::FalseCourseHeads(fch_set.into_iter().collect_vec())
                    }
                    Truth::False => FalsenessEntry::SelfFalse,
                };
                (range, entry)
            })
            .collect();
        Self { falseness }
    }

    /// Return the [`FalsenessEntry`] for a given [`RowRange`]
    pub fn falseness_entry(&self, range: RowRange) -> &FalsenessEntry {
        self.falseness.get(&range).unwrap()
    }
}

/// Gets which masks
fn ch_masks_from_links(
    layout: &Layout,
) -> (HashMap<RowIdx, Vec<Mask>>, HashMap<RowIdx, Vec<&Mask>>) {
    // For each range, determine which CH masks must match its courses
    let mut masks_by_end = HashSet::<(RowIdx, &Mask)>::with_capacity(layout.links.len());
    let mut masks_by_start = HashSet::<(RowIdx, Mask)>::with_capacity(layout.links.len());
    for link in &layout.links {
        masks_by_end.insert((link.from, &link.ch_mask));
        masks_by_start.insert((link.to, &link.ch_mask * link.ch_transposition.as_row()));
    }

    // Group these sets by RowIdx
    let mut grouped_masks_by_end = HashMap::<RowIdx, Vec<&Mask>>::new();
    for (idx, mask) in masks_by_end {
        grouped_masks_by_end
            .entry(idx)
            .or_insert_with(Vec::new)
            .push(mask);
    }
    let mut grouped_masks_by_start = HashMap::<RowIdx, Vec<Mask>>::new();
    for (idx, mask) in masks_by_start {
        grouped_masks_by_start
            .entry(idx)
            .or_insert_with(Vec::new)
            .push(mask);
    }

    (grouped_masks_by_start, grouped_masks_by_end)
}

/// Figure out which row masks are actually needed to encode the falseness for a given
/// [`RowRange`].  These correspond to a minimal set of [`Mask`]s which, between them, match the
/// course heads of every [`ChunkId`] in the graph.
fn get_masks_for_range<'a>(
    range: RowRange,
    chs: &[&Row],
    masks_by_range_start: &'a HashMap<RowIdx, Vec<Mask>>,
    masks_by_range_end: &HashMap<RowIdx, Vec<&'a Mask>>,
    layout: &Layout,
) -> Vec<Cow<'a, Mask>> {
    // Use the link masks to determine which CH masks this range could have
    let mut possible_masks = HashSet::<Cow<Mask>>::new();
    if let Some(start_masks) = masks_by_range_start.get(&range.start) {
        for start_mask in start_masks {
            possible_masks.insert(Cow::Borrowed(start_mask));
        }
    }
    if let Some(end) = layout.last_row_idx(range) {
        if let Some(end_masks) = masks_by_range_end.get(&end) {
            for end_mask in end_masks {
                possible_masks.insert(Cow::Borrowed(end_mask));
            }
        }
    }
    // If no masks are generated, then the `Layout` must have no links.  In this case, we simply
    // take all the possible course heads as CH masks (we'll have as many CH masks as start chunks,
    // which is fine).
    if possible_masks.is_empty() {
        possible_masks.extend(chs.iter().cloned().map(Mask::full_row).map(Cow::Owned));
    }

    // Because the speed of the falseness generation is quadratic in the number of
    // `(range, mask)` groups, it is worth removing redundant CH masks.  Also, we want the
    // most specific set of CH masks (since that will constrain the falseness as much as
    // possible).  Therefore, we remove any CH mask who's set of CHs be completely
    // covered by a set of more specific masks.  For example, `1xxxxx78` may be removed if
    // `1xxx6578` and `1xxx5678` are also valid and satisfy all the CHs.
    //
    // Note that this is also quadratic in the number of masks, but is performed only once per pair
    // of chunk **classes** whereas the falseness test is called for every pair of chunks (~10,000
    // times more calls).
    let mut unnecessary_masks = HashSet::<Cow<Mask>>::new();
    for m in &possible_masks {
        // If this mask doesn't satisfy any of the course heads in the chunk graph, then
        // there's no point determining its truth
        if !chs.iter().any(|ch| m.matches(ch)) {
            unnecessary_masks.insert(m.clone());
            continue;
        }

        // Determine which masks strictly refine this one (e.g. `1xxx6578` strictly
        // refines `1xxxxx78`).
        let refinements = possible_masks
            .iter()
            .filter(|m2| *m2 != m && m2.is_subset_of(m))
            .cloned()
            .collect_vec();
        // If there are some refined masks ...
        if !refinements.is_empty() {
            // ... then check if every CH matching `m` is also covered by at least one of
            // the refinements.  In that case, `m` adds no extra information and can safely
            // be removed from the falseness table
            let mut should_keep_m = false;
            for ch in chs {
                if m.matches(ch) && refinements.iter().all(|r| !r.matches(ch)) {
                    // If there's a course head which is matched by `self` but none of the
                    // refinements, then we can't remove `m` without losing information
                    should_keep_m = true;
                    break;
                }
            }

            if !should_keep_m {
                // If `m` doesn't need to be kept, then reject it
                unnecessary_masks.insert(m.clone());
            } else {
                // If `m` does need to be kept, then all the falseness covered by
                // `refinements` is also covered by `m`, making all the refinements
                // unnecessary.
                unnecessary_masks.extend(refinements);
            }
        }
    }

    // Filter out the unnecessary masks
    possible_masks.retain(|mask| !unnecessary_masks.contains(mask));

    // Debug assert that all the CHs are still covered by the masks
    for ch in chs {
        debug_assert!(possible_masks.iter().any(|m| m.matches(ch)));
    }

    possible_masks.into_iter().collect_vec()
}

/// Group some [`Row`]s by a given [`Mask`] transposed by each [`Row`].
fn group_rows<'r>(
    rows: impl IntoIterator<Item = &'r Row>,
    mask: &Mask,
) -> HashMap<Mask, Vec<&'r Row>> {
    let mut row_groups: HashMap<Mask, Vec<&'r Row>> = HashMap::new();
    // Use each row to transpose each course head mask to group the rows (e.g. if `1xxxxx78` is the
    // only course head mask, then `15372846` and `13275864` would both end up being grouped under
    // `1xx7x8xx`).
    for r in rows {
        let transposed_mask = mask * r;
        row_groups
            .entry(transposed_mask)
            .or_insert_with(Vec::new)
            .push(r);
    }
    // Dedup the row groups before returning, if two `Mask`s course head masks are compatible then
    // they will generate the same rows multiple times.  This doesn't technically change the
    // result, but wastes time and makes things harder to debug.
    for rows in row_groups.values_mut() {
        rows.sort();
        rows.dedup();
    }
    row_groups
}
