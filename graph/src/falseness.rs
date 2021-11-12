//! Utilities for computing falseness between graph nodes

// This algorithm is fiddly, and I think that the somewhat verbose type hints are the best way to
// show how the code works (both to the reader and the compiler)
#![allow(clippy::type_complexity)]

use std::collections::{HashMap, HashSet};

use bellframe::{Mask, Row, RowBuf};
use itertools::Itertools;

use crate::layout::{Layout, NodeId, RowIdx, RowRange};

/// A pre-computed table used to quickly determine the falseness in an entire
/// [`Graph`](crate::Graph).
///
/// Naively iterating through every pair of nodes is far too slow, so this instead stores a set of
/// false course heads between the different node types.  This way, computing the falseness of a
/// node is one [`HashMap`] lookup and some row transpositions.
#[derive(Debug, Clone)]
pub(crate) struct FalsenessTable {
    falseness: HashMap<RowRange, Vec<(RowRange, RowBuf)>>,
}

impl FalsenessTable {
    /// Creates a new `FalsenessTable` which describes the falseness between [`Segment`]s in a
    /// given [`Layout`].
    pub fn from_layout<'a>(
        layout: &Layout,
        nodes: impl IntoIterator<Item = &'a (NodeId, usize)>,
    ) -> Self {
        // Extract (from the layout's links) which CH masks apply to each range start/end
        let (masks_by_range_start, masks_by_range_end) = ch_masks_from_links(layout);

        // For each `RowRange`, which course heads are used
        let mut chs_by_range = HashMap::<RowRange, Vec<&Row>>::new();
        for (id, length) in nodes {
            if let NodeId::Standard(id) = id {
                chs_by_range
                    .entry(RowRange::new(id.row_idx, *length))
                    .or_insert_with(Vec::new)
                    .push(&id.course_head);
            }
        }

        // Determine which masks/range pairs are actually needed (pre-filtering the number of masks
        // is very worthwhile, because the overall time requirement for proving is at least
        // quadratic in the length of `mask_ranges`).
        let mut range_masks: Vec<(RowRange, &Mask)> = Vec::new();
        for (range, chs) in chs_by_range {
            range_masks.extend(
                get_masks_for_range(
                    range,
                    &chs,
                    &masks_by_range_start,
                    &masks_by_range_end,
                    layout,
                )
                .into_iter()
                .map(|mask| (range, mask)),
            );
        }

        // For every `(RowRange, CH mask)` pair, group the rows by the locations of the bells in
        // the CH mask.  For example, if the CH mask is `1xxxxx78` then the first few rows of
        // Cornwall would end up something like:
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
        // This is important because falseness can exist between two rows **only** if their 'group'
        // masks are compatible.  For example, any rows of the form `xx1xx8x7` can't be false
        // against a row of the form `x1xxx8x7` because the treble would have to be in two
        // different places.
        let grouped_rows: Vec<(RowRange, HashMap<Mask, Vec<&Row>>)> = range_masks
            .into_iter()
            .map(|(range, mask)| {
                (
                    range,
                    group_rows(layout.untransposed_rows(range.start, range.len), mask),
                )
            })
            .collect_vec();

        // For the 'plain course' of every `RowRange`, which other `RowRange`s are false.  We use a
        // `HashSet` rather than a `Vec` to prevent duplication of FCHs
        let mut falseness = HashMap::<RowRange, HashSet<(RowRange, RowBuf)>>::new();

        // For every pair of segments ...
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
                        falseness
                            .entry(*range1)
                            .or_insert_with(HashSet::new)
                            .insert((*range2, false_course_head));
                    }
                }
            }
        }

        Self {
            // Convert `HashMap<RowRange, HashSet<(RowRange, RowBuf)>>`
            //      to `HashMap<RowRange,     Vec<(RowRange, RowBuf)>>`
            falseness: falseness
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().collect_vec()))
                .collect(),
        }
    }

    /// For a given [`RowRange`], which course head transpositions are false
    pub fn false_course_heads(&self, range: RowRange) -> &[(RowRange, RowBuf)] {
        self.falseness.get(&range).map(Vec::as_slice).unwrap_or(&[])
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
/// course heads of every [`NodeId`] in the graph.
fn get_masks_for_range<'a>(
    range: RowRange,
    chs: &[&Row],
    masks_by_range_start: &'a HashMap<RowIdx, Vec<Mask>>,
    masks_by_range_end: &HashMap<RowIdx, Vec<&'a Mask>>,
    layout: &Layout,
) -> Vec<&'a Mask> {
    // Use the link masks to determine which CH masks this range could have
    let mut possible_masks = HashSet::<&Mask>::new();
    if let Some(start_masks) = masks_by_range_start.get(&range.start) {
        for start_mask in start_masks {
            possible_masks.insert(start_mask);
        }
    }
    if let Some(end) = layout.last_row_idx(range) {
        if let Some(end_masks) = masks_by_range_end.get(&end) {
            for end_mask in end_masks {
                possible_masks.insert(end_mask);
            }
        }
    }

    // Because the speed of the falseness generation is quadratic in the number of
    // `(range, mask)` groups, it is worth removing redundant CH masks.  Also, we want the
    // most specific set of CH masks (since that will constrain the falseness as much as
    // possible).  Therefore, we remove any CH mask who's set of nodes be completely
    // covered by a set of more specific masks.  For example, `1xxxxx78` may be removed if
    // `1xxx6578` and `1xxx5678` are also valid and satisfy all the CHs.
    //
    // Note that this is also quadratic in the number of masks, but is performed only once per pair
    // of node **classes** whereas the falseness test is called for every pair of nodes (~10,000
    // times more calls).
    let mut unnecessary_masks = HashSet::<&Mask>::new();
    for m in &possible_masks {
        // If this mask doesn't satisfy any of the course heads in the node graph, then
        // there's no point determining its truth
        if !chs.iter().any(|ch| m.matches(ch)) {
            unnecessary_masks.insert(m);
            continue;
        }

        // Determine which masks strictly refine this one (e.g. `1xxx6578` strictly
        // refines `1xxxxx78`).
        let refinements = possible_masks
            .iter()
            .filter(|m2| *m2 != m && m2.is_subset_of(m))
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
                unnecessary_masks.insert(m);
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
