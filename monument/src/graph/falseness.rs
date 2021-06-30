//! Utilities for computing falseness between graph nodes

use std::collections::{HashMap, HashSet};

use bellframe::{Row, RowBuf};
use itertools::Itertools;

use super::NodeId;
use crate::{
    layout::{Layout, SegmentId},
    mask::Mask,
};

#[derive(Debug, Clone)]
pub(super) struct FalsenessTable {
    /// If `(i, j, r)` is in this set, then segment `i` of course `c` is false against segment `j` of
    /// course `c * r`.
    falseness: HashSet<(SegmentId, SegmentId, RowBuf)>,
}

impl FalsenessTable {
    /// Creates a new `FalsenessTable` which describes the falseness between [`Segment`]s in a
    /// given [`Layout`].
    pub fn from_layout(layout: &Layout) -> Self {
        // Group the rows in each segment by the transposed course head masks
        let grouped_rows = layout
            .segments
            .iter()
            .enumerate()
            .map(|(i, s)| {
                group_rows(
                    layout.segment_rows(SegmentId::from(i)),
                    &s.course_head_masks,
                )
            })
            .collect_vec();

        let mut falseness: HashSet<(SegmentId, SegmentId, RowBuf)> = HashSet::new();
        // For every pair of segments ...
        for ((i1, groups1), (i2, groups2)) in grouped_rows
            .iter()
            .enumerate()
            .cartesian_product(grouped_rows.iter().enumerate())
        {
            // ... and for every pair of row groups within them ...
            for ((mask1, rows1), (mask2, rows2)) in groups1.iter().cartesian_product(groups2) {
                // ... if the masks are compatible ...
                if mask1.is_compatible_with(mask2) {
                    // ... then falseness is possible and every pair of rows in `rows1 x rows2`
                    // will generate a false course head between `i1` and `i2`
                    for (row1, row2) in rows1.iter().cartesian_product(rows2) {
                        let false_course_head = Row::solve_xa_equals_b(row2, row1).unwrap();
                        falseness.insert((
                            SegmentId::from(i1),
                            SegmentId::from(i2),
                            false_course_head,
                        ));
                    }
                }
            }
        }

        Self { falseness }
    }

    /// Returns `true` if `node1` and `node2` are false against each other (slightly
    /// counterintuitive, but read the function name).
    pub fn are_false(&self, node1: &NodeId, node2: &NodeId) -> bool {
        let course_head_transposition = Row::solve_ax_equals_b(&node1.row, &node2.row).unwrap();
        self.falseness
            .contains(&(node1.seg, node2.seg, course_head_transposition))
    }
}

fn group_rows<'r>(
    rows: impl IntoIterator<Item = &'r Row>,
    masks: &[Mask],
) -> HashMap<Mask, Vec<&'r Row>> {
    let mut row_groups: HashMap<Mask, Vec<&'r Row>> = HashMap::new();
    // Use each row to transpose each course head mask to group the rows (e.g. if `1xxxxx78` is the
    // only course head mask, then `15372846` and `13275864` would both end up being grouped under
    // `1xx7x8xx`).
    for r in rows {
        for m in masks {
            let transposed_mask = m * r;
            row_groups
                .entry(transposed_mask)
                .or_insert_with(Vec::new)
                .push(r);
        }
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
