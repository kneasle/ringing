use std::collections::{HashMap, HashSet};

use bellframe::{Bell, Row, RowBuf};
use itertools::Itertools;

use crate::{
    fast_row::FastRow,
    music::{MusicTable, MusicType},
    Segment, SegmentID,
};

/// A table of static compiled data about a single course segment.
#[derive(Debug, Clone)]
pub struct SegmentTable {
    false_segments: Vec<(FastRow, SegmentID)>,
    length: usize,
    music: MusicTable,
}

impl SegmentTable {
    pub fn from_segments(
        segments: &[Segment],
        fixed_bells: &[Bell],
        music_types: &[MusicType],
    ) -> Vec<Self> {
        // Create segment tables with empty falseness
        let mut tables = segments
            .iter()
            .map(|s| SegmentTable {
                false_segments: Vec::new(),
                length: s.rows.len(),
                music: MusicTable::from_types(&s.rows, music_types, fixed_bells),
            })
            .collect_vec();

        // Fill in the falseness between the segments
        for (i, j, r) in inter_segment_falseness(segments, fixed_bells) {
            tables[i]
                .false_segments
                .push((FastRow::from(&*r), SegmentID::from(j)));
        }

        tables
    }
}

/// Takes a list of [`Segment`]s and a list of fixed [`Bell`]s, and computes the falseness between
/// the segments.  If (i, j, r) is in this set, then it means that range `i` of the plain course is
/// false against range `j` of the course starting with `r`.
///
/// Equivalently, if (i, j, r) is in this set then it means that the range `i` of some course `s`
/// is false against the range `j` of the course starting with `s * r`.
fn inter_segment_falseness(
    segments: &[Segment],
    fixed_bells: &[Bell],
) -> HashSet<(usize, usize, RowBuf)> {
    /* Firstly, we group the rows in each `Segment` by the locations of the fixed bells (because
     * cross-segment falseness can only occur between two rows with the fixed bells in the same
     * locations). */

    /// A mapping from fixed bell indices to the rows which contain that pattern
    type FalseMap<'a> = HashMap<Vec<usize>, Vec<&'a RowBuf>>;

    let grouped_rows: Vec<FalseMap> = segments
        .iter()
        .map(|segment| {
            let mut rows_by_fixed_bell_indices: FalseMap =
                HashMap::with_capacity(segment.rows.len());
            for r in &segment.rows {
                let fixed_bell_inds = get_bell_inds(fixed_bells, r);
                rows_by_fixed_bell_indices
                    .entry(fixed_bell_inds)
                    .or_insert_with(Vec::new)
                    .push(r);
            }
            rows_by_fixed_bell_indices
        })
        .collect();

    /* Now that we have grouped the rows, we iterate over every pair of segments and check if they
     * share rows in a specific pattern.  In this case, falseness can occur and we compute which
     * transposition maps one to the other (this is the course that's false against that plain
     * course).
     *
     * For example, suppose that segment A contains `18765432` and segment B contains `18756342`
     * and 1, 7 and 8 are fixed.  These contain the fixed bells in the same pattern (`187xxxxx`)
     * and therefore can cause falseness.  However, we want to know what Row segment B starts with
     * in order to be false against segment A of the plain course.  In this case, the corresponding
     * course is `12436578`, so the output set would contain `(A, B, 12436578)`. */

    // If (i, j, r) is in this set, then it means that range `i` of the plain course is false
    // against range `j` of the course starting with `r`.
    //
    // Equivalently, if (i, j, r) is in this set then it means that the range `i` of some course
    // `s` is false against the range `j` of the course starting with `s * r`.
    let mut falseness_map: HashSet<(usize, usize, RowBuf)> = HashSet::new();
    // Iterate over every pair of ranges to compute the relative falseness
    for ((i1, map1), (i2, map2)) in grouped_rows
        .iter()
        .enumerate()
        .cartesian_product(grouped_rows.iter().enumerate())
    {
        // If `map1` and `map2` contain entries with the same locations of the fixed bells,
        // then this will cause some transposition of them to be false
        for (fixed_bell_inds, rows1) in map1.iter() {
            if let Some(rows2) = map2.get(fixed_bell_inds) {
                for (&r1, &r2) in rows1.iter().cartesian_product(rows2.iter()) {
                    // If
                    //      `r1` from range `i1`
                    //    has the same pattern of fixed bells as
                    //      `r2` from range `i2`,
                    // then
                    //      the range `i1` of the plain course
                    //    is false against
                    //      the range `i2` of `r2.tranposition_to(r1)`
                    //
                    //  (i.e. we find `X` where `X * r2 == r1`)
                    let false_course = unsafe { r2.tranposition_to_unchecked(r1) };
                    falseness_map.insert((i1, i2, false_course));
                }
            }
        }
    }

    falseness_map
}

/// Returns the indices of a set of [`Bells`] within a given [`Row`]
fn get_bell_inds(bells: &[Bell], r: &Row) -> Vec<usize> {
    bells.iter().map(|b| r.place_of(*b).unwrap()).collect_vec()
}
