use std::collections::{HashMap, HashSet};

use bellframe::{Bell, Row, RowBuf};
use itertools::Itertools;

use crate::{
    graph::NodeId,
    layout::{Segment, SegmentId, SegmentLink},
    music::{MusicTable, MusicType},
};

/// A table of compiled data about a set of [`Segment`]s
#[derive(Debug, Clone)]
pub(crate) struct SegmentTable {
    pub entries: Vec<SegmentTableEntry>,
    pub start_nodes: Vec<StartEndNode>,
    pub end_nodes: Vec<StartEndNode>,
    /// Map of which `CentralNodeId`s will be truncated into end nodes.  This contains the same
    /// data as `end_nodes` but in the form of a hash table from resulting IDs to lookups.
    truncation_map: HashMap<NodeId, usize>,
}

impl SegmentTable {
    pub fn from_segments(
        segments: &[Segment],
        fixed_bells: &[Bell],
        music_types: &[MusicType],
    ) -> Self {
        /*
        // Determine which `NodeId`s can contain rounds, and split them into start and end sections
        let mut start_nodes = Vec::<StartEndNode>::new();
        let mut end_nodes = Vec::<StartEndNode>::new();

        for (seg_idx, s) in segments.iter().enumerate() {
            'row_loop: for (row_idx, r) in s.rows.iter().enumerate() {
                // Check that the fixed bells are all in their home positions in this row (by
                // testing for misplaced bells and breaking the loop in that case).
                for fixed_bell in fixed_bells {
                    if r[fixed_bell.index()] != *fixed_bell {
                        continue 'row_loop;
                    }
                }
                // If we've got here then the current row contains all the fixed bells in their
                // home positions, and thus rounds could occur in this location.  Therefore, the
                // course head which would contain rounds in this location is the inverse of the
                // current row.
                let course_head_with_rounds = r.inv();
                // Split this node at rounds into a start and end node
                start_nodes.push(StartEndNode::new(
                    &s.rows[row_idx..],
                    course_head_with_rounds.clone(),
                    seg_idx,
                    music_types,
                ));
                end_nodes.push(StartEndNode::new(
                    &s.rows[..row_idx],
                    course_head_with_rounds.clone(),
                    seg_idx,
                    music_types,
                ));
            }
        }

        // Create segment tables with empty falseness
        let mut entries = segments
            .iter()
            .map(|s| SegmentTableEntry {
                false_segments: Vec::new(),
                length: s.rows.len(),
                music: MusicTable::from_types(&s.rows, music_types, fixed_bells),
                links: s.links.clone(),
            })
            .collect_vec();

        // Fill in the falseness between the segments
        for (i, j, r) in inter_segment_falseness(segments, fixed_bells) {
            entries[i].false_segments.push((r, SegmentId::from(j)));
        }

        Self {
            entries,
            start_nodes,
            truncation_map: end_nodes
                .iter()
                .enumerate()
                .map(|(i, se_node)| (se_node.super_node.clone(), i))
                .collect(),
            end_nodes,
        }
        */
        todo!()
    }
}

/// A table of static compiled data about a single course segment.
#[derive(Debug, Clone)]
pub(crate) struct SegmentTableEntry {
    pub false_segments: Vec<(RowBuf, SegmentId)>,
    pub length: usize,
    pub music: MusicTable,
    pub links: Vec<SegmentLink>,
}

/// A table of static compiled data about a single course segment.
#[derive(Debug, Clone)]
pub(crate) struct StartEndNode {
    pub false_nodes: Vec<NodeId>,
    pub length: usize,
    pub score: f32,
    /// The ID of the [`Node`] that this is a (possibly trivial) subsection of
    pub super_node: NodeId,
}

impl StartEndNode {
    fn new(rows: &[RowBuf], course_head: RowBuf, segment_idx: usize, music: &[MusicType]) -> Self {
        // Calculate music (reusing `row_buffer`'s allocation to store the transposed row).
        let mut score = 0f32;
        let mut transposed_row = RowBuf::empty();
        for r in rows {
            course_head.mul_into_buf(r, &mut transposed_row).unwrap();

            for music_type in music {
                for regex in &music_type.regexes {
                    if regex.matches(&transposed_row) {
                        score += music_type.weight;
                    }
                }
            }
        }
        // Construct `self` and return
        Self {
            false_nodes: Vec::new(),
            length: rows.len(),
            score,
            super_node: NodeId::new(SegmentId::from(segment_idx), course_head),
        }
    }
}

/*
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
*/
