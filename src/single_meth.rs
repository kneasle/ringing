use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use itertools::Itertools;
use proj_core::{Bell, Method, Row};

pub struct SingleMethodTable {
    pub falseness: Vec<Vec<(usize, Row)>>,
    pub lengths: Vec<usize>,
}

impl SingleMethodTable {
    pub fn new(
        method: &Method,
        ranges: &[Range<usize>],
        fixed_bells: &[Bell],
    ) -> SingleMethodTable {
        // Generate the plain course of the given method upfront
        let plain_course = method.plain_course();
        let lead_len = method.lead_len();

        /* Group rows in each range by the locations of the fixed bells.  By the definition of
         * fixed bells, we only consider falseness between rows which have the fixed bells in the
         * same places. */
        type FalsenessMap<'a> = HashMap<Vec<usize>, Vec<&'a Row>>;

        let grouped_rows: Vec<FalsenessMap> = ranges
            .iter()
            .map(|lead_range| {
                // Convert the range of leads to a range of rows
                let row_range = (lead_range.start * lead_len)..(lead_range.end * lead_len);
                // Group all the rows by the indices of the fixed bells
                let mut rows_by_fixed_bell_indices: FalsenessMap =
                    HashMap::with_capacity(row_range.len());
                for annot_r in &plain_course.annot_rows()[row_range] {
                    let r = annot_r.row();
                    let fixed_bell_inds = fixed_bells
                        .iter()
                        .map(|b| r.place_of(*b).unwrap())
                        .collect_vec();
                    rows_by_fixed_bell_indices
                        .entry(fixed_bell_inds)
                        .or_insert(Vec::new())
                        .push(r);
                }
                // Return this grouping so it can be combined to generate the falseness table
                rows_by_fixed_bell_indices
            })
            .collect();

        /* Use these grouped rows to iterate over all pairs of ranges and use this to generate a
         * map of ranges are false against which ranges of the plain course. */

        // If (i, j, r) is in this set, then it means that range `i` of the plain course is false
        // against range `j` of the course starting with `r`.
        //
        // Equivalently, if (i, j, r) is in this set then it means that the range `i` of some course
        // `s` is false against the range `j` of the course starting with `s * r`.
        let mut falseness_map: HashSet<(usize, usize, Row)> = HashSet::new();
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
                        //      the range `i2` of `r1.transposition_to(r2)`
                        let r1_to_r2 = unsafe { r1.tranposition_to_unchecked(r2) };
                        falseness_map.insert((i1, i2, r1_to_r2));
                    }
                }
            }
        }

        // Convert the hash table into a jagged 2D array, indexed by the first element of the tuple
        // (so that the lookups we want to do are faster).
        let mut final_table: Vec<Vec<(usize, Row)>> = vec![Vec::new(); ranges.len()];
        for (i, j, r) in falseness_map {
            final_table[i].push((j, r));
        }

        SingleMethodTable {
            falseness: final_table,
            lengths: ranges.iter().map(|r| r.len() * method.lead_len()).collect(),
        }
    }
}
