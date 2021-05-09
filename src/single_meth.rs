use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter},
    ops::Range,
};

use itertools::Itertools;
use proj_core::{Bell, Method, Row, Stage};

use crate::engine::{self, Node};

/// A section of a course of a single method
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Section {
    ind: usize,
}

impl Section {
    const fn new(ind: usize) -> Self {
        Section { ind }
    }
}

impl Display for Section {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ind)
    }
}

impl engine::Section for Section {
    type Table = Table;

    #[inline(always)]
    fn stage(table: &Self::Table) -> Stage {
        table.stage
    }

    #[inline(always)]
    fn start() -> Self {
        Self::new(0)
    }

    #[inline(always)]
    fn is_end(node: &Node<Self>) -> bool {
        node.row.is_rounds() && node.section.ind == 0
    }

    #[inline(always)]
    fn length(self, table: &Self::Table) -> usize {
        table.lengths[self.ind]
    }

    #[inline(always)]
    fn falseness(self, table: &Self::Table) -> &[(Row, Self)] {
        table.falseness[self.ind].as_slice()
    }

    #[inline(always)]
    fn expand(self, table: &Self::Table) -> &[(String, Row, Self)] {
        table.next_nodes[self.ind].as_slice()
    }
}

/// The persistent state table for a single method
#[derive(Debug, Clone)]
pub struct Table {
    falseness: Vec<Vec<(Row, Section)>>,
    next_nodes: Vec<Vec<(String, Row, Section)>>,
    lengths: Vec<usize>,
    stage: Stage,
}

impl Table {
    pub fn new(
        method: &Method,
        ranges: &[Range<usize>],
        fixed_bells: &[Bell],
        next_nodes: &[Vec<(&str, Row, usize)>],
    ) -> Table {
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
                        .or_insert_with(Vec::new)
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
                        //      the range `i2` of `r2.tranposition_to(r1)`
                        //
                        //  (i.e. we find `X` where `X * r2 == r1`)
                        let false_course = unsafe { r2.tranposition_to_unchecked(r1) };
                        falseness_map.insert((i1, i2, false_course));
                    }
                }
            }
        }

        // Convert the hash table into a jagged 2D array, indexed by the first element of the tuple
        // (so that the lookups we want to do are faster).
        let mut final_table: Vec<Vec<(Row, Section)>> = vec![Vec::new(); ranges.len()];
        for (i, j, r) in falseness_map {
            final_table[i].push((r, Section::new(j)));
        }

        // Sort the falseness tables
        final_table
            .iter_mut()
            .for_each(|v| v.sort_by(|a, b| (a.1.ind, &a.0).cmp(&(b.1.ind, &b.0))));

        Table {
            falseness: final_table,
            lengths: ranges.iter().map(|r| r.len() * method.lead_len()).collect(),
            stage: method.stage(),
            next_nodes: next_nodes
                .iter()
                .map(|vs| {
                    vs.iter()
                        .map(|(s, r, i)| (String::from(*s), r.clone(), Section::new(*i)))
                        .collect()
                })
                .collect(),
        }
    }

    pub fn print_falseness(&self) {
        for (i, secs) in self.falseness.iter().enumerate() {
            println!("{}", i);
            for (r, sec) in secs {
                println!("   {}: {}", sec.ind, r);
            }
        }
    }
}
