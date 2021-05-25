use std::{
    collections::{HashMap, HashSet},
    iter::once,
};

use itertools::Itertools;
use proj_core::{Bell, Row, Stage};

use crate::engine::CompRow;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
#[allow(dead_code)]
pub enum RowRegexElem {
    /// One single specific [`Bell`].  Only matched by that exact [`Bell`].
    Bell(Bell),
    /// A single wild-card usually written as `x` or `.`.  Matches one of any [`Bell`].
    Wildcard,
    /// A wild-card usually written as `*`.  Matches any number of any [`Bell`]s.
    Glob,
}

impl RowRegexElem {
    /// If this is a [`RowRegexElem::Bell`], this returns the contained [`Bell`], otherwise `None`
    #[inline]
    pub fn bell(self) -> Option<Bell> {
        match self {
            Self::Bell(b) => Some(b),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MusicPattern {
    regex: Vec<RowRegexElem>,
    weight: f32,
}

impl MusicPattern {
    #[allow(dead_code)]
    pub fn from_regex(regex: &str, weight: f32) -> Self {
        MusicPattern {
            regex: regex
                .chars()
                .filter_map(|c| match c {
                    'x' | '.' => Some(RowRegexElem::Wildcard),
                    '*' => Some(RowRegexElem::Glob),
                    _ => Bell::from_name(c).map(RowRegexElem::Bell),
                })
                .collect_vec(),
            weight,
        }
    }

    pub fn runs_front_or_back(stage: Stage, len: usize, weight: f32) -> Vec<MusicPattern> {
        let num_bells = stage.as_usize();

        let mut runs = Vec::with_capacity(num_bells.saturating_sub(3) * 2);
        for i in 0..=num_bells - len {
            let run_iterator = (i..i + len).map(Bell::from_index).map(RowRegexElem::Bell);

            // Descending runs on the back
            runs.push(
                once(RowRegexElem::Glob)
                    .chain(run_iterator.clone())
                    .collect_vec(),
            );
            // Ascending runs on the back
            runs.push(
                once(RowRegexElem::Glob)
                    .chain(run_iterator.clone().rev())
                    .collect_vec(),
            );
            // Descending runs off the front
            runs.push(
                run_iterator
                    .clone()
                    .chain(once(RowRegexElem::Glob))
                    .collect_vec(),
            );
            // Ascending runs on the back
            runs.push(
                run_iterator
                    .rev()
                    .chain(once(RowRegexElem::Glob))
                    .collect_vec(),
            );
        }

        runs.into_iter()
            .map(|regex| MusicPattern { regex, weight })
            .collect_vec()
    }
}

/// The information required to quickly compute the music of a segment of some course
#[derive(Debug, Clone)]
pub struct MusicTable {
    /// Music score which is generated regardless of the course head.  This corresponds to the
    /// course head mask `xxxxxx...`.
    guarunteed_music: f32,
    /// In order to speed up music detection, we chose some place and then split our table by which
    /// bell is in that place.  This usually reduces the max table length by about 5, and thus
    /// causes ~5x speedup of music detection.
    pivot_place: usize,
    compiled_masks: Vec<Vec<(u128, u128, f32)>>,
}

impl MusicTable {
    pub fn from_rows<'r>(
        stage: Stage,
        fixed_bells: &[Bell],
        rows: impl IntoIterator<Item = &'r Row> + Clone,
        patterns: &[MusicPattern],
    ) -> Self {
        let course_head_masks = generate_course_head_masks(stage, fixed_bells, rows, patterns);
        Self::from_course_masks(stage, fixed_bells, course_head_masks)
    }

    #[inline(always)]
    pub fn evaluate(&self, row: &impl CompRow) -> f32 {
        let partition_bell = row.bell_at(self.pivot_place);

        let mut total_music = self.guarunteed_music;
        if let Some(masks) = self.compiled_masks.get(partition_bell.index()) {
            for &(mask, expected_bells, score) in masks {
                // Use the mask to overwrite every byte we don't care about with 0xff
                let masked_row = row.pack_u128() | mask;
                // Compare the masked row to the expected_bells we've set up
                if masked_row == expected_bells {
                    total_music += score;
                }
            }
        }
        total_music
    }

    /// Helper function which builds a search-ready `MusicTable` from a set of course head masks
    /// and their music scores
    fn from_course_masks(
        stage: Stage,
        fixed_bells: &[Bell],
        mut masks: HashMap<Vec<(usize, Bell)>, f32>,
    ) -> MusicTable {
        let mut guarunteed_music = masks.remove(&Vec::new()).unwrap_or(0.0);

        // TODO: Upper bound the best possible music density here

        let non_fixed_bells = (0..stage.as_usize())
            .map(Bell::from_index)
            .filter(|b| !fixed_bells.contains(b))
            .collect_vec();

        /// Mapping from bells to sets of masks which apply if that bell is in `pivot_place`
        type MaskByBell<'a> = HashMap<Bell, Vec<(&'a [(usize, Bell)], f32)>>;

        // Check each non-fixed bell's home position as a possible pivot, choosing the best one
        let (pivot_place, masks_by_bell): (usize, MaskByBell) = non_fixed_bells
            .iter()
            .map(|b| {
                let place = b.index();

                // How large the partition for each bell will be, given this pivot place
                let mut masks_by_bell: MaskByBell = HashMap::new();

                // Go through each mask, and figure out which pivot bells' partitions it belongs in
                for (mask, score) in &masks {
                    // Compute which bell is in the pivot place
                    let bell_in_pivot = mask
                        .iter()
                        .find(|(p, _)| *p == place)
                        .map(|(_, bell)| *bell);

                    match bell_in_pivot {
                        // If there is a bell in the pivot, then this mask can only exist in that
                        // bell's partition
                        Some(b) => masks_by_bell
                            .entry(b)
                            .or_insert_with(Vec::new)
                            .push((mask.as_slice(), *score)),
                        // If no bell is in the pivot, then every non-fixed bell that isn't already
                        // in the mask could go here
                        None => non_fixed_bells
                            .iter()
                            .filter(|b| mask.iter().find(|(_, b2)| *b == b2).is_none())
                            .for_each(|b| {
                                masks_by_bell
                                    .entry(*b)
                                    .or_insert_with(Vec::new)
                                    .push((mask.as_slice(), *score))
                            }),
                    }
                }

                // Assuming the bells in the pivot are generated uniformly, the average length will
                // be the expected number of equality checks required to perform this music check.
                //
                // However, if we get a tie-break between two pivots then we go for the one that's
                // the most consistent (i.e. has the smallest max value)
                let length_sum = masks_by_bell.values().map(Vec::len).sum::<usize>();
                let length_max = masks_by_bell.values().map(Vec::len).max().unwrap_or(0);
                (place, masks_by_bell, (length_sum, length_max))
            })
            .min_by_key(|(_, _, sum_max)| *sum_max)
            // Throw away the values which we used to rank the pivots
            .map(|(place, masks, _)| (place, masks))
            // Perhaps we should do something sensible - this unwrap will trigger if no music is
            // specified.  In that case, we should probably early return an empty pivot map
            .unwrap();

        println!(
            "pivoting at {}: len reduction: {} -> {}",
            pivot_place,
            masks.len(),
            masks_by_bell.values().map(|p| p.len()).sum::<usize>() as f32
                / masks_by_bell.len() as f32
        );

        let max_pivot_bell_index = masks_by_bell.keys().map(|b| b.index()).max().unwrap_or(0);
        // The `+ 1` here is necessary to make sure that the last entry in `partition_table` has
        // index `max_pivot_bell_index`
        let mut partition_table = vec![Vec::new(); max_pivot_bell_index + 1];
        // Compile all the masks and put them in the partition table
        for (bell, masks) in masks_by_bell {
            partition_table[bell.index()].extend(
                masks
                    .into_iter()
                    .map(|(bell_locs, score)| compile_mask(&bell_locs, score)),
            );
        }

        MusicTable {
            guarunteed_music,
            pivot_place,
            compiled_masks: partition_table,
        }
    }
}

/// Compile a list of bell locations into a pair of bitmasks which can be used to efficiently check
/// the mask
fn compile_mask(bell_locs: &[(usize, Bell)], score: f32) -> (u128, u128, f32) {
    // Generate the masks for their locations.
    // - `mask` has 0x00 in the locations of the bells and 0xff otherwise (it's
    //   inverted in the return expression).
    // - `bells` has each bell's byte in the locations of the bells and 0xff otherwise.
    let mut mask = 0u128;
    let mut bells = (-1i128) as u128; // Start off with all 1s
    for (index, bell) in bell_locs {
        // Fill the byte in the mask with 1s (which will be turned into 0s after the
        // bitwise not at the end)
        mask |= 0xffu128 << (index * 8);
        // Zero out the current bell byte
        bells &= !(0xffu128 << (index * 8));
        // Fill the zeroed byte with the bell index
        bells |= (bell.index() as u128) << (index * 8);
    }
    // println!("{:>16x}\n{:>32x}", !mask, bells);
    (!mask, bells, score)
}

/// Combines rows and music patterns to generate a list of course head masks and their music
/// scores.  These masks will never contain only one missing non-fixed bell (i.e. `x3254xxx` is
/// normalised to `x32546xx`).
fn generate_course_head_masks<'r>(
    stage: Stage,
    fixed_bells: &[Bell],
    rows: impl IntoIterator<Item = &'r Row> + Clone,
    patterns: &[MusicPattern],
) -> HashMap<Vec<(usize, Bell)>, f32> {
    let non_fixed_bells = (0..stage.as_usize())
        .map(Bell::from_index)
        .filter(|b| !fixed_bells.contains(b))
        .collect_vec();

    /* The first step of compiling the music table is to parse the regexes into a single list of
     * which bells are required to go where.
     *
     * The jagged list corresponds to:
     * ```
     * // One per bell position requirement
     * Vec<
     *     (
     *         // Which bells are required to go at what indices for this requirement to be satisfied
     *         Vec<(usize, Bell)>,
     *         // What weight to give this requirement
     *         f32
     *     )
     * >
     * ```
     *
     * Therefore, if this list is
     * ```
     * [
     *     ([(0, '2'), (1, '3'), (2, '4'), (3, '5')], 1.0),
     *     ([(0, '5'), (1, '4'), (2, '3'), (3, '2')], 1.0),
     * ]
     * ```
     * then this would corresponds to wanting "2345" or "5432" in the range `0..4` with a weight
     * of 1 each.
     */
    let bell_requirements: Vec<(Vec<(usize, Bell)>, f32)> = patterns
        .iter()
        .map(|m| {
            let regex_len = m.regex.len();
            let glob_locations = m
                .regex
                .iter()
                .positions(|v| *v == RowRegexElem::Glob)
                .collect_vec();
            // Sanity check that the regex doesn't specify a row longer than the stage.  Perhaps it
            // might be wiser to warn the user instead of just crashing, but hey at least we don't
            // get UB
            assert!(regex_len - glob_locations.len() <= stage.as_usize());
            // Convert the regex into a list of locations where bells are needed
            let bell_requirements = match glob_locations.len() {
                0 => {
                    // If there are no globs, then the bells must specify the entire row
                    assert_eq!(regex_len, stage.as_usize());
                    m.regex
                        .iter()
                        .enumerate()
                        .filter_map(|(i, r)| r.bell().map(|b| (i, b)))
                        .collect_vec()
                }
                1 => {
                    let glob_ind = glob_locations[0];
                    let post_glob_index = stage.as_usize() + 1 - regex_len;

                    let mut bell_requirements = Vec::with_capacity(regex_len - 1);
                    // Pre-glob bells and their required indices within the row
                    bell_requirements.extend(
                        m.regex
                            .iter()
                            .take(glob_ind)
                            .enumerate()
                            .filter_map(|(i, r)| r.bell().map(|b| (i, b))),
                    );
                    // Post-glob bells and their required indices within the row
                    bell_requirements.extend(
                        m.regex
                            .iter()
                            // Skip to just past the glob
                            .skip(glob_ind + 1)
                            .enumerate()
                            .filter_map(|(i, r)| r.bell().map(|b| (i + post_glob_index, b))),
                    );
                    // Return all the rows
                    bell_requirements
                }
                _ => {
                    panic!("Can't handle music patterns with more than one glob");
                }
            };
            // Important thing to note here is that bell_requirements is always sorted by index,
            // which is required for later algorithms to work properly
            (bell_requirements, m.weight)
        })
        .collect_vec();

    /* The second step is to convert these requirements into a hash table which maps a list of
     * indices to all the bell patterns which use those indices so that we can prevent work later
     * on.
     *
     * For example, if `[0, 1, 2, 3]` maps to `[("2345", 1.0), ("5432", 1.0)]` then it means that
     * we'd count either 2345 or 5432 in places `0..4`, each with weight 1 (and no other bell
     * pattern of indices [0..3] will generate music). */
    let mut index_requirements: HashMap<Vec<usize>, Vec<(Vec<Bell>, f32)>> = HashMap::new();
    for (bell_indices, weight) in bell_requirements {
        // Unzip the iterators
        let indices = bell_indices.iter().map(|(i, _)| *i).collect_vec();
        let bells = bell_indices.iter().map(|(_, b)| *b).collect_vec();
        index_requirements
            .entry(indices)
            .or_insert_with(Vec::new)
            .push((bells, weight));
    }

    /* Now, for each of the possible sets of indices, we check every row to determine which course
     * head masks would cause the required music.
     *
     * **The bell location lists must be sorted so that their indices are in increasing order,
     * otherwise they will be non-unique.**
     *
     * For example, if this table maps `[(1, '3'), (2, '2'), (3, '5'), (4, '4')]` to `7.0`, then it
     * means that any course head matching `x3254*` would generate a music score of `7.0` within
     * this segment. */
    let mut course_head_masks: HashMap<Vec<(usize, Bell)>, f32> = HashMap::new();
    for (indices, bell_patterns) in index_requirements {
        // Firstly, we count the number of times each bell pattern appears in the source rows.
        // Doing this upfront will prevent us redoing work each time a bell pattern appears in the
        // source.
        let mut source_pattern_counts: HashMap<Vec<Bell>, usize> = HashMap::new();
        for r in rows.clone() {
            let bell_pattern = indices.iter().map(|i| r[*i]).collect_vec();
            // Pick the bells from the right indices out of this row, and add one to the count of
            // this pattern
            *source_pattern_counts.entry(bell_pattern).or_insert(0) += 1;
        }

        // Now go through this condensed list and test each music pattern against it to see (given
        // the fixed bells) which course heads would produce music in this location
        for (source_pattern, count) in source_pattern_counts {
            'music_loop: for (music_pattern, weight) in &bell_patterns {
                // First of all, check if these patterns agree on the locations of the fixed bells.
                // If they don't then this particular musical pattern can never be generated in
                // this location.
                for fb in fixed_bells {
                    let source_index = source_pattern.iter().position(|b| b == fb);
                    let music_index = music_pattern.iter().position(|b| b == fb);
                    if source_index != music_index {
                        continue 'music_loop;
                    }
                }

                // If they do agree on the locations of the source bells, then figure out which
                // course heads we'd need in order to generate this music using the given source
                // pattern.
                let mut course_head_mask: Vec<(usize, Bell)> =
                    // Zip the source and music patterns together
                    source_pattern
                    .iter()
                    .zip(music_pattern.iter())
                    // Filter out the fixed bells since they're fixed in place
                    .filter(|(_, b)| !fixed_bells.contains(b))
                    .map(|(source_bell, music_bell)| (source_bell.index(), *music_bell))
                    .collect_vec();

                // If this mask has only one empty place, then there is only one possible bell and
                // we add it to normalise the mask.  For example, this maps `13254x78` to
                // `13254678` (filling in the 6).  This prevents accidental duplication of masks
                // which would cause the music detection to do unnecessary work.
                if course_head_mask.len() + 1 == non_fixed_bells.len() {
                    // Compute which places/bells are missing
                    let mut missing_bells: HashSet<Bell> =
                        non_fixed_bells.iter().cloned().collect();
                    let mut missing_places: HashSet<usize> =
                        non_fixed_bells.iter().map(|b| b.index()).collect();
                    for (i, b) in &course_head_mask {
                        missing_places.remove(i);
                        missing_bells.remove(b);
                    }
                    assert_eq!(missing_bells.len(), 1);
                    assert_eq!(missing_places.len(), 1);
                    let missing_bell = missing_bells.into_iter().next().unwrap();
                    let missing_place = missing_places.into_iter().next().unwrap();
                    // Add this missing pair to the course head mask
                    course_head_mask.push((missing_place, missing_bell));
                }

                // IMPORTANT: Sort this mask by the indices so that the same mask can't have two
                // different representations
                course_head_mask.sort_unstable_by_key(|(i, _bell)| *i);
                // Add the additional music score from this pattern to this course head mask (which
                // may generate music in other patterns)
                *course_head_masks.entry(course_head_mask).or_insert(0.0) +=
                    (count as f32) * weight;
            }
        }
    }

    // Return the hashtable in full (since it will be used to generate a `MusicTable`)
    course_head_masks
}

#[allow(dead_code)]
fn format_mask(stage: Stage, mask: &[(usize, Bell)]) -> String {
    let mut row = vec![String::from("x"); stage.as_usize()];
    for (ind, bell) in mask {
        row[*ind] = bell.name();
    }
    row.iter().join("")
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use crate::single_meth::tenors_together_fixed_bells;

    use super::*;
    use proj_core::{Method, PnBlock, RowTrait};

    fn test_plain_course_run_masks(pn: &str, stage: Stage, exp_masks: &str) {
        let mut plain_course =
            Method::with_lead_end(String::new(), &PnBlock::parse(pn, stage).unwrap())
                .plain_course()
                .into_rows();
        // Pop the duplicated rounds at the end
        assert!(plain_course.pop().unwrap().is_rounds());

        let course_head_masks = generate_course_head_masks(
            stage,
            &tenors_together_fixed_bells(stage),
            &plain_course,
            &MusicPattern::runs_front_or_back(stage, 4, 1.0),
        );

        let mut formatted_masks = course_head_masks
            .iter()
            .map(|(mask, score)| format!("{}: {}", format_mask(stage, mask), score))
            .collect_vec();
        // We have to sort the masks because they from a hash table's iterator which doesn't
        // guarantee any ordering
        formatted_masks.sort();

        // Join the sorted lines and check it's what we expect
        let full_format = formatted_masks.iter().join("\n");
        assert_eq!(full_format, exp_masks);
    }

    #[test]
    fn course_head_mask() {
        test_plain_course_run_masks(
            "-5-4.5-5.36.4-4.5-4-1,8",
            Stage::MAJOR,
            "x23456xx: 8
x23465xx: 8
x234xxxx: 4
x23x4xxx: 4
x24356xx: 8
x24365xx: 8
x243xxxx: 4
x2x3x4xx: 4
x324xxxx: 4
x32546xx: 8
x32564xx: 8
x32x4xxx: 4
x342xxxx: 4
x34526xx: 8
x34562xx: 8
x35246xx: 8
x35264xx: 8
x35426xx: 8
x35462xx: 8
x3x2x4xx: 4
x42536xx: 8
x42563xx: 8
x42x3xxx: 4
x43625xx: 8
x43652xx: 8
x43x2xxx: 4
x45236xx: 8
x45263xx: 8
x46325xx: 8
x46352xx: 8
x4x2x3xx: 4
x4x3x2xx: 4
x53426xx: 8
x53462xx: 8
x53624xx: 8
x53642xx: 8
x54326xx: 8
x54362xx: 8
x54x6xxx: 8
x56324xx: 8
x56342xx: 8
x64523xx: 8
x64532xx: 8
x64x5xxx: 8
x65423xx: 8
x65432xx: 8
xx6x5xxx: 8
xxx5x6xx: 8
xxxx56xx: 8",
        );
    }

    fn test_music_table(
        stage: Stage,
        method_pn: &str,
        pc_range: Range<usize>,
        tests: &[(&str, f32)],
    ) {
        let meth = Method::with_lead_end(String::new(), &PnBlock::parse(method_pn, stage).unwrap());

        let first_lead_rows = meth.plain_course().into_rows();

        let table = MusicTable::from_rows(
            Stage::MAJOR,
            &tenors_together_fixed_bells(stage),
            &first_lead_rows[pc_range],
            &MusicPattern::runs_front_or_back(Stage::MAJOR, 4, 1.0),
        );

        for &(row_rep, exp_score) in tests {
            assert_eq!(table.evaluate(&Row::parse(row_rep).unwrap()), exp_score);
        }
    }

    #[test]
    fn bristol_s8() {
        // First lead
        test_music_table(
            Stage::MAJOR,
            "x58x14.58x58.36.14x14.58x14x18,18",
            0..32,
            &[
                ("12345678", 8.0f32),
                ("12435678", 8.0f32),
                ("13245678", 6.0f32),
                ("14325678", 4.0f32),
                ("12346578", 4.0f32),
                ("16542378", 2.0f32),
                ("15432678", 2.0f32),
                ("15243678", 0.0f32),
                ("12364578", 0.0f32),
            ],
        );
        // First 4 leads
        test_music_table(
            Stage::MAJOR,
            "x58x14.58x58.36.14x14.58x14x18,18",
            0..128,
            &[
                ("12345678", 16.0f32),
                ("12435678", 14.0f32),
                ("13245678", 10.0f32),
                ("14325678", 6.0f32),
                ("12346578", 8.0f32),
                ("16542378", 4.0f32),
                ("15432678", 6.0f32),
                ("15243678", 0.0f32),
                ("12364578", 2.0f32),
            ],
        );
    }
}
