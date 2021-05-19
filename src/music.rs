use std::{collections::HashMap, iter::once};

use itertools::Itertools;
use proj_core::{Bell, Row, Stage};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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

    pub fn four_bell_runs(stage: Stage) -> Vec<MusicPattern> {
        let num_bells = stage.as_usize();

        let mut runs = Vec::with_capacity(num_bells.saturating_sub(3) * 2);
        for i in 0..=num_bells - 4 {
            // Collect the run of bells into a Vec which can be cloned
            let run_iterator = (i..i + 4).map(Bell::from_index).map(RowRegexElem::Bell);

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
                    .clone()
                    .rev()
                    .chain(once(RowRegexElem::Glob))
                    .collect_vec(),
            );
        }

        runs.into_iter()
            .map(|regex| MusicPattern { regex, weight: 1.0 })
            .collect_vec()
    }
}

pub fn generate_music_table<'a>(
    stage: Stage,
    fixed_bells: &[Bell],
    rows: impl IntoIterator<Item = &'a Row> + Clone,
    music_types: &[MusicPattern],
) {
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
     * then this would corresponds to wanting "2345" or "5432" in bell range `0..4` with a weight
     * of 1 each.
     */
    let bell_requirements: Vec<(Vec<(usize, Bell)>, f32)> = music_types
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
                    panic!("Can't handle music regexes with more than one glob");
                }
            };
            // Important thing to note here is that bell_requirements is always sorted by index,
            // which is required for later algorithms to work properly
            (bell_requirements, m.weight)
        })
        .collect_vec();

    /* The second step is to convert these requirements into a hash table from a list of indices to
     * all the bell patterns which use those indices so that we can prevent work later on.
     *
     * For example, if `[0, 1, 2, 3]` maps to `[("2345", 1.0), ("5432", 1.0)]` then it means that
     * we'd count either 2345 or 5432 in places `0..4`, each with weight 1. */
    let mut index_requirements: HashMap<Vec<usize>, Vec<(Vec<Bell>, f32)>> = HashMap::new();
    for (bell_indices, weight) in bell_requirements {
        // Unzip the iterators
        let indices = bell_indices.iter().map(|(i, _)| *i).collect_vec();
        let bells = bell_indices.iter().map(|(_, b)| *b).collect_vec();
        index_requirements
            .entry(indices)
            .or_insert(Vec::new())
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

    // Print the course head masks
    for (mask, score) in course_head_masks {
        let mut row = vec![String::from("x"); stage.as_usize()];
        for (ind, bell) in mask {
            row[ind] = bell.name();
        }
        println!("{}: {}", row.iter().join(""), score);
    }
}
