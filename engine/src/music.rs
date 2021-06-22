use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use bellframe::{
    music::{Regex, RegexElem},
    Bell, Row, RowBuf, Stage,
};
use itertools::Itertools;

/// The information required to quickly compute the music of a segment of some course
#[derive(Debug, Clone)]
pub(crate) struct MusicTable {
    /// Music score which is generated regardless of the course head.  This corresponds to the
    /// course head mask `xxxxxx...`.
    guaranteed_music: f32,
    /// In order to speed up music detection, we chose some place and then split our table by which
    /// bell is in that place.  This usually reduces the max table length by about 5, and thus
    /// causes ~5x speedup of music detection.
    pivot_place: usize,
    compiled_masks: Vec<Vec<(CourseHeadMask, f32)>>,
}

impl MusicTable {
    pub fn from_types(rows: &[RowBuf], music_types: &[MusicType], fixed_bells: &[Bell]) -> Self {
        let stage = rows[0].stage();
        let course_head_masks = generate_course_head_masks(
            stage,
            fixed_bells,
            rows.iter().map(|r| r.deref()),
            music_types,
        );
        Self::from_course_masks(stage, fixed_bells, course_head_masks).0
    }

    #[inline(always)]
    pub fn evaluate(&self, row: &Row) -> f32 {
        let partition_bell = row[self.pivot_place];

        let mut total_music = self.guaranteed_music;
        if let Some(masks) = self.compiled_masks.get(partition_bell.index()) {
            for (mask, score) in masks {
                if mask.evaluate(row) {
                    total_music += score;
                }
            }
        }
        total_music
    }

    /// Helper function which builds a search-ready `MusicTable` from a set of course head masks
    /// and their music scores.  This function also returns the best possible score achievable in
    /// this chunk.
    fn from_course_masks(
        stage: Stage,
        fixed_bells: &[Bell],
        mut masks: HashMap<Vec<(usize, Bell)>, f32>,
    ) -> (MusicTable, f32) {
        let guaranteed_music = masks.remove(&Vec::new()).unwrap_or(0.0);

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
                    // Which bell is in the pivot place
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

        /* println!(
            "pivoting at {}: len reduction: {} -> {}",
            pivot_place,
            masks.len(),
            masks_by_bell.values().map(|p| p.len()).sum::<usize>() as f32
                / masks_by_bell.len() as f32
        ); */

        // Compute best possible music score from partitioned masks, and then take the max of these
        let best_possible_score = guaranteed_music
            + masks_by_bell
                .iter()
                .map(|(_bell, masks)| best_music_score(&masks))
                // We shouldn't be able to get a score of NaN, so if we do then panic
                .max_by(|f1, f2| f1.partial_cmp(f2).unwrap())
                // If there are no course head masks, then no course heads produce music and this
                // should have score 0
                .unwrap_or(0.0);

        let max_pivot_bell_index = masks_by_bell.keys().map(|b| b.index()).max().unwrap_or(0);
        // The `+ 1` here is necessary to make sure that the last entry in `partition_table` has
        // index `max_pivot_bell_index`
        let mut partition_table = vec![Vec::new(); max_pivot_bell_index + 1];
        // Compile all the masks and put them in the partition table
        for (bell, masks) in masks_by_bell {
            partition_table[bell.index()].extend(
                masks
                    .into_iter()
                    .map(|(bell_locs, score)| (CourseHeadMask::from_slice(bell_locs), score)),
            );
        }

        let table = MusicTable {
            guaranteed_music,
            pivot_place,
            compiled_masks: partition_table,
        };
        (table, best_possible_score)
    }
}

#[derive(Debug, Clone)]
struct CourseHeadMask {
    mask: Vec<(usize, Bell)>,
}

impl CourseHeadMask {
    fn from_slice(s: &[(usize, Bell)]) -> Self {
        Self { mask: s.to_vec() }
    }

    fn evaluate(&self, r: &Row) -> bool {
        // Check if any of the bells in the row are in the wrong place
        for &(i, bell) in &self.mask {
            if r[i] != bell {
                return false;
            }
        }
        // If all the indices in `r` contain the right bells, then the mask matches
        true
    }
}

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub struct MusicType {
    pub regexes: Vec<Regex>,
    pub weight: f32,
}

/// Combines rows and music patterns to generate a list of course head masks and their music
/// scores.  These masks will never contain only one missing non-fixed bell (i.e. `x3254xxx` is
/// normalised to `x32546xx`).
fn generate_course_head_masks<'r>(
    stage: Stage,
    fixed_bells: &[Bell],
    rows: impl IntoIterator<Item = &'r Row> + Clone,
    patterns: &[MusicType],
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
    // Flatten the jagged music types into a flat list of regexes and weights
    let mut flat_patterns: Vec<(&Regex, f32)> = Vec::new();
    for p in patterns {
        flat_patterns.extend(p.regexes.iter().map(|r| (r, p.weight)));
    }
    // Now iterate over the flattened patterns
    let bell_requirements: Vec<(Vec<(usize, Bell)>, f32)> = flat_patterns
        .into_iter()
        .map(|(regex, weight)| {
            let regex_elems = regex.elems();
            let regex_len = regex_elems.len();
            let glob_locations = regex_elems
                .iter()
                .positions(|v| *v == RegexElem::Glob)
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
                    regex_elems
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
                        regex_elems
                            .iter()
                            .take(glob_ind)
                            .enumerate()
                            .filter_map(|(i, r)| r.bell().map(|b| (i, b))),
                    );
                    // Post-glob bells and their required indices within the row
                    bell_requirements.extend(
                        regex_elems
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
            (bell_requirements, weight)
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
            'pattern_loop: for (music_pattern, weight) in &bell_patterns {
                // First of all, check if these patterns agree on the locations of the fixed bells.
                // If they don't then this particular musical pattern can never be generated in
                // this location.
                for fb in fixed_bells {
                    let source_index = source_pattern.iter().position(|b| b == fb);
                    let music_index = music_pattern.iter().position(|b| b == fb);
                    if source_index != music_index {
                        continue 'pattern_loop;
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

/* HEURISTIC COMPUTATION */

/// Computes the highest achievable music score, given a set of course head masks
fn best_music_score(masks: &[(&[(usize, Bell)], f32)]) -> f32 {
    /* For the A*-style DFS to work, we only need an upper bound for the best possible score for a
     * given segment.  However, the tighter the bound the better the pruning so it pays to spend
     * some more time computing this exactly.  I want this to work even when there are large
     * numbers of non-fixed bells, so instead of computing every possible course head we instead
     * perform tree search over which sets of masks are compatible. This is NP-hard (of course),
     * but we only need it to work for small lists of masks (<= 100) */

    /* Pre-compute useful information to speed up the tree search */

    let scores = masks.iter().map(|(_, score)| *score).collect_vec();
    // If we encounter mask `i` in tree search, then next_masks[i] contains all compatible masks
    // with index greater than `i`.  These are the nodes that we should recurse onto
    let mut next_masks: Vec<Vec<usize>> = vec![Vec::new(); masks.len()];
    // Set of pairs of masks (i, j) where i < j and i, j are compatible
    let mut compatible_masks: HashSet<(usize, usize)> = HashSet::new();
    // Run through every pair of masks and populate `next_masks` and `compatible_masks`
    for (i1, &(m1, _)) in masks.iter().enumerate() {
        for (i2, &(m2, _)) in masks[..i1].iter().enumerate() {
            if are_compatible(m1, m2) {
                // This way round is correct because i2 < i1
                next_masks[i2].push(i1);
                compatible_masks.insert((i2, i1));
            }
        }
    }

    /* Perform the tree search */

    // Lookup table for which masks have been added at any point
    let mut used_masks: Vec<usize> = Vec::new();
    // Tracker for the best score found so far
    let mut best_score_so_far = 0.0f32;
    // Iterate through every node, and compute all the valid sets which contain it as the smallest
    // indexed mask.  By the end, this will have enumerated all sets of compatible masks once and
    // only once each
    for i in 0..masks.len() {
        // `best_score_tree_search` doesn't check if `i` is valid to insert, but this is fine
        // because no masks are added yet so any mask is compatible with no masks.
        best_score_tree_search(
            i,
            0.0,
            &scores,
            &next_masks,
            &compatible_masks,
            &mut used_masks,
            &mut best_score_so_far,
        );
    }

    // After the tree search, every compatible set of masks has been searched, so
    // `best_score_so_far` will contain the best possible score for this segment
    best_score_so_far
}

/// Perform DFS search over which sets of masks can be used simultaneously.  In order to only
/// search each mask once, we only add masks in ascending order by index.  Once this function is
/// called, it is a assumed that the `mask_ind` is valid
fn best_score_tree_search(
    // Info about this node
    mask_ind: usize,
    cumulative_score: f32,
    // Persistent lookup tables
    scores: &[f32],
    next_masks: &[Vec<usize>],
    compatible_masks: &HashSet<(usize, usize)>,
    // Mutable state
    used_masks: &mut Vec<usize>,
    best_score_so_far: &mut f32,
) {
    // Firstly, mark this mask as used
    used_masks.push(mask_ind);

    // See if adding this mask generates a new high-score
    let score = cumulative_score + scores[mask_ind];
    *best_score_so_far = best_score_so_far.max(score);

    // Try to add more masks that are compatible with this one
    'next_mask_loop: for &new_mask_ind in &next_masks[mask_ind] {
        // Check that this new mask is compatible with **all** the existing masks (we already know
        // it's compatible with the most recently added mask, but compatibility isn't transiative
        // so that isn't enough).
        for m_ind in used_masks.iter() {
            // Reject this node completely if any of the existing masks are incompatible
            if !compatible_masks.contains(&(*m_ind, new_mask_ind)) {
                continue 'next_mask_loop;
            }
        }
        // If all the existing masks are compatible, then add this mask and keep searching
        best_score_tree_search(
            new_mask_ind,
            score,
            scores,
            next_masks,
            compatible_masks,
            used_masks,
            best_score_so_far,
        );
    }

    // Mark this mask as unused once we return
    used_masks.pop();
}

fn are_compatible(m1: &[(usize, Bell)], m2: &[(usize, Bell)]) -> bool {
    // Two masks are compatible if and only if, for every (i, b) in m1, m2 doesn't put either a
    // different bell in `i` or put `b` in a different place

    for &(i, b) in m1 {
        // Check that m2 doesn't put a different bell in place `i`
        if !m2
            .iter()
            .find(|(i2, _)| i == *i2)
            .map_or(true, |(_, b2)| b == *b2)
        {
            // If they disagree, then immediately return false
            return false;
        }
        // Check that m1 doesn't put bell `b` in a different place
        if !m2
            .iter()
            .find(|(_, b2)| b == *b2)
            .map_or(true, |(i2, _)| i == *i2)
        {
            // If they disagree, then immediately return false
            return false;
        }
    }
    true
}
