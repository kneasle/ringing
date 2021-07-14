use std::{collections::HashMap, ops::Mul};

use bellframe::{method::LABEL_LEAD_END, Bell, Method, PlaceNot, Row, RowBuf, Stage};
use itertools::Itertools;
use log::Level;

use crate::{
    layout::{Layout, Link, RowIdx},
    mask::Mask,
    Config,
};

/// Helper function to generate a [`Layout`] for a single [`Method`]
pub fn single_method_layout(
    method: &Method,
    calls: &[CallSpec],
    // The course head masks, along with which bell is 'calling bell' during that course.
    // Allowing different calling bells allows us to do things like keep using W,M,H during
    // courses of e.g. `1xxxxx0987`.
    input_course_head_masks: &[(Mask, Bell)],
    config: &Config,
    // Which sub-lead indices are considered valid starting or finishing points for the
    // composition.  If these are `None`, then any location is allowed
    allowed_start_indices: Option<&[usize]>,
    allowed_end_indices: Option<&[usize]>,
) -> Result<Layout, SingleMethodError> {
    // Generate useful values
    let plain_course = method.plain_course();
    let course_len = plain_course.len();

    /* STEP 1: PRE-PROCESS COURSE HEAD MASKS */

    let course_head_masks = {
        // TODO: Detect fixed bells (e.g. treble) and add them to all the course heads.  This will
        // prevent Monument from generating an unnecessary amount of falseness data which will have
        // to be optimised out of the node graph.

        /* EXPAND COURSE HEADS WHICH DON'T FIX THEIR CALLING BELL */

        // It is possible that there could be course head masks which don't specify a location for
        // their calling bell.  This is perfectly fine from a usability point of view, but
        // generates a situation where the same call is given different names depending on the
        // course head.  This is not allowed, but we can fix it by duplicating the offending mask
        // with the calling bell at each possible location.  These masks refer to the same set of
        // possible courses, and each of them will give the calls different names.
        let mut course_head_masks_to_duplicate = Vec::<(&Mask, Bell)>::new();
        let mut fully_specified_ch_masks = Vec::<(Mask, Bell)>::new();
        for (mask, calling_bell) in input_course_head_masks {
            if mask.place_of(*calling_bell).is_some() {
                // If the mask does give a location for calling_bell, then this mask can be left
                // untouched
                fully_specified_ch_masks.push((mask.clone(), *calling_bell));
            } else {
                // If the mask doesn't give a fixed location for calling_bell, then we need to
                // duplicate this mask
                course_head_masks_to_duplicate.push((mask, *calling_bell));
            }
        }
        // Now actually duplicate the course head masks
        for (mask, calling_bell) in course_head_masks_to_duplicate {
            for i in mask.unspecified_indices() {
                let mut new_mask = mask.to_owned();
                new_mask.require_bell_at(calling_bell, i);
                fully_specified_ch_masks.push((new_mask, calling_bell));
            }
        }

        /* REMOVE COURSE HEADS WHICH ARE SPECIFIED BY OTHER COURSE HEADS */

        // Remove any course head masks which are direct subsets of another.  For example, if both
        // `1xxxxx78` and `1xxxxxx8` are present, then `1xxxxx78` can be removed.
        let deduped_ch_masks: Vec<(Mask, Bell)> = fully_specified_ch_masks
            .iter()
            .enumerate()
            .filter(|&(i, (mask, _))| {
                !fully_specified_ch_masks
                    .iter()
                    .enumerate()
                    // Look for any mask which is a subset of `mask` but isn't `mask` itself (since all
                    // masks are subsets of themselves
                    .any(|(i2, (mask2, _))| i != i2 && mask.is_subset_of(mask2))
            })
            .map(|(_i, m)| m.to_owned())
            .collect_vec();

        deduped_ch_masks
    };

    /* STEP 2: CHECK THE COURSE HEAD MASKS */

    // Look for situations where two masks match **different** leads of the same course.
    // This would generate courses where the idea of a 'course head' is not well defined.  For
    // example, in a method with Plain Bob lead heads, `1xxxxx78` and `1x56x8x7` would create an
    // ambiguity because some courses could either be labelled `15xxx678` or `1x56x8x7`.
    //
    // For every course head mask ...
    for (i, (mask, _)) in course_head_masks.iter().enumerate() {
        // ... and for every lead head (except the rounds at the start) ...
        for annot_row in &plain_course.annot_rows()[1..course_len] {
            if annot_row.annot().0 == 0 {
                // ... compute mask for this lead head at this location ...
                let lead_head = annot_row.row();
                let transposed_mask = mask.mul(lead_head);
                // ... and check that it isn't compatible with any of the course head mask we've
                // seen so far.  We only need to check each pair of CH masks once (since ambiguity
                // is reflexive).
                for (mask2, _) in &course_head_masks[..=i] {
                    match transposed_mask.combine(mask2) {
                        // If the lead-end is compatible with a course head mask, then derive which
                        // leads are ambiguous, and return an error
                        Some(course_head_2) => {
                            // Generate the second course head by transposing `mask2` at this lead
                            // back to the course head satisfying `mask`
                            let course_head_1 = mask2.mul(&lead_head.inv()).combine(mask).unwrap();
                            return Err(SingleMethodError::AmbiguousCourseHeads {
                                mask1: course_head_1,
                                input_mask1: mask.clone(),
                                mask2: course_head_2,
                                input_mask2: mask2.clone(),
                            });
                        }
                        // If they're not compatible, then no ambiguity is generated
                        None => {}
                    }
                }
            }
        }
    }

    /* Step 3: Generate the locations of the `Row`s where calls could be applied (and the masks
     * that they should satisfy for each input CH mask). */

    /// Data structure to hold data about call end locations
    #[derive(Debug, Clone)]
    struct CallEnd<'pc> {
        lead_location: &'pc str,
        row_mask: Mask,
        course_head_mask_idx: usize,
        row_idx: usize,
    }

    // This maps lead locations to a list of row indices which occur just before this label.  These
    // correspond to lead **ends**.  For example, Yorkshire Surprise Major with only lead end
    // calls generates `{"LE": [31, 63, 95, 127, 159, 191, 223]}`
    let mut call_starts: HashMap<&str, Vec<usize>> = HashMap::new();
    // This maps `(lead_location, transposed_masks)` to the index of the row within the course
    // where that pattern occurs.  These correspond to lead **heads**.
    let mut call_ends: Vec<CallEnd> = Vec::new();

    // For every labelled row ...
    for (row_idx, annot_row) in plain_course.annot_rows()[..course_len].iter().enumerate() {
        if let Some(lead_location) = annot_row.annot().1 {
            let row_after_call = annot_row.row();
            // ... for every course head mask ...
            for (mask_idx, (mask, _)) in course_head_masks.iter().enumerate() {
                // ... add a `CallEnd` corresponding to calling a call here
                call_ends.push(CallEnd {
                    lead_location,
                    row_mask: mask.mul(row_after_call),
                    course_head_mask_idx: mask_idx,
                    row_idx,
                });
            }

            // Store the location of the row just before the call (i.e. the lead **end**)
            call_starts
                .entry(lead_location)
                .or_insert_with(Vec::new)
                // We add `plain_course` before subtracting one so that we don't cause an underflow
                // when i is 0
                .push((row_idx + course_len - 1) % course_len);
        }
    }

    // Sanity check that any compatible call-end masks also agree on `row_idx`.  This should be
    // guaranteed if the course heads are unambiguous, but it is required for the rest of the code
    // to work - so we may as well check here
    for call_end1 in &call_ends {
        for call_end2 in &call_ends {
            if call_end1.row_mask.is_compatible_with(&call_end2.row_mask) {
                assert_eq!(call_end1.row_idx, call_end2.row_idx);
            }
        }
    }

    /* STEP 4: DETERMINE WHICH ROWS CAN BE `Link`ED BY CALLS (OR PLAIN LEADS) */

    /// The index of the block who's rows we're using.  Because we are only considering single
    /// methods, there is only one block (the plain course of the method) and that block has index
    /// 0.  If we extend this to handle multiple methods, then this would have to be calculated.
    const BLOCK_IDX: usize = 0;

    let mut links = Vec::<Link>::new();
    for (ch_mask, calling_bell) in &course_head_masks {
        for (call_idx, call) in calls.iter().enumerate() {
            let lead_loc = call.lead_location.as_str();

            // Test this call at every possible (correctly named) location in the course
            for &from_idx in call_starts
                .get(lead_loc)
                .ok_or_else(|| SingleMethodError::UndefinedLeadLocation(lead_loc.to_owned()))?
            {
                let row_after_call = call
                    .place_not
                    .permute_new(plain_course.get_row(from_idx).unwrap())
                    .unwrap();
                let mask_after_call = ch_mask.mul(&row_after_call);

                // Find the calling position of this call, returning an error if the call doesn't
                // have enough calling positions
                let tenor_place = mask_after_call
                    .place_of(*calling_bell)
                    .expect("Course head mask doesn't fix the calling bell");
                let calling_position =
                    call.calling_positions.get(tenor_place).ok_or_else(|| {
                        SingleMethodError::CallingPositionsTooShort {
                            call_name: call.debug_symbol.to_owned(),
                            call_idx,
                            calling_position_len: call.calling_positions.len(),
                            stage: method.stage(),
                        }
                    })?;

                for call_end in &call_ends {
                    if call_end.row_mask.is_compatible_with(&mask_after_call) {
                        // If the mask generated by this call is compatible with some call end,
                        // then the course we're going into satisfies some course head mask and
                        // this call should be included in the resulting Layout.

                        // Compute the course head transposition generated by this call
                        let course_head_transposition = Row::solve_xa_equals_b(
                            plain_course.get_row(call_end.row_idx).unwrap(),
                            &row_after_call,
                        )
                        .unwrap();

                        let ch_mask_of_new_course =
                            &course_head_masks[call_end.course_head_mask_idx].0;
                        // The course head mask for which courses can appear **before** this call.
                        // This mask may be stricter than `ch_mask` if the `call_end`'s CH mask
                        // specifies more bells than `ch_mask`.
                        let source_mask = ch_mask
                            .combine(&ch_mask_of_new_course.mul(&course_head_transposition.inv()))
                            .unwrap();

                        // The `Link` referring to the call happening at this lead
                        links.push(Link {
                            from: RowIdx::new(BLOCK_IDX, from_idx),
                            to: RowIdx::new(BLOCK_IDX, call_end.row_idx),
                            course_head_mask: source_mask.clone(),
                            course_head_transposition,
                            debug_name: format!("{}{}", call.debug_symbol, calling_position),
                            display_name: format!("{}{}", call.display_symbol, calling_position),
                        });
                        // The `Link` referring to a plain call at this lead.  This will generate a
                        // large number of duplicate leads (since many calls could happen in the
                        // same location), but the links will be de-duplicated anyway and this way
                        // makes it very difficult to accidentally miss plain leads
                        links.push(Link {
                            from: RowIdx::new(BLOCK_IDX, from_idx),
                            to: RowIdx::new(BLOCK_IDX, (from_idx + 1) % course_len),
                            course_head_mask: source_mask,
                            // Plain leads don't cause a course head transposition
                            course_head_transposition: RowBuf::rounds(method.stage()),
                            debug_name: format!("p{}", calling_position),
                            // Plain leads shouldn't be displayed
                            display_name: String::new(),
                        });
                    }
                }
            }
        }
    }

    /* STEP 5: REMOVE REDUNDANT LINKS */

    /* This algorithm produces a large number of essentially identical plain call links if there
     * are multiple calls at the same position.  This is not technically wrong (the graph
     * generation code has to de-duplicate node links anyway), including loads of duplicate links
     * makes the `Layout` much harder to debug.  Therefore, we de-duplicate the call links before
     * generating the `Layout`. */

    // The indices of links which are special cases of some other link (or are identical to other
    // links)
    let mut redundant_link_idxs = Vec::<usize>::new();
    for (i, link) in links.iter().enumerate() {
        for (i2, link2) in links.iter().enumerate() {
            // Links are always compatible with themselves, and there's no point 'de-duplicating' a
            // link because it's redundant against itself
            if i == i2 {
                continue;
            }

            // This is 'true' if `link` and `link2` are equal apart from their course head masks.
            // We don't check the names, since it's possible that the same link is given two
            // different names (often in the case of plain leads or if the user inputs two
            // identical but differently named calls).  The tree search will likely treat these two
            // paths as different and search them separately (i.e. run that part of the search
            // twice), so it's worth de-duplicating them here.
            let are_links_otherwise_equal = link.eq_without_name_or_ch_mask(link2);

            if are_links_otherwise_equal {
                if link.course_head_mask == link2.course_head_mask {
                    // If the links are identical, then we remove the one with the least index.
                    // This way, exactly one link from a group of identical links (the one with the
                    // highest index) will survive
                    if i < i2 {
                        redundant_link_idxs.push(i);
                    }
                } else if link.course_head_mask.is_subset_of(&link2.course_head_mask) {
                    // If `link2`'s CH mask is more general than `link`'s, and `link` and `link2`
                    // are otherwise equal, then `link` is a special case of `link2` and is
                    // therefore redundant
                    redundant_link_idxs.push(i);
                }
            }
        }
    }

    // Print the unduplicated call links if running in debug mode
    if config.log_level >= Level::Debug {
        println!("PRE-DEDUPLICATION");
        for (i, l) in links.iter().enumerate() {
            println!(
                "({:>2}{:>1}):   {} {:>3}   --[{:>2}]->   {} {:>3}",
                i,
                if redundant_link_idxs.contains(&i) {
                    "*"
                } else {
                    ""
                },
                l.course_head_mask,
                l.from.row,
                l.debug_name,
                l.course_head_mask.mul(&l.course_head_transposition),
                l.to.row
            );
        }
    }

    // Now actually remove the unnecessary links, making sure to iterate backwards so that the
    // indices keep pointing to the right elements
    redundant_link_idxs.sort();
    redundant_link_idxs.dedup();
    for idx in redundant_link_idxs.into_iter().rev() {
        links.remove(idx);
    }

    if config.log_level >= Level::Debug {
        println!("POST-DEDUPLICATION");
        for (i, l) in links.iter().enumerate() {
            println!(
                "({:>2}):    {} {:>3}   --[{:>2}]->   {} {:>3}",
                i,
                l.course_head_mask,
                l.from.row,
                l.debug_name,
                l.course_head_mask.mul(&l.course_head_transposition),
                l.to.row
            );
        }
    }

    /* STEP 6: GENERATE STARTS/ENDS, AND CREATE A LAYOUT */

    // Figure out where rounds can appear in courses which satisfy the course head masks
    let mut starts = Vec::<(RowBuf, RowIdx, String)>::new();
    let mut ends = Vec::<(RowBuf, RowIdx, String)>::new();

    let rounds = RowBuf::rounds(method.stage());
    for (ch_mask, _) in course_head_masks {
        for (row_idx, annot_row) in plain_course.annot_rows()[..course_len].iter().enumerate() {
            let transposed_mask = ch_mask.mul(annot_row.row());
            // If rounds satisfies `transposed_mask`, then this location can contain rounds
            if transposed_mask.matches(&rounds) {
                // Decide whether this is snap start/finish
                let sub_lead_index = annot_row.annot().0;
                let is_snap = sub_lead_index != 0;
                let course_head_containing_rounds = annot_row.row().inv();

                if allowed_start_indices
                    .as_ref()
                    .map_or(true, |idxs| idxs.contains(&sub_lead_index))
                {
                    starts.push((
                        course_head_containing_rounds.clone(),
                        RowIdx::new(BLOCK_IDX, row_idx),
                        (if is_snap { "<" } else { "" }).to_owned(),
                    ));
                }
                if allowed_end_indices
                    .as_ref()
                    .map_or(true, |idxs| idxs.contains(&sub_lead_index))
                {
                    ends.push((
                        course_head_containing_rounds,
                        RowIdx::new(BLOCK_IDX, row_idx),
                        (if is_snap { ">" } else { "" }).to_owned(),
                    ));
                }
            }
        }
    }

    // Collect the plain course into a block of rows
    let mut plain_rows = plain_course.into_rows();
    // Remove the leftover rounds at the end of the block
    assert!(plain_rows.pop().unwrap().is_rounds());

    Ok(Layout {
        blocks: vec![plain_rows],
        links,
        // Convert the `rounds_locations` into starts and finishes
        starts,
        ends,
    })
}

#[derive(Debug, Clone)]
pub enum SingleMethodError {
    UndefinedLeadLocation(String),
    CallingPositionsTooShort {
        call_name: String,
        call_idx: usize,
        calling_position_len: usize,
        stage: Stage,
    },
    AmbiguousCourseHeads {
        /// The first possible course head for the ambiguous course
        mask1: Mask,
        /// The course head mask given by the user which `mask1` satisfies
        input_mask1: Mask,
        /// The second possible course head for the ambiguous course
        mask2: Mask,
        /// The course head mask given by the user which `mask1` satisfies
        input_mask2: Mask,
    },
}

/// The specification for a call that can be used in a composition
#[derive(Debug, Clone)]
pub struct CallSpec {
    display_symbol: String,
    debug_symbol: String,
    lead_location: String,
    place_not: PlaceNot,
    calling_positions: Vec<String>,
}

impl CallSpec {
    pub fn new(
        display_symbol: String,
        debug_symbol: String,
        lead_location: String,
        place_not: PlaceNot,
        calling_positions: Option<Vec<String>>,
    ) -> Self {
        Self {
            display_symbol,
            debug_symbol,
            lead_location,
            calling_positions: calling_positions
                .unwrap_or_else(|| default_calling_positions(&place_not)),
            place_not,
        }
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_bob(place_not: PlaceNot) -> Self {
        Self::new(
            String::new(),
            "-".to_owned(),
            LABEL_LEAD_END.to_owned(),
            place_not,
            None,
        )
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_single(place_not: PlaceNot) -> Self {
        Self::new(
            "s".to_owned(),
            "s".to_owned(),
            LABEL_LEAD_END.to_owned(),
            place_not,
            None,
        )
    }
}

fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
    let named_positions = "LIBFVXSEN";

    // Generate calling positions that aren't M, W or H
    let mut positions =
        // Start off with the single-char position names
        named_positions
        .chars()
        .map(|c| c.to_string())
        // Extending forever with numbers
        .chain((named_positions.len()..).map(|i| (i + 1).to_string()))
        // But we consume one value per place in the Stage
        .take(place_not.stage().as_usize())
        .collect_vec();

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a given (0-indexed) place
    macro_rules! replace_pos {
        ($idx: expr, $new_val: expr) => {
            if let Some(v) = positions.get_mut($idx) {
                v.clear();
                v.push($new_val);
            }
        };
    }

    // Edge case: if 2nds are made in `place_not`, then I/B are replaced with B/T.  Note that
    // places are 0-indexed
    if place_not.contains(1) {
        replace_pos!(1, 'B');
        replace_pos!(2, 'T');
    }

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a place indexed from the end of the stage (so 0 is the highest place)
    macro_rules! replace_mwh {
        ($ind: expr, $new_val: expr) => {
            if let Some(place) = place_not.stage().as_usize().checked_sub(1 + $ind) {
                if place >= 4 {
                    if let Some(v) = positions.get_mut(place) {
                        v.clear();
                        v.push($new_val);
                    }
                }
            }
        };
    }

    // Add MWH (M and W are swapped round for odd stages)
    if place_not.stage().is_even() {
        replace_mwh!(2, 'M');
        replace_mwh!(1, 'W');
        replace_mwh!(0, 'H');
    } else {
        replace_mwh!(2, 'W');
        replace_mwh!(1, 'M');
        replace_mwh!(0, 'H');
    }

    positions
}

#[cfg(test)]
mod tests {
    use bellframe::{PlaceNot, Stage};
    use itertools::Itertools;

    fn char_vec(string: &str) -> Vec<String> {
        string.chars().map(|c| c.to_string()).collect_vec()
    }

    #[test]
    fn default_calling_positions() {
        #[rustfmt::skip]
        let cases = &[
            ("145", Stage::DOUBLES, char_vec("LIBFH")),
            ("125", Stage::DOUBLES, char_vec("LBTFH")),
            ("1", Stage::DOUBLES, char_vec("LIBFH")),

            ("14", Stage::MINOR, char_vec("LIBFWH")),
            ("1234", Stage::MINOR, char_vec("LBTFWH")),
            ("1456", Stage::MINOR, char_vec("LIBFWH")),

            ("147", Stage::TRIPLES, char_vec("LIBFWMH")),
            ("12347", Stage::TRIPLES, char_vec("LBTFWMH")),

            ("14", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1234", Stage::MAJOR, char_vec("LBTFVMWH")),
            ("16", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1678", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1256", Stage::MAJOR, char_vec("LBTFVMWH")),
            ("123456", Stage::MAJOR, char_vec("LBTFVMWH")),

            ("14", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("16", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("18", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("1890", Stage::ROYAL, char_vec("LIBFVXSMWH")),

            ("14", Stage::MAXIMUS, char_vec("LIBFVXSENMWH")),
            ("1234", Stage::MAXIMUS, char_vec("LBTFVXSENMWH")),
        ];

        for (pn_str, stage, exp_positions) in cases {
            let positions =
                super::default_calling_positions(&PlaceNot::parse(pn_str, *stage).unwrap());
            assert_eq!(positions, *exp_positions);
        }
    }
}
