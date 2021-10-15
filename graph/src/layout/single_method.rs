use std::{
    collections::{HashMap, HashSet},
    ops::Mul,
};

use bellframe::{method, AnnotBlock, Bell, Mask, Method, Row, RowBuf, Stage};
use itertools::Itertools;

use super::{CourseHeadMask, Layout, Link, RowIdx, StartOrEnd};

/// Helper function to generate a [`Layout`] for a single [`Method`]
pub(super) fn single_method_layout(
    method: &Method,
    calls: &[super::Call],
    // The course head masks, along with which bell is 'calling bell' during that course.
    // Allowing different calling bells allows us to do things like keep using W,M,H during
    // courses of e.g. `1xxxxx0987`.
    ch_masks: Vec<(Mask, Bell)>,
    // Which sub-lead indices are considered valid starting or finishing points for the
    // composition.  If these are `None`, then any location is allowed
    allowed_start_indices: Option<&[usize]>,
    allowed_end_indices: Option<&[usize]>,
) -> Result<Layout> {
    // Generate useful values
    let plain_course = method.plain_course();
    let lead_heads = method.lead_head().closure_from_rounds();

    /* PRE-PROCESS COURSE HEAD MASKS */

    // Add fixed bells (e.g. the treble) to the CH masks.  Skipping this would preserve the
    // correctness of the graph but makes the falseness detection consume a completely unnecessary
    // amount of time and memory.
    let ch_masks = add_fixed_bells(ch_masks, method, calls);
    // Convert the (Mask, Bell) pairs into `CourseHeadMask`s
    let ch_masks = ch_masks
        .into_iter()
        .flat_map(|(mask, bell)| CourseHeadMask::new(mask, bell))
        .collect_vec();
    // Remove redundant CH masks, and check that no courses can have two different calling bells.
    let ch_masks = dedup_ch_masks(&ch_masks)?;
    // Check that two CH masks don't label the same course at two different leads.
    check_for_ambiguous_courses(&ch_masks, &lead_heads)?;

    // The possible ways that courses can be stitched together
    let links = generate_links(&plain_course, &ch_masks, calls)?;

    // Places where the composition can start
    let starts = rounds_locations(&ch_masks, &plain_course, allowed_start_indices, "<");
    // Places where the composition can start
    let ends = rounds_locations(&ch_masks, &plain_course, allowed_end_indices, ">");

    Ok(Layout {
        blocks: vec![plain_course.map_annots(|_| ())],
        links,
        // Convert the `rounds_locations` into starts and finishes
        starts,
        ends,
    })
}

/// The ways that [`Layout::single_method`] can fail
#[derive(Debug, Clone)]
pub enum Error {
    UndefinedLeadLocation(String),
    CallingPositionsTooShort {
        call_name: String,
        call_idx: usize,
        calling_position_len: usize,
        stage: Stage,
    },
    /// Some courses match two different [`CourseHeadMask`]s with **different** calling bells.
    ConflictingCallingBell(CourseHeadMask, CourseHeadMask),
    AmbiguousCourseHeadPosition {
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

pub type Result<T> = std::result::Result<T, Error>;

/// The index of the block who's rows we're using.  Because we are only considering single
/// methods, there is only one block (the plain course of the method) and that block has index
/// 0.  If we extend this to handle multiple methods, then this would have to be calculated.
const BLOCK_IDX: usize = 0;

/// Detect fixed bells (a place bell which is preserved by all methods' calls and plain leads) and
/// fix it in all the course heads
fn add_fixed_bells(
    ch_masks: Vec<(Mask, Bell)>,
    method: &Method,
    calls: &[super::Call],
) -> Vec<(Mask, Bell)> {
    let fixed_bells = fixed_bells(method, calls);

    let mut fixed_ch_masks = Vec::with_capacity(ch_masks.len());
    'mask_loop: for (mut mask, calling_bell) in ch_masks {
        // Attempt to add the fixed bells to this mask
        for &b in &fixed_bells {
            if let Err(()) = mask.fix(b) {
                // If a bell is known to be fixed in its home position but a mask requires it to be
                // outside of its home position, then that mask will never be satisfied and can be
                // removed
                continue 'mask_loop;
            }
        }
        // If all fixed bells could be set, then keep this mask
        fixed_ch_masks.push((mask, calling_bell))
    }
    fixed_ch_masks
}

/// Returns the place bells which are always preserved by plain leads and all calls (e.g. hunt
/// bells in non-variable-hunt compositions).
fn fixed_bells(method: &Method, calls: &[super::Call]) -> Vec<Bell> {
    // Start the set with the bells which are fixed by the plain lead
    let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
    // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in Grandsire is
    // unaffected by a plain lead, but affected by calls)
    for call in calls {
        filter_bells_fixed_by_call(method, call, &mut fixed_bells);
    }
    fixed_bells.into_iter().collect_vec()
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(method: &Method, call: &super::Call, set: &mut HashSet<Bell>) {
    // Note that all calls are required to only substitute one piece of place notation.
    for sub_lead_idx_after_call in method.label_indices(&call.lead_location) {
        let idx_before_call = (sub_lead_idx_after_call + method.lead_len() - 1) % method.lead_len();
        let idx_after_call = idx_before_call + 1; // in range `1..=method.lead_len()`

        // The row before a call in this location in the _first lead_
        let row_before_call = method.first_lead().get_row(idx_before_call).unwrap();
        // The row after a plain call in this location in the _first lead_
        let row_after_no_call = method.first_lead().get_row(idx_after_call).unwrap();
        // The row after a call in this location in the _first lead_
        let mut row_after_call = row_before_call.to_owned();
        call.place_not.permute(&mut row_after_call).unwrap();

        // A bell is _affected_ by the call iff it's in a different place in `row_after_call` than
        // `row_after_no_call`.  These should be removed from the set, because they are no longer
        // fixed.
        for (bell_after_no_call, bell_after_call) in
            row_after_no_call.bell_iter().zip(&row_after_call)
        {
            if bell_after_call != bell_after_no_call {
                set.remove(&bell_after_call);
            }
        }
    }
}

/////////////////////////////////
// COURSE HEAD MASK PROCESSING //
/////////////////////////////////

/// Remove course head masks which are an exact subset of another mask.  E.g. if `1xxx5678` and
/// `1xxxxx78` are both defined, then `1xxx5678` can be removed (assuming they both specify the
/// same calling bell).  It is an error if two masks are compatible but specify different calling
/// bells.
fn dedup_ch_masks(course_head_masks: &[CourseHeadMask]) -> Result<Vec<CourseHeadMask>> {
    let mut deduped_ch_masks = Vec::with_capacity(course_head_masks.len());
    'outer: for (i, ch_mask) in course_head_masks.iter().enumerate() {
        // Look for another CH mask who's matched CHs are a superset of `mask`
        for (other_i, other_ch_mask) in course_head_masks.iter().enumerate() {
            if i == other_i {
                continue; // Don't compare masks against themselves
            }
            if ch_mask.mask.is_subset_of(&other_ch_mask.mask)
                && ch_mask.calling_bell == other_ch_mask.calling_bell
            {
                continue 'outer; // Skip this `ch_mask` if it is implied by another mask
            }
        }
        deduped_ch_masks.push(ch_mask.clone());
    }
    Ok(deduped_ch_masks)
}

/// Generate an error if any course could be given two course heads at **different** lead
/// locations.
///
/// For example, in a method with Plain Bob lead heads, `1xxxxx78` and `1x56x8x7` would create an
/// ambiguity because some courses could either be labelled `15xxx678` or `1x56x8x7`.
fn check_for_ambiguous_courses(
    course_head_masks: &[CourseHeadMask],
    lead_heads: &[RowBuf],
) -> Result<()> {
    for (i, ch_mask) in course_head_masks.iter().enumerate() {
        let mask = ch_mask.mask();
        // `skip(1)` removes rounds, since it's OK for multiple `CourseHeadMask`s to agree on the
        // same lead.
        for lead_head in lead_heads.iter().skip(1) {
            // ... compute mask for this lead head at this location ...
            let transposed_mask = mask.mul(lead_head);
            // ... and check that it isn't compatible with any of the course head mask we've
            // seen so far.  We only need to check each pair of CH masks once (since ambiguity
            // is symmetric).
            for other_ch_mask in &course_head_masks[..=i] {
                let other_mask = other_ch_mask.mask();
                match transposed_mask.combine(other_ch_mask.mask()) {
                    // If the lead-end is compatible with a course head mask, then derive which
                    // leads are ambiguous, and return an error
                    Some(course_head_2) => {
                        // Generate the second course head by transposing `mask2` at this lead
                        // back to the course head satisfying `mask`
                        let course_head_1 = other_mask.mul(&lead_head.inv()).combine(mask).unwrap();
                        return Err(Error::AmbiguousCourseHeadPosition {
                            mask1: course_head_1,
                            input_mask1: mask.clone(),
                            mask2: course_head_2,
                            input_mask2: other_mask.clone(),
                        });
                    }
                    // If they're not compatible, then no ambiguity is generated
                    None => {}
                }
            }
        }
    }
    Ok(())
}

/////////////////////
// LINK GENERATION //
/////////////////////

/// Generates the set of [`Link`]s which are valid according to the `course_head_masks`
fn generate_links(
    plain_course: &AnnotBlock<method::RowAnnot>,
    course_head_masks: &[CourseHeadMask],
    calls: &[super::Call],
) -> Result<Vec<Link>> {
    /*
    The general approach here is to attempt to place calls in every position, compute which row
    they would lead to, the try to find a `CallEnd` which corresponds to that row.  If such a
    `CallEnd` exists, then this call is valid and should be included in the node graph.  However,
    we can also choose to **not** place a call here, so each valid call position generates _two_
    `Link`s (one for calling the call and one 'plain' `Link` for choosing not to).  This generates
    a lot of duplicated plain `Link`s, so these `Link`s are de-duplicated before returning.
    */

    // This maps lead locations to a list of row indices which occur just before this label.  These
    // correspond to lead **ends**.  For example, Yorkshire Surprise Major with only lead end
    // calls generates `{"LE": [223, 31, 63, 95, 127, 159, 191]}`
    let call_starts_by_label = call_starts_by_label(&plain_course);
    let call_ends = call_ends(&plain_course, &course_head_masks);

    generate_all_links(
        course_head_masks,
        plain_course,
        calls,
        &call_ends,
        &call_starts_by_label,
    )
    .map(dedup_links)
}

/// For each lead label, list the positions in the course where calls at that label could start
/// (Monument assumes that all courses always replace one piece of [`PlaceNot`]ation).
fn call_starts_by_label<'meth>(
    plain_course: &AnnotBlock<method::RowAnnot<'meth>>,
) -> HashMap<&'meth str, Vec<usize>> {
    let course_len = plain_course.len();

    let mut label_indices = HashMap::new();
    for (row_idx, annot) in plain_course.annots().enumerate() {
        if let Some(label) = annot.label() {
            label_indices
                .entry(label)
                .or_insert_with(Vec::new)
                // Calls start at the row **before** the lead label.  We do `+ course_len` to
                // avoid underflow if `row_idx == 0`.
                .push((row_idx + course_len - 1) % course_len);
        }
    }
    label_indices
}

/// A single location where a call could **end**.  Note that this doesn't include information about
/// _which_ calls appear here, just that they _could_.
#[derive(Debug, Clone)]
struct CallEnd<'meth> {
    lead_location: &'meth str,
    row_mask: Mask,
    course_head_mask_idx: usize,
    row_idx: usize,
}

fn call_ends<'meth>(
    plain_course: &AnnotBlock<method::RowAnnot<'meth>>,
    course_head_masks: &[CourseHeadMask],
) -> Vec<CallEnd<'meth>> {
    let mut call_ends = Vec::new();

    // For every labelled row ...
    for (row_idx, annot_row) in plain_course.annot_rows().enumerate() {
        if let Some(label) = annot_row.annot().label() {
            let row_after_call = annot_row.row();
            // ... for every course head mask ...
            for (mask_idx, ch_mask) in course_head_masks.iter().enumerate() {
                // ... add a `CallEnd` corresponding to placing a call at this location to
                // **leave** the course
                call_ends.push(CallEnd {
                    lead_location: label,
                    row_mask: ch_mask.mask().mul(row_after_call),
                    course_head_mask_idx: mask_idx,
                    row_idx,
                });
            }
        }
    }

    // Sanity check that any compatible call-end masks also agree on `row_idx`.  This should be
    // guaranteed by the ambiguity check, but checking is quick enough to be worth it.
    for call_end1 in &call_ends {
        for call_end2 in &call_ends {
            if call_end1.row_mask.is_compatible_with(&call_end2.row_mask) {
                assert_eq!(call_end1.row_idx, call_end2.row_idx);
            }
        }
    }

    call_ends
}

/// Generate all allowed links, potentially with duplicates.
fn generate_all_links(
    course_head_masks: &[CourseHeadMask],
    plain_course: &AnnotBlock<method::RowAnnot>,

    calls: &[super::Call],
    call_ends: &[CallEnd],
    call_starts_by_label: &HashMap<&str, Vec<usize>>,
) -> Result<Vec<Link>> {
    let stage = plain_course.stage();

    let mut links = Vec::<Link>::new();
    for ch_mask in course_head_masks {
        let mask = ch_mask.mask();

        for (call_idx, call) in calls.iter().enumerate() {
            let lead_loc = call.lead_location.as_str();

            // Test this call at every possible (correctly named) location in the course
            for &from_idx in call_starts_by_label
                .get(lead_loc)
                .ok_or_else(|| Error::UndefinedLeadLocation(lead_loc.to_owned()))?
            {
                let row_after_call = call
                    .place_not
                    .permute_new(plain_course.get_row(from_idx).unwrap())
                    .unwrap();
                let mask_after_call = mask.mul(&row_after_call);

                // Find the calling position of this call, returning an error if the call doesn't
                // have enough calling positions
                let tenor_place = mask_after_call
                    .place_of(ch_mask.calling_bell())
                    .expect("Course head mask doesn't fix the calling bell");
                let calling_position =
                    call.calling_positions.get(tenor_place).ok_or_else(|| {
                        Error::CallingPositionsTooShort {
                            call_name: call.debug_symbol.to_owned(),
                            call_idx,
                            calling_position_len: call.calling_positions.len(),
                            stage,
                        }
                    })?;

                for call_end in call_ends {
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
                            course_head_masks[call_end.course_head_mask_idx].mask();
                        // The course head mask for which courses can appear **before** this call.
                        // This mask may be stricter than `ch_mask` if the `call_end`'s CH mask
                        // specifies more bells than `ch_mask`.
                        let source_mask = mask
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
                            to: RowIdx::new(BLOCK_IDX, (from_idx + 1) % plain_course.len()),
                            course_head_mask: source_mask,
                            // Plain leads don't cause a course head transposition
                            course_head_transposition: RowBuf::rounds(stage),
                            debug_name: format!("p{}", calling_position),
                            // Plain leads shouldn't be displayed
                            display_name: String::new(),
                        });
                    }
                }
            }
        }
    }

    Ok(links)
}

/// Remove any [`Link`]s which are equal to another [`Link`] (ignoring names).
///
/// This is required because [`generate_all_links`] creates a large number of identical plain call
/// links if there are multiple calls at the same position (which there almost always are).
///
/// This doesn't actually lead to the generation of duplicate compositions because the graph
/// generation code has to perform de-duplication regardless.  However, de-duplication makes the
/// code both more performant and, more importantly, makes the resulting [`Layout`]s easier to
/// debug.
fn dedup_links(mut links: Vec<Link>) -> Vec<Link> {
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
            // different names (e.g. using 4ths place bobs and singles would generate `pI` and
            // `pT`, which are essentially identical).  If the links do have different names, then
            // one of them is picked arbitrarily.
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

    // Now actually remove the unnecessary links, making sure to iterate backwards so that the
    // indices keep pointing to the right elements
    redundant_link_idxs.sort();
    redundant_link_idxs.dedup();
    for idx in redundant_link_idxs.into_iter().rev() {
        links.remove(idx);
    }
    links
}

/////////////////
// STARTS/ENDS //
/////////////////

fn rounds_locations(
    course_head_masks: &[CourseHeadMask],
    plain_course: &AnnotBlock<method::RowAnnot>,
    allowed_sub_lead_indices: Option<&[usize]>,
    snap_label: &str,
) -> Vec<StartOrEnd> {
    let rounds = RowBuf::rounds(plain_course.stage());

    let mut positions = Vec::new();
    for ch_mask in course_head_masks {
        for (row_idx, annot_row) in plain_course.annot_rows().enumerate() {
            let transposed_mask = ch_mask.mask().mul(annot_row.row());
            // If rounds satisfies `transposed_mask`, then this location can contain rounds
            if transposed_mask.matches(&rounds) {
                // Decide whether this is snap start/finish
                let sub_lead_index = annot_row.annot().sub_lead_idx();
                let is_snap = sub_lead_index != 0;
                let course_head_containing_rounds = annot_row.row().inv();

                if allowed_sub_lead_indices.map_or(true, |idxs| idxs.contains(&sub_lead_index)) {
                    positions.push(StartOrEnd {
                        course_head: course_head_containing_rounds.clone(),
                        row_idx: RowIdx::new(BLOCK_IDX, row_idx),
                        label: (if is_snap { snap_label } else { "" }).to_owned(),
                    });
                }
            }
        }
    }

    positions
}
