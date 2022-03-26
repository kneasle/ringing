//! Utility code for generating [`Layout`](crate::layout::Layout)s

use std::{collections::HashSet, ops::Mul};

use bellframe::{mask::BellAlreadySet, Bell, Mask, RowBuf, Stage};
use itertools::Itertools;

use crate::{layout::Link, CallVec};

use super::{Error, Result};

/// Label used when the comp starts part-way through a lead
pub(super) const SNAP_START_LABEL: &str = "<";
/// Label used when the comp finishes part-way through a lead
pub(super) const SNAP_FINISH_LABEL: &str = ">";

/// A way of labelling the calls in a set of courses.
#[derive(Debug, Clone)]
pub struct CourseHeadMask {
    mask: Mask,
    /// The bell who's position determines the names of the [`Call`]s in this course.
    ///
    /// **Invariant**: `mask` must specify a location for this [`Bell`].  This means that every
    /// call at a given position through a course will be given the same name.
    calling_bell: Bell,
}

impl CourseHeadMask {
    /// Converts a [`Mask`] and a `calling_bell` into a set of [`CourseHeadMask`]s which, between
    /// them, match the same rows as the source [`Mask`] but all **explicitly** specify a position
    /// for the `calling_bell`.  This way, if two calls have the same position within a course,
    /// they must be given the same calling position (this makes graph expansion much simpler).
    ///
    /// An example where this expansion is needed is if the tenor (the `8`) is used as a calling
    /// bell for the course mask `12345xxx`.  This generates a situation where the same call is
    /// given different names depending on the exact course head used (e.g. a call at `123458xx`
    /// would be called a `M`, whereas a call at `12345xx8` would be called `H`).
    pub(super) fn new(mask: Mask, calling_bell: Bell) -> Vec<Self> {
        if mask.place_of(calling_bell).is_some() {
            return vec![Self { mask, calling_bell }];
        } else {
            mask.unspecified_places()
                .map(|pl| {
                    let mut new_mask = mask.to_owned();
                    // Unwrap is safe because the calling bell can't already be in the mask
                    new_mask.set_bell(calling_bell, pl).unwrap();
                    Self {
                        mask: new_mask,
                        calling_bell,
                    }
                })
                .collect_vec()
        }
    }

    pub fn mask(&self) -> &Mask {
        &self.mask
    }

    pub fn calling_bell(&self) -> Bell {
        self.calling_bell
    }
}

pub(super) fn preprocess_ch_masks(
    methods: &mut [super::Method],
    calls: &CallVec<super::Call>,
    stage: Stage,
) -> Result<()> {
    set_fixed_bells(methods, calls, stage);
    for m in methods {
        m.ch_masks = super::utils::dedup_ch_masks(&m.ch_masks)?;
    }
    Ok(())
}

/// Detect fixed bells (a place bell which is preserved by all methods' calls and plain leads) and
/// fix it in all the course heads of the methods.  In most cases, this will add the treble as a
/// fixed bell (allowing the falseness detection to use it to reduce the size of the falseness
/// table).
fn set_fixed_bells(methods: &mut [super::Method], calls: &CallVec<super::Call>, stage: Stage) {
    let fixed_bells = fixed_bells(methods, calls, stage);
    for m in methods {
        add_fixed_bells_to_method(m, &fixed_bells);
    }
}

fn add_fixed_bells_to_method(method: &mut super::Method, fixed_bells: &[Bell]) {
    let mut fixed_ch_masks = Vec::with_capacity(method.ch_masks.len());
    'mask_loop: for mut ch_mask in method.ch_masks.iter().cloned() {
        // Attempt to add the fixed bells to this mask
        for &b in fixed_bells {
            if let Err(BellAlreadySet) = ch_mask.mask.fix(b) {
                // If a bell is known to be fixed in its home position but a mask requires it to be
                // outside of its home position, then that mask will never be satisfied and can be
                // removed.  For example, this would remove `x1xxx...` as a course head in Surprise
                // with standard calls because we know that the treble cannot leave 1st place.
                continue 'mask_loop;
            }
        }
        // If all fixed bells could be set, then keep this mask
        fixed_ch_masks.push(ch_mask)
    }
    method.ch_masks = fixed_ch_masks;
}

/// Returns the place bells which are always preserved by plain leads and all calls of all methods
/// (e.g. hunt bells in non-variable-hunt compositions).
pub(super) fn fixed_bells(
    methods: &[super::Method],
    calls: &CallVec<super::Call>,
    stage: Stage,
) -> Vec<Bell> {
    let mut all_bells = stage.bells().collect_vec();
    for m in methods {
        let f = fixed_bells_of_method(m, calls);
        all_bells.retain(|b| f.contains(b));
    }
    all_bells
}

/// Returns the place bells which are always preserved by plain leads and all calls of a single
/// method (e.g. hunt bells in non-variable-hunt compositions).
fn fixed_bells_of_method(method: &super::Method, calls: &CallVec<super::Call>) -> HashSet<Bell> {
    // Start the set with the bells which are fixed by the plain lead of every method
    let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
    for call in calls {
        // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
        // Grandsire is unaffected by a plain lead, but affected by calls)
        filter_bells_fixed_by_call(method, call, &mut fixed_bells);
    }
    fixed_bells
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(
    method: &bellframe::Method,
    call: &super::Call,
    set: &mut HashSet<Bell>,
) {
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
            if ch_mask.mask().is_subset_of(other_ch_mask.mask())
                && ch_mask.calling_bell() == other_ch_mask.calling_bell()
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
/// For example, in a method with Plain Bob lead heads, `1xx3xx78` and `1x56x8x7` would create an
/// ambiguity because some courses could either be labelled `15x3x678` or `1356x8x7`.
pub(super) fn check_for_ambiguous_courses(
    course_head_masks: &[CourseHeadMask],
    lead_heads: &[RowBuf],
) -> Result<()> {
    for (i, ch_mask) in course_head_masks.iter().enumerate() {
        let mask = &ch_mask.mask;
        for lead_head in lead_heads.iter() {
            if lead_head.is_rounds() {
                continue; // It's OK for two CH masks to label the same lead as a course head
            }

            // ... compute mask for this lead head at this location ...
            let transposed_mask = mask.mul(lead_head);
            // ... and check that it isn't compatible with any of the course head mask we've
            // seen so far.  We only need to check each pair of CH masks once (since ambiguity
            // is symmetric).
            for other_ch_mask in &course_head_masks[..=i] {
                let other_mask = &other_ch_mask.mask;
                // If the lead-end is compatible with a course head mask, then derive which
                // leads are ambiguous, and return an error
                if let Some(course_head_2) = transposed_mask.combine(other_mask) {
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
            }
        }
    }
    Ok(())
}

//////////
// MISC //
//////////

/// Return an error if two methods share a shorthand
pub(super) fn check_duplicate_shorthands(methods: &[super::Method]) -> super::Result<()> {
    for (i1, m1) in methods.iter().enumerate() {
        for m2 in &methods[..i1] {
            if m1.shorthand == m2.shorthand {
                return Err(Error::DuplicateShorthand {
                    shorthand: m1.shorthand.to_owned(),
                    title1: m1.title().to_owned(),
                    title2: m2.title().to_owned(),
                });
            }
        }
    }
    Ok(())
}

/// Remove any [`Link`]s which are equal to another [`Link`] (ignoring names).
///
/// This is required because [`generate_all_links`] creates a large number of identical plain call
/// links if there are multiple calls at the same position (which there almost always are).
///
/// This doesn't always actually lead to the generation of duplicate compositions (because there
/// could be two identical calls with different but compatible course head masks), so the graph
/// generation code has to perform de-duplication regardless.  However, de-duplication makes the
/// code both more performant and, more importantly, makes the resulting [`Layout`]s easier to
/// debug.
pub(super) fn dedup_links(links: &mut Vec<Link>) {
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
            // different names (e.g. near near would generate `pI` and `pT` which should be
            // considered identical).  If the links do have different names, then one of them is
            // picked arbitrarily.
            let are_links_otherwise_equal = link.eq_without_name_or_ch_mask(link2);

            if are_links_otherwise_equal {
                if link.ch_mask == link2.ch_mask {
                    // If the links are identical, then we remove the one with the least index.
                    // This way, exactly one link from a group of identical links (the one with the
                    // highest index) will survive
                    if i < i2 {
                        redundant_link_idxs.push(i);
                    }
                } else if link.ch_mask.is_subset_of(&link2.ch_mask) {
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
    redundant_link_idxs.sort_unstable();
    redundant_link_idxs.dedup();
    for idx in redundant_link_idxs.into_iter().rev() {
        links.remove(idx);
    }
}
