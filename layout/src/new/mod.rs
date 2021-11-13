//! Utility code for building [`Layout`]s in common scenarios.

use std::collections::HashSet;

use bellframe::{Bell, Mask, PlaceNot, Stage};
use itertools::Itertools;
use serde::Deserialize;

use crate::Link;

pub mod coursewise;
pub mod leadwise;

/// The different styles of spliced that can be generated
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label
    #[serde(rename = "leads")]
    LeadLabels,
    /// Splice only happen whenever a call _could_ have happened
    #[serde(rename = "call locations")]
    CallLocations,
    /// Splices only happen when calls are actually made
    #[serde(rename = "calls")]
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

/// Label used when the comp starts part-way through a lead
const SNAP_START_LABEL: &str = "<";
/// Label used when the comp finishes part-way through a lead
const SNAP_FINISH_LABEL: &str = ">";

//////////////
// CH MASKS //
//////////////

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
    pub(crate) fn new(mask: Mask, calling_bell: Bell) -> Vec<Self> {
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

///////////
// CALLS //
///////////

/// The specification for a call that can be used in a composition
#[derive(Debug, Clone)]
pub struct Call {
    display_symbol: String,
    debug_symbol: String,
    calling_positions: Vec<String>,

    lead_location: String,
    place_not: PlaceNot,

    weight: f32,
}

impl Call {
    pub fn new(
        display_symbol: String,
        debug_symbol: String,
        calling_positions: Option<Vec<String>>,
        lead_location: String,
        place_not: PlaceNot,
        weight: f32,
    ) -> Self {
        Self {
            display_symbol,
            debug_symbol,
            calling_positions: calling_positions
                .unwrap_or_else(|| default_calling_positions(&place_not)),
            lead_location,
            place_not,
            weight,
        }
    }

    pub fn set_weight(&mut self, weight: f32) {
        self.weight = weight;
    }

    ////////////////////////
    // DEFAULT CALL TYPES //
    ////////////////////////

    /// Generates `14` bob and `1234` single, both at the lead end (i.e. label `"LE"`).  Returns
    /// `None` for any [`Stage`] smaller than [`Stage::MINIMUS`].
    pub fn near_calls(stage: Stage) -> Option<Vec<Self>> {
        let bob = Self::lead_end_bob(PlaceNot::parse("14", stage).ok()?);
        let single = Self::lead_end_bob(PlaceNot::parse("1234", stage).ok()?);
        Some(vec![bob, single])
    }

    /// Generates `1(n-2)` bob and `1(n-2)(n-1)n` single, both at the lead end (i.e. label `"LE"`).
    /// Returns `None` for any [`Stage`] smaller than [`Stage::MINIMUS`].
    pub fn far_calls(stage: Stage) -> Option<Vec<Self>> {
        if stage < Stage::MINIMUS {
            return None;
        }

        let n = stage.num_bells();
        // Unsafety and unwrapping is OK because, in both cases, the places are sorted and within
        // the stage (because we early return when `n < 4`).
        let bob_notation = unsafe { PlaceNot::from_sorted_slice(&[1, n - 2], stage).unwrap() };
        let single_notation =
            unsafe { PlaceNot::from_sorted_slice(&[1, n - 2, n - 1, n], stage).unwrap() };

        let bob = Self::lead_end_bob(bob_notation);
        let single = Self::lead_end_bob(single_notation);
        Some(vec![bob, single])
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_bob(place_not: PlaceNot) -> Self {
        Self::new(
            String::new(),
            "-".to_owned(),
            None,
            bellframe::method::LABEL_LEAD_END.to_owned(),
            place_not,
            -1.8, // Slightly punish bobs
        )
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_single(place_not: PlaceNot) -> Self {
        Self::new(
            "s".to_owned(),
            "s".to_owned(),
            None,
            bellframe::method::LABEL_LEAD_END.to_owned(),
            place_not,
            -2.3, // Punish singles slightly more than bobs
        )
    }
}

#[allow(clippy::branches_sharing_code)]
fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
    let named_positions = "LIBFVXSEN"; // TODO: Does anyone know any more than this?

    // Generate calling positions that aren't M, W or H
    let mut positions =
        // Start off with the single-char position names
        named_positions
        .chars()
        .map(|c| c.to_string())
        // Extending forever with numbers (extended with `ths` to avoid collisions with positional
        // calling positions)
        .chain((named_positions.len()..).map(|i| format!("{}ths", i + 1)))
        // But we consume one value per place in the Stage
        .take(place_not.stage().num_bells())
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
            if let Some(place) = place_not.stage().num_bells().checked_sub(1 + $ind) {
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

////////////////////////////////////////////////////
// UTILITIES COMMON BETWEEN LEAD- AND COURSE-WISE //
////////////////////////////////////////////////////

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
fn dedup_links(links: &mut Vec<Link>) {
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

/// Returns the place bells which are always preserved by plain leads and all calls (e.g. hunt
/// bells in non-variable-hunt compositions).
fn fixed_bells(
    methods: &[(bellframe::Method, String)],
    calls_per_method: &[Vec<&self::Call>],
    stage: Stage,
) -> Vec<Bell> {
    // Start off with all bells fixed
    let mut all_fixed_bells = stage.bells().collect_vec();
    for ((method, _shorthand), calls) in methods.iter().zip_eq(calls_per_method) {
        // Start the set with the bells which are fixed by the plain lead of every method
        let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
        for call in calls {
            // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
            // Grandsire is unaffected by a plain lead, but affected by calls)
            filter_bells_fixed_by_call(method, call, &mut fixed_bells);
        }
        // Intersect the fixed bells of this method with the full list
        all_fixed_bells.retain(|b| fixed_bells.contains(b));
    }
    all_fixed_bells
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(
    method: &bellframe::Method,
    call: &self::Call,
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
