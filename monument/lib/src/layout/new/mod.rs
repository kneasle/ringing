//! Utility code for building [`Layout`]s in common scenarios.

use bellframe::{Bell, Mask, PlaceNot, Row, Stage};
use itertools::Itertools;
use serde::Deserialize;

use super::Layout;

pub mod coursewise;
pub mod leadwise;
mod utils;

impl Layout {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        methods: &[(bellframe::Method, String)],
        calls: &[self::Call],
        splice_style: SpliceStyle,

        ch_preset: CourseHeadMaskPreset,
        part_head: &Row,
        leadwise: Option<bool>,

        start_indices: Option<&[usize]>,
        end_indices: Option<&[usize]>,
    ) -> Result<Self> {
        let stage = methods
            .iter()
            .map(|(m, _)| m.stage())
            .max()
            .expect("Can't find stage of 0 methods");

        let ch_masks = ch_preset.into_masks(stage);

        let leadwise = leadwise.unwrap_or_else(|| {
            // Set 'coursewise' as the default iff the part head doesn't preserve the positions of
            // all calling bells
            !ch_masks
                .iter()
                .all(|(_mask, calling_bell)| part_head.is_fixed(*calling_bell))
        });

        if leadwise {
            leadwise::leadwise(methods, calls, start_indices, end_indices)
        } else {
            coursewise::coursewise(
                methods,
                calls,
                splice_style,
                &ch_masks,
                start_indices,
                end_indices,
            )
        }
    }
}

/// The ways that [`Layout`] creation can fail
#[derive(Debug, Clone)]
pub enum Error {
    NoMethods,
    UndefinedLeadLocation(String),
    DuplicateShorthand {
        shorthand: String,
        title1: String,
        title2: String,
    },
    CallingPositionsTooShort {
        call_name: String,
        calling_position_len: usize,
        stage: Stage,
    },
    /// Some courses match two different [`CourseHeadMask`]s with **different** calling bells.
    ConflictingCallingBell((Mask, Bell), (Mask, Bell)),
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

#[derive(Debug, Clone)]
pub enum CourseHeadMaskPreset {
    TenorsTogether,
    SplitTenors,
    Custom(Vec<(Mask, Bell)>),
}

impl CourseHeadMaskPreset {
    fn into_masks(self, stage: Stage) -> Vec<(Mask, Bell)> {
        let tenor = stage.tenor();
        match self {
            Self::TenorsTogether => vec![(tenors_together_mask(stage), tenor)],
            // Only fix the tenor for split tenors comps
            Self::SplitTenors => vec![(Mask::fix_bells(stage, vec![tenor]), tenor)],
            Self::Custom(ch_masks) => ch_masks,
        }
    }
}

/// Generate the course head mask representing the tenors together.  This corresponds to
/// `xxxxxx7890ET...` or just the tenor.
fn tenors_together_mask(stage: Stage) -> Mask {
    let mut fixed_bells = vec![];
    if stage <= Stage::MINOR {
        // On Minor or below, only fix the tenor
        fixed_bells.push(stage.tenor());
    } else {
        // On stages above minor, fix 7-tenor.  Note that we're using 0-indexing here so bell #6 is
        // actually the 7th
        fixed_bells.extend((6..stage.num_bells()).map(Bell::from_index));
    }
    Mask::fix_bells(stage, fixed_bells)
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
