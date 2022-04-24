//! Utility code for building [`Layout`]s in common scenarios.

use std::{
    fmt::{Display, Formatter},
    ops::Range,
};

use bellframe::{method::RowAnnot, Bell, Block, Mask, PlaceNot, Row, RowBuf, Stage};
use itertools::Itertools;

use crate::{CallVec, SpliceStyle};

use super::{Layout, MethodBlock};

pub mod coursewise;
pub mod leadwise;
mod utils;

impl Layout {
    pub fn new(
        methods: Vec<self::Method>,
        calls: &CallVec<self::Call>,
        calling_bell: Bell,
        splice_style: SpliceStyle,
        part_head: &Row,
        leadwise: Option<bool>,
    ) -> Result<Self> {
        // Set 'coursewise' as the default iff the part head doesn't preserve the positions of
        // the calling bell
        let leadwise = leadwise.unwrap_or_else(|| !part_head.is_fixed(calling_bell));

        if leadwise {
            leadwise::leadwise(&methods, calls, splice_style)
        } else {
            coursewise::coursewise(methods, calls, calling_bell, splice_style, part_head)
        }
    }
}

/// The ways that [`Layout`] creation can fail
#[derive(Debug, Clone)]
pub enum Error {
    NoMethods,
    UndefinedLeadLocation {
        call_name: String,
        label: String,
    },
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
    NoCourseHeadInPart {
        mask_in_first_part: Mask,
        part_head: RowBuf,
        mask_in_other_part: Mask,
    },
    /// Some courses match two different CH masks which specify **different** calling bells.
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

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoMethods => write!(f, "Can't have a composition with no methods"),
            Self::DuplicateShorthand {
                shorthand,
                title1,
                title2,
            } => write!(
                f,
                "Methods {:?} and {:?} share a shorthand ({})",
                title1, title2, shorthand
            ),
            Self::UndefinedLeadLocation { call_name, label } => write!(
                f,
                "Call {:?} refers to a lead location {:?}, which doesn't exist",
                call_name, label
            ), // TODO: Suggest one that does exist
            Self::NoCourseHeadInPart {
                mask_in_first_part,
                part_head,
                mask_in_other_part,
            } => {
                writeln!(f,
                    "course head `{}` becomes `{}` in the part starting `{}`, which isn't in `course_heads`.",
                    mask_in_first_part, mask_in_other_part, part_head
                )?;
                write!(
                    f,
                    "   help: consider adding `{}` to `course_heads`",
                    mask_in_other_part
                )
            }
            Self::CallingPositionsTooShort {
                call_name,
                calling_position_len,
                stage,
            } => {
                write!(
                    f,
                    "Call {:?} only specifies {} calling positions, but the stage is {}",
                    call_name, calling_position_len, stage
                )
            }
            // TODO: Rename 'calling bells' to 'observation bell'
            Self::ConflictingCallingBell((mask1, calling_bell1), (mask2, calling_bell2)) => {
                write!(
                    f,
                    "Conflicting observation bells:\n      {} wants {}\n  but {} wants {}",
                    mask1, calling_bell1, mask2, calling_bell2
                )
            } // TODO: Make a test case for this once custom calling bells are possible
            Self::AmbiguousCourseHeadPosition {
                mask1,
                input_mask1,
                mask2,
                input_mask2,
            } => {
                write!(
                    f,
                    "The same course could be given two different course heads:"
                )?;
                write!(f, "\n     {}, satisfying {}", mask1, input_mask1)?;
                write!(f, "\n  or {}, satisfying {}", mask2, input_mask2)
            }
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum CourseHeadMaskPreset {
    TenorsTogether,
    SplitTenors,
    Custom(Vec<Mask>),
}

impl CourseHeadMaskPreset {
    pub fn into_masks(self, stage: Stage) -> Vec<Mask> {
        match self {
            Self::TenorsTogether => vec![tenors_together_mask(stage)],
            // Only fix the tenor for split tenors comps
            Self::SplitTenors => vec![Mask::fix_bells(stage, vec![stage.tenor()])],
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
        // On Triples and above, fix >=7 (i.e. skip the first 6 bells)
        fixed_bells.extend(stage.bells().skip(6));
    }
    Mask::fix_bells(stage, fixed_bells)
}

/////////////
// METHODS //
/////////////

#[derive(Debug, Clone)]
pub struct Method {
    inner: bellframe::Method,
    shorthand: String,

    /// The number of rows of this method must fit within this [`Range`]
    count_range: Range<usize>,
    /// The indices in which we can start a composition during this `Method`.  `None` means any
    /// index is allowed (provided the CH masks are satisfied).
    ///
    /// **Invariant:** These must be in `0..method.lead_len()`
    start_indices: Option<Vec<usize>>,
    /// The indices in which we can end a composition during this `Method`.  `None` means any index
    /// is allowed (provided the CH masks are satisfied).
    ///
    /// **Invariant:** These must be in `0..method.lead_len()`
    end_indices: Option<Vec<usize>>,
    /// The course head masks, along with which bell is 'calling bell' during that course. Allowing
    /// different calling bells allows us to do things like keep using W,M,H during courses of e.g.
    /// `1xxxxx0987`.
    ch_masks: Vec<utils::CourseHeadMask>,

    /// The plain course of this [`Method`], annotated with sub-lead indices and labels
    plain_course: Block<Annot>,
}

impl Method {
    pub fn new(
        method: bellframe::Method,
        ch_masks: Vec<Mask>,
        calling_bell: Bell,
        shorthand: String,
        count_range: Range<usize>,
        start_indices: Option<&[isize]>,
        end_indices: Option<&[isize]>,
    ) -> Self {
        let convert_indices = |idxs: &[isize]| -> Vec<usize> {
            let l = method.lead_len() as isize;
            idxs.iter()
                .map(|&idx| (((idx % l) + l) % l) as usize)
                .collect_vec()
        };

        Self {
            plain_course: method.plain_course().map_annots(Annot::from),
            start_indices: start_indices.map(convert_indices),
            end_indices: end_indices.map(convert_indices),

            inner: method,

            shorthand,
            count_range,
            ch_masks: ch_masks
                .into_iter()
                .flat_map(|mask| utils::CourseHeadMask::new(mask, calling_bell))
                .collect_vec(),
        }
    }

    fn allowed_indices(&self, boundary: Boundary) -> Option<&[usize]> {
        match boundary {
            Boundary::Start => self.start_indices.as_deref(),
            Boundary::End => self.end_indices.as_deref(),
        }
    }

    fn course_method_block(&self, is_spliced: bool) -> MethodBlock {
        let block = self.plain_course.clone().map_annots_with_index(|idx, _| {
            let sub_lead_idx = idx % self.lead_len();
            (sub_lead_idx == 0 && is_spliced).then(|| self.shorthand.clone())
        });
        MethodBlock {
            block,
            count_range: self.count_range.clone(),
        }
    }
}

impl std::ops::Deref for Method {
    type Target = bellframe::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The annotation given to each row in the plain course of a [`Method`]
#[derive(Debug, Clone)]
struct Annot {
    sub_lead_idx: usize,
    label: Option<String>,
}

impl From<RowAnnot<'_>> for Annot {
    fn from(a: RowAnnot) -> Self {
        Self {
            sub_lead_idx: a.sub_lead_idx(),
            label: a.label().map(str::to_owned),
        }
    }
}

///////////
// CALLS //
///////////

/// The specification for a call that can be used in a composition
#[derive(Debug, Clone)]
pub struct Call {
    pub display_symbol: String,
    pub debug_symbol: String,
    pub calling_positions: Vec<String>,

    pub lead_location: String,
    pub place_not: PlaceNot,

    pub weight: f32,
}

impl Call {
    ////////////////////////
    // DEFAULT CALL TYPES //
    ////////////////////////

    /// Generates `14` bob and `1234` single, both at the lead end (i.e. label `"LE"`).  Returns
    /// `None` for any [`Stage`] smaller than [`Stage::MINIMUS`].
    pub fn near_calls(stage: Stage) -> Option<Vec<Self>> {
        let bob = Self::lead_end_bob(PlaceNot::parse("14", stage).ok()?);
        let single = Self::lead_end_single(PlaceNot::parse("1234", stage).ok()?);
        Some(vec![bob, single])
    }

    /// Generates `1(n-2)` bob and `1(n-2)(n-1)n` single, both at the lead end (i.e. label `"LE"`).
    /// Returns `None` for any [`Stage`] smaller than [`Stage::MINIMUS`].
    pub fn far_calls(stage: Stage) -> Option<Vec<Self>> {
        if stage < Stage::MINIMUS {
            return None;
        }

        let n = stage.num_bells_u8();
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
        Self {
            display_symbol: String::new(),
            debug_symbol: "-".to_owned(),
            calling_positions: default_calling_positions(&place_not),
            lead_location: bellframe::method::LABEL_LEAD_END.to_owned(),
            place_not,
            weight: -1.8, // Slightly punish bobs
        }
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_single(place_not: PlaceNot) -> Self {
        Self {
            display_symbol: "s".to_owned(),
            debug_symbol: "s".to_owned(),
            calling_positions: default_calling_positions(&place_not),
            lead_location: bellframe::method::LABEL_LEAD_END.to_owned(),
            place_not,
            weight: -2.3, // Punish singles slightly more than bobs
        }
    }
}

#[allow(clippy::branches_sharing_code)]
pub fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
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

///////////
// UTILS //
///////////

#[derive(Debug, Clone, Copy)]
enum Boundary {
    Start,
    End,
}

impl Boundary {
    fn is_start(self) -> bool {
        matches!(self, Self::Start)
    }

    fn snap_label(self) -> &'static str {
        match self {
            Self::Start => utils::SNAP_START_LABEL,
            Self::End => utils::SNAP_FINISH_LABEL,
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
