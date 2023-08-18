//! Instructions for Monument about what compositions should be generated.

use std::{
    collections::HashSet,
    marker::PhantomData,
    ops::{Range, RangeInclusive},
    sync::atomic::AtomicBool,
};

use bellframe::{
    method::LABEL_LEAD_END, music::Pattern, Bell, Mask, PlaceNot, RowBuf, Stage, Stroke,
};
use itertools::Itertools;

use crate::{
    group::PartHeadGroup,
    utils::lengths::{PerPartLength, TotalLength},
    Composition, Config, Search, Update,
};

/// Fully built specification for which [`Composition`](crate::Composition)s should be generated.
///
/// Compare this to [`search::Config`](crate::search::Config), which determines _how_ those
/// [`Composition`](crate::Composition)s are generated (and therefore determines how quickly the
/// results are generated).
#[derive(Debug, Clone)]
pub struct Parameters {
    // GENERAL
    pub length: RangeInclusive<TotalLength>,
    pub stage: Stage,
    pub num_comps: usize,
    pub require_truth: bool,

    // METHODS & CALLING
    pub maybe_unused_methods: Vec<Method>,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32, // TODO: Do we need so many instances of 'Score'
    pub maybe_unused_calls: Vec<Call>,
    pub call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?
    pub atw_weight: Option<f32>,

    // COURSES
    //
    // NOTE: Course masks are defined on each `Method`
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    // TODO: Explicitly compute PH group, leave user to just input rows?
    pub part_head_group: PartHeadGroup,
    /// [`Score`]s applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub course_weights: Vec<(Mask, f32)>,

    // NON-DUFFERS
    pub max_contiguous_duffer: Option<PerPartLength>,
    pub max_total_duffer: Option<TotalLength>,

    // MUSIC
    pub maybe_unused_music_types: Vec<MusicType>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    // TODO: Compute this automatically from sub-lead index
    pub start_stroke: Stroke,
}

impl Parameters {
    /// Finish building and run the search with the default [`Config`], blocking until the required
    /// compositions have been generated.
    pub fn run(self) -> crate::Result<Vec<Composition>> {
        self.run_with_config(Config::default())
    }

    /// Finish building and run the search with a custom [`Config`], blocking until the required
    /// number of [`Composition`]s have been generated.
    pub fn run_with_config(self, config: Config) -> crate::Result<Vec<Composition>> {
        let mut comps = Vec::<Composition>::new();
        let update_fn = |update| {
            if let Update::Comp(comp) = update {
                comps.push(comp);
            }
        };
        Search::new(self, config)?.run(update_fn, &AtomicBool::new(false));
        Ok(comps)
    }

    pub fn max_length(&self) -> TotalLength {
        *self.length.end()
    }

    // TODO: Remove this, now that the field is public anyway
    pub fn length_range_usize(&self) -> RangeInclusive<usize> {
        let start = self.length.start().as_usize();
        let end = self.length.end().as_usize();
        start..=end
    }

    pub fn is_spliced(&self) -> bool {
        self.methods_used() > 1
    }

    pub fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    pub fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }

    pub fn methods_used(&self) -> usize {
        self.maybe_unused_methods.iter().filter(|m| m.used).count()
    }

    pub fn calls_used(&self) -> usize {
        self.maybe_unused_calls.iter().filter(|c| c.used).count()
    }
}

/////////////
// METHODS //
/////////////

/// A `Method` used in a [`Query`].
#[derive(Debug, Clone)]
pub struct Method {
    pub id: MethodId,
    pub used: bool,
    pub inner: bellframe::Method,

    /// Short [`String`] used to identify this method in spliced.  If empty, a default value will
    /// be generated.
    pub custom_shorthand: String,

    /// The number of rows of this method must fit within this range
    pub count_range: OptionalRangeInclusive,

    /// The indices in which we can start a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub start_indices: Vec<isize>,
    /// The indices in which we can end a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub end_indices: Vec<isize>,

    /// The [`Mask`]s which *course heads* must satisfy, as set by [`crate::SearchBuilder::courses`].
    pub allowed_courses: Vec<CourseSet>,
    /// The [`Mask`]s which *course heads* must satisfy, as set by [`crate::SearchBuilder::courses`].
    pub non_duffer_courses: Vec<CourseSet>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId(pub u16);

impl From<u16> for MethodId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<MethodId> for u16 {
    fn from(value: MethodId) -> Self {
        value.0
    }
}

/// The different styles of spliced that can be generated.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label (usually just
    /// [lead ends](bellframe::method::LABEL_LEAD_END)).
    LeadLabels,
    /// Splices only happen at calls.
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

impl Method {
    pub fn shorthand(&self) -> String {
        if self.custom_shorthand.is_empty() {
            default_shorthand(&self.title())
        } else {
            self.custom_shorthand.clone()
        }
    }

    pub fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }
}

impl std::ops::Deref for Method {
    type Target = bellframe::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Get a default shorthand given a method's title.
pub fn default_shorthand(title: &str) -> String {
    title
        .chars()
        .next()
        .expect("Can't have empty method title")
        .to_string()
}

///////////
// CALLS //
///////////

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone)]
pub struct Call {
    pub id: CallId,
    pub used: bool,

    pub symbol: String,
    pub calling_positions: Vec<String>,

    pub label_from: String,
    pub label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub place_notation: PlaceNot,

    pub weight: f32,
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct CallId(pub u16);

impl From<u16> for CallId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<CallId> for u16 {
    fn from(value: CallId) -> Self {
        value.0
    }
}

/// How the calls in a given composition should be displayed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallDisplayStyle {
    /// Calls should be displayed as a count since the last course head.
    Positional,
    /// Calls should be displayed based on the position of the provided 'observation' [`Bell`].
    CallingPositions(Bell),
}

impl Call {
    /// Return the symbol used for this call in compact call strings.  Bobs use an empty string,
    /// while all other calls are unaffected.  Thus, compositions render like `WsWWsWH` rather than
    /// `-WsW-WsW-H`.
    pub(crate) fn short_symbol(&self) -> &str {
        match self.symbol.as_str() {
            "-" | "–" => "", // Convert `-` to ``
            s => s,          // All other calls use their explicit symbol
        }
    }

    /// Create a [`parameters::Call`] which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_call(id: CallId, place_not: PlaceNot, symbol: &str, weight: f32) -> Self {
        Self {
            id,
            used: true,

            symbol: symbol.to_owned(),
            calling_positions: default_calling_positions(&place_not),
            label_from: LABEL_LEAD_END.to_owned(),
            label_to: LABEL_LEAD_END.to_owned(),
            place_notation: place_not,
            weight,
        }
    }
}

/// The different types of [`BaseCalls`] that can be created.
#[derive(Debug, Clone, Copy)]
pub enum BaseCallType {
    /// `14` bobs and `1234` singles
    Near,
    /// `1<n-2>` bobs and `1<n-2><n-1><n>` singles
    Far,
}

/// Default weight given to bobs.
pub const DEFAULT_BOB_WEIGHT: f32 = -1.8;
/// Default weight given to singles.
pub const DEFAULT_SINGLE_WEIGHT: f32 = -2.3;
/// Default weight given to user-specified [`Call`]s.
pub const DEFAULT_MISC_CALL_WEIGHT: f32 = -3.0;

pub fn base_calls(
    id_generator: &mut IdGenerator<CallId>,
    type_: BaseCallType,
    bob_weight: Option<f32>,
    single_weight: Option<f32>,
    stage: Stage,
) -> Vec<Call> {
    let n = stage.num_bells_u8();

    let mut calls = Vec::new();
    // Add bob
    if let Some(bob_weight) = bob_weight {
        let bob_pn = match type_ {
            BaseCallType::Near => PlaceNot::parse("14", stage).unwrap(),
            BaseCallType::Far => PlaceNot::from_slice(&mut [0, n - 3], stage).unwrap(),
        };
        calls.push(Call::lead_end_call(
            id_generator.next(),
            bob_pn,
            "-",
            bob_weight,
        ));
    }
    // Add single
    if let Some(single_weight) = single_weight {
        let single_pn = match type_ {
            BaseCallType::Near => PlaceNot::parse("1234", stage).unwrap(),
            BaseCallType::Far => {
                PlaceNot::from_slice(&mut [0, n - 3, n - 2, n - 1], stage).unwrap()
            }
        };
        calls.push(Call::lead_end_call(
            id_generator.next(),
            single_pn,
            "s",
            single_weight,
        ));
    }

    calls
}

#[allow(clippy::branches_sharing_code)]
pub fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
    let named_positions = "LIBFVXSEN"; // TODO: Does anyone know any more than this?

    // TODO: Replace 'B' with 'O' for calls which don't affect the tenor

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
// MUSIC //
///////////

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub struct MusicType {
    pub id: MusicTypeId,
    pub used: bool,

    pub patterns: Vec<Pattern>,
    pub strokes: StrokeSet,
    pub weight: f32,
    pub count_range: OptionalRangeInclusive,
}

impl MusicType {
    /// Return the total number of possible instances of this music type, or `None` if the
    /// computation caused `usize` to overflow.
    pub(crate) fn max_count(&self) -> Option<usize> {
        let mut sum = 0;
        for r in &self.patterns {
            sum += r.num_matching_rows()?;
        }
        Some(sum)
    }
}

/// A set of at least one [`Stroke`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StrokeSet {
    Hand,
    Back,
    Both,
}

impl StrokeSet {
    pub fn contains(self, stroke: Stroke) -> bool {
        match self {
            Self::Both => true,
            Self::Hand => stroke == Stroke::Hand,
            Self::Back => stroke == Stroke::Back,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MusicTypeId(pub u16);

impl From<u16> for MusicTypeId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<MusicTypeId> for u16 {
    fn from(value: MusicTypeId) -> Self {
        value.0
    }
}

////////////////
// MISC TYPES //
////////////////

/// Convenient description of a set of courses via [`Mask`]s.
#[derive(Debug, Clone)]
pub struct CourseSet {
    /// List of [`Mask`]s, of which courses should match at least one.
    pub masks: Vec<Mask>,
    /// If `true`, the [`Mask`]s will be expanded so that the mask matches on both strokes.  For
    /// example, `"*5678"` might expand to `["*5678", "*6587"]`.
    pub any_stroke: bool,
    /// If `true`, every [`Bell`] in every [`Mask`] will be added/subtracted from to get every
    /// combination.  For example, `"*3456"` would expand to
    /// `["*1234", "*2345", "*3456", "*4567", "*5678"]`.
    pub any_bells: bool,
}

impl CourseSet {
    /// Convert many `CourseSet`s into the corresponding lead head [`Mask`]s.
    pub(crate) fn to_lead_masks(
        allowed_courses: &[CourseSet],
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        allowed_courses
            .iter()
            .flat_map(|c| c.as_lead_masks(method, fixed_bells))
            .collect_vec()
    }

    /// Convert many `CourseSet`s into the corresponding course head [`Mask`]s.
    pub(crate) fn to_course_masks(
        allowed_courses: &[CourseSet],
        method: &Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        allowed_courses
            .iter()
            .flat_map(|c| c.as_course_masks(method, fixed_bells))
            .collect_vec()
    }

    /// Returns a list of [`Mask`]s which match any lead head of the courses represented by `self`.
    fn as_lead_masks(
        &self,
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        let course_masks = self.as_course_masks(method, fixed_bells);
        // Convert *course* head masks into *lead* head masks (course heads are convenient for the
        // user, but Monument is internally based completely on lead heads - courses are only used to
        // interact with the user).
        let mut lead_head_masks = HashSet::new();
        for course_mask in course_masks {
            for lead_head in method.lead_head().closure() {
                lead_head_masks.insert(&course_mask * &lead_head);
            }
        }
        // Remove any lh masks which are a subset of others (for example, if `xx3456` and `xxxx56`
        // are present, then `xx3456` can be removed because it is implied by `xxxx56`).  This is
        // useful to speed up the falseness table generation.  Making `lead_head_masks` a `HashSet`
        // means that perfect duplicates have already been eliminated, so we only need to check for
        // strict subset-ness.
        let mut filtered_lead_head_masks = Vec::new();
        for mask in &lead_head_masks {
            let is_implied_by_another_mask = lead_head_masks
                .iter()
                .any(|mask2| mask.is_strict_subset_of(mask2));
            if !is_implied_by_another_mask {
                filtered_lead_head_masks.push(mask.clone());
            }
        }
        filtered_lead_head_masks
    }

    /// Expand `self` into a single set of [`Mask`]s
    fn as_course_masks(
        &self,
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        let mut course_masks: Vec<Mask> = self.masks.clone();
        // Expand `any_bells`, by offsetting the bells in the mask in every possible way
        if self.any_bells {
            let mut expanded_masks = Vec::new();
            for mask in &course_masks {
                let min_bell = mask.bells().flatten().min().unwrap_or(Bell::TREBLE);
                let max_bell = mask.bells().flatten().max().unwrap_or(Bell::TREBLE);
                let min_offset = -(min_bell.index_u8() as i16);
                let max_offset = (method.stage().num_bells_u8() - max_bell.number()) as i16;
                for offset in min_offset..=max_offset {
                    expanded_masks.push(Mask::from_bells(
                        mask.bells()
                            .map(|maybe_bell| maybe_bell.map(|b| b + offset)),
                    ));
                }
            }
            course_masks = expanded_masks;
        }
        // Expand `any_stroke`
        if self.any_stroke {
            course_masks = course_masks
                .into_iter()
                .flat_map(|mask| [&mask * method.lead_end(), mask])
                .collect_vec();
        }
        // Set fixed bells
        course_masks
            .into_iter()
            .filter_map(|mask| try_fixing_bells(mask, fixed_bells))
            .collect_vec()
    }
}

impl From<Mask> for CourseSet {
    fn from(value: Mask) -> Self {
        Self::from(vec![value])
    }
}

impl From<Vec<Mask>> for CourseSet {
    fn from(masks: Vec<Mask>) -> Self {
        Self {
            masks,
            any_bells: false,
            any_stroke: false,
        }
    }
}

/// Attempt to fix the `fixed_bells` in the [`Mask`], returning `None` if it was not possible.
fn try_fixing_bells(mut mask: Mask, fixed_bells: &[(Bell, usize)]) -> Option<Mask> {
    for &(bell, place) in fixed_bells {
        mask.set_bell(bell, place).ok()?;
    }
    Some(mask)
}

/// An inclusive range where each side is optionally bounded.
///
/// This is essentially a combination of [`RangeInclusive`](std::ops::RangeInclusive)
/// (`min..=max`), [`RangeToInclusive`](std::ops::RangeToInclusive) (`..=max`),
/// [`RangeFrom`](std::ops::RangeFrom) (`min..`) and [`RangeFull`](std::ops::RangeFull) (`..`).
#[derive(Debug, Clone, Copy, Default)]
pub struct OptionalRangeInclusive {
    pub min: Option<usize>,
    pub max: Option<usize>,
}

impl OptionalRangeInclusive {
    /// An [`OptionalRangeInclusive`] which is unbounded at both ends.  Equivalent to
    /// `OptionalRangeInclusive { min: None, max: None }`.
    pub const OPEN: Self = Self {
        min: None,
        max: None,
    };

    /// Returns `true` if at least one of `min` or `max` is set
    pub fn is_set(self) -> bool {
        self.min.is_some() || self.max.is_some()
    }

    /// Applies [`Option::or`] to both `min` and `max`
    pub fn or(self, other: Self) -> Self {
        Self {
            min: self.min.or(other.min),
            max: self.max.or(other.max),
        }
    }

    pub fn or_range(self, other: &Range<usize>) -> Range<usize> {
        let min = self.min.unwrap_or(other.start);
        let max = self
            .max
            .map(|x| x + 1) // +1 because `OptRange` is inclusive
            .unwrap_or(other.end);
        min..max
    }
}

/// Struct which generates unique `Id`s.  Can be used with any of [`MethodId`] or [`CallId`].
#[derive(Debug, Clone)]
pub struct IdGenerator<Id> {
    next_id: u16,
    _id: PhantomData<Id>,
}

impl<Id: From<u16> + Into<u16>> IdGenerator<Id> {
    pub fn starting_at_zero() -> Self {
        Self {
            next_id: 0,
            _id: PhantomData,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Id {
        let id = Id::from(self.next_id);
        self.next_id += 1;
        id
    }
}

index_vec::define_index_type! { pub(crate) struct MethodIdx = usize; }
index_vec::define_index_type! { pub(crate) struct CallIdx = usize; }
index_vec::define_index_type! { pub(crate) struct MusicTypeIdx = usize; }
pub(crate) type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub(crate) type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub(crate) type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;

#[cfg(test)]
mod tests {
    use bellframe::{PlaceNot, Stage};
    use itertools::Itertools;

    /// Converts a string to a list of strings, one of each [`char`] in the input.
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
