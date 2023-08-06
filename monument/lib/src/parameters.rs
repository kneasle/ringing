//! Instructions for Monument about what compositions should be generated.

use std::ops::{Deref, Range, RangeInclusive};

use bellframe::{
    method::LABEL_LEAD_END, music::Pattern, Bell, Block, Mask, PlaceNot, Row, RowBuf, Stage, Stroke,
};
use itertools::Itertools;

use crate::{
    graph::ChunkId,
    group::PartHeadGroup,
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary,
    },
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
    pub methods: MethodVec<Method>,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32, // TODO: Do we need so many instances of 'Score'
    pub calls: CallVec<Call>,
    pub call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?
    pub fixed_bells: Vec<(Bell, usize)>,
    pub atw_weight: Option<f32>,

    // COURSES
    //
    // NOTE: Course masks are defined on each `Method`
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head_group: PartHeadGroup,
    /// [`Score`]s applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub course_weights: Vec<(Mask, f32)>,

    // NON-DUFFERS
    pub max_contiguous_duffer: Option<PerPartLength>,
    pub max_total_duffer: Option<TotalLength>,

    // MUSIC
    pub music_types: MusicTypeVec<MusicType>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    // TODO: Compute this automatically from sub-lead index
    pub start_stroke: Stroke,
}

impl Parameters {
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
        self.methods.len() > 1
    }

    pub fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    pub fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }

    pub fn methods_used(&self) -> usize {
        self.methods.iter().filter(|m| m.used).count()
    }

    pub fn calls_used(&self) -> usize {
        self.calls.iter().filter(|c| c.used).count()
    }

    /// For a given chunk, split that chunk's range into segments where each one falls within a
    /// unique lead.  For example, a chunk with ID `ChunkId { <Little Bob>, 12345678, sub_lead_idx: 2 }`
    /// and length 18 would return the following regions:
    /// ```text
    ///                           12345678
    ///                            1
    ///                  +     +    1
    ///                  |     |     1
    ///                  |     |     1
    ///                  |     |    1
    ///                  |     |   1
    ///                  |     +  1
    ///                  |     +  16482735
    ///                  |     |   1
    ///                  |     |    1
    ///   Original range |     |     1
    ///                  |     |     1
    ///                  |     |    1
    ///                  |     |   1
    ///                  |     +  1
    ///                  |     +  17856342
    ///                  |     |   1
    ///                  |     |    1
    ///                  +     +     1
    ///                      /       1
    ///                     /       1
    ///    Output ranges --/       1
    ///                           1
    /// ```
    pub(crate) fn chunk_lead_regions(
        &self,
        id: &ChunkId,
        length: PerPartLength,
    ) -> Vec<(RowBuf, Range<usize>)> {
        let method = &self.methods[id.method];

        let mut lead_head: RowBuf = id.lead_head.deref().to_owned();
        let mut length_left = length.as_usize();
        let mut sub_lead_idx = id.sub_lead_idx;

        let mut lead_regions = Vec::new();
        while length_left > 0 {
            // Add a region for as much of this lead as we can
            let length_left_in_lead = method.lead_len() - sub_lead_idx;
            let chunk_len = usize::min(length_left_in_lead, length_left);
            let chunk_end = sub_lead_idx + chunk_len;
            lead_regions.push((lead_head.clone(), sub_lead_idx..chunk_end));
            // Move to after this region, moving forward a lead if necessary
            assert!(chunk_len <= method.lead_len());
            length_left -= chunk_len;
            sub_lead_idx += chunk_len;
            assert!(sub_lead_idx <= method.lead_len());
            if sub_lead_idx == method.lead_len() {
                // Next chunk starts in a new lead, so update the lead head accordingly
                sub_lead_idx = 0;
                lead_head *= method.lead_head();
            }
        }
        lead_regions
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

    pub name: String, // TODO: Move this into `bellframe::Method`

    pub inner: bellframe::Method,

    /// Short [`String`] used to identify this method in spliced.  If empty, a default value will
    /// be generated.
    pub custom_shorthand: String,

    // TODO: Move these somewhere else, or compute them
    /// A [`Block`] containing the entire plain course of `inner`.  Each row is annotated with the
    /// labels assigned to that row.
    pub plain_course: Block<Vec<String>>,

    /// The number of rows of this method must fit within this range
    pub count_range: OptionalRangeInclusive,

    /// The indices in which we can start a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub start_indices: Vec<usize>,
    /// The indices in which we can end a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub end_indices: Vec<usize>,

    /// The [`Mask`]s which *course heads* must satisfy, as set by [`crate::SearchBuilder::courses`].
    pub allowed_course_masks: Vec<Mask>,
    /// The [`Mask`]s which lead heads must satisfy in order to be a lead head within
    /// [`crate::SearchBuilder::courses`].
    pub allowed_lead_masks: Vec<Mask>,
    /// List of lead heads which are part of
    /// [`non_duffer_courses`](crate::SearchBuilder::non_duffer_courses).
    pub non_duffer_lead_masks: Vec<Mask>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId(pub u16);

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
            default_shorthand(self.title())
        } else {
            self.custom_shorthand.clone()
        }
    }

    pub fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }

    /// Checks if `row` is a valid lead head in this method (according to the CH masks provided).
    pub(crate) fn is_lead_head_allowed(&self, lead_head: &Row) -> bool {
        self.allowed_lead_masks.iter().any(|m| m.matches(lead_head))
    }

    pub(crate) fn is_lead_head_duffer(&self, lead_head: &Row) -> bool {
        !self
            .non_duffer_lead_masks
            .iter()
            .any(|mask| mask.matches(lead_head))
    }

    pub(crate) fn start_or_end_indices(&self, boundary: Boundary) -> &[usize] {
        match boundary {
            Boundary::Start => &self.start_indices,
            Boundary::End => &self.end_indices,
        }
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

////////////////
// MISC TYPES //
////////////////

// TODO: Replace these with `ID`s and use `HashMap`s for everything
index_vec::define_index_type! { pub struct MethodIdx = usize; }
index_vec::define_index_type! { pub struct CallIdx = usize; }
index_vec::define_index_type! { pub struct MusicTypeIdx = usize; }
pub type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;

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
