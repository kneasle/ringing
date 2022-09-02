//! Instructions for Monument about what compositions should be generated.

mod builder;

use std::ops::{Range, RangeInclusive};

use bellframe::{music::Pattern, Bell, Mask, PlaceNot, RowBuf, Stage, Stroke};
use ordered_float::OrderedFloat;
use serde::Deserialize;

use crate::{
    group::PartHeadGroup,
    utils::{PerPartLength, Score, TotalLength},
};

pub use builder::*;

/// Specification for what [`Composition`](crate::Composition)s should be generated.  These should
/// be created using a [`QueryBuilder`].
///
/// Compare this to [`search::Config`](crate::search::Config), which determines _how_ those
/// [`Composition`](crate::Composition)s are generated (and therefore determines how quickly the
/// results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    // GENERAL
    pub(crate) length_range: RangeInclusive<TotalLength>,
    pub(crate) stage: Stage,
    pub(crate) num_comps: usize,
    pub(crate) require_truth: bool,

    // METHODS & CALLING
    pub(crate) methods: MethodVec<Method>,
    pub(crate) splice_style: SpliceStyle,
    pub(crate) splice_weight: Score,
    pub(crate) calls: CallVec<Call>,
    pub(crate) call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?

    // COURSES
    //
    // NOTE: Course masks are defined on each `Method`
    pub(crate) start_row: RowBuf,
    pub(crate) end_row: RowBuf,
    pub(crate) part_head_group: PartHeadGroup,
    /// [`Score`]s applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub(crate) course_weights: Vec<(Mask, Score)>,

    // MUSIC
    pub(crate) music_types: MusicTypeVec<MusicType>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    pub(crate) start_stroke: Stroke,
}

impl Query {
    pub(crate) fn max_length(&self) -> TotalLength {
        *self.length_range.end()
    }

    pub fn length_range(&self) -> RangeInclusive<usize> {
        let start = self.length_range.start().as_usize();
        let end = self.length_range.end().as_usize();
        start..=end
    }

    pub fn methods(&self) -> &MethodVec<Method> {
        &self.methods
    }

    pub fn music_types(&self) -> &MusicTypeVec<MusicType> {
        &self.music_types
    }

    pub fn is_spliced(&self) -> bool {
        self.methods.len() > 1
    }

    pub fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    /// Does this `Query` generate [`Composition`](crate::Composition)s with more than one part?
    pub fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }

    /// Gets the [`effective_stage`](bellframe::Row::effective_stage) of the part heads used in
    /// this `Query`.  The short form of every possible part head will be exactly this length.
    pub fn effective_part_head_stage(&self) -> Stage {
        self.part_head_group.effective_stage()
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub inner: bellframe::Method,
    pub shorthand: String,

    /// The number of rows of this method must fit within this range
    pub count_range: OptionalRangeInclusive,
    /// The indices in which we can start a composition during this `Method`.  `None` means any
    /// index is allowed (provided the CH masks are satisfied).  These are interpreted modulo the
    /// lead length of the method.
    pub start_indices: Vec<isize>,
    /// The indices in which we can end a composition during this `Method`.  `None` means any index
    /// is allowed (provided the CH masks are satisfied).  These are interpreted modulo the lead
    /// length of the method.
    pub end_indices: Option<Vec<isize>>,
    pub courses: Vec<Mask>,
}

impl Method {
    pub fn shorthand(&self) -> &str {
        &self.shorthand
    }

    pub(crate) fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }
}

impl std::ops::Deref for Method {
    type Target = bellframe::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The different styles of spliced that can be generated
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label
    #[serde(rename = "leads")]
    LeadLabels,
    /// Splices only happen at calls
    #[serde(rename = "calls")]
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone)]
pub struct Call {
    pub(crate) display_symbol: String,
    pub(crate) debug_symbol: String,
    pub(crate) calling_positions: Vec<String>,

    pub(crate) label_from: String,
    pub(crate) label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub(crate) place_notation: PlaceNot,

    pub(crate) weight: Score,
}

/// How the calls in a given composition should be displayed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallDisplayStyle {
    /// Calls should be displayed as a count since the last course head
    Positional,
    /// Calls should be displayed based on the position of the provided 'observation' [`Bell`]
    CallingPositions(Bell),
}

///////////
// MUSIC //
///////////

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub struct MusicType {
    pub patterns: Vec<Pattern>,
    pub strokes: StrokeSet,

    pub weight: Score,
    pub count_range: OptionalRangeInclusive,
}

impl MusicType {
    pub fn new(
        patterns: Vec<Pattern>,
        stroke_set: StrokeSet,
        weight: f32,
        count_range: OptionalRangeInclusive,
    ) -> Self {
        Self {
            patterns,
            weight: OrderedFloat(weight),
            count_range,
            strokes: stroke_set,
        }
    }

    /// Return the total number of possible instances of this music type, or `None` if the
    /// computation caused `usize` to overflow.
    pub fn max_count(&self) -> Option<usize> {
        let mut sum = 0;
        for r in &self.patterns {
            sum += r.num_matching_rows()?;
        }
        Some(sum)
    }
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

/// A set of at least one [`Stroke`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StrokeSet {
    Hand,
    Back,
    Both,
}

impl StrokeSet {
    pub(crate) fn contains(self, stroke: Stroke) -> bool {
        match self {
            Self::Both => true,
            Self::Hand => stroke == Stroke::Hand,
            Self::Back => stroke == Stroke::Back,
        }
    }
}

impl Default for StrokeSet {
    fn default() -> Self {
        StrokeSet::Both
    }
}

//////////////////////
// TYPE DEFINITIONS //
//////////////////////

index_vec::define_index_type! { pub struct MethodIdx = usize; }
index_vec::define_index_type! { pub struct CallIdx = usize; }
index_vec::define_index_type! { pub struct MusicTypeIdx = usize; }
pub type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;
