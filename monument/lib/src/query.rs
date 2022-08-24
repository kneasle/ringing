//! Instructions for Monument about what compositions should be generated.

use std::ops::RangeInclusive;

use bellframe::{music::Pattern, Bell, Mask, PlaceNot, RowBuf, Stage, Stroke};
use ordered_float::OrderedFloat;
use serde::Deserialize;

use crate::{
    utils::{group::PartHeadGroup, PerPartLength, Score, TotalLength},
    OptRange,
};

/// Information provided to Monument which specifies what compositions are generated.
///
/// Compare this to [`Config`](crate::Config), which determines _how_ those compositions are
/// generated (and therefore determines how quickly the results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    // GENERAL
    pub length_range: RangeInclusive<usize>,
    pub num_comps: usize,
    pub allow_false: bool,
    pub stage: Stage,

    // METHODS & CALLING
    pub methods: MethodVec<Method>,
    pub calls: CallVec<Call>,
    // TODO: Make this defined per-method?
    pub call_display_style: CallDisplayStyle,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32,

    // COURSES
    //
    // (CH masks are defined on each `Method`)
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head_group: PartHeadGroup,
    /// The `f32` is the weight given to every row in any course matching the given [`Mask`]
    pub ch_weights: Vec<(Mask, f32)>,

    // MUSIC
    pub music_types: MusicTypeVec<MusicType>,
    /// The [`Stroke`] of the first [`Row`](bellframe::Row) in the composition that isn't
    /// `self.start_row`
    pub start_stroke: Stroke,
}

impl Query {
    /// Same as [`Query::length_range`], but with the lengths represented as [`TotalLength`]s.
    pub(crate) fn total_length_range(&self) -> RangeInclusive<TotalLength> {
        let start = TotalLength::new(*self.length_range.start());
        let end = self.max_length();
        start..=end
    }

    pub(crate) fn max_length(&self) -> TotalLength {
        TotalLength::new(*self.length_range.end())
    }

    pub fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    pub fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }

    pub fn is_spliced(&self) -> bool {
        self.methods.len() > 1
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub inner: bellframe::Method,
    pub shorthand: String,

    /// The number of rows of this method must fit within this range
    pub count_range: OptRange,
    /// The indices in which we can start a composition during this `Method`.  `None` means any
    /// index is allowed (provided the CH masks are satisfied).  These are interpreted modulo the
    /// lead length of the method.
    pub start_indices: Vec<isize>,
    /// The indices in which we can end a composition during this `Method`.  `None` means any index
    /// is allowed (provided the CH masks are satisfied).  These are interpreted modulo the lead
    /// length of the method.
    pub end_indices: Option<Vec<isize>>,
    pub ch_masks: Vec<Mask>,
}

impl Method {
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

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone)]
pub struct Call {
    pub display_symbol: String,
    pub debug_symbol: String,
    pub calling_positions: Vec<String>,

    pub label_from: String,
    pub label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub place_not: PlaceNot,

    pub weight: f32,
}

/// How the calls in a given composition should be displayed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallDisplayStyle {
    /// Calls should be displayed as a count since the last course head
    Positional,
    /// Calls should be displayed based on the position of the provided 'observation' [`Bell`]
    CallingPositions(Bell),
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

///////////
// MUSIC //
///////////

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub struct MusicType {
    pub patterns: Vec<Pattern>,
    pub strokes: StrokeSet,

    pub weight: Score,
    pub count_range: OptRange,
}

impl MusicType {
    pub fn new(
        patterns: Vec<Pattern>,
        stroke_set: StrokeSet,
        weight: f32,
        count_range: OptRange,
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
