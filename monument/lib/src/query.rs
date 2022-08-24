//! Instructions for Monument about what compositions should be generated.

use std::{fmt::Write, ops::RangeInclusive};

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
    // TODO: Make this defined per-method?  Or per-stage?
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
    pub music_displays: Vec<MusicDisplay>,
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

/// How music counts can be displayed.
///
/// This could take its counts from multiple [`MusicType`]s, and takes a few forms:
/// 1. Just a total count: e.g. `*5678: 23`
/// 2. Just a breakdown: e.g. `5678s: 24f,81i,24b`
/// 3. A breakdown and a total count: e.g. `4-runs: 312 (73f,132i,107b)`
///
/// Setting all sources to `None` is allowed, but will create an empty column.
#[derive(Debug, Clone)]
pub struct MusicDisplay {
    /// The name used to identify this type of music
    pub name: String,

    /// The index of the [`MusicType`] which provides the total count
    pub source_total: Option<MusicTypeIdx>,

    /// The index of the [`MusicType`] which provides the count off the front
    pub source_front: Option<MusicTypeIdx>,
    /// The index of the [`MusicType`] which provides the internal count
    pub source_internal: Option<MusicTypeIdx>,
    /// The index of the [`MusicType`] which provides the count off the back
    pub source_back: Option<MusicTypeIdx>,
}

impl MusicDisplay {
    /// Creates a new [`MusicDisplay`] with no corresponding [`MusicType`]s
    pub fn empty(name: String) -> Self {
        Self {
            name,
            source_total: None,
            source_front: None,
            source_internal: None,
            source_back: None,
        }
    }

    /// Return the width of the smallest column large enough to be guaranteed to hold (almost)
    /// every instance of this [`MusicDisplay`] (assuming rows can't be repeated).
    pub fn col_width(&self, music_types: &MusicTypeVec<MusicType>) -> usize {
        // We always pad the counts as much as required, so displaying a set of 0s results in a
        // maximum-width string (i.e. all output strings are the same length)
        let max_count_width = self
            .display_counts(music_types, &vec![0; music_types.len()])
            .len();
        max_count_width.max(self.name.len())
    }

    /// Generate a compact string representing a given set of music counts
    pub fn display_counts(
        &self,
        music_types: &MusicTypeVec<MusicType>,
        counts: &[usize],
    ) -> String {
        let mut s = String::new();

        // Add total count
        if let Some(total_idx) = self.source_total {
            write!(
                s,
                "{:>width$}",
                counts[total_idx.index()],
                width = max_count_len(&music_types[total_idx])
            )
            .unwrap();
        }

        // Add specific counts (if there are any)
        if self.source_front.is_some()
            || self.source_internal.is_some()
            || self.source_back.is_some()
        {
            // Add brackets if there's a total score
            if self.source_total.is_some() {
                s.push_str(" (");
            }
            // Add every front/internal/back count for which we have a source
            let mut is_first_count = true;
            for (source, position_char) in [
                (&self.source_front, 'f'),
                (&self.source_internal, 'i'),
                (&self.source_back, 'b'),
            ] {
                if let Some(music_type_idx) = source {
                    // Add separating comma
                    if !is_first_count {
                        s.push(' ');
                    }
                    is_first_count = false;
                    // Add the number
                    write!(
                        s,
                        "{:>width$}",
                        counts[music_type_idx.index()],
                        width = max_count_len(&music_types[*music_type_idx])
                    )
                    .unwrap();
                    s.push(position_char);
                }
            }
            if self.source_total.is_some() {
                s.push(')'); // Add closing brackets if there's a total score
            }
        }

        s
    }
}

/// Prints the width of the largest count possible for a [`MusicType`] (assuming that rows can't be
/// repeated).
fn max_count_len(music_type: &MusicType) -> usize {
    // Determine how to display the music summary
    let max_music_count = music_type.max_count().unwrap_or(usize::MAX);
    // `min(4)` because we don't expect more than 9999 instances of a music type, even
    // if more theoretically exist
    max_music_count.to_string().len().min(4)
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
