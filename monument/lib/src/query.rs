//! Instructions for Monument about what compositions should be generated.

use std::ops::RangeInclusive;

use bellframe::{music::Pattern, Bell, Block, Mask, PlaceNot, Row, RowBuf, Stage, Stroke};

use crate::{
    builder::{CallDisplayStyle, OptionalRangeInclusive, SpliceStyle},
    group::PartHeadGroup,
    utils::{Boundary, PerPartLength, Score, TotalLength},
};

/// Fully built specification for which [`Composition`](crate::Composition)s should be generated.
///
/// Compare this to [`search::Config`](crate::search::Config), which determines _how_ those
/// [`Composition`](crate::Composition)s are generated (and therefore determines how quickly the
/// results are generated).
#[derive(Debug, Clone)]
pub(crate) struct Query {
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
    pub(crate) fixed_bells: Vec<(Bell, usize)>,

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

    pub(crate) fn length_range_usize(&self) -> RangeInclusive<usize> {
        let start = self.length_range.start().as_usize();
        let end = self.length_range.end().as_usize();
        start..=end
    }

    pub(crate) fn is_spliced(&self) -> bool {
        self.methods.len() > 1
    }

    pub(crate) fn num_parts(&self) -> usize {
        self.part_head_group.size()
    }

    pub(crate) fn is_multipart(&self) -> bool {
        self.num_parts() > 1
    }
}

/////////////
// METHODS //
/////////////

/// A `Method` used in a [`Query`].
#[derive(Debug, Clone)]
pub(crate) struct Method {
    pub(crate) inner: bellframe::Method,
    /// A [`Block`] containing the entire plain course of `inner`.  Each row is annotated with the
    /// labels assigned to that row.
    pub(crate) plain_course: Block<Vec<String>>,

    /// Short [`String`] used to identify this method in spliced
    pub(crate) shorthand: String,
    /// The number of rows of this method must fit within this range
    pub(crate) count_range: OptionalRangeInclusive,

    /// The indices in which we can start a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub(crate) start_indices: Vec<usize>,
    /// The indices in which we can end a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub(crate) end_indices: Vec<usize>,

    /// The [`Mask`]s which *course heads* must satisfy, as set by [`crate::SearchBuilder::courses`].
    pub(crate) allowed_course_masks: Vec<Mask>,
    /// The [`Mask`]s which lead heads must satisfy in order to be a lead head within
    /// [`crate::SearchBuilder::courses`].
    pub(crate) allowed_lead_masks: Vec<Mask>,
}

impl Method {
    pub(crate) fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }

    /// Checks if `row` is a valid lead head in this method (according to the CH masks provided).
    pub(crate) fn is_lead_head(&self, lead_head: &Row) -> bool {
        self.allowed_lead_masks.iter().any(|m| m.matches(lead_head))
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

///////////
// CALLS //
///////////

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone)]
pub(crate) struct Call {
    pub(crate) display_symbol: String,
    pub(crate) debug_symbol: String,
    pub(crate) calling_positions: Vec<String>,

    pub(crate) label_from: String,
    pub(crate) label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub(crate) place_notation: PlaceNot,

    pub(crate) weight: Score,
}

///////////
// MUSIC //
///////////

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub(crate) struct MusicType {
    pub(crate) patterns: Vec<Pattern>,
    pub(crate) strokes: StrokeSet,
    pub(crate) weight: Score,
    pub(crate) count_range: OptionalRangeInclusive,
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
pub(crate) enum StrokeSet {
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

////////////////
// MISC TYPES //
////////////////

index_vec::define_index_type! { pub(crate) struct MethodIdx = usize; }
index_vec::define_index_type! { pub(crate) struct CallIdx = usize; }
index_vec::define_index_type! { pub(crate) struct MusicTypeIdx = usize; }
pub(crate) type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub(crate) type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub(crate) type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;
