//! Instructions for Monument about what compositions should be generated.

use std::ops::{Deref, Range, RangeInclusive};

use bellframe::{music::Pattern, Bell, Block, Mask, PlaceNot, Row, RowBuf, Stage, Stroke};

use crate::{
    builder::{CallDisplayStyle, OptionalRangeInclusive, SpliceStyle},
    graph::ChunkId,
    group::PartHeadGroup,
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary, Score,
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
    pub length_range: RangeInclusive<TotalLength>,
    pub stage: Stage,
    pub num_comps: usize,
    pub require_truth: bool,

    // METHODS & CALLING
    pub methods: MethodVec<Method>,
    pub splice_style: SpliceStyle,
    pub splice_weight: Score,
    pub calls: CallVec<Call>,
    pub call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?
    pub fixed_bells: Vec<(Bell, usize)>,
    pub atw_weight: Option<Score>,

    // COURSES
    //
    // NOTE: Course masks are defined on each `Method`
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head_group: PartHeadGroup,
    /// [`Score`]s applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub course_weights: Vec<(Mask, Score)>,

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
        *self.length_range.end()
    }

    pub fn length_range_usize(&self) -> RangeInclusive<usize> {
        let start = self.length_range.start().as_usize();
        let end = self.length_range.end().as_usize();
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
    /// List of lead heads which are part of
    /// [`non_duffer_courses`](crate::SearchBuilder::non_duffer_courses).
    pub(crate) non_duffer_lead_masks: Vec<Mask>,
}

impl Method {
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

    pub(crate) fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
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
pub struct Call {
    pub symbol: String,
    pub calling_positions: Vec<String>,

    pub label_from: String,
    pub label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub place_notation: PlaceNot,

    pub weight: Score,
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

index_vec::define_index_type! { pub struct MethodIdx = usize; }
index_vec::define_index_type! { pub struct CallIdx = usize; }
index_vec::define_index_type! { pub struct MusicTypeIdx = usize; }
pub type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;
