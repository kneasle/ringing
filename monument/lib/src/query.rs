//! Instructions for Monument about what compositions should be generated.

use std::{
    ops::RangeInclusive,
    sync::{atomic::AtomicBool, Arc},
};

use bellframe::{music::Pattern, Bell, Mask, PlaceNot, Row, RowBuf, Stage, Stroke};
use ordered_float::OrderedFloat;
use serde::Deserialize;

use crate::{
    group::PartHeadGroup,
    search::{SearchData, SearchUpdate},
    utils::{PerPartLength, Score, TotalLength},
    Composition, Config, OptRange,
};

/// Specification for what [`Composition`]s should be generated.  These should be created using a
/// [`QueryBuilder`].
///
/// Compare this to [`Config`](crate::Config), which determines _how_ those compositions are
/// generated (and therefore determines how quickly the results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    // GENERAL
    pub(crate) length_range: RangeInclusive<TotalLength>,
    pub(crate) stage: Stage,
    pub(crate) num_comps: usize,
    pub(crate) allow_false: bool, // TODO: Rename to `require_truth`

    // METHODS & CALLING
    pub(crate) methods: MethodVec<Method>,
    pub(crate) splice_style: SpliceStyle,
    pub(crate) splice_weight: Score,
    pub(crate) calls: CallVec<Call>,
    pub(crate) call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?

    // COURSES
    //
    // (CH masks are defined on each `Method`)
    pub(crate) start_row: RowBuf,
    pub(crate) end_row: RowBuf,
    pub(crate) part_head_group: PartHeadGroup,
    /// [`Score`]s applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub(crate) ch_weights: Vec<(Mask, Score)>,

    // MUSIC
    pub(crate) music_types: MusicTypeVec<MusicType>,
    /// The [`Stroke`] of the first [`Row`] in the composition that isn't `self.start_row`
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

    /// Does this `Query` generate [`Composition`]s with more than one part?
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

/////////////
// BUILDER //
/////////////

/// Builder API for creating [`Query`]s.
pub struct QueryBuilder {
    query: Query,
}

/// A length range of a [`Query`].
pub enum Length {
    /// Practice night touch.  Equivalent to `Custom(0..=300)`.
    Practice,
    /// Equivalent to `Custom(1250..=1350)`.
    QuarterPeal,
    /// Equivalent to `Custom(2500..=2600)`.
    HalfPeal,
    /// Equivalent to `Custom(5000..=5200)`.
    Peal,
    /// Custom range
    Range(RangeInclusive<usize>),
}

impl QueryBuilder {
    /// Finish building and run the search with the default [`Config`], blocking until the required
    /// compositions have been generated.
    pub fn run(self) -> crate::Result<Vec<Composition>> {
        self.run_with_config(&Config::default())
    }

    /// Finish building and run the search with a custom [`Config`], blocking until the required
    /// number of [`Composition`]s have been generated.
    pub fn run_with_config(self, config: &Config) -> crate::Result<Vec<Composition>> {
        let mut comps = Vec::<Composition>::new();
        let update_fn = |update| {
            if let SearchUpdate::Comp(comp) = update {
                comps.push(comp);
            }
        };
        let abort_flag = Arc::new(AtomicBool::new(true));
        SearchData::new(&self.build(), config)?.search(abort_flag, update_fn);
        Ok(comps)
    }

    /// Finish building and return the [`Query`].
    pub fn build(self) -> Query {
        self.query
    }
}

impl QueryBuilder {
    /* LENGTHS */

    /// Create a new `QueryBuilder` with a custom length range
    pub fn new(stage: Stage, length: Length) -> Self {
        let (min_length, max_length) = match length {
            Length::Practice => (0, 300),
            Length::QuarterPeal => (1250, 1350),
            Length::HalfPeal => (2500, 2600),
            Length::Peal => (5000, 5200),
            Length::Range(range) => (*range.start(), *range.end()),
        };

        QueryBuilder {
            query: Query {
                length_range: TotalLength::new(min_length)..=TotalLength::new(max_length),
                num_comps: 100,
                allow_false: false,
                stage,

                methods: MethodVec::new(),
                splice_style: SpliceStyle::LeadLabels,
                splice_weight: OrderedFloat(0.0),
                calls: CallVec::new(),
                call_display_style: CallDisplayStyle::CallingPositions(stage.tenor()),

                start_row: RowBuf::rounds(stage),
                end_row: RowBuf::rounds(stage),
                part_head_group: PartHeadGroup::one_part(stage),
                ch_weights: Vec::new(),

                music_types: MusicTypeVec::new(),
                start_stroke: Stroke::Hand,
            },
        }
    }

    /* GENERAL PARAMETERS */

    /// Sets how many [`Composition`]s Monument will generate.  If unset, defaults to 100.
    pub fn num_comps(mut self, num_comps: usize) -> Self {
        self.query.num_comps = num_comps;
        self
    }

    /// Allow Monument to generate false [`Composition`]s.  By default, Monument requires truth.
    pub fn allow_false(mut self, allow_false: bool) -> Self {
        self.query.allow_false = allow_false;
        self
    }

    /* METHODS */

    /// Add some [`Method`]s that can be used in the [`Composition`]s.
    pub fn add_methods(mut self, methods: impl IntoIterator<Item = Method>) -> Self {
        self.query.methods.extend(methods);
        self
    }

    /// Determines when splices are allowed.  If unset, defaults to [`SpliceStyle::LeadLabels`].
    pub fn splice_style(mut self, style: SpliceStyle) -> Self {
        self.query.splice_style = style;
        self
    }

    /// Sets the score applied every time the conductor would have to call a method splice.  If
    /// unset, defaults to `0.0`.
    pub fn splice_weight(mut self, weight: f32) -> Self {
        self.query.splice_weight = OrderedFloat(weight);
        self
    }

    /* CALLS */

    /// Adds some custom [`Call`]s to be used in [`Composition`]s.  This can be called multiple
    /// times without calls being overwritten.
    pub fn add_calls(mut self, calls: impl IntoIterator<Item = Call>) -> Self {
        self.query.calls.extend(calls);
        self
    }

    /// Sets the score applied every time the conductor would have to call a method splice.  If
    /// unset, defaults to `0.0`.
    pub fn call_display_style(mut self, style: CallDisplayStyle) -> Self {
        self.query.call_display_style = style;
        self
    }

    /* COURSES */

    /// Sets the [`Row`]s at which the [`Composition`]s must start and finish.
    /// Defaults to starting and finishing in [`rounds`](RowBuf::rounds).
    pub fn start_end_rows(mut self, start_row: RowBuf, end_row: RowBuf) -> Self {
        self.query.start_row = start_row;
        self.query.end_row = end_row;
        self
    }

    /// Sets the part heads to be the [`closure`](Row::closure) of some [`Row`].  Monument will
    /// make sure that every part head is actually generated.
    pub fn part_head(mut self, row: &Row) -> Self {
        self.query.part_head_group = PartHeadGroup::new(row);
        self
    }

    /// Weights applied to every [`Row`] of every course which contains a lead head which satisfies
    /// the corresponding [`Mask`].
    pub fn course_head_weights(mut self, weights: impl IntoIterator<Item = (Mask, f32)>) -> Self {
        self.query.ch_weights.extend(
            weights
                .into_iter()
                .map(|(mask, weight)| (mask, OrderedFloat(weight))),
        );
        self
    }

    /* MUSIC */

    /// Adds [`MusicType`]s on which [`Composition`]s are scored.
    pub fn music_types(mut self, types: impl IntoIterator<Item = MusicType>) -> Self {
        self.query.music_types.extend(types);
        self
    }

    /// Sets the [`Stroke`] of the first [`Row`] that isn't the start row.  Defaults to
    /// [`Stroke::Hand`].
    pub fn start_stroke(mut self, stroke: Stroke) -> Self {
        self.query.start_stroke = stroke;
        self
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
