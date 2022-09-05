//! Code for the [`QueryBuilder`] API for creating [`Query`]s.

use std::ops::RangeInclusive;

use bellframe::{Mask, Row, RowBuf, Stage, Stroke};
use ordered_float::OrderedFloat;

use crate::{
    group::PartHeadGroup,
    search::{Config, Search, Update},
    utils::TotalLength,
    Composition,
};

use super::{CallDisplayStyle, CallVec, MethodVec, MusicTypeVec, Query, SpliceStyle};

/// Builder API for creating [`Query`]s.
pub struct QueryBuilder {
    query: Query,
}

impl QueryBuilder {
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
        Search::new(self.build(), config)?.run(update_fn);
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
                require_truth: true,
                stage,

                methods: MethodVec::new(),
                splice_style: SpliceStyle::LeadLabels,
                splice_weight: OrderedFloat(0.0),
                calls: CallVec::new(),
                call_display_style: CallDisplayStyle::CallingPositions(stage.tenor()),

                start_row: RowBuf::rounds(stage),
                end_row: RowBuf::rounds(stage),
                part_head_group: PartHeadGroup::one_part(stage),
                course_weights: Vec::new(),

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
        self.query.require_truth = !allow_false;
        self
    }

    /* METHODS */

    /// Add some [`Method`](super::Method)s that can be used in the [`Composition`]s.
    pub fn add_methods(mut self, methods: impl IntoIterator<Item = super::Method>) -> Self {
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

    /// Adds some custom [`Call`](super::Call)s to be used in [`Composition`]s.  This can be called
    /// multiple times without calls being overwritten.
    pub fn add_calls(mut self, calls: impl IntoIterator<Item = super::Call>) -> Self {
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
        self.query.course_weights.extend(
            weights
                .into_iter()
                .map(|(mask, weight)| (mask, OrderedFloat(weight))),
        );
        self
    }

    /* MUSIC */

    /// Adds [`MusicType`](super::MusicType)s on which [`Composition`]s are scored.
    pub fn music_types(mut self, types: impl IntoIterator<Item = super::MusicType>) -> Self {
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

/// The length range of a [`Query`].
pub enum Length {
    /// Practice night touch.  Equivalent to `Range(0..=300)`.
    Practice,
    /// Equivalent to `Range(1250..=1350)`.
    QuarterPeal,
    /// Equivalent to `Range(2500..=2600)`.
    HalfPeal,
    /// Equivalent to `Range(5000..=5200)`.
    Peal,
    /// Custom range
    Range(RangeInclusive<usize>),
}
