//! Builder API for constructing [`Search`]es.

pub mod methods;

use std::ops::{Range, RangeInclusive};

use bellframe::{
    method::LABEL_LEAD_END,
    music::{Elem, Pattern},
    Bell, Mask, PlaceNot, RowBuf, Stage, Stroke,
};
use itertools::Itertools;

use crate::{
    group::PartHeadGroup,
    parameters::{self, CallVec, MethodVec, MusicTypeIdx, MusicTypeVec, Parameters, StrokeSet},
    search::{Config, Search, Update},
    utils::lengths::{PerPartLength, TotalLength},
    Composition,
};

pub use methods::{CourseSet, Method, SpliceStyle};

#[allow(unused_imports)] // Only used for doc comments
use bellframe::Row;

use self::methods::MethodSet;

/// Builder API for constructing searches.
#[derive(Debug)]
pub struct SearchBuilder {
    /* GENERAL */
    length_range: RangeInclusive<TotalLength>,
    pub num_comps: usize,
    pub require_truth: bool,
    stage: Stage,

    /* METHODS */
    methods: MethodVec<(bellframe::Method, Method)>,
    pub default_method_count: OptionalRangeInclusive,
    pub default_start_indices: Vec<isize>,
    pub default_end_indices: EndIndices,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32,
    pub atw_weight: Option<f32>,

    /* CALLS */
    pub base_calls: Option<BaseCalls>,
    pub custom_calls: Vec<Call>,
    pub call_display_style: CallDisplayStyle,

    /* MUSIC */
    music_types: MusicTypeVec<parameters::MusicType>,
    pub start_stroke: Stroke,

    /* COURSES */
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head: RowBuf,
    pub courses: Option<Vec<Mask>>,
    pub course_weights: Vec<(Mask, f32)>,

    /* NON-DUFFERS */
    pub non_duffer_courses: Option<Vec<CourseSet>>,
    pub max_contiguous_duffer: Option<usize>,
    pub max_total_duffer: Option<usize>,
}

impl SearchBuilder {
    /* START */

    /// Start building a [`Search`] with the given [`Method`]s and [`Length`] range.  The
    /// [`Method`]s will be assigned unique [`MethodId`]s, but you won't know which ones
    /// apply to which unless you build a [`MethodSet`] and use [`SearchBuilder::with_method_set`].
    pub fn with_methods(
        methods: impl IntoIterator<Item = Method>,
        length: Length,
    ) -> crate::Result<Self> {
        let method_set = MethodSet {
            vec: methods.into_iter().collect(),
        };
        Self::with_method_set(method_set, length)
    }

    /// Create a new `SearchBuilder` with the given [`Method`]s and [`Length`] range.
    pub fn with_method_set(method_set: MethodSet, length: Length) -> crate::Result<Self> {
        let method_builders = method_set.vec;

        // Process methods
        if method_builders.is_empty() {
            return Err(crate::Error::NoMethods);
        }
        let cc_lib = bellframe::MethodLib::cc_lib().expect("Can't load Central Council library.");
        let mut methods = MethodVec::new();
        let mut stage = Stage::ONE;
        for method_builder in method_builders {
            let bellframe_method = method_builder.bellframe_method(&cc_lib)?;
            stage = stage.max(bellframe_method.stage());
            methods.push((bellframe_method, method_builder));
        }
        // Process length
        let (min_length, max_length) = match length {
            Length::Practice => (0, 300),
            Length::QuarterPeal => (1250, 1350),
            Length::HalfPeal => (2500, 2600),
            Length::Peal => (5000, 5200),
            Length::Range(range) => (*range.start(), *range.end()),
        };
        let length_range = TotalLength::new(min_length)..=TotalLength::new(max_length);

        Ok(SearchBuilder {
            stage,
            length_range,
            num_comps: 100,
            require_truth: true,

            methods,
            default_method_count: OptionalRangeInclusive::OPEN,
            default_start_indices: vec![0], // Just 'normal' starts by default
            default_end_indices: EndIndices::Any,
            splice_style: SpliceStyle::LeadLabels,
            splice_weight: 0.0,
            atw_weight: None,

            base_calls: Some(BaseCalls::default()),
            custom_calls: vec![],
            call_display_style: CallDisplayStyle::CallingPositions(stage.tenor()),

            music_types: MusicTypeVec::new(),
            start_stroke: Stroke::Hand,

            start_row: RowBuf::rounds(stage),
            end_row: RowBuf::rounds(stage),
            part_head: RowBuf::rounds(stage),
            courses: None, // Defer setting the defaults until we know the part head
            course_weights: vec![],

            non_duffer_courses: None, // All courses are non-duffers
            max_contiguous_duffer: None,
            max_total_duffer: None,
        })
    }

    /// Gets the [`Stage`] of the [`Search`] being built.  All extra components ([`Mask`]s,
    /// [`Row`]s, etc.) must all have this [`Stage`], or the builder will panic.
    pub fn get_stage(&self) -> Stage {
        self.stage
    }

    /* SETTERS */

    /// Add [`MusicType`]s from the given [`Iterator`].
    pub fn music_types(mut self, music_types: impl IntoIterator<Item = MusicType>) -> Self {
        self.music_types
            .extend(music_types.into_iter().map(|ty| ty.music_type));
        self
    }

    /// Adds [`course_weights`](Self::course_weights) representing every handbell pair coursing,
    /// each with the given `weight`.
    pub fn handbell_coursing_weight(mut self, weight: f32) -> Self {
        if weight == 0.0 {
            return self; // Ignore weights of zero
        }
        // For every handbell pair ...
        for right_bell in self.stage.bells().step_by(2) {
            let left_bell = right_bell + 1;
            // ... add patterns for `*<left><right>` and `*<right><left>`
            for (b1, b2) in [(left_bell, right_bell), (right_bell, left_bell)] {
                let pattern =
                    Pattern::from_elems([Elem::Star, Elem::Bell(b1), Elem::Bell(b2)], self.stage)
                        .expect("Handbell patterns should always be valid regexes");
                let mask = Mask::from_pattern(&pattern)
                    .expect("Handbell patterns should only have one `*`");
                self.course_weights.push((mask, weight));
            }
        }
        self
    }

    /* FINISH */

    /// Finish building and start the [`Search`].
    pub fn build(self, config: Config) -> crate::Result<Search> {
        Search::new(self.into_query()?, config)
    }

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
        Search::new(self.into_query()?, config)?.run(update_fn);
        Ok(comps)
    }

    /// Finish building and construct the resulting [`Query`]
    fn into_query(self) -> crate::Result<Parameters> {
        let Self {
            stage,
            length_range,
            num_comps,
            require_truth,

            methods,
            default_method_count,
            default_start_indices,
            default_end_indices,
            splice_style,
            splice_weight,
            atw_weight,

            base_calls,
            custom_calls,
            call_display_style,

            music_types,
            start_stroke,

            start_row,
            end_row,
            part_head,
            courses,
            course_weights,

            non_duffer_courses,
            max_contiguous_duffer,
            max_total_duffer,
        } = self;

        // If no courses are set, fix any bell >=7 which aren't affected by the part head.  Usually
        // this will be either all (e.g. 1-part or a part head of `1342` or `124365`) or all (e.g.
        // cyclic), but any other combinations are possible.  E.g. a composition of Maximus with
        // part head of `1765432` will still preserve 8 through 12.
        let courses = courses.unwrap_or_else(|| {
            let tenors_unaffected_by_part_head =
                stage.bells().skip(6).filter(|&b| part_head.is_fixed(b));
            vec![Mask::with_fixed_bells(
                stage,
                tenors_unaffected_by_part_head,
            )]
        });

        // Calls
        let mut calls = CallVec::new();
        if let Some(base_calls) = base_calls {
            calls.extend(base_calls.into_calls(stage));
        }
        calls.extend(custom_calls.into_iter().map(Call::build));

        // Methods
        let fixed_bells = self::methods::fixed_bells(&methods, &calls, &start_row, stage);
        let mut built_methods = MethodVec::new();
        for (bellframe_method, method_builder) in methods {
            built_methods.push(method_builder.build(
                bellframe_method,
                &fixed_bells,
                default_method_count,
                &default_start_indices,
                &default_end_indices,
                &courses,
                non_duffer_courses.as_deref(),
                &part_head,
                stage,
            )?);
        }

        // Non-duffer
        //
        // Here we also force `max_contiguous_duffer` to be no bigger than `max_total_duffer`
        // (which clearly can't happen because we'd blow our entire duffer budget in one place).
        let max_contiguous_duffer = match (max_contiguous_duffer, max_total_duffer) {
            (Some(contiguous), Some(total)) => Some(usize::min(contiguous, total)),
            (None, Some(one_val)) | (Some(one_val), None) => Some(one_val),
            (None, None) => None,
        };

        let part_head_group = PartHeadGroup::new(&part_head);

        Ok(Parameters {
            length: length_range,
            stage,
            num_comps,
            require_truth,

            methods: built_methods,
            splice_style,
            splice_weight,
            calls,
            call_display_style,
            fixed_bells,
            atw_weight,

            start_row,
            end_row,
            course_weights,

            music_types: music_types.into_iter().collect(),
            start_stroke,
            max_contiguous_duffer: max_contiguous_duffer.map(PerPartLength::new),
            max_total_duffer: max_total_duffer.map(TotalLength::new), // TODO: Cap `total <= contiguous`
            part_head_group,
        })
    }
}

///////////
// CALLS //
///////////

// TODO: More negative weights to calls

/// Default weight given to bobs.
pub const DEFAULT_BOB_WEIGHT: f32 = -1.8;
/// Default weight given to singles.
pub const DEFAULT_SINGLE_WEIGHT: f32 = -2.3;
/// Default weight given to user-specified [`Call`]s.
pub const DEFAULT_MISC_CALL_WEIGHT: f32 = -3.0;

/// Builder API for a call.
#[derive(Debug)]
pub struct Call {
    symbol: String,
    calling_positions: Option<Vec<String>>,

    label_from: String,
    label_to: String,
    place_notation: PlaceNot,

    weight: f32,
}

impl Call {
    /// Starts building a call which replaces the [lead end](LABEL_LEAD_END) with a given
    /// [`PlaceNot`]ation and is displayed with the given `symbol`.
    pub fn new(symbol: impl Into<String>, place_notation: PlaceNot) -> Self {
        Self {
            symbol: symbol.into(),
            calling_positions: None, // Compute default calling positions
            label_from: LABEL_LEAD_END.to_owned(),
            label_to: LABEL_LEAD_END.to_owned(),
            place_notation,
            weight: DEFAULT_MISC_CALL_WEIGHT,
        }
    }

    /// Forces the use of a given set of calling positions.  The supplied [`Vec`] contains one
    /// [`String`] for every place, in order, *including leading*.  For example, default calling
    /// positions for near bobs in Major would be `vec!["L", "I", "B", "F", "V", "M", "W", "H"]`.
    pub fn calling_positions(mut self, positions: Vec<String>) -> Self {
        self.calling_positions = Some(positions);
        self
    }

    /// If `Some(_)` is passed, behaves the same as [`Call::calling_positions`].  If `None`
    /// is passed, the `calling_positions` are reverted to the default.
    pub fn maybe_calling_positions(mut self, positions: Option<Vec<String>>) -> Self {
        self.calling_positions = positions;
        self
    }

    /// Sets the label marking where this call can be placed.  If unset, all calls are applied to
    /// the [lead end](LABEL_LEAD_END).
    pub fn label(mut self, label: impl Into<String>) -> Self {
        let label = label.into();
        self.label_from = label.clone();
        self.label_to = label;
        self
    }

    /// Makes this call go `to` a different label than it goes `from`.
    pub fn label_from_to(mut self, from: impl Into<String>, to: impl Into<String>) -> Self {
        self.label_from = from.into();
        self.label_to = to.into();
        self
    }

    /// Sets the weight which is added every time this call is used.
    pub fn weight(mut self, weight: f32) -> Self {
        self.weight = weight;
        self
    }

    /// Builds a [`Call`] into a [`crate::query::Call`].
    fn build(self) -> parameters::Call {
        parameters::Call {
            symbol: self.symbol,
            calling_positions: self
                .calling_positions
                .unwrap_or_else(|| default_calling_positions(&self.place_notation)),

            label_from: self.label_from,
            label_to: self.label_to,
            place_notation: self.place_notation,

            weight: self.weight,
        }
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

////////////////
// BASE CALLS //
////////////////

/// Default bobs and singles created automatically by Monument.
#[derive(Debug, Clone)]
pub struct BaseCalls {
    /// The type of bobs and/or singles generated
    pub ty: BaseCallType,
    /// `None` for no bobs
    pub bob_weight: Option<f32>,
    /// `None` for no singles
    pub single_weight: Option<f32>,
}

/// The different types of [`BaseCalls`] that can be created.
#[derive(Debug, Clone, Copy)]
pub enum BaseCallType {
    /// `14` bobs and `1234` singles
    Near,
    /// `1<n-2>` bobs and `1<n-2><n-1><n>` singles
    Far,
}

impl BaseCalls {
    fn into_calls(self, stage: Stage) -> Vec<parameters::Call> {
        let n = stage.num_bells_u8();

        let mut calls = Vec::new();
        // Add bob
        if let Some(bob_weight) = self.bob_weight {
            let bob_pn = match self.ty {
                BaseCallType::Near => PlaceNot::parse("14", stage).unwrap(),
                BaseCallType::Far => PlaceNot::from_slice(&mut [0, n - 3], stage).unwrap(),
            };
            calls.push(lead_end_call(bob_pn, "-", bob_weight)); // Hide in display; '-' in debug
        }
        // Add single
        if let Some(single_weight) = self.single_weight {
            let single_pn = match self.ty {
                BaseCallType::Near => PlaceNot::parse("1234", stage).unwrap(),
                BaseCallType::Far => {
                    PlaceNot::from_slice(&mut [0, n - 3, n - 2, n - 1], stage).unwrap()
                }
            };
            calls.push(lead_end_call(single_pn, "s", single_weight));
        }

        calls
    }
}

impl Default for BaseCalls {
    // Default to fourths-place bobs and singles, with a fairly small negative weight
    fn default() -> Self {
        Self {
            ty: BaseCallType::Near,
            bob_weight: Some(DEFAULT_BOB_WEIGHT),
            single_weight: Some(DEFAULT_SINGLE_WEIGHT),
        }
    }
}

/// Create a [`query::Call`] which replaces the lead end with a given [`PlaceNot`]
fn lead_end_call(place_not: PlaceNot, symbol: &str, weight: f32) -> parameters::Call {
    parameters::Call {
        symbol: symbol.to_owned(),
        calling_positions: default_calling_positions(&place_not),
        label_from: LABEL_LEAD_END.to_owned(),
        label_to: LABEL_LEAD_END.to_owned(),
        place_notation: place_not,
        weight,
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

///////////
// MUSIC //
///////////

/// A type of music that Monument should care about.
pub struct MusicType {
    music_type: parameters::MusicType,
}

impl MusicType {
    pub fn new(patterns: impl IntoIterator<Item = Pattern>) -> Self {
        Self {
            music_type: parameters::MusicType {
                patterns: patterns.into_iter().collect_vec(),
                strokes: StrokeSet::Both,
                weight: 1.0,
                count_range: OptionalRangeInclusive::OPEN,
            },
        }
    }

    /// Set the weight of each instance of this music type.  If unset, defaults to `1.0`.
    pub fn weight(mut self, weight: f32) -> Self {
        self.music_type.weight = weight;
        self
    }

    /// Sets this music type to only be scored at [backstrokes](bellframe::Stroke::Back).
    pub fn at_backstroke(mut self) -> Self {
        self.music_type.strokes = StrokeSet::Back;
        self
    }

    /// Sets this music type to only be scored at [handstrokes](bellframe::Stroke::Hand).
    pub fn at_handstroke(mut self) -> Self {
        self.music_type.strokes = StrokeSet::Hand;
        self
    }

    /// Sets range constraints for this music type.  By default, no constraints are enforced.
    pub fn count_range(mut self, range: OptionalRangeInclusive) -> Self {
        self.music_type.count_range = range;
        self
    }

    /// Finish building and add this music type to a [`Search`], returning its unique
    /// [`MusicTypeId`].
    #[allow(clippy::should_implement_trait)]
    pub fn add(self, search: &mut SearchBuilder) -> MusicTypeId {
        let index = search.music_types.push(self.music_type);
        MusicTypeId { index }
    }
}

/// The unique identifier for a [`MusicType`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MusicTypeId {
    pub(crate) index: MusicTypeIdx,
}

////////////////
// MISC TYPES //
////////////////

/// The length range of a [`Search`].
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

/// Where in a lead can a method finish.
#[derive(Debug)]
pub enum EndIndices {
    /// The composition is allowed to end at any index.
    Any,
    /// The composition can only finish at these indices (modulo the lead lengths).
    Specific(Vec<isize>),
}
