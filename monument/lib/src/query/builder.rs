//! Code for the [`QueryBuilder`] API for creating [`Query`]s.

use std::{collections::HashMap, ops::RangeInclusive};

use bellframe::{
    method::LABEL_LEAD_END,
    method_lib::QueryError,
    music::{Elem, Pattern},
    Mask, MethodLib, RowBuf, Stage, Stroke,
};
use hmap::hmap;
use ordered_float::OrderedFloat;

use crate::{
    group::PartHeadGroup,
    search::{Config, Search, Update},
    utils::TotalLength,
    Composition,
};

use super::{
    CallDisplayStyle, MethodVec, MusicTypeVec, OptionalRangeInclusive, Query, SpliceStyle,
};

#[allow(unused_imports)] // Only used for doc comments
use bellframe::Row;

/// Builder API for creating [`Query`]s.
pub struct QueryBuilder {
    /* GENERAL */
    length_range: RangeInclusive<TotalLength>,
    pub num_comps: usize,
    pub require_truth: bool,
    stage: Stage,

    /* METHODS */
    methods: MethodVec<(bellframe::Method, MethodBuilder)>,
    pub default_method_count: OptionalRangeInclusive,
    pub default_start_indices: Vec<isize>,
    pub default_end_indices: Option<Vec<isize>>, // `None` means 'any index'
    pub splice_style: SpliceStyle,
    pub splice_weight: f32,

    /* CALLS */
    pub custom_calls: Vec<super::Call>,
    pub call_display_style: CallDisplayStyle,

    /* MUSIC */
    pub music_types: MusicTypeVec<super::MusicType>,
    pub start_stroke: Stroke,

    /* COURSES */
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head: RowBuf,
    pub courses: Option<Vec<Mask>>,
    pub course_weights: Vec<(Mask, f32)>,
}

impl QueryBuilder {
    /* START */

    /// Create a new `QueryBuilder` with a custom length range
    // TODO: Store the error until `self.build`
    pub fn new(method_builders: MethodVec<MethodBuilder>, length: Length) -> crate::Result<Self> {
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

        Ok(QueryBuilder {
            stage,
            length_range,
            num_comps: 100,
            require_truth: true,

            methods,
            default_method_count: OptionalRangeInclusive::OPEN,
            default_start_indices: vec![0], // Just 'normal' starts by default
            default_end_indices: None,
            splice_style: SpliceStyle::LeadLabels,
            splice_weight: 0.0,

            custom_calls: vec![],
            call_display_style: CallDisplayStyle::CallingPositions(stage.tenor()),

            music_types: MusicTypeVec::new(),
            start_stroke: Stroke::Hand,

            start_row: RowBuf::rounds(stage),
            end_row: RowBuf::rounds(stage),
            part_head: RowBuf::rounds(stage),
            courses: None, // Defer setting the defaults until we know the part head
            course_weights: vec![],
        })
    }

    /// Gets the [`Stage`] of the [`Query`] being built.  All extra components ([`Mask`]s,
    /// [`Row`]s, etc.) must all have this [`Stage`], or the builder will panic.
    pub fn get_stage(&self) -> Stage {
        self.stage
    }

    /* SETTERS */

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
        Search::new(self.build()?, config)?.run(update_fn);
        Ok(comps)
    }

    /// Finish building and return the [`Query`].
    pub fn build(self) -> crate::Result<Query> {
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

            custom_calls,
            call_display_style,

            music_types,
            start_stroke,

            start_row,
            end_row,
            part_head,
            courses,
            course_weights,
        } = self;

        let courses = courses.unwrap_or_else(|| {
            // Fix the tenors which aren't affected by the part head.  Usually this will be either
            // all (e.g. part head of rounds or `1342`) or all (e.g. cyclic), but any other
            // combinations are possible.  E.g. a composition of Maximus with part head of
            // `1765432` will still preserve 8 through 12
            let tenors_unaffected_by_part_head =
                stage.bells().skip(6).filter(|&b| part_head.is_fixed(b));
            vec![Mask::with_fixed_bells(
                stage,
                tenors_unaffected_by_part_head,
            )]
        });

        // Query to build a `MethodBuilder` into a `query::Method`, falling back on any defaults
        // when required.
        let mut built_methods = MethodVec::new();
        for (mut bellframe_method, method_builder) in methods {
            // Set lead locations for the `method`
            for (label, indices) in method_builder.lead_labels {
                for index in indices {
                    let lead_len_i = bellframe_method.lead_len() as isize;
                    let index = ((index % lead_len_i) + lead_len_i) % lead_len_i;
                    bellframe_method.add_label(index as usize, label.clone());
                }
            }
            // Parse the courses
            let courses = match method_builder.override_courses {
                Some(unparsed_courses) => unparsed_courses
                    .into_iter()
                    .map(|mask_str| {
                        Mask::parse_with_stage(&mask_str, stage).map_err(|error| {
                            crate::Error::CustomCourseMaskParse {
                                method_title: bellframe_method.title().to_owned(),
                                mask_str,
                                error,
                            }
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                None => courses.clone(),
            };
            // Add the method, falling back on any defaults if necessary
            built_methods.push(super::Method {
                shorthand: method_builder
                    .custom_shorthand
                    .unwrap_or_else(|| default_shorthand(bellframe_method.title())),
                count_range: method_builder.override_count_range.or(default_method_count),
                start_indices: method_builder
                    .override_start_indices
                    .unwrap_or_else(|| default_start_indices.clone()),
                end_indices: method_builder
                    .override_end_indices
                    .or_else(|| default_end_indices.clone()),
                courses,

                inner: bellframe_method,
            });
        }

        Ok(Query {
            length_range,
            stage,
            num_comps,
            require_truth,

            methods: built_methods,
            splice_style,
            splice_weight: OrderedFloat(splice_weight),
            calls: custom_calls.into_iter().collect(),
            call_display_style,

            start_row,
            end_row,
            part_head_group: PartHeadGroup::new(&part_head),
            course_weights: course_weights
                .into_iter()
                .map(|(mask, weight)| (mask, OrderedFloat(weight)))
                .collect(),
            music_types: music_types.into_iter().collect(),
            start_stroke,
        })
    }
}

/////////////
// METHODS //
/////////////

const NUM_METHOD_SUGGESTIONS: usize = 10;

/// Builder API for a [`Method`](super::Method).
pub struct MethodBuilder {
    source: MethodSource,
    // TODO: Add builder API for these fields
    pub lead_labels: HashMap<String, Vec<isize>>,

    pub custom_shorthand: Option<String>,
    pub override_count_range: OptionalRangeInclusive,
    pub override_start_indices: Option<Vec<isize>>,
    pub override_end_indices: Option<Vec<isize>>,
    pub override_courses: Option<Vec<String>>, // TODO: Allow these to be pre-parsed
}

impl MethodBuilder {
    /// Start building a [`Method`](super::Method) which should be loaded from the Central Council
    /// library.
    pub fn with_title(title: String) -> Self {
        Self::new(MethodSource::Title(title))
    }

    /// Start building a [`Method`](super::Method) from custom place notation.
    pub fn with_custom_pn(name: String, pn_str: String, stage: Stage) -> Self {
        Self::new(MethodSource::CustomPn {
            name,
            pn_str,
            stage,
        })
    }

    fn new(source: MethodSource) -> Self {
        Self {
            source,
            lead_labels: hmap! { LABEL_LEAD_END.to_owned() => vec![0] },

            custom_shorthand: None,
            override_count_range: OptionalRangeInclusive::OPEN,
            override_start_indices: None,
            override_end_indices: None,
            override_courses: None,
        }
    }

    /// Force use of a specific shorthand for this [`Method`](super::Method).  By default, the
    /// first character of the method's title will be used as a shorthand.
    pub fn shorthand(mut self, shorthand: String) -> Self {
        self.custom_shorthand = Some(shorthand);
        self
    }

    /// Override the globally set [`method_count`](QueryBuilder::default_method_count) for just this
    /// method.
    pub fn count_range(mut self, range: OptionalRangeInclusive) -> Self {
        self.override_count_range = range;
        self
    }

    /// Override the globally set [`start_indices`](QueryBuilder::default_start_indices) for just
    /// this method.  As with [`QueryBuilder::default_start_indices`], these indices are taken
    /// modulo the method's lead length.
    pub fn start_indices(mut self, indices: Vec<isize>) -> Self {
        self.override_start_indices = Some(indices);
        self
    }

    /// Override the globally set [`end_indices`](QueryBuilder::default_end_indices) for just this
    /// method.  As with [`QueryBuilder::default_end_indices`], these indices are taken modulo the
    /// method's lead length.
    pub fn end_indices(mut self, indices: Vec<isize>) -> Self {
        self.override_end_indices = Some(indices);
        self
    }

    /// Override [`QueryBuilder::courses`] to specify which courses are allowed for this method.
    pub fn courses(mut self, courses: Vec<String>) -> Self {
        self.override_courses = Some(courses);
        self
    }

    /// Applies labels to [`Row`] indices within every lead of this method.  By default,
    /// [`bellframe::method::LABEL_LEAD_END`] is at index `0` (i.e. the lead end).
    pub fn lead_labels(mut self, labels: HashMap<String, Vec<isize>>) -> Self {
        self.lead_labels = labels;
        self
    }

    /// Create a [`bellframe::Method`] representing this method.
    fn bellframe_method(&self, cc_lib: &MethodLib) -> crate::Result<bellframe::Method> {
        match &self.source {
            MethodSource::Title(title) => cc_lib
                .get_by_title_with_suggestions(title, NUM_METHOD_SUGGESTIONS)
                .map_err(|err| match err {
                    QueryError::PnParseErr { pn, error } => panic!(
                        "Invalid pn `{}` in CC library entry for {}: {}",
                        pn, title, error
                    ),
                    QueryError::NotFound(suggestions) => crate::Error::MethodNotFound {
                        title: title.to_owned(),
                        suggestions,
                    },
                }),
            MethodSource::CustomPn {
                name,
                pn_str: place_notation_string,
                stage,
            } => bellframe::Method::from_place_not_string(
                name.to_owned(),
                *stage,
                place_notation_string,
            )
            .map_err(|error| crate::Error::MethodPnParse {
                name: name.to_owned(),
                place_notation_string: place_notation_string.to_owned(),
                error,
            }),
        }
    }
}

enum MethodSource {
    /// A method with this title should be found in the Central Council's method library.
    Title(String),
    /// The method should be loaded from some custom place notation
    CustomPn {
        name: String,
        pn_str: String,
        stage: Stage, // TODO: Make stage optional
    },
}

/// Get a default shorthand given a method's title.
fn default_shorthand(title: &str) -> String {
    title
        .chars()
        .next()
        .expect("Can't have empty method title")
        .to_string()
}

////////////////
// MISC TYPES //
////////////////

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
