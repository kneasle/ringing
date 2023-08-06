//! Builder API for methods

use std::{collections::HashMap, hash::Hash};

use bellframe::{method::LABEL_LEAD_END, method_lib::QueryError, Mask, MethodLib, Row, Stage};
use itertools::Itertools;

use crate::parameters::{self, CourseSet, MethodIdx, MethodVec, OptionalRangeInclusive};

use super::EndIndices;

#[allow(unused_imports)] // Only used by doc comments
use super::SearchBuilder;

const NUM_METHOD_SUGGESTIONS: usize = 10;

/// Builder API for a method.
#[derive(Debug)]
pub struct Method {
    source: MethodSource,
    lead_labels: HashMap<String, Vec<isize>>,

    custom_shorthand: Option<String>,
    override_count_range: OptionalRangeInclusive,
    override_start_indices: Option<Vec<isize>>,
    override_end_indices: Option<Vec<isize>>,
    override_courses: Option<Vec<String>>,
}

#[derive(Debug)]
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

impl Method {
    /// Create a new [`Method`] by loading a method from the Central Council library by its
    /// [`title`](bellframe::Method::title).
    pub fn with_title(title: String) -> Self {
        Self::from_source(MethodSource::Title(title))
    }

    /// Create a new [`Method`] with custom place notation.
    pub fn with_custom_pn(name: String, pn_str: String, stage: Stage) -> Self {
        Self::from_source(MethodSource::CustomPn {
            name,
            pn_str,
            stage,
        })
    }

    fn from_source(source: MethodSource) -> Self {
        Self {
            source,
            lead_labels: hmap::hmap! { LABEL_LEAD_END.to_owned() => vec![0] },

            custom_shorthand: None,
            override_count_range: OptionalRangeInclusive::OPEN,
            override_start_indices: None,
            override_end_indices: None,
            override_courses: None,
        }
    }

    /// Force use of a specific shorthand for this [`Method`].  If `None` is given, the first
    /// character of the method's title will be used as a shorthand.
    pub fn shorthand(mut self, shorthand: Option<String>) -> Self {
        self.custom_shorthand = shorthand;
        self
    }

    /// Override the globally set [`method_count`](SearchBuilder::default_method_count) for just
    /// this method.
    pub fn count_range(mut self, range: OptionalRangeInclusive) -> Self {
        self.override_count_range = range;
        self
    }

    /// Sets the sub-lead indices where this method is allowed to start.  If `None` is supplied,
    /// this will fall back on the indices set by [`SearchBuilder::default_start_indices`].
    ///
    /// As with [`SearchBuilder::default_start_indices`], these indices are taken modulo the
    /// method's lead length.
    pub fn start_indices(mut self, indices: Option<Vec<isize>>) -> Self {
        self.override_start_indices = indices;
        self
    }

    /// Sets the sub-lead indices where this method is allowed to finish.  If `None` is supplied,
    /// this will fall back on the indices set by [`SearchBuilder::default_end_indices`] (**NOTE**:
    /// the behaviour on `None` is different to [`SearchBuilder::default_end_indices`], where
    /// `None` indicates that any end is allowed).
    ///
    /// As with [`SearchBuilder::default_end_indices`], these indices are taken modulo the
    /// method's lead length.
    pub fn end_indices(mut self, indices: Option<Vec<isize>>) -> Self {
        self.override_end_indices = indices;
        self
    }

    /// Specify which courses are allowed for this method.  If `None` is given, this will fall back
    /// on the courses set by [`SearchBuilder::courses`].
    pub fn courses(mut self, courses: Option<Vec<String>>) -> Self {
        self.override_courses = courses;
        self
    }

    /// Applies labels to [`Row`] indices within every lead of this method.  By default,
    /// [`bellframe::method::LABEL_LEAD_END`] is at index `0` (i.e. the lead end).
    pub fn lead_labels(mut self, labels: HashMap<String, Vec<isize>>) -> Self {
        self.lead_labels = labels;
        self
    }

    /// Finishes building and adds `self` to the supplied [`MethodSet`], returning the [`MethodId`]
    /// now used to refer to this [`Method`].
    ///
    /// This is equivalent to [`MethodSet::add`], but makes for cleaner code.
    // TODO: Code example
    #[allow(clippy::should_implement_trait)]
    pub fn add(self, set: &mut MethodSet) -> MethodId {
        set.add(self)
    }
}

/// The unique identifier for a [`Method`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodId {
    pub(crate) index: MethodIdx,
}

/// A set of [`Method`]s.
// TODO: Remove this and add methods directly to `SearchBuilder`?
pub struct MethodSet {
    pub(super) vec: MethodVec<Method>,
}

impl MethodSet {
    /// Creates a `MethodSet` containing no methods.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a [`Method`] to this set, returning its unique [`MethodId`].
    ///
    /// You can also use [`Method::add`], which usually makes for cleaner code.
    pub fn add(&mut self, method: Method) -> MethodId {
        let index = self.vec.push(method);
        MethodId { index }
    }
}

impl Default for MethodSet {
    fn default() -> Self {
        Self {
            vec: MethodVec::new(),
        }
    }
}

impl<I> From<I> for MethodSet
where
    I: IntoIterator<Item = Method>,
{
    fn from(iter: I) -> Self {
        Self {
            vec: iter.into_iter().collect(),
        }
    }
}

//////////////////
// INTERNAL API //
//////////////////

impl Method {
    /// Create a [`bellframe::Method`] representing this method.
    pub(super) fn bellframe_method(&self, cc_lib: &MethodLib) -> crate::Result<bellframe::Method> {
        let bellframe_method = match &self.source {
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
        };
        let mut bellframe_method = bellframe_method?;

        // Set lead labels
        for (label, indices) in &self.lead_labels {
            for index in indices {
                let lead_len_i = bellframe_method.lead_len() as isize;
                let index = ((index % lead_len_i) + lead_len_i) % lead_len_i;
                bellframe_method.add_label(index as usize, label.clone());
            }
        }

        Ok(bellframe_method)
    }

    /// Convert this method builder into a [`query::Method`]
    #[allow(clippy::too_many_arguments)]
    pub(super) fn build(
        self,
        id: crate::parameters::MethodId,
        bellframe_method: bellframe::Method,
        default_method_count: OptionalRangeInclusive,
        default_start_indices: &[isize],
        default_end_indices: &EndIndices,
        allowed_course_masks: &[Mask],
        non_duffer_courses: Vec<CourseSet>,
        part_head: &Row,
        stage: Stage,
    ) -> crate::Result<parameters::Method> {
        // Parse the allowed courses and expand them into allowed *lead* masks
        let allowed_course_masks = match self.override_courses {
            Some(unparsed_courses) => unparsed_courses
                .into_iter()
                .map(|mask_str| {
                    Mask::parse_with_stage(&mask_str, stage).map_err(|error| {
                        crate::Error::CustomCourseMaskParse {
                            method_title: bellframe_method.title(),
                            mask_str,
                            error,
                        }
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
            None => allowed_course_masks.to_owned(),
        };
        let allowed_course_set = CourseSet {
            masks: allowed_course_masks,
            any_stroke: false,
            any_bells: false,
        };

        // Compute the start/end indices
        let not_wrapped_start_indices = self
            .override_start_indices
            .as_deref()
            .unwrap_or(default_start_indices);
        let mut start_indices = wrap_sub_lead_indices(not_wrapped_start_indices, &bellframe_method);
        let mut end_indices = match (&self.override_end_indices, default_end_indices) {
            (Some(indices), _) | (None, EndIndices::Specific(indices)) => {
                wrap_sub_lead_indices(indices, &bellframe_method)
            }
            (None, EndIndices::Any) => (0..bellframe_method.lead_len()).collect_vec(),
        };
        // If ringing a multi-part, the `{start,end}_indices` have to match.   Therefore, it makes
        // no sense to generate any starts/ends which don't have a matching end/start.  To achieve
        // this, we set both `{start,end}_indices` to the union between `start_indices` and
        // `end_indices`.
        if !part_head.is_rounds() {
            let union = start_indices
                .iter()
                .filter(|idx| end_indices.contains(idx))
                .copied()
                .collect_vec();
            start_indices = union.clone();
            end_indices = union;
        }

        Ok(parameters::Method {
            id,
            used: true,

            custom_shorthand: self.custom_shorthand.unwrap_or_default(),
            count_range: self.override_count_range.or(default_method_count),

            start_indices,
            end_indices,
            allowed_courses: vec![allowed_course_set],
            non_duffer_courses,

            inner: bellframe_method,
        })
    }
}

fn wrap_sub_lead_indices(indices: &[isize], method: &bellframe::Method) -> Vec<usize> {
    indices
        .iter()
        .map(|idx| {
            let lead_len_i = method.lead_len() as isize;
            (*idx % lead_len_i + lead_len_i) as usize % method.lead_len()
        })
        .collect_vec()
}
