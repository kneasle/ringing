//! Builder API for methods

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use bellframe::{
    method::LABEL_LEAD_END, method_lib::QueryError, Bell, Mask, MethodLib, Row, Stage,
};
use itertools::Itertools;
use serde::Deserialize;

use crate::query::{self, CallVec, MethodIdx, MethodVec};

use super::{EndIndices, OptionalRangeInclusive};

#[allow(unused_imports)] // Only used by doc comments
use super::SearchBuilder;

const NUM_METHOD_SUGGESTIONS: usize = 10;

/// Builder API for a method.
pub struct Method {
    source: MethodSource,
    lead_labels: HashMap<String, Vec<isize>>,

    custom_shorthand: Option<String>,
    override_count_range: OptionalRangeInclusive,
    override_start_indices: Option<Vec<isize>>,
    override_end_indices: Option<Vec<isize>>,
    override_courses: Option<Vec<String>>,
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

impl Method {
    /// Create a new [`Method`] by loading a method from the Central Council library by its
    /// [`title`](bellframe::Method::title).
    pub fn with_title(title: String) -> Self {
        Self::new(MethodSource::Title(title))
    }

    /// Create a new [`Method`] with custom place notation.
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

/// The different styles of spliced that can be generated.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label (usually just
    /// [lead ends](bellframe::method::LABEL_LEAD_END)).
    #[serde(rename = "leads")]
    LeadLabels,
    /// Splices only happen at calls.
    #[serde(rename = "calls")]
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
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
        bellframe_method: bellframe::Method,
        fixed_bells: &[(Bell, usize)],
        default_method_count: OptionalRangeInclusive,
        default_start_indices: &[isize],
        default_end_indices: &EndIndices,
        allowed_course_masks: &[Mask],
        non_duffer_courses: Option<&[CourseSet]>,
        part_head: &Row,
        stage: Stage,
    ) -> crate::Result<query::Method> {
        // Parse the allowed courses and expand them into allowed *lead* masks
        let allowed_course_masks = match self.override_courses {
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
            None => allowed_course_masks.to_owned(),
        };
        let allowed_course_set = CourseSet {
            masks: allowed_course_masks,
            both_strokes: false,
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

        Ok(query::Method {
            shorthand: self
                .custom_shorthand
                .unwrap_or_else(|| default_shorthand(bellframe_method.title())),
            count_range: self.override_count_range.or(default_method_count),
            plain_course: bellframe_method
                .plain_course()
                .map_annots(|annot| annot.labels.to_vec()),

            start_indices,
            end_indices,
            allowed_course_masks: allowed_course_set
                .as_course_masks(&bellframe_method, fixed_bells),
            allowed_lead_masks: allowed_course_set.as_lead_masks(&bellframe_method, fixed_bells),
            non_duffer_lead_masks: match non_duffer_courses {
                Some(course_sets) => course_sets
                    .iter()
                    .flat_map(|c| c.as_lead_masks(&bellframe_method, fixed_bells))
                    .collect_vec(),
                None => vec![Mask::empty(stage)], // `None` means all courses are non-duffers
            },

            inner: bellframe_method,
        })
    }
}

////////////////////////////////
// COURSE MASKS -> LEAD MASKS //
////////////////////////////////

/// Convenient description of a set of courses via [`Mask`]s.
#[derive(Debug, Clone)]
pub struct CourseSet {
    /// List of [`Mask`]s, of which courses should match at least one.
    pub masks: Vec<Mask>,
    /// If `true`, the [`Mask`]s will be expanded so that the mask matches on both strokes.  For
    /// example, `"*5678"` might expand to `["*5678", "*6587"]`.
    pub both_strokes: bool,
    /// If `true`, every [`Bell`] in every [`Mask`] will be added/subtracted from to get every
    /// combination.  For example, `"*3456"` would expand to
    /// `["*1234", "*2345", "*3456", "*4567", "*5678"]`.
    pub any_bells: bool,
}

impl CourseSet {
    /// Returns a list of [`Mask`]s which match any lead head of the courses represented by `self`.
    fn as_lead_masks(
        &self,
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        let course_masks = self.as_course_masks(method, fixed_bells);
        // Convert *course* head masks into *lead* head masks (course heads are convenient for the
        // user, but Monument is internally based completely on lead heads - courses are only used to
        // interact with the user).
        let mut lead_head_masks = HashSet::new();
        for course_mask in course_masks {
            for lead_head in method.lead_head().closure() {
                lead_head_masks.insert(&course_mask * &lead_head);
            }
        }
        // Remove any lh masks which are a subset of others (for example, if `xx3456` and `xxxx56`
        // are present, then `xx3456` can be removed because it is implied by `xxxx56`).  This is
        // useful to speed up the falseness table generation.  Making `lead_head_masks` a `HashSet`
        // means that perfect duplicates have already been eliminated, so we only need to check for
        // strict subset-ness.
        let mut filtered_lead_head_masks = Vec::new();
        for mask in &lead_head_masks {
            let is_implied_by_another_mask = lead_head_masks
                .iter()
                .any(|mask2| mask.is_strict_subset_of(mask2));
            if !is_implied_by_another_mask {
                filtered_lead_head_masks.push(mask.clone());
            }
        }
        filtered_lead_head_masks
    }

    /// Expand `self` into a single set of [`Mask`]s
    fn as_course_masks(
        &self,
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        let mut course_masks: Vec<Mask> = self.masks.clone();
        // Expand `any_bells`, by offsetting the bells in the mask in every possible way
        if self.any_bells {
            let mut expanded_masks = Vec::new();
            for mask in &course_masks {
                let min_bell = mask.bells().flatten().min().unwrap_or(Bell::TREBLE);
                let max_bell = mask.bells().flatten().max().unwrap_or(Bell::TREBLE);
                let min_offset = -(min_bell.index_u8() as i16);
                let max_offset = (method.stage().num_bells_u8() - max_bell.number()) as i16;
                for offset in min_offset..=max_offset {
                    expanded_masks.push(Mask::from_bells(
                        mask.bells()
                            .map(|maybe_bell| maybe_bell.map(|b| b + offset)),
                    ));
                }
            }
            course_masks = expanded_masks;
        }
        // Expand `both_strokes`
        if self.both_strokes {
            course_masks = course_masks
                .into_iter()
                .flat_map(|mask| [&mask * method.lead_end(), mask])
                .collect_vec();
        }
        // Set fixed bells
        course_masks
            .into_iter()
            .filter_map(|mask| try_fixing_bells(mask, fixed_bells))
            .collect_vec()
    }
}

/// Attempt to fix the `fixed_bells` in the [`Mask`], returning `None` if it was not possible.
fn try_fixing_bells(mut mask: Mask, fixed_bells: &[(Bell, usize)]) -> Option<Mask> {
    for &(bell, place) in fixed_bells {
        mask.set_bell(bell, place).ok()?;
    }
    Some(mask)
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

/// Get a default shorthand given a method's title.
fn default_shorthand(title: &str) -> String {
    title
        .chars()
        .next()
        .expect("Can't have empty method title")
        .to_string()
}

/// Returns the place bells which are always preserved by plain leads and all calls of all methods
/// (e.g. hunt bells in non-variable-hunt compositions).
pub(super) fn fixed_bells(
    methods: &MethodVec<(bellframe::Method, self::Method)>,
    calls: &CallVec<query::Call>,
    start_row: &Row,
    stage: Stage,
) -> Vec<(Bell, usize)> {
    let mut fixed_bells = stage.bells().collect_vec();
    for (m, _) in methods {
        let f = fixed_bells_of_method(m, calls);
        fixed_bells.retain(|b| f.contains(b));
    }
    // Currently, these `fixed_bells` assume that the start_row is rounds
    fixed_bells
        .iter()
        .map(|b| (start_row[b.index()], b.index()))
        .collect_vec()
}

/// Returns the place bells which are always preserved by plain leads and all calls of a single
/// method (e.g. hunt bells in non-variable-hunt compositions).
fn fixed_bells_of_method(
    method: &bellframe::Method,
    calls: &CallVec<query::Call>,
) -> HashSet<Bell> {
    // Start the set with the bells which are fixed by the plain lead of every method
    let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
    for call in calls {
        // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
        // Grandsire is unaffected by a plain lead, but affected by calls)
        filter_bells_fixed_by_call(method, call, &mut fixed_bells);
    }
    fixed_bells
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(
    method: &bellframe::Method,
    call: &query::Call,
    set: &mut HashSet<Bell>,
) {
    // Note that all calls are required to only substitute one piece of place notation.
    for sub_lead_idx_after_call in method.label_indices(&call.label_from) {
        // TODO: Handle different from/to locations
        let idx_before_call = (sub_lead_idx_after_call + method.lead_len() - 1) % method.lead_len();
        let idx_after_call = idx_before_call + 1; // in range `1..=method.lead_len()`

        // The row before a call in this location in the _first lead_
        let row_before_call = method.first_lead().get_row(idx_before_call).unwrap();
        // The row after a plain call in this location in the _first lead_
        let row_after_no_call = method.first_lead().get_row(idx_after_call).unwrap();
        // The row after a call in this location in the _first lead_
        let mut row_after_call = row_before_call.to_owned();
        call.place_notation.permute(&mut row_after_call).unwrap();

        // A bell is _affected_ by the call iff it's in a different place in `row_after_call` than
        // `row_after_no_call`.  These should be removed from the set, because they are no longer
        // fixed.
        for (bell_after_no_call, bell_after_call) in
            row_after_no_call.bell_iter().zip(&row_after_call)
        {
            if bell_after_call != bell_after_no_call {
                set.remove(&bell_after_call);
            }
        }
    }
}
