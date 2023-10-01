use std::{
    collections::HashSet,
    ops::{Deref, Range},
};

use bellframe::{Bell, Block, Mask, Row, RowBuf, Stage};
use itertools::Itertools;

use crate::{
    graph::ChunkId,
    parameters::{
        CallVec, CourseSet, MethodId, MethodIdx, MethodVec, MusicType, MusicTypeId, MusicTypeVec,
        Parameters,
    },
    utils::PerPartLength,
};

/// Extra data precomputed from a set of [`Parameters`], to be used while generating the graph.
///
/// This is required because using the [`Parameters`] directly in a search is not useful for two
/// major reasons:
///
/// 1. To allow the GUI to edit the [`Parameters`] directly, it is highly useful to be able to
///    include methods/calls/music types in a set of [`Parameters`] but then mark them as 'unused'.
///    However, the graph build and search algorithms only care about which methods are used, so
///    in a [`Query`] we can do that filtering and provide cheap access to only used
///    methods/calls/music types.
///
/// 2. We want to pre-compute and cache useful data which is used often in the core algorithms.  We
///    don't want to add this 'derived' data directly into [`Parameters`] directly, since the
///    consumer of the API would have to keep internal data up-to-date.  By putting it in `Query`,
///    we can precompute them and provide them to the rest of the code.
#[derive(Debug)]
pub(crate) struct Query {
    pub parameters: Parameters,
    pub methods: MethodVec<Method>,
    pub calls: CallVec<crate::parameters::Call>,
    pub music_types: MusicTypeVec<MusicType>,
}

// TODO: Remove this and calculate the values manually
#[derive(Debug)]
pub(crate) struct Method {
    pub inner: crate::parameters::Method,
    /// A [`Block`] containing the entire plain course of `inner`.  Each row is annotated with the
    /// labels assigned to that row.
    pub plain_course: Block<Vec<String>>,

    /// The expanded version of `inner.allowed_courses`
    pub specified_course_head_masks: Vec<Mask>,
    /// The [`Mask`]s which lead heads must satisfy in order to be a lead head within
    /// [`crate::SearchBuilder::courses`].
    // TODO: Remove this
    pub allowed_lead_masks: Vec<Mask>,
}

impl Query {
    pub(crate) fn get_method_by_id(&self, id: MethodId) -> MethodIdx {
        self.methods
            .iter()
            .find_position(|m| m.id == id)
            .unwrap()
            .0
            .into()
    }

    pub(crate) fn get_music_type_by_id(&self, id: MusicTypeId) -> &MusicType {
        // TODO: if this is a bottleneck, we can optimise this with a hashmap
        self.music_types.iter().find(|mt| mt.id == id).unwrap()
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

impl Deref for Query {
    type Target = Parameters;

    fn deref(&self) -> &Self::Target {
        &self.parameters
    }
}

impl Deref for Method {
    type Target = crate::parameters::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/////////////////////////
// BUILDING EXTRA DATA //
/////////////////////////

impl Query {
    pub(crate) fn new(parameters: Parameters) -> Self {
        // Filter methods and calls
        let used_methods = parameters
            .maybe_unused_methods
            .iter()
            .filter(|m| m.used)
            .cloned()
            .collect_vec();
        let used_calls: CallVec<_> = parameters
            .maybe_unused_calls
            .iter()
            .filter(|c| c.used)
            .cloned()
            .collect();
        let used_music_types: MusicTypeVec<_> = parameters
            .maybe_unused_music_types
            .iter()
            .filter(|mt| mt.used)
            .cloned()
            .collect();
        // Generate fixed bells
        let fixed_bells = fixed_bells(
            &used_methods,
            used_calls.as_raw_slice(),
            &parameters.start_row,
            parameters.stage,
        );

        Self {
            methods: used_methods
                .into_iter()
                .map(|m| Method::new(m, &fixed_bells))
                .collect(),
            calls: used_calls,
            music_types: used_music_types,

            parameters,
        }
    }
}

impl Method {
    fn new(method: crate::parameters::Method, fixed_bells: &[(Bell, usize)]) -> Self {
        let plain_course = method.plain_course().map_annots(|a| a.labels.to_vec());
        Self {
            plain_course,

            specified_course_head_masks: CourseSet::to_course_masks(
                &method.allowed_courses,
                &method,
                fixed_bells,
            ),
            allowed_lead_masks: CourseSet::to_lead_masks(
                &method.allowed_courses,
                &method,
                fixed_bells,
            ),

            inner: method,
        }
    }
}

//////////////////
// FIXING BELLS //
//////////////////

/// Returns the place bells which are always preserved by plain leads and all calls of all methods
/// (e.g. hunt bells in non-variable-hunt compositions).
pub(super) fn fixed_bells(
    methods: &[crate::parameters::Method],
    calls: &[crate::parameters::Call],
    start_row: &Row,
    stage: Stage,
) -> Vec<(Bell, usize)> {
    let mut fixed_bells = stage.bells().collect_vec();
    for m in methods {
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
    calls: &[crate::parameters::Call],
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
    call: &crate::parameters::Call,
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
