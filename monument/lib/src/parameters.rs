//! Instructions for Monument about what compositions should be generated.

use std::{
    collections::HashSet,
    fmt::Write,
    ops::{Deref, Range, RangeInclusive},
};

use bellframe::{
    method::LABEL_LEAD_END,
    music::{AtRowPositions, RowPosition},
    Bell, Mask, PlaceNot, Row, RowBuf, Stage, Stroke,
};
use itertools::Itertools;

use crate::{
    graph::ChunkId,
    group::PartHeadGroup,
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary, IdGenerator,
    },
};

/// Fully built specification for which [`Composition`]s should be generated.
///
/// Compare this to [`Config`], which determines _how_ those
/// [`Composition`]s are generated (and therefore determines how quickly the
/// results are generated).
#[derive(Debug, Clone, PartialEq)]
pub struct Parameters {
    // GENERAL
    pub length: RangeInclusive<TotalLength>,
    pub stage: Stage,
    pub num_comps: usize,
    pub require_truth: bool,

    // METHODS & CALLING
    pub methods: MethodVec<Method>,
    pub splice_style: SpliceStyle,
    pub splice_weight: f32,
    pub calls: CallVec<Call>,
    pub call_display_style: CallDisplayStyle, // TODO: Make this defined per-method?
    pub atw_weight: Option<f32>,
    pub require_atw: bool, // `true` to make Monument only output atw comps

    // COURSES
    //
    // NOTE: Course masks are defined on each `Method`
    pub start_row: RowBuf,
    pub end_row: RowBuf,
    pub part_head_group: PartHeadGroup,
    /// Score applied to every row in every course containing a lead head matching the
    /// corresponding [`Mask`].
    pub course_weights: Vec<(Mask, f32)>,

    // MUSIC
    pub music_types: MusicTypeVec<MusicType>,
    /// The [`Stroke`] of the first [`Row`] in the composition that isn't `self.start_row`
    // TODO: Compute this automatically from sub-lead index
    pub start_stroke: Stroke,
}

impl Parameters {
    pub fn max_length(&self) -> TotalLength {
        *self.length.end()
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

    /// Returns `start_row` or `end_row`, based on the given [`Boundary`]
    pub fn boundary_row(&self, boundary: Boundary) -> &Row {
        match boundary {
            Boundary::Start => &self.start_row,
            Boundary::End => &self.end_row,
        }
    }

    pub fn fixed_bells(&self) -> Vec<(Bell, usize)> {
        let mut fixed_bells = self.stage.bells().collect_vec();
        for m in &self.methods {
            let f = self.fixed_bells_of_method(m);
            fixed_bells.retain(|b| f.contains(b));
        }
        // Currently, these `fixed_bells` assume that the start_row is rounds
        fixed_bells
            .iter()
            .map(|b| (self.start_row[b.index()], b.index()))
            .collect_vec()
    }

    /// The [`Bell`]s which **aren't** in [`Self::fixed_bells`]
    pub fn working_bells(&self) -> Vec<Bell> {
        self.stage
            .bells()
            .filter(|b| !self.is_fixed_bell(*b))
            .collect_vec()
    }

    pub fn is_fixed_bell(&self, bell: Bell) -> bool {
        self.fixed_bells().iter().any(|(b, _place)| *b == bell)
    }

    pub fn lead_labels_used(&self) -> HashSet<String> {
        let mut defined_labels = HashSet::<String>::new();
        for m in &self.methods {
            for labels in m.first_lead().annots() {
                defined_labels.extend(labels.iter().cloned());
            }
        }
        defined_labels
    }

    pub fn method_id_to_idx(&self, id: MethodId) -> MethodIdx {
        self.methods.position(|m| m.id == id).unwrap()
    }

    pub fn call_id_to_idx(&self, id: CallId) -> CallIdx {
        self.calls.position(|c| c.id == id).unwrap()
    }

    pub fn get_method_by_id(&self, id: MethodId) -> &Method {
        &self.methods[self.method_id_to_idx(id)]
    }

    pub fn get_call_by_id(&self, id: CallId) -> &Call {
        &self.calls[self.call_id_to_idx(id)]
    }

    //////////////////////
    // HELPER FUNCTIONS //
    //////////////////////

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

    /// Returns the place bells which are always preserved by plain leads and all calls of a single
    /// method (e.g. hunt bells in non-variable-hunt compositions).
    fn fixed_bells_of_method(&self, method: &bellframe::Method) -> HashSet<Bell> {
        // Start the set with the bells which are fixed by the plain lead of every method
        let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
        for call in &self.calls {
            // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
            // Grandsire is unaffected by a plain lead, but affected by calls)
            Self::filter_bells_fixed_by_call(method, call, &mut fixed_bells);
        }
        fixed_bells
    }

    // For every position that this call could be placed, remove any bells which **aren't** preserved
    // by placing the call at this location.
    fn filter_bells_fixed_by_call(
        method: &bellframe::Method,
        call: &Call,
        set: &mut HashSet<Bell>,
    ) {
        // Note that all calls are required to only substitute one piece of place notation.
        for sub_lead_idx_after_call in method.label_indices(&call.label_from) {
            // TODO: Handle different from/to locations
            let idx_before_call =
                (sub_lead_idx_after_call + method.lead_len() - 1) % method.lead_len();
            let idx_after_call = idx_before_call + 1; // in range `1..=method.lead_len()`

            // The row before a call in this location in the _first lead_
            let row_before_call = method.first_lead().get_row(idx_before_call).unwrap();
            // The row after a plain call in this location in the _first lead_
            let row_after_no_call = method.first_lead().get_row(idx_after_call).unwrap();
            // The row after a call in this location in the _first lead_
            let mut row_after_call = row_before_call.to_owned();
            call.place_notation.permute(&mut row_after_call);

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

    pub(crate) fn valid_end_labels(&self) -> HashSet<String> {
        let mut valid_labels = HashSet::new();
        for m in &self.methods {
            let wrapped_indices = m.wrapped_indices(Boundary::End, self);
            for (sub_lead_idx, label) in m.inner.all_label_indices() {
                if wrapped_indices.contains(&sub_lead_idx) {
                    valid_labels.insert(label.to_owned());
                }
            }
        }
        valid_labels
    }

    /// Returns a human-readable string representing the given methods.
    ///
    /// This is:
    /// - `"all methods"` if all methods are given
    /// - `"X"` if only one method is given
    /// - `"X, Y and Z"` if more methods are given
    pub(crate) fn method_list_string(&self, methods: &[MethodIdx]) -> String {
        if methods.len() == self.methods.len() {
            "all methods".to_owned()
        } else {
            let shorthand = |idx: &MethodIdx| -> String { self.methods[*idx].shorthand() };

            match methods {
                [] => unreachable!(),
                [idx] => shorthand(idx),
                // Write 'idxs[0], idxs[1], ... idxs[n], idx2 and idx3'
                [idxs @ .., idx2, idx3] => {
                    let mut s = String::new();
                    for idx in idxs {
                        s.push_str(&shorthand(idx));
                        s.push_str(", ");
                    }
                    s.push_str(&shorthand(idx2));
                    s.push_str(" and ");
                    s.push_str(&shorthand(idx3));
                    s
                }
            }
        }
    }

    pub fn get_method(&self, id: MethodId) -> &Method {
        self.methods.iter().find(|mt| mt.id == id).unwrap()
    }

    pub fn get_call(&self, id: CallId) -> &Call {
        self.calls.iter().find(|mt| mt.id == id).unwrap()
    }

    pub fn music_types_to_show(&self) -> Vec<(MusicTypeIdx, &MusicType)> {
        self.music_types
            .iter_enumerated()
            .filter(|(_idx, mt)| mt.should_show())
            .collect_vec()
    }
}

/////////////
// METHODS //
/////////////

/// A `Method` used in a [`Search`].
#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub id: MethodId,
    pub inner: bellframe::Method,

    /// Short [`String`] used to identify this method in spliced.  If empty, a default value will
    /// be generated.
    pub custom_shorthand: String,

    /// The number of rows of this method must fit within this range
    pub count_range: OptionalRangeInclusive,

    /// The indices in which we can start a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub start_indices: Vec<isize>,
    /// The indices in which we can end a composition during this `Method`.  These are guaranteed
    /// to fit within `inner.lead_len()`.
    pub end_indices: Vec<isize>,

    /// The [`Mask`]s which *course heads* must satisfy
    pub allowed_courses: Vec<CourseSet>,
}

impl Method {
    pub fn shorthand(&self) -> String {
        if self.custom_shorthand.is_empty() {
            default_shorthand(&self.title())
        } else {
            self.custom_shorthand.clone()
        }
    }

    pub fn add_sub_lead_idx(&self, sub_lead_idx: usize, len: PerPartLength) -> usize {
        (sub_lead_idx + len.as_usize()) % self.lead_len()
    }

    pub fn lead_head_weights(&self, params: &Parameters) -> Vec<(Mask, f32)> {
        let mut mask_weights = Vec::new();
        for lead_head in self.lead_head().closure() {
            for (mask, weight) in &params.course_weights {
                mask_weights.push((mask * &lead_head, *weight));
            }
        }
        mask_weights
    }

    /////////////////////////
    // START/END LOCATIONS //
    /////////////////////////

    #[allow(clippy::type_complexity)]
    pub fn start_and_end_locations(
        &self,
        params: &Parameters,
    ) -> (Vec<(RowBuf, usize)>, Vec<(RowBuf, usize)>) {
        let start_locations = self.boundary_locations(&params.start_row, Boundary::Start, params);
        let end_locations = self.boundary_locations(&params.end_row, Boundary::End, params);
        (start_locations, end_locations)
    }

    /// Return `(lead head, sub lead index)` of every position that the given [`Row`] can be rung at
    /// the given [`Boundary`].
    pub fn boundary_locations(
        &self,
        row: &Row,
        boundary: Boundary,
        params: &Parameters,
    ) -> Vec<(RowBuf, usize)> {
        let mut locations = Vec::new();
        for sub_lead_idx in self.wrapped_indices(boundary, params) {
            let lead_head = Row::solve_xa_equals_b(self.row_in_plain_lead(sub_lead_idx), row);
            // This start is valid if it matches at least one of this method's lead head masks
            if self.is_lead_head_allowed(&lead_head, params) {
                locations.push((lead_head, sub_lead_idx));
            }
        }
        locations
    }

    pub fn wrapped_indices(&self, boundary: Boundary, params: &Parameters) -> Vec<usize> {
        let (start_indices, end_indices) = self.wrapped_start_end_indices(params);
        match boundary {
            Boundary::Start => start_indices,
            Boundary::End => end_indices,
        }
    }

    pub fn wrapped_start_end_indices(&self, params: &Parameters) -> (Vec<usize>, Vec<usize>) {
        // Wrap indices
        let mut start_indices = self.wrap_sub_lead_indices(&self.start_indices);
        let mut end_indices = self.wrap_sub_lead_indices(&self.end_indices);
        // If ringing a multi-part, the `{start,end}_indices` have to match.   Therefore, it makes
        // no sense to generate any starts/ends which don't have a matching end/start.  To achieve
        // this, we set both `{start,end}_indices` to the union between `start_indices` and
        // `end_indices`.
        if params.part_head_group.is_multi_part() {
            let r#union = start_indices
                .iter()
                .filter(|idx| end_indices.contains(idx))
                .copied()
                .collect_vec();
            start_indices = r#union.clone();
            end_indices = r#union;
        }
        (start_indices, end_indices)
    }

    fn wrap_sub_lead_indices(&self, indices: &[isize]) -> Vec<usize> {
        let wrap_index = |idx: &isize| -> usize {
            let lead_len_i = self.inner.lead_len() as isize;
            (*idx % lead_len_i + lead_len_i) as usize % self.inner.lead_len()
        };
        indices.iter().map(wrap_index).collect_vec()
    }

    ///////////////////
    // ALLOWED LEADS //
    ///////////////////

    pub fn is_lead_head_allowed(&self, row: &Row, params: &Parameters) -> bool {
        self.allowed_lead_head_masks(params)
            .into_iter()
            .any(|m| m.matches(row))
    }

    pub fn allowed_lead_head_masks(&self, params: &Parameters) -> Vec<Mask> {
        self.allowed_lead_head_masks_with_extras(params).0
    }

    pub fn allowed_lead_head_masks_with_extras(
        &self,
        params: &Parameters,
    ) -> (Vec<Mask>, ExtraMasks) {
        CourseSet::to_lead_masks(&self.allowed_courses, &self.inner, params)
    }
}

impl std::ops::Deref for Method {
    type Target = bellframe::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId(pub u32);

impl From<u32> for MethodId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<MethodId> for u32 {
    fn from(value: MethodId) -> Self {
        value.0
    }
}

/// The different styles of spliced that can be generated.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label (usually just
    /// [lead ends](bellframe::method::LABEL_LEAD_END)).
    LeadLabels,
    /// Splices only happen at calls.
    Calls,
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

/// Get a default shorthand given a method's title.
pub fn default_shorthand(title: &str) -> String {
    title
        .chars()
        .next()
        .expect("Can't have empty method title")
        .to_string()
}

///////////
// CALLS //
///////////

/// A type of call (e.g. bob or single)
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub id: CallId,

    // These fields determine what 'functionally' defines a specific call.  Modifying these will
    // likely invalidate any existing compositions, and thus should not be modified without also
    // changing the `CallId`.
    pub label_from: String,
    pub label_to: String,
    // TODO: Allow calls to cover multiple PNs (e.g. singles in Grandsire)
    pub place_notation: PlaceNot,

    pub symbol: String,
    pub calling_positions: Vec<String>,

    pub weight: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallId(pub u32);

impl From<u32> for CallId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<CallId> for u32 {
    fn from(value: CallId) -> Self {
        value.0
    }
}

/// How the calls in a given composition should be displayed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallDisplayStyle {
    /// Calls should be displayed as a count since the last course head.
    Positional,
    /// Calls should be displayed based on the position of the provided 'observation' [`Bell`].
    CallingPositions(Bell),
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

    /// Create a `Call` which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_call(id: CallId, place_not: PlaceNot, symbol: &str, weight: f32) -> Self {
        Self {
            id,

            symbol: symbol.to_owned(),
            calling_positions: default_calling_positions(&place_not),
            label_from: LABEL_LEAD_END.to_owned(),
            label_to: LABEL_LEAD_END.to_owned(),
            place_notation: place_not,
            weight,
        }
    }
}

/// The different types of base calls that can be created.
#[derive(Debug, Clone, Copy)]
pub enum BaseCallType {
    /// `14` bobs and `1234` singles
    Near,
    /// `1<n-2>` bobs and `1<n-2><n-1><n>` singles
    Far,
}

/// Default weight given to bobs.
pub const DEFAULT_BOB_WEIGHT: f32 = -1.8;
/// Default weight given to singles.
pub const DEFAULT_SINGLE_WEIGHT: f32 = -2.3;
/// Default weight given to user-specified [`Call`]s.
pub const DEFAULT_MISC_CALL_WEIGHT: f32 = -3.0;

pub fn base_calls(
    id_generator: &mut IdGenerator<CallId>,
    type_: BaseCallType,
    bob_weight: Option<f32>,
    single_weight: Option<f32>,
    stage: Stage,
) -> CallVec<Call> {
    let n = stage.num_bells_u8();

    let mut calls = CallVec::new();
    // Add bob
    if let Some(bob_weight) = bob_weight {
        let bob_pn = match type_ {
            BaseCallType::Near => PlaceNot::parse("14", stage).unwrap(),
            BaseCallType::Far => PlaceNot::from_slice(&mut [0, n - 3], stage).unwrap(),
        };
        calls.push(Call::lead_end_call(
            id_generator.next(),
            bob_pn,
            "-",
            bob_weight,
        ));
    }
    // Add single
    if let Some(single_weight) = single_weight {
        let single_pn = match type_ {
            BaseCallType::Near => PlaceNot::parse("1234", stage).unwrap(),
            BaseCallType::Far => {
                PlaceNot::from_slice(&mut [0, n - 3, n - 2, n - 1], stage).unwrap()
            }
        };
        calls.push(Call::lead_end_call(
            id_generator.next(),
            single_pn,
            "s",
            single_weight,
        ));
    }

    calls
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

///////////
// MUSIC //
///////////

/// A class of music that Monument should care about
#[derive(Debug, Clone, PartialEq)]
pub struct MusicType {
    pub show_total: bool,
    pub show_positions: AtRowPositions<bool>,
    pub name: String,

    pub inner: bellframe::MusicType,
    pub weights: AtRowPositions<f32>,
    pub count_range: OptionalRangeInclusive,
    // TODO: Count ranges for front/internal/back/wrap
}

impl MusicType {
    pub fn as_overall_score(&self, counts: AtRowPositions<usize>) -> f32 {
        (counts.map(|x| x as f32) * self.weights).total()
    }

    /// Return the total counts, only from [`RowPosition`](bellframe::music::RowPosition)s for which
    /// `Self::optional_weights` are not [`None`].
    pub fn masked_total(&self, counts: AtRowPositions<usize>) -> usize {
        counts.masked(!self.show_positions, 0).total()
    }

    pub fn should_show(&self) -> bool {
        self.show_total || self.show_positions.any()
    }

    /// Return the width of the smallest column large enough to be guaranteed to hold (almost)
    /// every instance of this [`MusicDisplay`] (assuming rows can't be repeated).
    pub fn col_width(&self, stage: Stage) -> usize {
        // We always pad the counts as much as required, so displaying a set of 0s results in a
        // maximum-width string (i.e. all output strings are the same length)
        let max_count_width = self.display_counts(AtRowPositions::ZERO, stage).len();
        max_count_width.max(self.name.len())
    }

    /// Generate a compact string representing a given set of music counts
    pub fn display_counts(&self, counts: AtRowPositions<usize>, stage: Stage) -> String {
        let max_counts = self.max_possible_count(stage);
        let num_items_to_show: usize = self.show_positions.map(|b| b as usize).total();

        let mut s = String::new();
        // Add total count
        if self.show_total {
            Self::write_music_count(
                &mut s,
                self.masked_total(counts),
                self.masked_total(max_counts),
            );
        }
        // Add specific counts (if there are any)
        let hide_counts = self.show_total && num_items_to_show == 1;
        if !hide_counts {
            // Add brackets if there's a total score
            if self.show_total {
                s.push_str(" (");
            }
            // Add every front/internal/back count for which we have a source
            let mut is_first_count = true;
            for (position, position_char) in [
                (RowPosition::Front, 'f'),
                (RowPosition::Internal, 'i'),
                (RowPosition::Back, 'b'),
                (RowPosition::Wrap, 'w'),
            ] {
                if !self.show_positions.get(position) {
                    continue; // Skip positions we're not showing
                }
                // Add separating comma
                if !is_first_count {
                    s.push(' ');
                }
                is_first_count = false;
                // Add the number
                Self::write_music_count(&mut s, *counts.get(position), *max_counts.get(position));
                s.push(position_char);
            }
            if self.show_total {
                s.push(')'); // Add closing brackets if there's a total score
            }
        }

        s
    }

    /// Prints the width of the largest count possible for a [`MusicType`] (assuming that rows can't be
    /// repeated).
    fn write_music_count(s: &mut String, count: usize, max_possible_count: usize) {
        // `min(4)` because we don't expect more than 9999 instances of a music type, even
        // if more are theoretically possible
        let max_count_width = max_possible_count.to_string().len().min(4);
        write!(s, "{:>width$}", count, width = max_count_width).unwrap();
    }
}

impl Deref for MusicType {
    type Target = bellframe::MusicType;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

////////////////
// MISC TYPES //
////////////////

/// Convenient description of a set of courses via [`Mask`]s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CourseSet {
    /// List of [`Mask`]s, of which courses should match at least one.
    pub masks: Vec<Mask>,
    /// If `true`, the [`Mask`]s will be expanded so that the mask matches on both strokes.  For
    /// example, `"*5678"` might expand to `["*5678", "*6587"]`.
    pub any_stroke: bool,
    /// If `true`, every [`Bell`] in every [`Mask`] will be added/subtracted from to get every
    /// combination.  For example, `"*3456"` would expand to
    /// `["*1234", "*2345", "*3456", "*4567", "*5678"]`.
    pub any_bells: bool,
}

type ExtraMasks = Vec<(Mask, RowBuf)>;

impl CourseSet {
    /// Convert many `CourseSet`s into the corresponding lead head [`Mask`]s.
    pub(crate) fn to_lead_masks(
        allowed_courses: &[CourseSet],
        method: &bellframe::Method,
        params: &Parameters,
    ) -> (Vec<Mask>, ExtraMasks) {
        let fixed_bells = params.fixed_bells();
        let specified_lead_head_masks: HashSet<Mask> = allowed_courses
            .iter()
            .flat_map(|c| c.as_lead_masks(method, &fixed_bells))
            .collect();
        let specified_course_head_masks: HashSet<Mask> = allowed_courses
            .iter()
            .flat_map(|c| c.as_course_masks(method, &fixed_bells))
            .collect();

        // Add new masks for courses which are specified in one part but not another.
        let mut extra_course_masks: ExtraMasks = Vec::new();
        let mut lead_head_masks = HashSet::<Mask>::new();
        for specified_course_mask in specified_course_head_masks {
            for part_head in params.part_head_group.rows() {
                // Add this new mask to the set of all new masks
                let ch_in_new_part = part_head * &specified_course_mask;
                for lead_head in method.lead_head().closure() {
                    lead_head_masks.insert(&ch_in_new_part * &lead_head);
                }
                // If unspecified, report that we created this new mask
                let is_specified = specified_lead_head_masks
                    .iter()
                    .any(|m| ch_in_new_part.is_subset_of(m));
                if !is_specified {
                    extra_course_masks.push((specified_course_mask.clone(), part_head.to_owned()));
                }
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

        (filtered_lead_head_masks, extra_course_masks)
    }

    /// Returns a list of [`Mask`]s which match any lead head of the courses represented by `self`.
    fn as_lead_masks(
        &self,
        method: &bellframe::Method,
        fixed_bells: &[(Bell, usize)],
    ) -> Vec<Mask> {
        let course_masks = self.as_course_masks(method, fixed_bells);
        // Convert *course* head masks into *lead* head masks (course heads are convenient for the
        // user, but Monument is internally based completely on lead heads - courses are only used
        // to interact with the user).
        let mut lead_head_masks = Vec::new();
        for course_mask in course_masks {
            for lead_head in method.lead_head().closure() {
                lead_head_masks.push(&course_mask * lead_head);
            }
        }
        lead_head_masks
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
                    expanded_masks.push(
                        Mask::from_bells(
                            mask.bells()
                                .map(|maybe_bell| maybe_bell.map(|b| b + offset)),
                        )
                        .unwrap(), // Substituting bells can't make the mask invalid
                    );
                }
            }
            course_masks = expanded_masks;
        }
        // Expand `any_stroke`
        if self.any_stroke {
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

impl From<Mask> for CourseSet {
    fn from(value: Mask) -> Self {
        Self::from(vec![value])
    }
}

impl From<Vec<Mask>> for CourseSet {
    fn from(masks: Vec<Mask>) -> Self {
        Self {
            masks,
            any_bells: false,
            any_stroke: false,
        }
    }
}

/// Attempt to fix the `fixed_bells` in the [`Mask`], returning `None` if it was not possible.
fn try_fixing_bells(mut mask: Mask, fixed_bells: &[(Bell, usize)]) -> Option<Mask> {
    for &(bell, place) in fixed_bells {
        mask.set_bell(bell, place).ok()?;
    }
    Some(mask)
}

/// An inclusive range where each side is optionally bounded.
///
/// This is essentially a combination of [`RangeInclusive`]
/// (`min..=max`), [`RangeToInclusive`](std::ops::RangeToInclusive) (`..=max`),
/// [`RangeFrom`](std::ops::RangeFrom) (`min..`) and [`RangeFull`](std::ops::RangeFull) (`..`).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
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

    pub fn contains(self, v: usize) -> bool {
        if let Some(min) = self.min {
            if v < min {
                return false; // `v` is too small
            }
        }
        if let Some(max) = self.max {
            if v > max {
                return false; // `v` is too big
            }
        }
        true // `v` is just right :D
    }

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

index_vec::define_index_type! { pub struct MethodIdx = usize; }
index_vec::define_index_type! { pub struct CallIdx = usize; }
index_vec::define_index_type! { pub struct MusicTypeIdx = usize; }
pub type MethodVec<T> = index_vec::IndexVec<MethodIdx, T>;
pub type CallVec<T> = index_vec::IndexVec<CallIdx, T>;
pub type MusicTypeVec<T> = index_vec::IndexVec<MusicTypeIdx, T>;

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
