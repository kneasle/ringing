//! Representation of a [`Composition`] generated by Monument.

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use bellframe::{music::AtRowPositions, Bell, Block, Mask, Row, RowBuf, Stage, Stroke, Truth};
use itertools::Itertools;
use lazy_st::lazy;

use crate::{
    parameters::{
        Call, CallDisplayStyle, CallId, CallIdx, MethodId, MethodIdx, MethodVec, MusicTypeVec,
        Parameters, SpliceStyle,
    },
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary,
    },
    PartHead,
};

#[allow(unused_imports)] // Used by doc comments
use crate::{
    parameters::{Method, MusicType},
    Search,
};

/// A [`Composition`] generated by Monument.
#[derive(Debug, Clone)]
pub struct Composition {
    pub(crate) id: CompositionId,
    pub(crate) stage: Stage,
    pub(crate) start_stroke: Stroke,
    pub(crate) path: Vec<PathElem>,

    // Cached values which don't change, even if a different set of params are used
    pub(crate) unique_place_bell_rows_per_bell: Vec<usize>, // TODO: Type-safe bell-vec
    pub(crate) truth: Truth,
    pub(crate) length: TotalLength,
    pub(crate) end_row: RowBuf,
    pub(crate) part_head: RowBuf,
}

/// A piece of a [`Composition`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct PathElem {
    pub start_row: RowBuf,
    pub method_id: MethodId,
    pub start_sub_lead_idx: usize,
    pub length: PerPartLength,
    pub call_to_end: Option<CallId>,
}

impl Composition {
    pub(crate) fn new(
        id: CompositionId,
        path: Vec<PathElem>,
        part_head: PartHead,
        param_data: &ParamsData,
    ) -> Self {
        let params = &param_data.params;
        let block = param_data.get_block(&path);
        Self {
            id,
            stage: params.stage,
            start_stroke: params.start_stroke,
            path,

            truth: block.truth(),
            part_head: params.part_head_group.get_row(part_head).to_owned(),
            unique_place_bell_rows_per_bell: Self::unique_place_bell_rows_per_bell(&block),
            end_row: params.end_row.clone(),
            length: TotalLength::new(block.len()),
        }
    }

    fn unique_place_bell_rows_per_bell(block: &Block<(MethodId, usize)>) -> Vec<usize> {
        // Collect which (place bell, method, sub-lead-idx) triples are rung by each bell
        let mut place_bell_rows: Vec<HashSet<(u8, MethodId, usize)>> =
            vec![HashSet::new(); block.stage().num_bells()];
        for (&(method_id, sub_lead_idx), row) in block.annot_rows() {
            for (place, bell) in row.bell_iter().enumerate() {
                place_bell_rows[bell.index()].insert((place as u8, method_id, sub_lead_idx));
            }
        }
        // Return the sizes of these sets
        place_bell_rows
            .into_iter()
            .map(|set| set.len())
            .collect_vec()
    }
}

impl PathElem {
    pub fn ends_with_plain(&self) -> bool {
        self.call_to_end.is_none()
    }

    pub(crate) fn end_sub_lead_idx(&self, params: &Parameters) -> usize {
        params
            .get_method(self.method_id)
            .add_sub_lead_idx(self.start_sub_lead_idx, self.length)
    }

    /// Returns true if going from `from` to `to` would be considered a 'splice'
    fn is_splice_between(from: &Self, to: &Self, params: &Parameters) -> bool {
        let is_continuation = from.method_id == to.method_id
            && from.end_sub_lead_idx(params) == to.start_sub_lead_idx;
        !is_continuation
    }

    fn lead_head(&self, method_map: &HashMap<MethodId, MethodData>) -> RowBuf {
        let row_in_first_lead = method_map[&self.method_id]
            .double_plain_course
            .get_row(self.start_sub_lead_idx)
            .unwrap();
        &self.start_row * !row_in_first_lead
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompositionId(pub u32);

impl From<u32> for CompositionId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<CompositionId> for u32 {
    fn from(value: CompositionId) -> Self {
        value.0
    }
}

////////////////
// PARAM DATA //
////////////////

// TODO: Move this section to `crate::params`

/// Data generated and cached purely from [`Parameters`]
#[derive(Debug)]
pub struct ParamsData<'params> {
    params: &'params Parameters,

    method_map: HashMap<MethodId, MethodData>,
    call_map: HashMap<CallId, CallIdx>,
    working_bells: Vec<Bell>,
    /// Set of labels at which the composition can end.  I.e. these are labels which are also a
    /// valid `end_index` for some method.
    valid_end_labels: HashSet<String>,
}

#[derive(Debug, Clone)]
struct MethodData {
    idx: MethodIdx,
    double_plain_course: Block<(MethodId, usize)>,
    lead_head_weights: Vec<(Mask, f32)>,
}

impl<'params> ParamsData<'params> {
    pub fn new(params: &'params Parameters) -> Self {
        ParamsData {
            params,

            method_map: Self::method_map(params),
            call_map: Self::call_map(params),
            valid_end_labels: params.valid_end_labels(),
            working_bells: params.working_bells(),
        }
    }

    /// If every [`MethodId`] in this `Composition` is in the [`Parameters`], returns `Some(map)`
    /// where `map` maps [`MethodId`]s to their corresponding [`MethodIdx`].  Otherwise, returns
    /// `None`.
    fn method_map(params: &Parameters) -> HashMap<MethodId, MethodData> {
        let mut method_map = HashMap::new();
        for (idx, method) in params.methods.iter_enumerated() {
            let mut double_plain_course = method
                .plain_course()
                .map_annots(|annot| (method.id, annot.sub_lead_idx));
            double_plain_course.extend_from_within(..);
            method_map.insert(
                method.id,
                MethodData {
                    idx,
                    double_plain_course,
                    lead_head_weights: method.lead_head_weights(params),
                },
            );
        }
        method_map
    }

    fn call_map(params: &Parameters) -> HashMap<CallId, CallIdx> {
        params
            .calls
            .iter_enumerated()
            .map(|(idx, call)| (call.id, idx))
            .collect()
    }

    /// Return a [`Block`] containing the [`Row`]s in this composition.  Each [`Row`] is annotated
    /// with a `(method index, index within a lead)` pair.  For example, splicing a lead of Bastow
    /// into Cambridge Major would create a [`Block`] which starts like:
    ///
    /// ```text
    /// Block {
    ///     12345678: (<ID of Bastow>, 0),
    ///     21436587: (<ID of Bastow>, 1),
    ///     21345678: (<ID of Bastow>, 2),
    ///     12436587: (<ID of Bastow>, 3),
    ///     14263857: (<ID of Cambridge>, 0),
    ///     41628375: (<ID of Cambridge>, 1),
    ///     14682735: (<ID of Cambridge>, 2),
    ///     41867253: (<ID of Cambridge>, 3),
    ///     48162735: (<ID of Cambridge>, 4),
    ///        ...
    /// }
    /// ```
    ///
    /// If this `Composition` uses methods or calls which are not specified in the [`Parameters`],
    /// then `None` is returned.
    fn get_block(&self, path: &[PathElem]) -> Block<(MethodId, usize)> {
        // Generate the first part
        let mut first_part =
            Block::<(MethodId, usize)>::with_leftover_row(self.params.start_row.clone());
        for elem in path {
            assert_eq!(first_part.leftover_row(), elem.start_row.as_row());
            // Copy the corresponding part of this method's (double) plain course
            let double_plain_course = &self.method_map[&elem.method_id].double_plain_course;
            let start_idx = elem.start_sub_lead_idx;
            let end_idx = start_idx + elem.length.as_usize();
            first_part.extend_range(double_plain_course, start_idx..end_idx);
            // If this PathElem ends in a call, then change the `leftover_row` to suit
            if let Some(call_id) = elem.call_to_end {
                let last_non_leftover_row = first_part.rows().next_back().unwrap();
                let new_leftover_row =
                    last_non_leftover_row * self.get_call(call_id).place_notation.transposition();
                first_part.leftover_row_mut().copy_from(&new_leftover_row);
            }
        }

        // Generate the other parts from the first
        let part_len = first_part.len();
        let mut comp = first_part;
        for _ in 0..self.params.num_parts() - 1 {
            comp.extend_from_within(..part_len);
        }
        assert_eq!(comp.leftover_row(), &self.params.end_row);
        comp
    }

    fn get_method(&self, id: MethodId) -> &Method {
        &self.params.methods[self.method_map[&id].idx]
    }

    fn get_call(&self, id: CallId) -> &Call {
        &self.params.calls[self.call_map[&id]]
    }
}

impl<'p> Deref for ParamsData<'p> {
    type Target = Parameters;

    fn deref(&self) -> &Self::Target {
        self.params
    }
}

/////////////
// CACHING //
/////////////

/// Struct which caches expensive properties about [`Composition`]s, so that these values will be
/// calculated once and then re-used for future queries.
#[derive(Debug, Default)]
pub struct CompositionCache {
    per_param_cache: Option<(Parameters, HashMap<CompositionId, PerParamCacheEntry>)>,
    music_counts: HashMap<bellframe::MusicType, PerMusicTypeCache>,
}

#[derive(Debug)]
enum PerParamCacheEntry {
    Invalid,
    Valid(ValidPerParamCache),
}

#[derive(Debug)]
struct ValidPerParamCache {
    // TODO: Add values here
}

#[derive(Debug)]
pub struct CacheWithParams<'cache> {
    param_wide_values: &'cache mut HashMap<CompositionId, PerParamCacheEntry>,
    music_type_cache: MusicTypeCache<'cache>,
}

#[derive(Debug)]
pub struct MusicTypeCache<'cache> {
    /// Map to which the contents of `music_type_caches` should be returned
    cache_source: &'cache mut HashMap<bellframe::MusicType, PerMusicTypeCache>,
    /// Set of [`PerMusicTypeCache`]s, to be added back to the cache once this is dropped
    values: Vec<(bellframe::MusicType, PerMusicTypeCache)>,
    /// For each of Monument's music types, which index from `music_type_caches` should be used
    /// to access the cache?
    indices: MusicTypeVec<usize>,
}

type PerMusicTypeCache = HashMap<CompositionId, AtRowPositions<usize>>;

impl Drop for MusicTypeCache<'_> {
    fn drop(&mut self) {
        for (mt, cache) in self.values.drain(..) {
            self.cache_source.insert(mt, cache);
        }
    }
}

impl CompositionCache {
    pub fn with_params<'cache>(&'cache mut self, params: &Parameters) -> CacheWithParams<'cache> {
        // Get per-param values, preserving the cache if the params haven't changed
        if self.per_param_cache.as_ref().map(|(p, _)| p) != Some(params) {
            self.per_param_cache = Some((params.clone(), HashMap::new()));
        }
        let param_wide_values = &mut self.per_param_cache.as_mut().unwrap().1;

        // Get the music type caches
        let mut values = Vec::new();
        let mut indices = MusicTypeVec::new();
        for music_type in &params.music_types {
            let music_type = &music_type.inner;
            if let Some(idx) = values.iter().position(|(t, _)| t == music_type) {
                indices.push(idx);
            } else {
                indices.push(values.len());
                let cache_entry = self.music_counts.remove(music_type).unwrap_or_default();
                values.push((music_type.clone(), cache_entry));
            }
        }

        CacheWithParams {
            param_wide_values,
            music_type_cache: MusicTypeCache {
                cache_source: &mut self.music_counts,
                values,
                indices,
            },
        }
    }
}

/////////////////
// COMP VALUES //
/////////////////

/// Struct which combines a [`Composition`] with a set of [`Parameters`] from which the extra data
/// is taken.
#[derive(Debug)]
pub struct CompositionValues<'comp> {
    pub composition: &'comp Composition,

    pub call_string: String,
    pub method_counts: MethodVec<TotalLength>,
    pub music_counts: MusicTypeVec<AtRowPositions<usize>>,
    pub music_score: f32,
    pub atw_factor: f32,
    // TODO: Replace this with a simple getter
    pub total_score: f32,
}

impl Deref for CompositionValues<'_> {
    type Target = Composition;

    fn deref(&self) -> &Self::Target {
        self.composition
    }
}

impl Composition {
    pub fn values(&self, params: &ParamsData) -> Option<CompositionValues> {
        if !self.do_cheap_checks(params) {
            return None;
        }

        let music_counts = self.calculate_music_counts(params);
        let music_score = music_counts_to_score(&music_counts, params);
        let atw_factor = self.atw_factor(params);
        let comp_values = CompositionValues {
            composition: self,

            call_string: self.call_string(params),
            method_counts: self.method_counts(params),
            music_counts,
            music_score,
            atw_factor,
            total_score: self.total_score(music_score, atw_factor, params),
        };

        if !comp_values.do_non_cheap_checks(params) {
            return None;
        }

        Some(comp_values)
    }

    pub fn values_with_cache<'comp>(
        &'comp self,
        params: &ParamsData,
        cache: &mut CacheWithParams,
    ) -> Option<CompositionValues<'comp>> {
        use PerParamCacheEntry::*;

        let per_param_cache = match cache.param_wide_values.get(&self.id) {
            Some(Invalid) => return None, // Known invalid
            Some(Valid(e)) => Some(e),    // Known good, with values
            None => None,                 // We just don't know yet
        };
        let is_known_good = per_param_cache.is_some();

        if !is_known_good && !self.do_cheap_checks(params) {
            cache.param_wide_values.insert(self.id, Invalid);
            return None;
        }

        let music_counts =
            self.calculate_music_counts_with_cache(params, &mut cache.music_type_cache);
        let music_score = music_counts_to_score(&music_counts, params);
        let atw_factor = self.atw_factor(params);
        let values = CompositionValues {
            composition: self,

            call_string: self.call_string(params),
            method_counts: self.method_counts(params),
            music_score: music_counts_to_score(&music_counts, params),
            music_counts,
            atw_factor,
            total_score: self.total_score(music_score, atw_factor, params),
        };

        if !is_known_good {
            if self.do_cheap_checks(params) {
                cache
                    .param_wide_values
                    .insert(self.id, Valid(ValidPerParamCache {}));
            } else {
                cache.param_wide_values.insert(self.id, Invalid);
                return None;
            }
        }

        Some(values)
    }

    fn calculate_music_counts_with_cache(
        &self,
        params: &ParamsData,
        cache: &mut MusicTypeCache,
    ) -> MusicTypeVec<AtRowPositions<usize>> {
        let block = lazy!({
            println!("Calculating block");
            params.get_block(&self.path)
        });

        params
            .music_types
            .iter_enumerated()
            .map(|(idx, music_type)| {
                let (_mt, comp_id_map) = &mut cache.values[cache.indices[idx]];
                *comp_id_map
                    .entry(self.id)
                    .or_insert_with(|| music_type.count_block(&*block, self.start_stroke))
            })
            .collect()
    }

    /// Generate a human-friendly [`String`] summarising the calling of this composition.  For
    /// example, [this composition](https://complib.org/composition/87419) would have a
    /// `call_string` of `D[B]BL[W]N[M]SE[sH]NCYW[sH]`.
    fn call_string(&self, params: &ParamsData) -> String {
        let needs_brackets =
            params.is_spliced() || params.call_display_style == CallDisplayStyle::Positional;
        let is_snap_start = self.path[0].start_sub_lead_idx > 0;
        let is_snap_finish = self.path.last().unwrap().end_sub_lead_idx(params) > 0;

        let mut path_iter = self.path.iter().peekable();

        let mut s = String::new();
        if params.call_display_style == CallDisplayStyle::Positional {
            s.push('#');
        }
        s.push_str(if is_snap_start { "<" } else { "" });
        while let Some(path_elem) = path_iter.next() {
            // Method text
            if params.is_spliced() || params.call_display_style == CallDisplayStyle::Positional {
                // Add one shorthand for every lead *covered* (not number of lead heads reached)
                //
                // TODO: Deal with half-lead spliced
                let method = params.get_method(path_elem.method_id);
                let num_leads_covered = num_leads_covered(
                    method.lead_len(),
                    path_elem.start_sub_lead_idx,
                    path_elem.length,
                );
                for _ in 0..num_leads_covered {
                    s.push_str(&method.shorthand());
                }
            }
            // Call text
            if let Some(call_id) = path_elem.call_to_end {
                let call = params.get_call(call_id);
                s.push_str(if needs_brackets { "[" } else { "" });
                // Call position
                match params.call_display_style {
                    CallDisplayStyle::CallingPositions => {
                        let row_after_call = path_iter
                            .peek()
                            .map_or(&self.part_head, |path_elem| &path_elem.start_row);
                        let place_of_calling_bell = row_after_call.place_of(params.calling_bell);
                        let calling_position =
                            call.calling_positions[place_of_calling_bell as usize];
                        s.extend(call.short_symbol());
                        s.push(calling_position);
                    }
                    // TODO: Compute actual counts for positional calls
                    CallDisplayStyle::Positional => s.push(call.symbol),
                }
                s.push_str(if needs_brackets { "]" } else { "" });
            }
        }
        s.push_str(if is_snap_finish { ">" } else { "" });

        s
    }

    /// A slice containing the number of [`Row`]s generated for each [`Method`] used in the
    /// [`Search`].  These are stored in the same order as the [`Method`]s.
    fn method_counts(&self, params: &ParamsData) -> MethodVec<TotalLength> {
        let mut method_counts = index_vec::index_vec![TotalLength::ZERO; params.methods.len()];
        for elem in &self.path {
            let idx = params.method_map[&elem.method_id].idx;
            method_counts[idx] += elem.length.as_total(&params.part_head_group);
        }
        method_counts
    }

    /// Returns a factor in `0.0..=1.0` where 0.0 means nothing was rung and 1.0 means everything
    /// was rung (i.e. the composition is atw)
    fn atw_factor(&self, params: &ParamsData) -> f32 {
        // Determine how many `(bell, place, method, sub-lead-idx)` quadruples are actually possible
        let total_unique_row_positions = params.working_bells.len() // Working bells
            * params.working_bells.len() // Working place bells
            * params.methods.iter().map(|m| m.lead_len()).sum::<usize>();
        // Determine how many we actuall rang
        let run_unique_row_positions = params
            .working_bells
            .iter()
            .map(|b| self.unique_place_bell_rows_per_bell[b.index()])
            .sum::<usize>();

        run_unique_row_positions as f32 / total_unique_row_positions as f32
    }

    /// The number of *instances* of each [`MusicType`] in the [`Parameters`].
    ///
    /// This function computes all the rows of the composition, and therefore is quite expensive to
    /// call.  It's fine to call it every so often (e.g. every time a valid composition is
    /// generated), but not if there is a lot of time pressure (e.g. recomputing it for every
    /// composition on every GUI frame).
    fn calculate_music_counts(&self, params: &ParamsData) -> MusicTypeVec<AtRowPositions<usize>> {
        let block = params.get_block(&self.path);
        params
            .music_types
            .iter()
            .map(|mt| mt.count(&block, !self.start_stroke))
            .collect()
    }

    /// The total score generated by this composition from all the different weights (music, calls,
    /// changes of method, handbell coursing, etc.).
    fn total_score(&self, music_score: f32, atw_factor: f32, params: &ParamsData) -> f32 {
        let mut total_score = 0.0;
        // Music
        total_score += music_score;
        // ATW
        if let Some(atw_weight) = params.atw_weight {
            total_score += atw_factor * atw_weight;
        }
        // Calls
        for elem in &self.path {
            if let Some(call_id) = elem.call_to_end {
                let call = params.get_call(call_id);
                total_score += call.weight * params.num_parts() as f32;
            }
        }
        // Splices
        let mut changes_of_method = 0;
        for (e1, e2) in self.path.iter().tuple_windows() {
            if PathElem::is_splice_between(e1, e2, params) {
                changes_of_method += params.num_parts();
            }
        }
        let first_elem = self.path.first().unwrap();
        let last_elem = self.path.last().unwrap();
        if PathElem::is_splice_between(last_elem, first_elem, params) {
            // -1 because there's no splice around the end/start of the composition
            changes_of_method += params.num_parts() - 1;
        }
        total_score += changes_of_method as f32 * params.splice_weight;
        // Course weights
        // TODO: Cache this
        for elem in &self.path {
            let lead_head_weights = &params.method_map[&elem.method_id].lead_head_weights;
            let lead_head = elem.lead_head(&params.method_map);
            for part_head in self.part_head.closure() {
                let lead_head_in_part = part_head * &lead_head;
                for (mask, weight) in lead_head_weights {
                    if mask.matches(&lead_head_in_part) {
                        total_score += *weight * elem.length.as_usize() as f32;
                    }
                }
            }
        }
        total_score
    }
}

/////////////////////
// VALIDITY CHECKS //
/////////////////////

impl Composition {
    /// Perform cheap checks on this composition which don't involve looking up methods or calls.
    /// The main reason to do this is to quickly reject compositions which are being generated by the
    /// search routine
    #[must_use]
    fn do_cheap_checks(&self, params: &ParamsData) -> bool {
        if self.stage != params.stage {
            return false; // Stage mismatch
        }
        if !params.length.contains(&self.length) {
            return false; // Length mismatch
        }
        if !params.part_head_group.is_row_generator(&self.part_head) {
            return false; // Composition doesn't end in a valid part
        }
        if self.path[0].start_row != params.start_row {
            return false; // Doesn't start in the right row
        }
        for elem in &self.path {
            if !params.method_map.contains_key(&elem.method_id) {
                return false; // Composition uses a method not in params
            }
            if let Some(call_id) = elem.call_to_end {
                if !params.call_map.contains_key(&call_id) {
                    return false; // Composition uses a call not in params
                }
            }
        }

        true // Can't reject composition this easily
    }
}

impl CompositionValues<'_> {
    fn do_non_cheap_checks(&self, params: &ParamsData) -> bool {
        if !self.are_methods_satisfied(params) {
            return false;
        }
        if !self.is_splice_style_satisfied(params) {
            return false;
        }
        if params.require_atw && !self.is_atw() {
            return false;
        }
        if params.require_truth && !self.is_true() {
            return false; // Composition is false but we needed it to be true
        }
        if self.end_row != params.end_row {
            return false; // Comps ends on the wrong row
        }
        for (mt, counts) in params.music_types.iter().zip_eq(&self.music_counts) {
            let total = mt.masked_total(*counts);
            if !mt.count_range.contains(total) {
                return false; // Music count range isn't satisfied
            }
        }
        // Start indices
        let first_elem = &self.path[0];
        let start_indices = params
            .get_method(first_elem.method_id)
            .wrapped_indices(Boundary::Start, params);
        if !start_indices.contains(&first_elem.start_sub_lead_idx) {
            return false; // Composition couldn't start in this way
        }
        // End indices
        let last_elem = self.path.last().unwrap();
        let end_sub_lead_idx = last_elem.end_sub_lead_idx(params);
        match last_elem.call_to_end {
            None => {
                // The last element ends with a plain lead, so we need to check that this chunk's
                // end sub-lead index is valid
                let end_indices = params
                    .get_method(last_elem.method_id)
                    .wrapped_indices(Boundary::End, params);
                if !end_indices.contains(&end_sub_lead_idx) {
                    return false; // End index isn't valid for this method
                }
            }
            Some(call_id) => {
                // If the composition ends with a call, then the situation is more complex; we need
                // to check that the call leads to a method which could end immediately
                // (conceptually, this introduces an imaginary 0-length 'path-elem' at the end)
                let end_label = &params.get_call(call_id).label_to;
                if !params.valid_end_labels.contains(end_label) {
                    return false; // Call's label_to can't correspond to a valid end idx
                }
            }
        }
        // Check for continuity over the part head (this checks for cases like finishing each part
        // at a snap and then starting the next part at the lead-end)
        if params.is_multipart() {
            let start_labels = params
                .get_method(first_elem.method_id)
                .get_labels(first_elem.start_sub_lead_idx);
            let end_labels = params
                .get_method(last_elem.method_id)
                .get_labels(end_sub_lead_idx);
            let is_splice_possible = start_labels.iter().any(|label| end_labels.contains(label));
            let is_continuous_lead = first_elem.start_sub_lead_idx == end_sub_lead_idx
                && first_elem.method_id == last_elem.method_id;
            if !is_splice_possible && !is_continuous_lead && last_elem.ends_with_plain() {
                return false; // No way to splice over the part heads
            }
            if first_elem.start_sub_lead_idx != end_sub_lead_idx {
                return false; // Composition isn't continuous over the part heads
            }
        }

        true // Composition is all OK
    }

    fn are_methods_satisfied(&self, params: &ParamsData) -> bool {
        let allowed_lead_heads: MethodVec<Vec<Mask>> = params
            .params
            .methods
            .iter()
            .map(|m| m.allowed_lead_head_masks(params))
            .collect();

        for path_elem in &self.composition.path {
            let method_idx = params.method_map[&path_elem.method_id].idx;
            // Check if lead head is valid
            let lead_head = path_elem.lead_head(&params.method_map);
            if !allowed_lead_heads[method_idx]
                .iter()
                .any(|m| m.matches(&lead_head))
            {
                return false; // If no lead heads matched, this course isn't valid anymore
            }
        }

        // TODO: Check method ranges using refined lengths

        true
    }

    fn is_splice_style_satisfied(&self, params: &ParamsData) -> bool {
        match params.splice_style {
            SpliceStyle::LeadLabels => true, // Assume all comps are still valid
            SpliceStyle::Calls => {
                let is_invalid_splice = |e1: &PathElem, e2: &PathElem| -> bool {
                    // PERF: use the method map to speed up the splicing check
                    PathElem::is_splice_between(e1, e2, params) && e1.ends_with_plain()
                };
                for (elem1, elem2) in self.composition.path.iter().tuple_windows() {
                    if is_invalid_splice(elem1, elem2) {
                        return false; // Splice but no call
                    }
                }
                if params.is_multipart() && !self.composition.path.is_empty() {
                    let first = self.composition.path.first().unwrap();
                    let last = self.composition.path.last().unwrap();
                    if is_invalid_splice(last, first) {
                        return false; // Splice but no call over part head
                    }
                }
                true
            }
        }
    }
}

/////////////
// GETTERS //
/////////////

impl<'comp> CompositionValues<'comp> {
    /// The number of [`Row`]s in this composition.
    pub fn length(&self) -> usize {
        self.composition.length.as_usize()
    }

    pub fn part_head(&self) -> &Row {
        &self.composition.part_head
    }

    pub fn is_true(&self) -> bool {
        self.composition.truth.is_true()
    }

    /// The average score generated by each [`Row`] in this composition.  This is equal to
    /// `self.total_score() / self.length() as f32`.
    pub fn score_per_row(&self) -> f32 {
        self.total_score / self.length() as f32
    }

    pub fn is_atw(&self) -> bool {
        self.atw_factor == 1.0
    }
}

///////////
// UTILS //
///////////

fn music_counts_to_score(counts: &MusicTypeVec<AtRowPositions<usize>>, params: &Parameters) -> f32 {
    let mut music_score = 0.0;
    for (count, music_type) in counts.iter().zip_eq(&params.music_types) {
        music_score += music_type.as_overall_score(*count);
    }
    music_score
}

/// Return the number of leads covered by some [`Chunk`]
fn num_leads_covered(lead_len: usize, start_sub_lead_idx: usize, length: PerPartLength) -> usize {
    assert_ne!(length, PerPartLength::ZERO); // 0-length chunks shouldn't exist
    let dist_to_end_of_first_lead = lead_len - start_sub_lead_idx;
    let rows_after_end_of_first_lead = length.as_usize().saturating_sub(dist_to_end_of_first_lead);
    // `+ 1` for the first lead
    crate::utils::div_rounding_up(rows_after_end_of_first_lead, lead_len) + 1
}

#[cfg(test)]
mod tests {
    use crate::utils::lengths::PerPartLength;

    #[test]
    fn num_leads_covered() {
        assert_eq!(super::num_leads_covered(32, 0, PerPartLength::new(32)), 1);
        assert_eq!(super::num_leads_covered(32, 2, PerPartLength::new(32)), 2);
        assert_eq!(super::num_leads_covered(32, 2, PerPartLength::new(30)), 1);
        assert_eq!(super::num_leads_covered(32, 0, PerPartLength::new(2)), 1);
        assert_eq!(super::num_leads_covered(32, 16, PerPartLength::new(24)), 2);
    }
}
