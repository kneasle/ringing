//! Representation of a [`Composition`] generated by Monument.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use bellframe::{music::AtRowPositions, Bell, Block, Mask, Row, RowBuf, Stage, Stroke};
use itertools::Itertools;

use crate::{
    parameters::{
        Call, CallDisplayStyle, CallId, CallIdx, MethodId, MethodIdx, MethodVec, MusicTypeVec,
        Parameters, SpliceStyle,
    },
    utils::{
        lengths::{PerPartLength, TotalLength},
        Boundary,
    },
};

#[allow(unused_imports)] // Used by doc comments
use crate::{
    parameters::{Method, MusicType},
    Search,
};

/// A [`Composition`] generated by Monument.
#[derive(Debug, Clone)]
pub struct Composition {
    pub(crate) stage: Stage,
    pub(crate) start_stroke: Stroke,
    pub(crate) path: Vec<PathElem>,
    pub(crate) length: TotalLength,
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

impl PathElem {
    pub fn ends_with_plain(&self) -> bool {
        self.call_to_end.is_none()
    }

    pub(crate) fn end_sub_lead_idx(&self, params: &Parameters) -> usize {
        params
            .get_method(self.method_id)
            .add_sub_lead_idx(self.start_sub_lead_idx, self.length)
    }

    /// Returns true if going from `self` to `next` would be considered a 'splice'
    fn is_splice_when_followed_by(&self, next: &Self, params: &Parameters) -> bool {
        let is_continuation = self.method_id == next.method_id
            && self.end_sub_lead_idx(params) == next.start_sub_lead_idx;
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

//////////////////
// CALCULATIONS //
//////////////////

/// Struct which combines a [`Composition`] with a set of [`Parameters`] from which the extra data
/// is taken.
#[derive(Debug, Clone)]
pub struct CompositionGetter<'c, 'p> {
    composition: &'c Composition,
    params: &'p Parameters,

    method_map: HashMap<MethodId, MethodData>,
    call_map: HashMap<CallId, CallIdx>,
    /// Set of labels at which the composition can end.  I.e. these are labels which are also a
    /// valid `end_index` for some method.
    valid_end_labels: HashSet<String>,
    block: Block<(MethodId, usize)>,
}

#[derive(Debug, Clone)]
struct MethodData {
    idx: MethodIdx,
    double_plain_course: Block<(MethodId, usize)>,
    lead_head_weights: Vec<(Mask, f32)>,
}

impl<'c, 'p> CompositionGetter<'c, 'p> {
    /* CONSTRUCTION/VALIDATION */

    pub fn new(composition: &'c Composition, params: &'p Parameters) -> Option<Self> {
        // Do cheap checks before calculating anything.  This is useful because the search
        // algorithm produces tons of too-short compositions, so it's worth validating length
        // quickly and thus rejecting these.
        if !Self::do_cheap_checks(composition, params) {
            return None;
        }

        // Once the cheap checks pass, compute useful cached values and build the `getter`
        let method_map = Self::method_map(composition, params)?;
        let call_map = Self::call_map(composition, params)?;
        let getter = CompositionGetter {
            composition,
            params,

            block: Self::block(composition, params, &method_map, &call_map),
            valid_end_labels: params.valid_end_labels(),
            method_map,
            call_map,
        };

        if !getter.do_non_cheap_checks() {
            return None;
        }

        Some(getter) // If all checks didn't find problems, the getter is valid
    }

    /// Perform cheap checks on this composition which don't involve looking up methods or calls.
    /// The main reason to do this is to quickly reject compositions which are being generated by the
    /// search routine
    #[must_use]
    fn do_cheap_checks(composition: &Composition, params: &Parameters) -> bool {
        if composition.stage != params.stage {
            return false; // Stage mismatch
        }
        if !params.length.contains(&composition.length) {
            return false; // Length mismatch
        }
        if !params
            .part_head_group
            .is_row_generator(&composition.part_head)
        {
            return false; // Composition doesn't end in a valid part
        }
        if composition.path[0].start_row != params.start_row {
            return false; // Doesn't start in the right row
        }

        true // Can't reject composition this easily
    }

    /// If every [`MethodId`] in this `Composition` is in the [`Parameters`], returns `Some(map)`
    /// where `map` maps [`MethodId`]s to their corresponding [`MethodIdx`].  Otherwise, returns
    /// `None`.
    fn method_map(
        composition: &Composition,
        params: &Parameters,
    ) -> Option<HashMap<MethodId, MethodData>> {
        let methods = composition
            .path
            .iter()
            .map(|e| e.method_id)
            .collect::<HashSet<_>>();

        let mut method_map = HashMap::new();
        for id in methods {
            let idx = params.methods.position(|m| m.id == id)?;
            let method = &params.methods[idx];
            let mut double_plain_course = method
                .plain_course()
                .map_annots(|annot| (id, annot.sub_lead_idx));
            double_plain_course.extend_from_within(..);
            method_map.insert(
                id,
                MethodData {
                    idx,
                    double_plain_course,
                    lead_head_weights: method.lead_head_weights(params),
                },
            );
        }
        Some(method_map)
    }

    /// If every [`CallId`] in this `Composition` is in the [`Parameters`], returns `Some(map)`
    /// where `map` maps [`CallId`]s to their corresponding [`CallId`].  Otherwise, returns `None`.
    fn call_map(
        composition: &Composition,
        params: &Parameters,
    ) -> Option<HashMap<CallId, CallIdx>> {
        let call_ids_used = composition
            .path
            .iter()
            .filter_map(|e| e.call_to_end)
            .collect::<HashSet<_>>();

        let mut call_map = HashMap::new();
        for id in call_ids_used {
            call_map.insert(id, params.calls.position(|c| c.id == id)?);
        }
        Some(call_map)
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
    fn block(
        composition: &Composition,
        params: &Parameters,
        method_map: &HashMap<MethodId, MethodData>,
        call_map: &HashMap<CallId, CallIdx>,
    ) -> Block<(MethodId, usize)> {
        // Generate the first part
        let mut first_part =
            Block::<(MethodId, usize)>::with_leftover_row(params.start_row.clone());
        for elem in &composition.path {
            assert_eq!(first_part.leftover_row(), elem.start_row.as_row());
            // Copy the corresponding part of this method's (double) plain course
            let double_plain_course = &method_map[&elem.method_id].double_plain_course;
            let start_idx = elem.start_sub_lead_idx;
            let end_idx = start_idx + elem.length.as_usize();
            first_part.extend_range(double_plain_course, start_idx..end_idx);
            // If this PathElem ends in a call, then change the `leftover_row` to suit
            if let Some(call_id) = elem.call_to_end {
                let call = &params.calls[call_map[&call_id]];
                let last_non_leftover_row = first_part.rows().next_back().unwrap();
                let new_leftover_row = last_non_leftover_row * call.place_notation.transposition();
                first_part.leftover_row_mut().copy_from(&new_leftover_row);
            }
        }

        // Generate the other parts from the first
        let part_len = first_part.len();
        let mut comp = first_part;
        for _ in 0..params.num_parts() - 1 {
            comp.extend_from_within(..part_len);
        }
        assert_eq!(comp.len(), composition.length.as_usize());
        assert_eq!(comp.leftover_row(), &params.end_row);
        comp
    }

    fn do_non_cheap_checks(&self) -> bool {
        if !self.are_methods_satisfied() {
            return false;
        }
        if !self.is_splice_style_satisfied() {
            return false;
        }
        if self.params.require_atw && !self.is_atw() {
            return false;
        }
        if self.params.require_truth && !self.block.is_true() {
            return false; // Composition is false but we needed it to be true
        }
        if self.block.leftover_row() != &self.params.end_row {
            return false; // Comps ends on the wrong row
        }
        for (mt, counts) in self.params.music_types.iter().zip_eq(self.music_counts()) {
            let total = mt.masked_total(counts);
            if !mt.count_range.contains(total) {
                return false; // Music count range isn't satisfied
            }
        }
        // Start indices
        let first_elem = &self.composition.path[0];
        let start_indices = self
            .get_method(first_elem.method_id)
            .wrapped_indices(Boundary::Start, self.params);
        if !start_indices.contains(&first_elem.start_sub_lead_idx) {
            return false; // Composition couldn't start in this way
        }
        // End indices
        let last_elem = self.composition.path.last().unwrap();
        let end_sub_lead_idx = last_elem.end_sub_lead_idx(self.params);
        match last_elem.call_to_end {
            None => {
                // The last element ends with a plain lead, so we need to check that this chunk's
                // end sub-lead index is valid
                let end_indices = self
                    .get_method(last_elem.method_id)
                    .wrapped_indices(Boundary::End, self.params);
                if !end_indices.contains(&end_sub_lead_idx) {
                    return false; // End index isn't valid for this method
                }
            }
            Some(call_id) => {
                // If the composition ends with a call, then the situation is more complex; we need
                // to check that the call leads to a method which could end immediately
                // (conceptually, this introduces an imaginary 0-length 'path-elem' at the end)
                let end_label = &self.get_call(call_id).label_to;
                if !self.valid_end_labels.contains(end_label) {
                    return false; // Call's label_to can't correspond to a valid end idx
                }
            }
        }
        // Check for continuity over the part head (this checks for cases like finishing each part
        // at a snap and then starting the next part at the lead-end)
        if self.params.is_multipart() {
            let start_labels = self
                .get_method(first_elem.method_id)
                .get_labels(first_elem.start_sub_lead_idx);
            let end_labels = self
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

    fn are_methods_satisfied(&self) -> bool {
        let allowed_lead_heads: MethodVec<Vec<Mask>> = self
            .params
            .methods
            .iter()
            .map(|m| m.allowed_lead_head_masks(self.params))
            .collect();

        let mut method_counts: MethodVec<_> =
            index_vec::index_vec![TotalLength::ZERO; self.params.methods.len()];
        for path_elem in &self.composition.path {
            let method_idx = self.method_map[&path_elem.method_id].idx;
            method_counts[method_idx] += path_elem.length.as_total(&self.params.part_head_group);
            // Check if lead head is valid
            let lead_head = path_elem.lead_head(&self.method_map);
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

    fn is_splice_style_satisfied(&self) -> bool {
        match self.params.splice_style {
            SpliceStyle::LeadLabels => true, // Assume all comps are still valid
            SpliceStyle::Calls => {
                let is_invalid_splice = |e1: &PathElem, e2: &PathElem| -> bool {
                    // PERF: use the method map to speed up the splicing check
                    e1.is_splice_when_followed_by(e2, self.params) && e1.ends_with_plain()
                };
                for (elem1, elem2) in self.composition.path.iter().tuple_windows() {
                    if is_invalid_splice(elem1, elem2) {
                        return false; // Splice but no call
                    }
                }
                if self.params.is_multipart() && !self.composition.path.is_empty() {
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

    /* GETTERS */

    /// The number of [`Row`]s in this composition.
    pub fn length(&self) -> usize {
        self.composition.length.as_usize()
    }

    pub fn part_head(&self) -> &Row {
        &self.composition.part_head
    }

    pub fn is_true(&self) -> bool {
        self.block.is_true()
    }

    /// Generate a human-friendly [`String`] summarising the calling of this composition.  For
    /// example, [this composition](https://complib.org/composition/87419) would have a
    /// `call_string` of `D[B]BL[W]N[M]SE[sH]NCYW[sH]`.
    pub fn call_string(&self) -> String {
        let Self {
            composition,
            params,
            ..
        } = self;

        let needs_brackets =
            params.is_spliced() || params.call_display_style == CallDisplayStyle::Positional;
        let is_snap_start = composition.path[0].start_sub_lead_idx > 0;
        let is_snap_finish = composition.path.last().unwrap().end_sub_lead_idx(params) > 0;

        let mut path_iter = composition.path.iter().peekable();

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
                let method = self.get_method(path_elem.method_id);
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
                let call = self.get_call(call_id);
                s.push_str(if needs_brackets { "[" } else { "" });
                // Call position
                match params.call_display_style {
                    CallDisplayStyle::CallingPositions(calling_bell) => {
                        let row_after_call = path_iter
                            .peek()
                            .map_or(&composition.part_head, |path_elem| &path_elem.start_row);
                        let place_of_calling_bell = row_after_call.place_of(calling_bell).unwrap();
                        let calling_position = &call.calling_positions[place_of_calling_bell];
                        s.push_str(call.short_symbol());
                        s.push_str(calling_position);
                    }
                    // TODO: Compute actual counts for positional calls
                    CallDisplayStyle::Positional => s.push_str(&call.symbol),
                }
                s.push_str(if needs_brackets { "]" } else { "" });
            }
        }
        s.push_str(if is_snap_finish { ">" } else { "" });

        s
    }

    /// The average score generated by each [`Row`] in this composition.  This is equal to
    /// `self.total_score() / self.length() as f32`.
    pub fn score_per_row(&self) -> f32 {
        self.total_score() / self.length() as f32
    }

    /// The total score generated by this composition from all the different weights (music, calls,
    /// changes of method, handbell coursing, etc.).
    pub fn total_score(&self) -> f32 {
        let mut total_score = 0.0;
        // Music
        total_score += self.music_score();
        // ATW
        if let Some(atw_weight) = self.params.atw_weight {
            total_score += self.atw_factor() * atw_weight;
        }
        // Calls
        for elem in &self.composition.path {
            if let Some(call_id) = elem.call_to_end {
                let call = self.get_call(call_id);
                total_score += call.weight * self.params.num_parts() as f32;
            }
        }
        // Splices
        let mut changes_of_method = 0;
        for (e1, e2) in self.composition.path.iter().tuple_windows() {
            if e1.is_splice_when_followed_by(e2, self.params) {
                changes_of_method += self.params.num_parts();
            }
        }
        let first_elem = self.composition.path.first().unwrap();
        let last_elem = self.composition.path.last().unwrap();
        if last_elem.is_splice_when_followed_by(first_elem, self.params) {
            // -1 because there's no splice around the end/start of the composition
            changes_of_method += self.params.num_parts() - 1;
        }
        total_score += changes_of_method as f32 * self.params.splice_weight;
        // Course weights
        for elem in &self.composition.path {
            let lead_head_weights = &self.method_map[&elem.method_id].lead_head_weights;
            let lead_head = elem.lead_head(&self.method_map);
            for part_head in self.composition.part_head.closure() {
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

    pub fn is_atw(&self) -> bool {
        self.atw_factor() == 1.0
    }

    /// Returns a factor in `0.0..=1.0` where 0.0 means nothing was rung and 1.0 means everything
    /// was rung (i.e. the composition is atw)
    pub fn atw_factor(&self) -> f32 {
        // Determine the working bells, and store this in a bitmap
        let working_bells = self.params.working_bells();
        let mut is_working = vec![false; self.composition.stage.num_bells()];
        for b in &working_bells {
            is_working[b.index()] = true;
        }
        // Determine how many `(bell, place, method, sub-lead-idx)` quadruples are actually possible
        let total_unique_row_positions = working_bells.len() // Working bells
            * working_bells.len() // Working place bells
            * self.params.methods.iter().map(|m| m.lead_len()).sum::<usize>();
        // Compute which `(bell, place, method, sub-lead-index)` quadruples have been rung in this
        // composition.
        //
        // TODO: Use a more efficient storage method for common cases like ringing whole leads
        let mut rung_place_bell_positions = HashSet::<(Bell, u8, MethodId, usize)>::new();
        for (&(method_id, sub_lead_idx), row) in self.block.annot_rows() {
            for (place, bell) in row.bell_iter().enumerate() {
                if is_working[bell.index()] {
                    rung_place_bell_positions.insert((bell, place as u8, method_id, sub_lead_idx));
                }
            }
        }

        rung_place_bell_positions.len() as f32 / total_unique_row_positions as f32
    }

    /// A slice containing the number of [`Row`]s generated for each [`Method`] used in the
    /// [`Search`].  These are stored in the same order as the [`Method`]s.
    pub fn method_counts(&self) -> MethodVec<TotalLength> {
        let mut method_counts = index_vec::index_vec![TotalLength::ZERO; self.params.methods.len()];
        for elem in &self.composition.path {
            let idx = self.method_map[&elem.method_id].idx;
            method_counts[idx] += elem.length.as_total(&self.params.part_head_group);
        }
        method_counts
    }

    /// Compute [`Self::music_counts`] and use it to calculate [`Self::music_score`].  Calling
    /// them separately would cause the music to be calculated twice.
    pub fn music_counts_and_score(&self) -> (MusicTypeVec<AtRowPositions<usize>>, f32) {
        let music_counts = self.music_counts();
        let mut music_score = 0.0;
        for (count, music_type) in music_counts.iter().zip_eq(&self.params.music_types) {
            music_score += music_type.as_overall_score(*count);
        }
        (music_counts, music_score)
    }

    /// Score generated by just the [`MusicType`]s (not including calls, changes of methods,
    /// etc.).
    pub fn music_score(&self) -> f32 {
        self.music_counts_and_score().1
    }

    /// The number of *instances* of each [`MusicType`] in the [`Parameters`].
    pub fn music_counts(&self) -> MusicTypeVec<AtRowPositions<usize>> {
        self.params
            .music_types
            .iter()
            .map(|mt| mt.count(&self.block, !self.composition.start_stroke))
            .collect()
    }

    fn get_method(&self, id: MethodId) -> &Method {
        &self.params.methods[self.method_map[&id].idx]
    }

    fn get_call(&self, id: CallId) -> &Call {
        &self.params.calls[self.call_map[&id]]
    }
}

///////////
// UTILS //
///////////

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
