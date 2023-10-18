//! Representation of a [`Composition`] generated by Monument.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use bellframe::{Bell, Block, Row, RowBuf, Stage, Stroke};
use itertools::Itertools;

use crate::{
    parameters::{CallDisplayStyle, CallId, MethodId, MethodVec, MusicTypeVec, Parameters},
    utils::{
        lengths::{PerPartLength, TotalLength},
        MusicBreakdown,
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

    /// The total score generated by this composition, accumulated from music, calls, coursing
    /// patterns, etc.
    pub(crate) total_score: f32,
}

impl Composition {
    /// The number of [`Row`]s in this composition.
    pub fn length(&self) -> usize {
        self.length.as_usize()
    }

    /// A slice containing the number of [`Row`]s generated for each [`Method`] used in the
    /// [`Search`].  These are stored in the same order as the [`Method`]s.
    pub fn method_counts(&self, params: &Parameters) -> MethodVec<usize> {
        let counts_per_method_id = self
            .rows(params)
            .annots()
            .counts_by(|(method_id, _)| *method_id);

        params
            .methods
            .iter()
            .map(|m| *counts_per_method_id.get(&m.id).unwrap_or(&0))
            .collect()
    }

    /// The number of *instances* of each [`MusicType`] in the [`Search`].
    pub fn music_counts(&self, params: &Parameters) -> MusicTypeVec<usize> {
        let breakdown = MusicBreakdown::from_rows(
            self.rows(params).rows(),
            &RowBuf::rounds(self.stage),
            params.music_types.as_raw_slice(),
            !self.start_stroke,
        );
        breakdown.counts.as_slice().iter().copied().collect()
    }

    pub fn part_head(&self) -> &Row {
        &self.part_head
    }

    /// The total score generated by this composition from all the different weights (music, calls,
    /// changes of method, handbell coursing, etc.).
    pub fn total_score(&self) -> f32 {
        self.total_score
    }

    /// The average score generated by each [`Row`] in this composition.  This is equal to
    /// `self.total_score() / self.length() as f32`.
    pub fn average_score(&self) -> f32 {
        self.total_score() / self.length() as f32
    }

    /// Returns a factor in `0.0..=1.0` where 0.0 means nothing was rung and 1.0 means everything
    /// was rung (i.e. the composition is atw)
    pub fn atw_factor(&self, params: &Parameters) -> f32 {
        // Determine the working bells, and store this in a bitmap
        let working_bells = params.working_bells();
        let mut is_working = vec![false; self.stage.num_bells()];
        for b in &working_bells {
            is_working[b.index()] = true;
        }
        // Determine how many `(bell, place, method, sub-lead-idx)` quadruples are actually possible
        let total_unique_row_positions = working_bells.len() // Working bells
            * working_bells.len() // Working place bells
            * params.methods.iter().map(|m| m.lead_len()).sum::<usize>();
        // Compute which `(bell, place, method, sub-lead-index)` quadruples have been rung in this
        // composition.
        //
        // TODO: Use a more efficient storage method
        let mut rung_place_bell_positions = HashSet::<(Bell, u8, MethodId, usize)>::new();
        for (&(method_id, sub_lead_idx), row) in self.rows(params).annot_rows() {
            for (place, bell) in row.bell_iter().enumerate() {
                if is_working[bell.index()] {
                    rung_place_bell_positions.insert((bell, place as u8, method_id, sub_lead_idx));
                }
            }
        }

        rung_place_bell_positions.len() as f32 / total_unique_row_positions as f32
    }

    /// Score generated by just the [`MusicType`]s (not including calls, changes of methods,
    /// etc.).
    pub fn music_score(&self, params: &Parameters) -> f32 {
        let music_counts = self.music_counts(params);
        let mut music_score = 0.0;
        for (count, music_type) in music_counts.into_iter().zip_eq(&params.music_types) {
            music_score += count as f32 * music_type.weight;
        }
        music_score
    }

    /// Generate a human-friendly [`String`] summarising the calling of this composition.  For
    /// example, [this composition](https://complib.org/composition/87419) would have a
    /// `call_string` of `D[B]BL[W]N[M]SE[sH]NCYW[sH]`.
    pub fn call_string(&self, params: &Parameters) -> String {
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
                    CallDisplayStyle::CallingPositions(calling_bell) => {
                        let row_after_call = path_iter
                            .peek()
                            .map_or(&self.part_head, |path_elem| &path_elem.start_row);
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
    pub fn rows(&self, params: &Parameters) -> Block<(MethodId, usize)> {
        assert_eq!(self.stage, params.stage);
        let mut plain_course_cache = HashMap::<MethodId, Block<(MethodId, usize)>>::new();

        // Generate the first part
        let mut first_part =
            Block::<(MethodId, usize)>::with_leftover_row(params.start_row.clone());
        for elem in &self.path {
            assert_eq!(first_part.leftover_row(), elem.start_row.as_row());
            // Get the plain course of the current method
            let plain_course = plain_course_cache.entry(elem.method_id).or_insert_with(|| {
                params
                    .get_method(elem.method_id)
                    .plain_course()
                    .map_annots(|annot| (elem.method_id, annot.sub_lead_idx))
            });
            // Add this elem to the first part
            let start_idx = elem.start_sub_lead_idx;
            let end_idx = start_idx + elem.length.as_usize();
            if end_idx > plain_course.len() {
                // `elem` wraps over the course head, so copy it in two pieces
                first_part.extend_range(plain_course, start_idx..);
                first_part.extend_range(plain_course, ..end_idx - plain_course.len());
            } else {
                // `elem` doesn't wrap over the course head, so copy it in one piece
                first_part.extend_range(plain_course, start_idx..end_idx);
            }
            // If this PathElem ends in a call, then change the `leftover_row` to suit
            if let Some(call_id) = elem.call_to_end {
                let last_non_leftover_row = first_part.rows().next_back().unwrap();
                let new_leftover_row =
                    last_non_leftover_row * params.get_call(call_id).place_notation.transposition();
                first_part.leftover_row_mut().copy_from(&new_leftover_row);
            }
        }

        // Generate the other parts from the first
        let part_len = first_part.len();
        let mut comp = first_part;
        for _ in 0..params.num_parts() - 1 {
            comp.extend_from_within(..part_len);
        }
        assert_eq!(comp.len(), self.length());
        assert_eq!(comp.leftover_row(), &params.end_row);
        comp
    }
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
}

/// A way to display a [`Composition`] by pairing it with a [`Parameters`]
#[derive(Debug, Clone, Copy)]
struct DisplayComposition<'a>(pub &'a Composition, pub &'a Parameters);

impl std::fmt::Display for DisplayComposition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisplayComposition(comp, params) = self;

        write!(f, "len: {}, ", comp.length)?;
        // Method counts for spliced
        if params.is_spliced() {
            write!(f, "ms: {:>3?}, ", comp.method_counts.as_slice())?;
        }
        // Part heads if multi-part with >2 parts (2-part compositions only have one possible part
        // head)
        if params.num_parts() > 2 {
            write!(f, "PH: {}, ", comp.part_head)?;
        }
        write!(
            f,
            "music: {:>6.2?}, avg score: {:.6}, str: {}",
            comp.music_score(params),
            comp.average_score(),
            comp.call_string(params)
        )
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
