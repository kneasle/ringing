use std::{
    fmt::Write,
    ops::{Add, AddAssign},
};

use crate::{
    utils::{Counts, OptRange},
    MusicTypeIdx, MusicTypeVec,
};
use bellframe::{music::Pattern, Row, RowBuf, Stage, Stroke};
use itertools::Itertools;
use ordered_float::OrderedFloat;
use serde::Deserialize;

pub type Score = OrderedFloat<f32>;

/// A class of music that Monument should care about
#[derive(Debug, Clone)]
pub struct MusicType {
    pub patterns: Vec<Pattern>,
    pub strokes: StrokeSet,

    pub weight: Score,
    pub count_range: OptRange,
    pub non_duffer: bool,
}

impl MusicType {
    pub fn new(
        patterns: Vec<Pattern>,
        stroke_set: StrokeSet,
        weight: f32,
        count_range: OptRange,
    ) -> Self {
        Self {
            patterns,
            weight: OrderedFloat(weight),
            count_range,
            non_duffer: false, // TODO: Implement non-duffers properly
            strokes: stroke_set,
        }
    }

    /// Return the total number of possible instances of this music type, or `None` if the
    /// computation caused `usize` to overflow.
    pub fn max_count(&self) -> Option<usize> {
        let mut sum = 0;
        for r in &self.patterns {
            sum += r.num_matching_rows()?;
        }
        Some(sum)
    }
}

/// A breakdown of the music generated by a composition
#[derive(Debug, Clone)]
pub struct Breakdown {
    pub score: Score,
    /// The number of occurrences of each [`MusicType`] specified in the current
    /// [`Query`](crate::Query)
    pub counts: Counts,
}

impl Breakdown {
    /// Creates the `Score` of 0 (i.e. the `Score` generated by no rows).
    pub fn zero(num_music_types: usize) -> Self {
        Self {
            score: Score::from(0.0),
            counts: Counts::zeros(num_music_types),
        }
    }

    /// Returns the `Score` generated by a sequence of [`Row`]s, (pre-)transposed by some course
    /// head.
    pub fn from_rows<'r>(
        rows: impl IntoIterator<Item = &'r Row>,
        pre_transposition: &Row,
        music_types: &[MusicType],
        start_stroke: Stroke,
    ) -> Self {
        let mut temp_row = RowBuf::rounds(Stage::ONE);
        let mut occurences = vec![0; music_types.len()];
        // For every (transposed) row ...
        for (idx, r) in rows.into_iter().enumerate() {
            pre_transposition.mul_into(r, &mut temp_row).unwrap();
            // ... for every music type ...
            for (num_instances, ty) in occurences.iter_mut().zip_eq(music_types) {
                if ty.strokes.contains(start_stroke.offset(idx)) {
                    // ... count the number of instances of that type of music
                    for pattern in &ty.patterns {
                        // Unwrap is safe because `pattern` must have the same `Stage` as the rest
                        // of the rows
                        if pattern.matches(&temp_row).unwrap() {
                            *num_instances += 1;
                        }
                    }
                }
            }
        }

        Self {
            score: occurences
                .iter()
                .zip_eq(music_types)
                .map(|(&num_instances, ty)| ty.weight * num_instances as f32)
                .sum(),
            counts: occurences.into(),
        }
    }

    /// # Panics
    ///
    /// Panics if the number of [`MusicType`]s in `rhs` is different to that of `self`.
    pub fn saturating_sub(&self, rhs: &Self) -> Self {
        Breakdown {
            score: self.score - rhs.score,
            counts: self.counts.saturating_sub(&rhs.counts),
        }
    }

    /// # Panics
    ///
    /// Panics if the number of [`MusicType`]s in `rhs` is different to that of `self`.
    pub fn saturating_sub_assign(&mut self, rhs: &Self) {
        self.score -= rhs.score;
        self.counts.saturating_sub_assign(&rhs.counts);
    }
}

impl Add for &Breakdown {
    type Output = Breakdown;

    /// Combines two [`Score`]s to create one [`Score`] representing both `self` and `rhs`.
    ///
    /// # Panics
    ///
    /// Panics if the number of [`MusicType`]s in `rhs` is different to that of `self`.
    fn add(self, rhs: &Breakdown) -> Self::Output {
        Breakdown {
            score: self.score + rhs.score,
            counts: &self.counts + &rhs.counts,
        }
    }
}

impl AddAssign<&Breakdown> for Breakdown {
    /// Combines the scores from another [`Score`] into `self` (so that `self` now represents the
    /// score generated by `self` and the RHS).
    ///
    /// # Panics
    ///
    /// Panics if the number of [`MusicType`]s in `rhs` is different to that of `self`.
    fn add_assign(&mut self, rhs: &Breakdown) {
        self.score += rhs.score;
        self.counts += &rhs.counts;
    }
}

///////////////////
// MUSIC DISPLAY //
///////////////////

/// How music counts can be displayed.  This could take its counts from multiple [`MusicType`]s,
/// and takes a few forms:
/// 1. Just a total count: e.g. `*5678: 23`
/// 2. Just a breakdown: e.g. `5678s: 24f,81i,24b`
/// 3. A breakdown and a total count: e.g. `4-runs: 312 (73f,132i,107b)`
///
/// Setting all sources to `None` is allowed, but will create an empty column.
#[derive(Debug, Clone)]
pub struct MusicDisplay {
    /// The name used to identify this type of music
    pub name: String,

    /// The index of the [`MusicType`] which provides the total count
    pub source_total: Option<MusicTypeIdx>,

    /// The index of the [`MusicType`] which provides the count off the front
    pub source_front: Option<MusicTypeIdx>,
    /// The index of the [`MusicType`] which provides the internal count
    pub source_internal: Option<MusicTypeIdx>,
    /// The index of the [`MusicType`] which provides the count off the back
    pub source_back: Option<MusicTypeIdx>,
}

impl MusicDisplay {
    /// Creates a new [`MusicDisplay`] with no corresponding [`MusicType`]s
    pub fn empty(name: String) -> Self {
        Self {
            name,
            source_total: None,
            source_front: None,
            source_internal: None,
            source_back: None,
        }
    }

    /// Return the width of the smallest column large enough to be guaranteed to hold (almost)
    /// every instance of this [`MusicDisplay`] (assuming rows can't be repeated).
    pub fn col_width(&self, music_types: &MusicTypeVec<MusicType>) -> usize {
        // We always pad the counts as much as required, so displaying a set of 0s results in a
        // maximum-width string (i.e. all output strings are the same length)
        let max_count_width = self
            .display_counts(music_types, &vec![0; music_types.len()])
            .len();
        max_count_width.max(self.name.len())
    }

    /// Generate a compact string representing a given set of music counts
    pub fn display_counts(
        &self,
        music_types: &MusicTypeVec<MusicType>,
        counts: &[usize],
    ) -> String {
        let mut s = String::new();

        // Add total count
        if let Some(total_idx) = self.source_total {
            write!(
                s,
                "{:>width$}",
                counts[total_idx.index()],
                width = max_count_len(&music_types[total_idx])
            )
            .unwrap();
        }

        // Add specific counts (if there are any)
        if self.source_front.is_some()
            || self.source_internal.is_some()
            || self.source_back.is_some()
        {
            // Add brackets if there's a total score
            if self.source_total.is_some() {
                s.push_str(" (");
            }
            // Add every front/internal/back count for which we have a source
            let mut is_first_count = true;
            for (source, position_char) in [
                (&self.source_front, 'f'),
                (&self.source_internal, 'i'),
                (&self.source_back, 'b'),
            ] {
                if let Some(music_type_idx) = source {
                    // Add separating comma
                    if !is_first_count {
                        s.push(' ');
                    }
                    is_first_count = false;
                    // Add the number
                    write!(
                        s,
                        "{:>width$}",
                        counts[music_type_idx.index()],
                        width = max_count_len(&music_types[*music_type_idx])
                    )
                    .unwrap();
                    s.push(position_char);
                }
            }
            if self.source_total.is_some() {
                s.push(')'); // Add closing brackets if there's a total score
            }
        }

        s
    }
}

/// Prints the width of the largest count possible for a [`MusicType`] (assuming that rows can't be
/// repeated).
fn max_count_len(music_type: &MusicType) -> usize {
    // Determine how to display the music summary
    let max_music_count = music_type.max_count().unwrap_or(usize::MAX);
    // `min(4)` because we don't expect more than 9999 instances of a music type, even
    // if more theoretically exist
    max_music_count.to_string().len().min(4)
}

////////////////
// STROKE SET //
////////////////

/// A set of at least one [`Stroke`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StrokeSet {
    Hand,
    Back,
    Both,
}

impl StrokeSet {
    fn contains(self, stroke: Stroke) -> bool {
        match (self, stroke) {
            (Self::Both, _) | (Self::Hand, Stroke::Hand) | (Self::Back, Stroke::Back) => true,
            (Self::Hand, Stroke::Back) | (Self::Back, Stroke::Hand) => false,
        }
    }
}

impl Default for StrokeSet {
    fn default() -> Self {
        StrokeSet::Both
    }
}
