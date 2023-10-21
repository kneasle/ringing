use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Range},
};

use itertools::Itertools;

use crate::{row::same_stage_vec::RowSlice, stroke::StrokeSet, Bell, Row, RowBuf, Stage, Stroke};

/// A collection of [`Pattern`]s which, together, form a group of music
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct MusicType {
    patterns: Vec<Pattern>,
    stroke_set: StrokeSet,
}

/// A `Pattern` of [`Bell`]s, with possible wildcards.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Pattern {
    /// The sequence of [`Bell`]s which make up this pattern.  `None` matches any [`Bell`] in that
    /// position.
    bells: Vec<Option<Bell>>,
}

//////////////////
// CONSTRUCTORS //
//////////////////

impl MusicType {
    pub fn parse(s: &str) -> Result<Self, PatternError> {
        Pattern::parse(s).map(Self::from)
    }

    pub const fn new(patterns: Vec<Pattern>) -> Self {
        Self {
            patterns,
            stroke_set: StrokeSet::Both,
        }
    }

    /// A `MusicType` with no [`Pattern`]s
    pub const fn empty() -> Self {
        Self::new(vec![])
    }

    /// Sets the [`StrokeSet`] determining on which strokes this music type will be counted.
    pub fn at_stroke(mut self, stroke_set: StrokeSet) -> Self {
        self.stroke_set = stroke_set;
        self
    }

    /* Common Musics */

    /// Creates a set of `Pattern`s which match runs of a given length.  If the run length is
    /// longer than the stage, no `Pattern`s are returned.
    pub fn runs(stage: Stage, len: u8) -> Self {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return Self::empty();
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in descending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Some);
            runs.push(Pattern::from_bells(run_iterator.clone()).unwrap()); // Descending runs (e.g. `1234`)
            runs.push(Pattern::from_bells(run_iterator.rev()).unwrap()); // Ascending runs (e.g. `4321`)
        }

        Self::new(runs)
    }

    pub fn reversed_tenors_at_back(stage: Stage) -> Self {
        assert!(stage.is_even());

        let bells = vec![Some(stage.tenor()), Some(stage.tenor() - 1)];
        let pattern = unsafe { Pattern::from_vec_unchecked(bells) };
        Self::from(pattern).at_stroke(StrokeSet::Back)
    }
}

impl From<Pattern> for MusicType {
    fn from(p: Pattern) -> Self {
        Self::new(vec![p])
    }
}

impl Pattern {
    /// Parses a `Pattern` from a string.  A [`Bell`] name matches only that [`Bell`], `'x'` or
    /// `'X'` match any [`Bell`].
    ///
    /// This checks that:
    /// 1. The [`Bell`]s in this `Pattern` are unique
    /// 2. The [`Bell`]s fit within the `Stage`
    pub fn parse_with_stage(s: &str, stage: Stage) -> Result<Self, PatternError> {
        let bells = Self::parse_bells_or_xs(s);
        // Check if any bells are out of the `Stage`
        for b in bells.iter().filter_map(|b| *b) {
            if !stage.contains(b) {
                return Err(PatternError::BellOutOfStage(b, stage));
            }
        }
        // If they're all in the stage, just check for bell uniqueness
        Self::from_vec(bells)
    }

    /// Parses a `Pattern` from a string.  A [`Bell`] name matches only that [`Bell`], `'x'` or
    /// `'X'` match any [`Bell`].  The only requirement is the [`Bell`]s specified are unique.
    pub fn parse(s: &str) -> Result<Self, PatternError> {
        Self::from_vec(Self::parse_bells_or_xs(s))
    }

    fn parse_bells_or_xs(s: &str) -> Vec<Option<Bell>> {
        s.chars()
            .filter_map(|c| match c {
                'x' | 'X' => Some(None),
                _ => Bell::from_name(c).map(Some),
            })
            .collect_vec()
    }

    /// Creates a normalised `Pattern` from an [`Iterator`] of [`Elem`]s
    pub fn from_bells(iter: impl IntoIterator<Item = Option<Bell>>) -> Result<Self, PatternError> {
        Self::from_vec(iter.into_iter().collect_vec())
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Elem`]s, checking the invariants and normalising
    /// the result.  This uses the allocation of the given [`Vec`].
    pub fn from_vec(bells: Vec<Option<Bell>>) -> Result<Self, PatternError> {
        // Check uniqueness of bells
        let bell_counts = bells.iter().filter_map(|b| *b).counts();
        for (b, count) in bell_counts {
            if count > 1 {
                return Err(PatternError::DuplicateBell(b));
            }
        }
        // SAFETY: All `Bell`s in `bells` are unique
        Ok(unsafe { Self::from_vec_unchecked(bells) })
    }

    /// Creates a `Pattern` from an [`Iterator`] of [`Elem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// Safe if all the [`Bell`]s returned by `iter` are unique.
    pub unsafe fn from_elems_unchecked(iter: impl IntoIterator<Item = Option<Bell>>) -> Self {
        Self::from_vec_unchecked(iter.into_iter().collect_vec())
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Elem`]s, without normalising or checking invariants.
    ///
    /// # Safety
    ///
    /// This is safe if all the bells in `elems` are unique.
    #[inline]
    pub unsafe fn from_vec_unchecked(bells: Vec<Option<Bell>>) -> Self {
        Self { bells }
    }
}

//////////////
// MATCHING //
//////////////

impl MusicType {
    pub fn count<'a>(&self, rows: impl Into<RowSlice<'a>>) -> AtPositions<usize> {
        Self::count_with_stroke(self, rows, Stroke::Back)
    }

    pub fn count_with_stroke<'a>(
        &self,
        rows: impl Into<RowSlice<'a>>,
        stroke_of_first_row: Stroke,
    ) -> AtPositions<usize> {
        let rows = rows.into();

        let mut counts = AtPositions::<usize>::ZERO;
        for pattern in &self.patterns {
            counts =
                counts + pattern.match_one_with_stroke(self.stroke_set, rows, stroke_of_first_row);
        }
        counts
    }
}

impl Pattern {
    fn match_one_with_stroke<'a>(
        &self,
        at_strokes: StrokeSet,
        rows: impl Into<RowSlice<'a>>,
        stroke: Stroke,
    ) -> AtPositions<usize> {
        // Like in Rust's regex crate, we will use an optimized SIMD search routine to search for
        // this substring then verify the rest of the pattern only once per potential match.

        // Get the longest sequence of consecutive bells
        let Some(bell_range) = self.longest_sequence_of_bells() else {
            todo!() // Music pattern is just `x`s
        };
        // Get the offsets of the other bells in the pattern, relative to `bell_range.start`
        let mut other_bells = Vec::<(usize, Bell)>::new();
        for (idx, bell) in self.bells.iter().enumerate() {
            if let Some(bell) = bell {
                if !bell_range.contains(&idx) {
                    other_bells.push((idx, *bell));
                }
            }
        }
        // Convert needle and haystack into raw bytes
        let rows: RowSlice = rows.into();
        let bells = self.bells[bell_range.clone()]
            .iter()
            .map(|b| b.unwrap())
            .collect_vec();
        let needle_bytes: &[u8] = bytemuck::cast_slice(&bells);
        let haystack_bytes: &[u8] = bytemuck::cast_slice(&rows.bells);

        // Use the fast string searches to find all instances of the long sub-region
        let mut counts = AtPositions::ZERO;
        'match_loop: for needle_start in memchr::memmem::find_iter(haystack_bytes, needle_bytes) {
            // Check that the entire pattern falls within the haystack.  If not, reject this match
            let Some(pattern_start) = needle_start.checked_sub(bell_range.start) else {
                continue 'match_loop; // Pattern would extend off the start
            };
            let pattern_end = pattern_start + self.bells.len();
            if pattern_end > rows.bells.len() {
                continue 'match_loop; // Pattern would extend off the end
            }
            // Check that other bells are valid.  If not, reject this match
            for &(offset, bell) in &other_bells {
                if rows.bells[pattern_start + offset] != bell {
                    continue 'match_loop; // This bell was specified in the wrong place
                }
            }

            // Determine where in the ringing this match happens
            let start_row_idx = pattern_start / rows.stage.num_bells();
            let end_row_idx = (pattern_end - 1) / rows.stage.num_bells();
            let starts_on_row_boundary = (pattern_start % rows.stage.num_bells()) == 0;
            let ends_on_row_boundary = (pattern_end % rows.stage.num_bells()) == 0;
            // Reject matches on the wrong stroke
            let match_stroke = stroke.offset(end_row_idx);
            if !at_strokes.contains(match_stroke) {
                continue; // Match is on the wrong stroke
            }
            // Classify the match
            let position = if start_row_idx != end_row_idx {
                MusicPosition::Wrap
            } else if starts_on_row_boundary {
                MusicPosition::Front
            } else if ends_on_row_boundary {
                MusicPosition::Back
            } else {
                MusicPosition::Internal
            };
            // Add the match
            *counts.get_mut(position) += 1;
        }

        counts
    }

    fn longest_sequence_of_bells(&self) -> Option<Range<usize>> {
        let mut bell_section_boundaries = vec![0];
        bell_section_boundaries.extend(
            self.bells
                .iter()
                .tuple_windows()
                .positions(|(a, b)| a.is_some() != b.is_some())
                .map(|idx| idx + 1),
        );
        bell_section_boundaries.push(self.bells.len());

        let mut best_substring: Option<Range<usize>> = None;
        let mut best_substring_len = 0;
        for (start_idx, end_idx) in bell_section_boundaries.into_iter().tuple_windows() {
            let slice = &self.bells[start_idx..end_idx];
            let substring_len = end_idx - start_idx;
            if slice[0].is_some() && substring_len > best_substring_len {
                best_substring_len = substring_len;
                best_substring = Some(start_idx..end_idx);
            }
        }
        best_substring
    }

    // TODO: Replace
    pub fn num_matching_rows(&self) -> Option<usize> {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MusicPosition {
    Front,
    Internal,
    Back,
    Wrap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtPositions<T> {
    front: T,
    internal: T,
    back: T,
    wrap: T,
}

impl AtPositions<usize> {
    const ZERO: AtPositions<usize> = AtPositions {
        front: 0,
        internal: 0,
        back: 0,
        wrap: 0,
    };
}

impl<T> AtPositions<T> {
    pub fn get(&self, position: MusicPosition) -> &T {
        match position {
            MusicPosition::Front => &self.front,
            MusicPosition::Internal => &self.internal,
            MusicPosition::Back => &self.back,
            MusicPosition::Wrap => &self.wrap,
        }
    }

    pub fn get_mut(&mut self, position: MusicPosition) -> &mut T {
        match position {
            MusicPosition::Front => &mut self.front,
            MusicPosition::Internal => &mut self.internal,
            MusicPosition::Back => &mut self.back,
            MusicPosition::Wrap => &mut self.wrap,
        }
    }
}

impl<T: Add> Add for AtPositions<T> {
    type Output = AtPositions<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        AtPositions {
            front: self.front + rhs.front,
            internal: self.internal + rhs.internal,
            back: self.back + rhs.back,
            wrap: self.wrap + rhs.wrap,
        }
    }
}

//////////
// MISC //
//////////

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for bell in &self.bells {
            match bell {
                Some(b) => write!(f, "{}", b)?,
                None => write!(f, "x")?,
            }
        }
        Ok(())
    }
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pattern({})", self)
    }
}

impl From<&Row> for Pattern {
    fn from(r: &Row) -> Self {
        Self::from_vec(r.bell_iter().map(Some).collect_vec())
            .expect("Bells in a row must be unique")
    }
}

impl From<RowBuf> for Pattern {
    fn from(r: RowBuf) -> Self {
        Self::from(r.as_row())
    }
}

////////////
// ERRORS //
////////////

/// Possible errors created when constructing a [`Pattern`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternError {
    DuplicateBell(Bell),
    BellOutOfStage(Bell, Stage),
}

impl PatternError {
    pub(crate) fn write_message(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            PatternError::BellOutOfStage(bell, stage) => {
                write!(f, "bell {} is out of stage {}", bell, stage)
            }
            PatternError::DuplicateBell(bell) => write!(f, "bell {} appears twice", bell),
        }
    }
}

impl Display for PatternError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.write_message(f)
    }
}

impl std::error::Error for PatternError {}

#[cfg(test)]
mod tests {
    use crate::{
        music::{AtPositions, MusicType},
        Bell, RowBuf, Stage,
    };

    use super::{Pattern, PatternError};

    #[test]
    fn pattern_parse_err() {
        #[track_caller]
        fn check_exceeds_stage(pattern: &str, num_bells: u8, bell: char) {
            let stage = Stage::new(num_bells);
            let bell = Bell::from_name(bell).unwrap();
            assert_eq!(
                Pattern::parse_with_stage(pattern, stage),
                Err(PatternError::BellOutOfStage(bell, stage))
            );
        }

        check_exceeds_stage("xxx5", 4, '5');
        check_exceeds_stage("*5", 4, '5');
        check_exceeds_stage("5*", 4, '5');
    }

    #[test]
    fn count_block() {
        let cc_lib = crate::MethodLib::cc_lib().unwrap();

        let check_course = |row: &str,
                            method: &str,
                            s: &str,
                            front: usize,
                            internal: usize,
                            back: usize,
                            wrap: usize| {
            let mt = MusicType::parse(s).unwrap();
            let method = cc_lib.get_by_title(method).unwrap();

            let mut plain_course = method.plain_course();
            let start_row = RowBuf::parse_with_stage(row, plain_course.stage()).unwrap();
            plain_course.pre_multiply(&start_row);

            assert_eq!(
                mt.count(&plain_course),
                AtPositions {
                    front,
                    internal,
                    back,
                    wrap,
                }
            );
        };
        let check_pc =
            |m: &str, s: &str, front: usize, internal: usize, back: usize, wrap: usize| {
                check_course("1", m, s, front, internal, back, wrap);
            };

        check_pc("Deva Surprise Major", "5678", 4, 0, 4, 0);
        check_pc("Deva Surprise Major", "5x78", 5, 3, 5, 0);
        check_pc("Bristol Surprise Major", "87654321", 1, 0, 0, 1);
        check_pc("Bristol Surprise Major", "5678", 0, 1, 4, 0);
        check_pc("Bristol Surprise Major", "1234", 2, 0, 0, 1);
        check_pc("Rapid Wrap Major", "12345678", 1, 0, 0, 8);
        check_pc("Bristol Surprise Maximus", "98765432", 0, 1, 4, 0);
        check_pc("Bristol Surprise Maximus", "5678", 0, 11, 0, 0);

        check_course("12354678", "Bristol Surprise Major", "5678", 1, 1, 3, 0);
        check_course("12348765", "Bristol Surprise Major", "5678", 4, 1, 0, 0);
    }

    #[test]
    fn backstroke_87s() {
        let cc_lib = crate::MethodLib::cc_lib().unwrap();

        let check = |method: &str, row: &str, expected_87s: usize| {
            // Get plain course
            let method = cc_lib.get_by_title(method).unwrap();
            let mut plain_course = method.plain_course();
            let start_row = RowBuf::parse_with_stage(row, plain_course.stage()).unwrap();
            plain_course.pre_multiply(&start_row);

            let mt = MusicType::reversed_tenors_at_back(method.stage());
            assert_eq!(mt.count(&plain_course).back, expected_87s);
        };

        check("Bristol Surprise Major", "12345678", 0); // Plain course of Bristol has no 87s
        check("Bristol Surprise Major", "12436587", 12); // Reversed plain course has 12
        check("Cooktown Orchid Delight Major", "12347658", 4);
        check("Rapid Wrap Major", "12345678", 3);

        check("Let's Ring! Delight Minor", "123456", 4);
    }
}
