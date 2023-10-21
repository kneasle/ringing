use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Range},
};

use itertools::Itertools;

use crate::{row::same_stage_vec::RowSlice, stroke::StrokeSet, Bell, Row, RowBuf, Stage, Stroke};

/// A `Pattern` of [`Bell`]s, with possible wildcards.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Pattern {
    /// The sequence of [`Bell`]s which make up this pattern.  `None` matches any [`Bell`] in that
    /// position.
    bells: Vec<Option<Bell>>,
    stroke_set: StrokeSet,
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

    /// Sets the [`StrokeSet`] determining on which strokes this music type will be counted.
    pub fn at_stroke(mut self, stroke_set: StrokeSet) -> Self {
        self.stroke_set = stroke_set;
        self
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
    pub unsafe fn from_vec_unchecked(elems: Vec<Option<Bell>>) -> Self {
        Self {
            bells: elems,
            stroke_set: StrokeSet::Both,
        }
    }

    ///////////////////
    // COMMON MUSICS //
    ///////////////////

    /// Creates a set of `Pattern`s which match runs of a given length.  If the run length is
    /// longer than the stage, no `Pattern`s are returned.
    pub fn runs(stage: Stage, len: u8) -> Vec<Pattern> {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return vec![];
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in descending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Some);
            runs.push(Self::from_bells(run_iterator.clone()).unwrap()); // Descending runs (e.g. `1234`)
            runs.push(Self::from_bells(run_iterator.rev()).unwrap()); // Ascending runs (e.g. `4321`)
        }

        runs
    }

    pub fn reversed_tenors_at_back(stage: Stage) -> Self {
        assert!(stage.is_even());

        let bells = vec![Some(stage.tenor()), Some(stage.tenor() - 1)];
        unsafe { Self::from_vec_unchecked(bells) }.at_stroke(StrokeSet::Back)
    }
}

//////////////
// MATCHING //
//////////////

impl Pattern {
    // TODO: Provide a way to access where the occurrences are

    /// Simultaneously match multiple patterns [`Pattern`] against every row in [`RowSlice`],
    /// counting the total number of occurences in each position (front, internal, back or wrapped).
    ///
    /// This assumes that the first row in `rows` occurs on [`Stroke::Back`].
    pub fn match_many<'a>(
        patterns: &[Self],
        rows: impl Into<RowSlice<'a>>,
    ) -> MusicPositions<usize> {
        Self::match_many_with_stroke(patterns, rows, Stroke::Back)
    }

    pub fn match_many_with_stroke<'a>(
        patterns: &[Self],
        rows: impl Into<RowSlice<'a>>,
        stroke: Stroke,
    ) -> MusicPositions<usize> {
        let rows = rows.into();

        let mut counts = MusicPositions::<usize>::ZERO;
        for pattern in patterns {
            counts = counts + pattern.match_one_with_stroke(rows, stroke);
        }
        counts
    }

    /// Match one [`Pattern`] against every row in [`RowSlice`], counting the number of occurences
    /// in each position (front, internal, back or wrapped).
    ///
    /// This assumes that the first row in `rows` occurs on [`Stroke::Back`].
    pub fn match_one<'a>(&self, rows: impl Into<RowSlice<'a>>) -> MusicPositions<usize> {
        Self::match_one_with_stroke(self, rows, Stroke::Back)
    }

    pub fn match_one_with_stroke<'a>(
        &self,
        rows: impl Into<RowSlice<'a>>,
        stroke: Stroke,
    ) -> MusicPositions<usize> {
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
        let mut counts = MusicPositions::ZERO;
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
            if !self.stroke_set.contains(match_stroke) {
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

    // TODO: Replace
    pub fn matches(&self, row: &Row) -> bool {
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
pub struct MusicPositions<T> {
    front: T,
    internal: T,
    back: T,
    wrap: T,
}

impl MusicPositions<usize> {
    const ZERO: MusicPositions<usize> = MusicPositions {
        front: 0,
        internal: 0,
        back: 0,
        wrap: 0,
    };
}

impl<T> MusicPositions<T> {
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

impl<T: Add> Add for MusicPositions<T> {
    type Output = MusicPositions<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        MusicPositions {
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
    use crate::{music::MusicPositions, Bell, RowBuf, Stage};

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
    fn match_block() {
        let cc_lib = crate::MethodLib::cc_lib().unwrap();

        let check_course = |row: &str,
                            method: &str,
                            s: &str,
                            front: usize,
                            internal: usize,
                            back: usize,
                            wrap: usize| {
            let p = Pattern::parse(s).unwrap();
            let method = cc_lib.get_by_title(method).unwrap();

            let mut plain_course = method.plain_course();
            let start_row = RowBuf::parse_with_stage(row, plain_course.stage()).unwrap();
            plain_course.pre_multiply(&start_row);

            assert_eq!(
                p.match_one(&plain_course),
                MusicPositions {
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

            let p = Pattern::reversed_tenors_at_back(method.stage());
            assert_eq!(p.match_one(&plain_course).back, expected_87s);
        };

        check("Bristol Surprise Major", "12345678", 0); // Plain course of Bristol has no 87s
        check("Bristol Surprise Major", "12436587", 12); // Reversed plain course has 12
        check("Cooktown Orchid Delight Major", "12347658", 4);
        check("Rapid Wrap Major", "12345678", 3);

        check("Let's Ring! Delight Minor", "123456", 4);
    }

    /*
    #[test]
    fn is_match() {
        #[track_caller]
        fn check(pattern_str: &str, row_str: &str, expected_match: bool) {
            let row = &RowBuf::parse(row_str).unwrap();
            let pattern = Pattern::parse(pattern_str).unwrap();
            let is_match = pattern.match_one(&row);

            match (expected_match, is_match) {
                (true, false) => {
                    panic!("Expected '{}' to match '{}', but it did not.", row, pattern);
                }
                (false, true) => {
                    panic!("Expected '{}' not to match '{}', but it did.", row, pattern);
                }
                // If the matches lined up, then the test passes
                _ => {}
            }
        }

        check("*6578", "12346578", true);
        check("*6578", "12345678", false);

        // Pattern with only 'x's
        check("xxxx", "3412", true);
        check("xxXx", "4321", true);
        check("xxxxX", "54321", true);
        // Pattern with only 'x's and numbers
        check("x1xx", "2134", true);
        check("x1xx", "2134", true);
        check("x1xx", "4321", false);
        check("x1xxX", "21345", true);
        // Pattern with stars
        check("*1234", "1234", true);
        check("*1234", "51234", true);
        check("*6578", "12346578", true);
        check("*6578", "12345678", false);
        check("*65*78", "12346578", true);
        check("*65*78", "12653478", true);
        check("*65*78", "12657834", false);
        // Pattern with everything
        check("x*12*34", "61257834", true);
        check("x*12*34", "12657834", false);
        check("x*12*34", "51234", true);
        check("x*12*34", "12534", false);
    }

    #[test]
    fn match_pattern() {
        #[track_caller]
        fn check(pattern_str: &str, row_str: &str, expected_pattern: Option<Vec<usize>>) {
            let row = &RowBuf::parse(row_str).unwrap();
            let pattern = Pattern::parse_with_stage(pattern_str, row.stage()).unwrap();
            assert_eq!(pattern.match_one(&row), expected_pattern);
        }

        // Patterns with only 'x's
        check("xxxx", "3412", Some(vec![]));
        check("xxXx", "4321", Some(vec![]));
        check("XXXXx", "54321", Some(vec![]));
        // Patterns with only 'x's and numbers
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "4321", None);
        check("x1xxx", "21345", Some(vec![1]));
        // Patterns with stars
        check("*1234", "1234", Some(vec![0, 1, 2, 3]));
        check("*1234", "51234", Some(vec![1, 2, 3, 4]));
        check("*6578", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12653478", Some(vec![2, 3, 6, 7]));
        check("*65*78", "12657834", None);
        // Patterns with everything
        check("x*12*34", "61257834", Some(vec![1, 2, 6, 7]));
        check("x*12*34", "12657834", None);
        check("x*12*34", "51234", Some(vec![1, 2, 3, 4]));
        check("x*12*34", "12534", None);
        check("*1x2x3x4*", "18273645", Some(vec![0, 2, 4, 6]));
        check("*1x2x3x4*", "091E273645T8", Some(vec![2, 4, 6, 8]));
    }
    */
}
