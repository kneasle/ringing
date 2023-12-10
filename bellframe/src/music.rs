use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, AddAssign, Mul, Not, Range},
};

use factorial::Factorial;
use itertools::Itertools;

use crate::{
    row::same_stage_vec::RowSlice, stroke::StrokeSet, Bell, Block, Row, RowBuf, Stage, Stroke,
};

/// A collection of [`Pattern`]s which, together, form a group of music
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MusicType {
    patterns: Vec<Pattern>,
    strokes: StrokeSet,
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
            strokes: StrokeSet::Both,
        }
    }

    /// A `MusicType` with no [`Pattern`]s
    pub const fn empty() -> Self {
        Self::new(vec![])
    }

    /// Sets the [`StrokeSet`] determining on which strokes this music type will be counted.
    pub fn at_stroke(mut self, stroke_set: StrokeSet) -> Self {
        self.strokes = stroke_set;
        self
    }

    pub fn strokes(&self) -> StrokeSet {
        self.strokes
    }

    /* Common Musics */

    /// Creates a set of `Pattern`s which match runs of a given length.  If the run length is
    /// longer than the stage, no `Pattern`s are returned.
    pub fn runs(len: u8, stage: Stage) -> Self {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return Self::empty();
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in descending order
            let run_iterator = (i..i + len).map(Bell::from_index);
            runs.push(Pattern::from_bells(run_iterator.clone()).unwrap()); // Descending runs (e.g. `1234`)
            runs.push(Pattern::from_bells(run_iterator.rev()).unwrap()); // Ascending runs (e.g. `4321`)
        }

        Self::new(runs)
    }

    pub fn combination_5678s_triples() -> Self {
        let mut patterns = Vec::new();
        for singles_row in &Stage::SINGLES.extent() {
            patterns.push(
                // Map `123` into `567` by adding 4 to each `Bell`
                Pattern::from_bells(singles_row.bell_iter().map(|b| b + 4))
                    .expect("567 combination pattern should always be valid"),
            );
        }
        Self::new(patterns)
    }

    pub fn combination_5678s_major() -> Self {
        let mut patterns = Vec::new();
        for minimus_row in &Stage::MINIMUS.extent() {
            patterns.push(
                // Map `1234` to `5678` by adding 4 to each `Bell`
                Pattern::from_bells(minimus_row.bell_iter().map(|b| b + 4))
                    .expect("5678 combination pattern should be valid"),
            );
        }
        Self::new(patterns)
    }

    pub fn near_misses(stage: Stage) -> Self {
        let patterns = (0..stage.num_bells() - 1)
            .map(|swap_idx| {
                let mut pattern_row = RowBuf::rounds(stage);
                pattern_row.swap(swap_idx, swap_idx + 1);
                Pattern::from(pattern_row)
            })
            .collect_vec();
        Self::new(patterns)
    }

    pub fn crus(stage: Stage) -> Self {
        assert!(stage >= Stage::TRIPLES);
        // Generate a pattern `*{b1}{b2}7890...` for b1 != b2 in {4,5,6}
        let pat_456 = [(4, 5), (5, 4), (4, 6), (6, 4), (5, 6), (6, 5)];
        let patterns = pat_456
            .into_iter()
            .map(|(b1, b2)| {
                // "*{b1}{b2}"
                let mut cru = vec![
                    Bell::from_number(b1).unwrap(),
                    Bell::from_number(b2).unwrap(),
                ];
                // "7890..."
                cru.extend(stage.bells().skip(6));
                Pattern::from_bells(cru).expect("CRU pattern should be valid")
            })
            .collect_vec();
        Self::new(patterns)
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

    /// Creates a `Pattern` from an [`Iterator`] of just [`Bell`]s.  I.e. this pattern will contain
    /// no `x`s.
    pub fn from_bells(iter: impl IntoIterator<Item = Bell>) -> Result<Self, PatternError> {
        Self::from_vec(iter.into_iter().map(Some).collect_vec())
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Option`]al [`Bell`]s.  `None`s are used as `x`s in
    /// the pattern.
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

    /// Creates a `Pattern` from a [`Vec`] of [`Option`]al [`Bell`]s, without checking
    /// whether they form a valid [`Pattern`].
    ///
    /// # Safety
    ///
    /// Safe if all the [`Bell`]s returned by `iter` are unique.
    #[inline]
    pub unsafe fn from_vec_unchecked(bells: Vec<Option<Bell>>) -> Self {
        Self { bells }
    }
}

//////////////
// MATCHING //
//////////////

impl MusicType {
    pub fn count_block<T>(
        &self,
        block: &Block<T>,
        stroke_of_first_row: Stroke,
    ) -> AtRowPositions<usize> {
        Self::count(self, block, stroke_of_first_row)
    }

    pub fn count<'a>(
        &self,
        rows: impl Into<RowSlice<'a>>,
        stroke_of_first_row: Stroke,
    ) -> AtRowPositions<usize> {
        let rows = rows.into();

        let mut counts = AtRowPositions::ZERO;
        for pattern in &self.patterns {
            counts += pattern.count(self.strokes, rows, stroke_of_first_row);
        }
        counts
    }

    /// Returns the maximum possible number of occurrences of this `MusicType` when matching against
    /// rows of the given [`Stage`].  I.e. this would be the result of music counting the extent on
    /// this [`Stage`].
    pub fn max_possible_count(&self, stage: Stage) -> AtRowPositions<usize> {
        let mut counts = AtRowPositions::ZERO;
        for p in &self.patterns {
            // Compute how many ways to re-arrange the unfixed_bells
            let num_fixed_bells = p.bells.iter().flatten().count();
            let num_unfixed_bells = stage.num_bells() - num_fixed_bells;
            let num_perms_of_unfixed_bells = num_unfixed_bells.factorial();
            // Add these counts
            let num_internal_places = stage.num_bells().saturating_sub(1 + p.bells.len());
            counts.front += num_perms_of_unfixed_bells;
            counts.internal += num_internal_places * num_perms_of_unfixed_bells;
            counts.back += num_perms_of_unfixed_bells;
            counts.wrap += num_perms_of_unfixed_bells * (p.bells.len() - 1);
        }
        counts
    }
}

impl Pattern {
    fn count<'a>(
        &self,
        at_strokes: StrokeSet,
        rows: impl Into<RowSlice<'a>>,
        stroke: Stroke,
    ) -> AtRowPositions<usize> {
        // Like in Rust's regex crate, we will use an optimized SIMD search routine to search for
        // a substring with no 'x's then verify the rest of the pattern only once per potential
        // match.  In almost all cases, the music patterns actually contain no 'x's and we can do
        // the entire search using the aggressively optimised `memmem` crate.

        // Get the longest sequence of consecutive bells
        let Some(bell_range) = self.longest_subsequence_without_xs() else {
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
        let haystack_bytes: &[u8] = bytemuck::cast_slice(rows.bells);

        // Use the fast string searches to find all instances of the long sub-region
        let mut counts = AtRowPositions::ZERO;
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
                RowPosition::Wrap
            } else if starts_on_row_boundary {
                RowPosition::Front
            } else if ends_on_row_boundary {
                RowPosition::Back
            } else {
                RowPosition::Internal
            };
            // Add the match
            *counts.get_mut(position) += 1;
        }
        counts
    }

    fn longest_subsequence_without_xs(&self) -> Option<Range<usize>> {
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
}

/// A position in a row where music can be counted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RowPosition {
    Front,
    Internal,
    Back,
    Wrap,
}

impl RowPosition {
    pub const ALL: [RowPosition; 4] = [Self::Front, Self::Internal, Self::Back, Self::Wrap];
}

/// A collection of data (usually counts) for each position in a [`Row`] that music can occur
/// (front, internal, back, wrap)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtRowPositions<T> {
    pub front: T,
    pub internal: T,
    pub back: T,
    pub wrap: T,
}

impl AtRowPositions<()> {
    pub const ZERO: AtRowPositions<usize> = AtRowPositions::splat(0);
    pub const ZERO_F32: AtRowPositions<f32> = AtRowPositions::splat(0.0);
}

impl AtRowPositions<bool> {
    /* Masks */
    pub const FALSE: Self = AtRowPositions::splat(false);
    pub const TRUE: Self = AtRowPositions::splat(true);

    pub const FRONT: Self = Self::new(true, false, false, false);
    pub const BACK: Self = Self::new(false, false, true, false);
    pub const FRONT_AND_BACK: Self = Self::new(true, false, true, false);
}

impl<T> AtRowPositions<T> {
    pub const fn new(front: T, internal: T, back: T, wrap: T) -> Self {
        Self {
            front,
            internal,
            back,
            wrap,
        }
    }

    pub fn front_and_back(value: T) -> Self
    where
        T: Default + Clone,
    {
        Self::new(value.clone(), T::default(), value, T::default())
    }

    pub const fn splat(value: T) -> Self
    where
        T: Copy,
    {
        Self::new(value, value, value, value)
    }

    pub fn get(&self, position: RowPosition) -> &T {
        match position {
            RowPosition::Front => &self.front,
            RowPosition::Internal => &self.internal,
            RowPosition::Back => &self.back,
            RowPosition::Wrap => &self.wrap,
        }
    }

    pub fn set(&mut self, position: RowPosition, value: T) {
        *self.get_mut(position) = value;
    }

    pub fn masked(mut self, mask: AtRowPositions<bool>, value: T) -> Self
    where
        T: Clone,
    {
        self.set_mask(mask, value);
        self
    }

    pub fn set_mask(&mut self, mask: AtRowPositions<bool>, value: T)
    where
        T: Clone,
    {
        if mask.front {
            self.front = value.clone();
        }
        if mask.internal {
            self.internal = value.clone();
        }
        if mask.back {
            self.back = value.clone();
        }
        if mask.wrap {
            self.wrap = value;
        }
    }

    pub fn get_mut(&mut self, position: RowPosition) -> &mut T {
        match position {
            RowPosition::Front => &mut self.front,
            RowPosition::Internal => &mut self.internal,
            RowPosition::Back => &mut self.back,
            RowPosition::Wrap => &mut self.wrap,
        }
    }

    pub fn as_array(self) -> [T; 4] {
        [self.front, self.internal, self.back, self.wrap]
    }

    pub fn as_ref_array(&self) -> [&T; 4] {
        [&self.front, &self.internal, &self.back, &self.wrap]
    }

    pub fn total<V>(&self) -> V
    where
        V: std::iter::Sum<T>,
        T: Copy,
    {
        V::sum([self.front, self.internal, self.back, self.wrap].into_iter())
    }

    pub fn map<S>(self, mut f: impl FnMut(T) -> S) -> AtRowPositions<S> {
        AtRowPositions {
            front: f(self.front),
            internal: f(self.internal),
            back: f(self.back),
            wrap: f(self.wrap),
        }
    }
}

impl AtRowPositions<bool> {
    pub fn any(self) -> bool {
        self.as_array().into_iter().any(|x| x)
    }

    pub fn all(self) -> bool {
        self.as_array().into_iter().all(|x| x)
    }
}

impl<T: Add> Add for AtRowPositions<T> {
    type Output = AtRowPositions<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        AtRowPositions {
            front: self.front + rhs.front,
            internal: self.internal + rhs.internal,
            back: self.back + rhs.back,
            wrap: self.wrap + rhs.wrap,
        }
    }
}

impl<T: Mul> Mul for AtRowPositions<T> {
    type Output = AtRowPositions<T::Output>;

    fn mul(self, rhs: Self) -> Self::Output {
        AtRowPositions {
            front: self.front * rhs.front,
            internal: self.internal * rhs.internal,
            back: self.back * rhs.back,
            wrap: self.wrap * rhs.wrap,
        }
    }
}

impl<T: Not> Not for AtRowPositions<T> {
    type Output = AtRowPositions<T::Output>;

    fn not(self) -> Self::Output {
        AtRowPositions {
            front: !self.front,
            internal: !self.internal,
            back: !self.back,
            wrap: !self.wrap,
        }
    }
}

impl<T: AddAssign> AddAssign for AtRowPositions<T> {
    fn add_assign(&mut self, rhs: Self) {
        self.front += rhs.front;
        self.internal += rhs.internal;
        self.back += rhs.back;
        self.wrap += rhs.wrap;
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
        music::{AtRowPositions, MusicType},
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
                mt.count(&plain_course, crate::Stroke::Back),
                AtRowPositions {
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
            let counts = mt.count(&plain_course, crate::Stroke::Back);
            assert_eq!(counts.back, expected_87s);
        };

        check("Bristol Surprise Major", "12345678", 0); // Plain course of Bristol has no 87s
        check("Bristol Surprise Major", "12436587", 12); // Reversed plain course has 12
        check("Cooktown Orchid Delight Major", "12347658", 4);
        check("Rapid Wrap Major", "12345678", 3);

        check("Let's Ring! Delight Minor", "123456", 4);
    }

    #[test]
    fn full_length_rows() {
        let patterns = [
            "123456", "132546", "135246", "142536", "145236", "154326", "213546", "321456",
            "341256", "342516", "531246", "532146", "654321",
        ];

        let cc_lib = crate::MethodLib::cc_lib().unwrap();
        let method = cc_lib.get_by_title("Norwich Surprise Minor").unwrap();
        let plain_course = method.plain_course();

        for p in patterns {
            let mt = MusicType::parse(p).unwrap();
            let counts = mt.count(&plain_course, crate::Stroke::Back);
            assert_eq!(counts.back, 0);
        }
    }
}
