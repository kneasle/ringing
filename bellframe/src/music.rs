use std::{
    fmt::{Debug, Display, Formatter},
    iter::once,
};

use factorial::Factorial;
use itertools::Itertools;

use crate::{utils, Bell, IncompatibleStages, InvalidRowError, Row, RowBuf, Stage};

/// A regex-like `Pattern` over [`Row`]s.
///
/// A `Pattern` is a sequence of [`Elem`]ents, which can be one of three types:
/// - An [`Elem::Bell`], written as the corresponding [`Bell`] name (e.g. `1`)
/// - An [`Elem::X`] which matches exactly one of any [`Bell`], written as `x`
/// - An [`Elem::Star`] which matches any number (including zero) of any [`Bell`], written as `*`
///
/// For example, `"*x5678x*"` matches at least one of any [`Bell`] (`"*x"`), followed by `"5678"`,
/// followed by at least one of any [`Bell`] (`"x*"`).  In other words, `"*x5678x*` matches
/// _internal 5678s_.
///
/// `x`s and `*`s are known as _'wildcards'_, since they aren't bound to a specific [`Bell`].
///
/// # Normalisation and Invariants
///
/// Sequences of wildcards are always normalised into a sequence of `x`s, followed by at most one
/// `*`.  For example, `x*xx***x` would be normalised to `xxxx*`.
///
/// Finally, all `Pattern`s must match at least one [`Row`] of its given [`Stage`].  This adds a
/// few extra invariants (the latter two are shared with [`Row`] and [`Mask`](crate::Mask)):
/// - The length of the `Pattern` is compatible with the [`Stage`] (e.g. `1234xxx` is too long for
///   Minor, and `123` is too short for Minimus)
/// - All [`Bell`]s must be within the [`Stage`] (e.g. `7*` is invalid for Minor)
/// - All [`Bell`]s must be unique (e.g. `1x1*` is invalid for any [`Stage`]).
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Pattern {
    /// The sequence of [`Elem`]ents representing this `Pattern`.
    ///
    /// See [`Self::from_vec_unchecked`] for the invariants that `elems` must uphold.
    elems: Vec<Elem>,
    /// The [`Stage`] of the [`Row`]s matched by this `Pattern`.  All `Pattern`s always have a
    /// specific [`Stage`] as a safety check, even if, in theory, they would be able to match
    /// [`Row`]s of many [`Stage`]s.
    stage: Stage,
}

/// A single `Elem`ent of a music [`Pattern`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Elem {
    /// Matches one specific [`Bell`].  Written as that [`Bell`]'s name (e.g. `5`)
    Bell(Bell),
    /// Matches one of any [`Bell`].  Written as `x` or `X` (`X` is not a valid [`Bell`] name for
    /// this reason).
    X,
    /// Matches any number (including zero) of any [`Bell`]s.  Written as `*`.
    Star,
}

impl Pattern {
    /// Parses a `Pattern` from a string.  A [`Bell`] name matches only that [`Bell`], `'x'` or
    /// `'X'` matches precisely one of any [`Bell`] whereas `'*'` matches zero or more of any
    /// [`Bell`].  For example, `"xxxx5678" and `"*5678"` both correspond to 56s on 8 bells.
    pub fn parse(s: &str, stage: Stage) -> Result<Self, PatternError> {
        Self::from_elems(
            s.chars().filter_map(|c| match c {
                'x' | 'X' => Some(Elem::X),
                '*' => Some(Elem::Star),
                _ => Bell::from_name(c).map(Elem::Bell),
            }),
            stage,
        )
    }

    /// Gets the [`Stage`] of [`Row`]s that are matched by this [`Pattern`]
    pub fn stage(&self) -> Stage {
        self.stage
    }

    /// Creates a normalised `Pattern` from an [`Iterator`] of [`Elem`]s
    pub fn from_elems(
        iter: impl IntoIterator<Item = Elem>,
        stage: Stage,
    ) -> Result<Self, PatternError> {
        Self::from_vec(iter.into_iter().collect_vec(), stage)
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Elem`]s, checking the invariants and normalising
    /// the result.  This uses the allocation of the given [`Vec`].
    pub fn from_vec(mut elems: Vec<Elem>, stage: Stage) -> Result<Self, PatternError> {
        // Invariant 1: Normalise all `*x`s to `x*`s.  This is equivalent to taking every sequence
        // of wildcards and rewriting it to have all the `x`s first.  (the `*`s will be compressed
        // later).
        //
        // As a running example, this step would normalise `x*xx***5x6x7x8*x*`
        //                                              to `xxx****5x6x7x8x**`
        for wildcard_region in elems.split_mut(|e| e.is_bell()) {
            let num_xs = wildcard_region.iter().filter(|e| e.is_x()).count();
            let (region_of_xs, region_of_stars) = wildcard_region.split_at_mut(num_xs);
            for e in region_of_xs {
                *e = Elem::X;
            }
            for e in region_of_stars {
                *e = Elem::Star;
            }
        }
        // Invariant 2: Normalise any `**`s into `*`s.  We do this by removing any `*`s which
        // precede a `*` (using `Vec::retain`).
        //
        // In the example,    `xxx****5x6x7x8x**`
        // would normalise to `xxx*   5x6x7x8x* `
        let mut is_last_elem_a_star = false;
        elems.retain(|e| {
            let should_remove = e.is_star() && is_last_elem_a_star;
            is_last_elem_a_star = e.is_star();
            !should_remove // `retain` keeps any elems which return `true`
        });
        // Invariants 3 & 4: `Bell`s are all unique and contained in the `Stage`
        match crate::utils::check_duplicate_or_out_of_stage(
            elems.iter().filter_map(|e| e.bell()),
            stage,
        ) {
            Ok(()) => {} // Row is OK
            Err(InvalidRowError::DuplicateBell(b)) => return Err(PatternError::DuplicateBell(b)),
            Err(InvalidRowError::BellOutOfStage(b, s)) => {
                return Err(PatternError::BellOutOfStage(b, s))
            }
            Err(InvalidRowError::NoBells | InvalidRowError::MissingBell(_)) => {
                unreachable!("Zero-sized `Stage`s aren't possible")
            }
        }
        // Invariant 5: length is compatible with the given `Stage`
        let num_non_stars = elems.iter().filter(|e| **e != Elem::Star).count();
        if num_non_stars > stage.num_bells() {
            return Err(PatternError::TooLong(num_non_stars, stage)); // Too long
        }
        if !elems.contains(&Elem::Star) && num_non_stars < stage.num_bells() {
            let string = elems.iter().map(Elem::to_string).join("");
            return Err(PatternError::TooShort(num_non_stars, stage, string)); // Too short
        }

        // Unsafety is OK because `elems` has been normalised
        Ok(unsafe { Self::from_vec_unchecked(elems, stage) })
    }

    /// Creates a `Pattern` from an [`Iterator`] of [`Elem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// See [`Pattern::from_vec_unchecked`] for safety requirements.
    /// [`Pattern::from_vec_unchecked`] is called after collecting the [`Iterator`] into a [`Vec`].
    pub unsafe fn from_elems_unchecked(iter: impl IntoIterator<Item = Elem>, stage: Stage) -> Self {
        Self::from_vec_unchecked(iter.into_iter().collect_vec(), stage)
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Elem`]s, without normalising or checking invariants.
    ///
    /// # Safety
    ///
    /// This is only safe if `elems` satisfies the following invariants:
    /// - `*x` doesn't appear anywhere; `*x` should be normalised to `x*`
    /// - `**` doesn't appear anywhere; `**` should be normalised to `*`
    /// - All [`Bell`]s are [`contained`](Stage::contains) within the given [`Stage`]
    /// - All [`Bell`]s in `elems` are unique
    /// - The resulting `Pattern` is able to match [`Row`]s of the given [`Stage`] (i.e. `elems` is
    ///   neither too long nor too short).
    #[inline]
    pub unsafe fn from_vec_unchecked(elems: Vec<Elem>, stage: Stage) -> Self {
        Self { elems, stage }
    }

    /// Creates a set of `Pattern`s which match runs of a given length off the **front** of a
    /// [`Row`].  If the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn runs_front(stage: Stage, len: u8) -> Vec<Self> {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return vec![];
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in ascending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Elem::Bell);

            // This unsafety is OK because all of the input patterns must be normalised because
            // they contain only one wildcard
            let descending_pattern = unsafe {
                // Descending runs on the front (e.g. `1234*`)
                Self::from_elems_unchecked(run_iterator.clone().chain(once(Elem::Star)), stage)
            };
            let ascending_pattern = unsafe {
                // Ascending runs on the front (e.g. `4321*`)
                Self::from_elems_unchecked(run_iterator.rev().chain(once(Elem::Star)), stage)
            };

            runs.push(descending_pattern);
            runs.push(ascending_pattern);
        }

        runs
    }

    /// Creates a set of `Pattern`s which match runs of a given length off the **front** of a
    /// [`Row`].  If the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn runs_internal(stage: Stage, len: u8) -> Vec<Self> {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return vec![];
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in ascending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Elem::Bell);

            // The unsafety is OK because all the input patterns are normalised

            // Descending runs (e.g. `x*1234x*`)
            let mut desc_elems = vec![Elem::X, Elem::Star];
            desc_elems.extend(run_iterator.clone());
            desc_elems.extend([Elem::X, Elem::Star]);
            runs.push(unsafe { Self::from_vec_unchecked(desc_elems, stage) });
            // Ascending runs (e.g. `x*4321x*`)
            let mut asc_elems = vec![Elem::X, Elem::Star];
            asc_elems.extend(run_iterator.rev());
            asc_elems.extend([Elem::X, Elem::Star]);
            runs.push(unsafe { Self::from_vec_unchecked(asc_elems, stage) });
        }

        runs
    }

    /// Creates a set of `Pattern`s which match runs of a given length off the **back** of a
    /// [`Row`].  If the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn runs_back(stage: Stage, len: u8) -> Vec<Self> {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return vec![];
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in ascending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Elem::Bell);
            // This unsafety is OK because all of the input patterns must be normalised because
            // they contain only one wildcard
            let descending_pattern = unsafe {
                // Descending runs on the back (e.g. `*1234`)
                Self::from_elems_unchecked(once(Elem::Star).chain(run_iterator.clone()), stage)
            };
            let ascending_pattern = unsafe {
                // Ascending runs on the back (e.g. `*4321`)
                Self::from_elems_unchecked(
                    once(Elem::Star).chain(run_iterator.clone().rev()),
                    stage,
                )
            };
            runs.push(descending_pattern);
            runs.push(ascending_pattern);
        }

        runs
    }

    /// Creates a set of `Pattern`s which match runs of a given length anywhere in a [`Row`].  If
    /// the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn all_runs(stage: Stage, len: u8) -> Vec<Self> {
        let num_bells = stage.num_bells_u8();
        if num_bells < len {
            return vec![];
        }

        let mut runs = Vec::with_capacity((num_bells as usize).saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in ascending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(Elem::Bell);

            // This unsafety is OK because all of the input patterns must be normalised because
            // they contain only one wildcard
            let descending_pattern = unsafe {
                // Descending runs (e.g. `*1234*`)
                Self::from_elems_unchecked(
                    once(Elem::Star)
                        .chain(run_iterator.clone())
                        .chain(once(Elem::Star)),
                    stage,
                )
            };
            let ascending_pattern = unsafe {
                // Ascending runs (e.g. `*4321*`)
                Self::from_elems_unchecked(
                    once(Elem::Star)
                        .chain(run_iterator.clone().rev())
                        .chain(once(Elem::Star)),
                    stage,
                )
            };

            runs.push(descending_pattern);
            runs.push(ascending_pattern);
        }
        runs
    }

    /// Creates a set of `Pattern`s which match runs of a given length off the front or back of a
    /// [`Row`].  If the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn runs_front_or_back(stage: Stage, len: u8) -> Vec<Self> {
        let mut runs = Vec::new();
        runs.extend(Self::runs_front(stage, len));
        runs.extend(Self::runs_back(stage, len));
        runs
    }

    /// Creates a set of `Pattern`s which match runs of a given length off the front or back of a
    /// [`Row`].  If the run length is longer than the stage, then no `Pattern`s are returned.
    pub fn runs(stage: Stage, len: u8, internal: bool) -> Vec<Self> {
        if internal {
            Self::all_runs(stage, len)
        } else {
            Self::runs_front_or_back(stage, len)
        }
    }

    /// Gets a slice over the [`Elem`]ents making up this `Pattern`
    #[inline(always)]
    pub fn elems(&self) -> &[Elem] {
        &self.elems
    }

    /// Returns the number of [`Row`]s with a given [`Stage`] which would match `self`.  Returns
    /// `None` if the any part of the computation causes integer overflow.
    pub fn num_matching_rows(&self) -> Option<usize> {
        // Each pattern has some (possibly empty) set of 'unfixed' bells: i.e. those which don't
        // appear in the pattern.  For example, the pattern `*1x*56*78` on Major has `{2,3,4}` as its
        // unfixed bells.  These unfixed bells can either be placed in 'x's (i.e. exactly one bell
        // per 'x') or into '*'s (i.e. any non-negative number of bells per '*').  All bells must
        // appear exactly once in the resulting row, so we must _partition_ the 'unfixed' bells
        // into the 'x's or the '*'s.
        //
        // To derive the formula for this, suppose that we have `n` unfixed bells, which must be
        // divided into `x` 'x's and `s` '*'s.  To enumerate every possibility, we must first
        // assign a bell to each of the 'x's, then (for each of these assignments) partition the
        // remaining bells between the '*'s.
        //
        // The number of assignments of `n` bells into `x` 'x's is simply `n choose x`.  Now we're
        // left with the remaining `n - x` bells to distribute into `s` '*'s.  This step is not as
        // simple, but nonetheless there is a nice way to re-frame the problem: instead of
        // distributing the bells into `s` '*'s, we choose to split the bells up by inserting
        // `s - 1` identical barriers.
        //
        // For example, if we have bells 1,2,3,4,5,6 to split into 3 '*'s, we instead insert two
        // barriers (denoted with `|`) into the sequence of bells being permuted.  So now, we are
        // looking for the number of arrangements of `123456||` (for example `62|35|41`, `|4|16532`
        // or `||654321`).  These correspond precisely to the arrangements into '*'s.
        //
        // Now to find the formula: there are `(n - x + s - 1)!` arrangements of the bells and
        // barriers, but the barriers are all identical so these permutations are over-counted by
        // `(s - 1)!` times (assuming that `(-1)! = 1`).  Putting this together, for each way of
        // assigning bells to 'x's there are `(n - x + s - 1)! / (s - 1)!` ways to distribute the
        // remaining bells into the '*'s.
        //
        // Multiplying both of these terms will give the total number of matching `Row`s

        // Compute the values of `n` (num_unfixed), `x` (num_xs) and `s` (num_stars) by
        // iterating over the pattern and counting.
        let mut num_unfixed = self.stage.num_bells(); // `n`, decremented for each fixed bell
        let mut num_xs = 0usize; // `x`
        let mut num_stars = 0usize; // `s`
        for e in &self.elems {
            match e {
                Elem::Bell(_) => num_unfixed -= 1,
                Elem::X => num_xs += 1,
                Elem::Star => num_stars += 1,
            }
        }

        let ways_to_fill_xs = utils::choice(num_unfixed, num_xs)?;

        // TODO: Compute the fraction directly to limit potential for overflow
        let num_bells_in_stars = num_unfixed - num_xs;
        let ways_to_fill_stars_numerator =
            (num_bells_in_stars + num_xs.saturating_sub(1)).checked_factorial()?;
        let ways_to_fill_stars_denominator = num_stars.saturating_sub(1).checked_factorial()?;
        let ways_to_fill_stars = ways_to_fill_stars_numerator / ways_to_fill_stars_denominator;

        Some(ways_to_fill_xs * ways_to_fill_stars)
    }

    /// Returns `true` if a given [`Row`] satisfies this `Pattern`, and `false` otherwise.  If the
    /// normalised `Pattern` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    pub fn matches(&self, r: &Row) -> Result<bool, IncompatibleStages> {
        // Run a match, ignoring the indices which are a match
        self.run_match(r, |_i| ())
    }

    /// Returns the **0-indexed** places of the [`Bell`]s which match this `Pattern` (or `None` if
    /// the [`Row`] doesn't match).  The places will always be sorted in ascending order.  If the
    /// [`Row`] isn't matched, `None` is returned.
    pub fn match_pattern(&self, r: &Row) -> Result<Option<Vec<usize>>, IncompatibleStages> {
        // Run the matching algorithm, storing where the bells match
        let mut matches = Vec::<usize>::new();
        let is_match = self.run_match(r, |i| matches.push(i))?;

        Ok(if is_match {
            Some(matches) // If the row did match, then return the places
        } else {
            None // If the row turns out not to match the row, return an empty match
        })
    }

    /// Helper function to test a match between a [`Row`] and this `Pattern`, calling some function
    /// every time a [`Bell`] is matched.
    ///
    /// If the normalised `Pattern` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    fn run_match(
        &self,
        r: &Row,
        mut match_fn: impl FnMut(usize),
    ) -> Result<bool, IncompatibleStages> {
        IncompatibleStages::test_err(self.stage(), r.stage())?;

        /* Note: This algorithm relies on the uniqueness of bells within rows, so cannot be used
         * on arbitrary iterators of bells */

        let mut bells = r.bell_iter().enumerate();
        let mut pattern_elems = self.elems.iter().copied();

        // This loop must terminate because `pattern_elems` gets shorter every iteration
        loop {
            // If we haven't run out of pattern elements, then test the patterns
            match pattern_elems.next() {
                // If the next element is a specific bell, then the next bell must be matched
                Some(Elem::Bell(exp_bell)) => {
                    match bells.next() {
                        Some((i, b)) => {
                            if b == exp_bell {
                                // If the bell from the row matches, then report the index of this
                                // match
                                match_fn(i);
                            } else {
                                // If the bell didn't match what we expected, the entire row
                                // doesn't match
                                return Ok(false);
                            }
                        }
                        // If the pattern expected a bell but the row finished, then the row doesn't
                        // match
                        None => return Ok(false),
                    }
                }
                // If the pattern is 'Any', then the row matches if it has more bells left
                Some(Elem::X) => {
                    if bells.next().is_none() {
                        return Ok(false);
                    }
                }
                // If the next element is a star, then we need to look ahead at the next expected
                // element
                Some(Elem::Star) => match pattern_elems.next() {
                    // We've encountered `<...>*b<...>` where b is a bell name.  To match this, we
                    // repeatedly consume bells until either:
                    // - we find a matching bell, in which case continue with the pattern
                    // - we run out of bells, in which case the pattern does not match because
                    Some(Elem::Bell(exp_bell)) => {
                        loop {
                            match bells.next() {
                                // If we find a matching bell, then report the match and continue
                                // reading the pattern
                                Some((i, b)) => {
                                    if b == exp_bell {
                                        match_fn(i);
                                        break;
                                    }
                                }
                                // If we consume the entire row without finding the wanted bell,
                                // the pattern doesn't match
                                None => return Ok(false),
                            }
                        }
                    }
                    // If the pattern ends with a star, then any extension of the row matches
                    None => return Ok(true),
                    // Because of normalisation, a star cannot be followed by anything other than a
                    // bell (or the end of the pattern).  This branch should only be reachable with
                    // `unsafe`
                    _ => unreachable!(),
                },
                // If we've run out of pattern elements, then the row matches iff we also run out of
                // bells at the same time
                None => return Ok(bells.next().is_none()),
            }
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for elem in &self.elems {
            write!(f, "{}", elem)?;
        }
        Ok(())
    }
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pattern({})", self)
    }
}

impl Elem {
    /// If this is a [`Elem::Bell`], this returns the contained [`Bell`], otherwise `None`
    #[inline]
    pub fn bell(self) -> Option<Bell> {
        match self {
            Self::Bell(b) => Some(b),
            _ => None,
        }
    }

    /// `true` if `self` is [`Elem::Bell`].  Equivalent to `self.bell().is_some()`
    #[inline]
    pub fn is_bell(self) -> bool {
        matches!(self, Self::Bell(_))
    }

    /// `true` if `self` is a wildcard (i.e. `*` or `x`).
    #[inline]
    pub fn is_wildcard(self) -> bool {
        matches!(self, Self::X | Self::Star)
    }

    /// `true` if `self` is an `x` (i.e. `Self::Any`)
    #[inline]
    pub fn is_x(self) -> bool {
        matches!(self, Self::X)
    }

    /// `true` if `self` is an `x` (i.e. `Self::Any`)
    #[inline]
    pub fn is_star(self) -> bool {
        matches!(self, Self::Star)
    }
}

impl Display for Elem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bell(b) => write!(f, "{}", b),
            Self::X => write!(f, "x"),
            Self::Star => write!(f, "*"),
        }
    }
}

impl From<&Row> for Pattern {
    fn from(r: &Row) -> Self {
        Self {
            elems: r.bell_iter().map(Elem::Bell).collect_vec(),
            stage: r.stage(),
        }
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
    TooShort(usize, Stage, String),
    TooLong(usize, Stage),
}

impl PatternError {
    pub(crate) fn write_message(
        &self,
        f: &mut impl std::fmt::Write,
        whats_being_parsed: &str,
    ) -> std::fmt::Result {
        match self {
            PatternError::BellOutOfStage(bell, stage) => {
                write!(f, "bell {} is out of stage {}", bell, stage)
            }
            PatternError::DuplicateBell(bell) => write!(f, "bell {} appears twice", bell),
            PatternError::TooShort(len, stage, string) => {
                let extended_tenors = stage
                    .bells()
                    .skip(*len)
                    .map(|b| b.to_char().unwrap())
                    .collect::<String>();
                write!(
                    f,
                    "{} is too short; did you mean `{}*` or `{}{}`?",
                    whats_being_parsed, string, string, extended_tenors
                )
            }
            PatternError::TooLong(len, stage) => write!(
                f,
                "{} is too long, needing at least {} bells (too many for {})",
                whats_being_parsed, len, stage
            ),
        }
    }
}

impl Display for PatternError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.write_message(f, "pattern")
    }
}

impl std::error::Error for PatternError {}

#[cfg(test)]
mod tests {
    use crate::{Bell, RowBuf, Stage};

    use super::{Pattern, PatternError};

    #[test]
    fn pattern_parse_err() {
        #[track_caller]
        fn check_exceeds_stage(pattern: &str, num_bells: u8, bell: char) {
            let stage = Stage::new(num_bells);
            let bell = Bell::from_name(bell).unwrap();
            assert_eq!(
                Pattern::parse(pattern, stage),
                Err(PatternError::BellOutOfStage(bell, stage))
            );
        }
        #[track_caller]
        fn check_too_short(pattern: &str, num_bells: u8, length: usize) {
            let stage = Stage::new(num_bells);
            assert!(length < stage.num_bells());
            assert_eq!(
                Pattern::parse(pattern, stage),
                Err(PatternError::TooShort(length, stage, pattern.to_owned()))
            );
        }
        #[track_caller]
        fn check_too_long(pattern: &str, num_bells: u8, length: usize) {
            let stage = Stage::new(num_bells);
            assert!(length > stage.num_bells());
            assert_eq!(
                Pattern::parse(pattern, stage),
                Err(PatternError::TooLong(length, stage))
            );
        }

        check_exceeds_stage("xxx5", 4, '5');
        check_exceeds_stage("*5", 4, '5');
        check_exceeds_stage("5*", 4, '5');
        check_too_short("xx3", 4, 3);
        check_too_short("3421", 8, 4);
        check_too_long("xxxx3x", 4, 6);
        check_too_long("xxxx3*", 4, 5);
    }

    #[test]
    fn pattern_normalise() {
        #[track_caller]
        fn check(inp_str: &str, exp_str: &str) {
            assert_eq!(
                Pattern::parse(inp_str, Stage::MAJOR).unwrap().to_string(),
                exp_str
            );
        }

        check("*", "*");
        check("**", "*");
        check("***", "*");
        check("****", "*");
        check("1x2*x**", "1x2x*");
        check("**5678", "*5678");
        check("*1**", "*1*");
        check("*1", "*1");
        check("xxxx5678", "xxxx5678");
        check("x*", "x*");
        check("1*x*3", "1x*3");
        check("1*x**x**", "1xx*");
        check("1*x*2*x**", "1x*2x*");
    }

    #[test]
    fn is_match() {
        #[track_caller]
        fn check(pattern_str: &str, row_str: &str, expected_match: bool) {
            let row = &RowBuf::parse(row_str).unwrap();
            let pattern = Pattern::parse(pattern_str, row.stage()).unwrap();
            let is_match = pattern.matches(&row).unwrap();

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
            let pattern = Pattern::parse(pattern_str, row.stage()).unwrap();
            assert_eq!(pattern.match_pattern(&row).unwrap(), expected_pattern);
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
}
