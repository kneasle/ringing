use std::{
    fmt::{Debug, Display, Formatter},
    iter::once,
};

use factorial::Factorial;
use itertools::Itertools;

use crate::{utils, Bell, Row, RowBuf, Stage};

/// A `Pattern` over sequences of [`Bell`]s (usually over [`Row`]s).
///
/// A `Pattern` is a sequence of [`Elem`]ents, which can be one of three types:
/// - An [`Elem::Bell`], written as the corresponding [`Bell`] name (e.g. `'1'`)
/// - An [`Elem::X`] which matches exactly one of any [`Bell`], written as `'x'`
/// - An [`Elem::Star`] which matches any number (including zero) of any [`Bell`], written as `'*'`
///
/// For example, `"*x5678x*"` matches at least one of any [`Bell`] (`"*x"`), followed by `"5678"`,
/// followed by at least one of any [`Bell`] (`"x*"`).  In other words, `"*x5678x*` matches
/// _internal 5678s_.
///
/// `x`s and `*`s are known as 'wildcards', since they aren't bound to a specific [`Bell`].
///
/// Wildcards in `Pattern`s are normalised according to two rules:
/// 1. If there is a sequence of consecutive `x`s and `*`s (e.g. `*x*x***`) then the `x`s
///    will always come first, followed by the `*`s (this example would get reordered to
///    `xx*****`).
/// 2. Consecutive `*`s are reduced to a single `*` (this example would get fully normalised from
///    `*x*x***` to `xx*****` then to `xx*`).
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Pattern {
    /// The pattern which represents this `Pattern`.  This is normalised with the following
    /// invariants:
    /// - There are never more than two stars in a row (because `**` is equivalent to `*`)
    /// - If there is a sequence of consecutive 'any's and stars (e.g. `*x*`) then the 'any's
    ///   will always come first, followed by the stars.
    ///
    /// These two rules feed into each other, so `*x*x` would be normalised to `xx**` (second rule:
    /// reorder 'any's and stars) and then to `xx*` (first rule: compress consecutive stars).
    ///
    /// Therefore, after a star we can only have either a bell or the end of the pattern.
    elems: Vec<Elem>,
    stage: Stage,
}

/// A single `Elem`ent of a music [`Pattern`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Elem {
    /// Matches one specific [`Bell`].  Written as that [`Bell`]'s name (e.g. `'5'`)
    Bell(Bell),
    /// Matches one of any [`Bell`].  Written as `x` or `X`.
    X,
    /// Matches any number (including zero) of any [`Bell`]s.  Written as `*`.
    Star,
}

impl Pattern {
    /// Parses a `Pattern` from a string.  A [`Bell`] name matches only that [`Bell`], `'x'` matches
    /// precisely one of any [`Bell`] whereas `'*'` matches zero or more of any [`Bell`].  For
    /// example, `"xxxx5678" and `"*5678"` both correspond to 56s on 8 bells.
    pub fn parse(s: &str, stage: Stage) -> Self {
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
    pub fn from_elems(iter: impl IntoIterator<Item = Elem>, stage: Stage) -> Self {
        Self::from_vec(iter.into_iter().collect_vec(), stage)
    }

    /// Creates a `Pattern` from an [`Iterator`] of [`Elem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// This is safe if the incoming [`Iterator`] satisfies the wildcard normalisation rules.
    pub unsafe fn from_elems_unchecked(iter: impl IntoIterator<Item = Elem>, stage: Stage) -> Self {
        Self::from_vec_unchecked(iter.into_iter().collect_vec(), stage)
    }

    /// Creates a normalised `Pattern` from a [`Vec`] of [`Elem`]s.
    #[inline(always)]
    pub fn from_vec(elems: Vec<Elem>, stage: Stage) -> Self {
        // This unsafety is OK because the only operation we perform on the un-normalised Pattern is
        // to normalise it (thus making it safe for other operations).
        let mut r = unsafe { Self::from_vec_unchecked(elems, stage) };
        r.normalise();
        r
    }

    /// Creates a `Pattern` from a [`Vec`] of [`Elem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// This is safe if the [`Vec`] satisfies the wildcard normalisation rules.
    #[inline(always)]
    pub unsafe fn from_vec_unchecked(elems: Vec<Elem>, stage: Stage) -> Self {
        Self { elems, stage }
    }

    /// Normalises wildcards and stars in a `Pattern`.  This should only in combination with `unsafe`
    /// - if only safe code is used, `self` will already be normalised.  This is the only operation
    /// on `Pattern`s which doesn't rely on the input being normalised.
    pub fn normalise(&mut self) {
        // TODO: Perform stricter validity checks based on stage

        let mut normalised_elems = Vec::<Elem>::with_capacity(self.elems.len());

        // Insert a rogue bell to the end `self.elems` so that all runs of consecutive
        // wildcards/stars end in a bell
        self.elems.push(Elem::Bell(Bell::TREBLE));
        // Split the elements into chunks of 0 or more wildcards/stars, each of which finishes with
        // a bell.
        for slice in self.elems.split_inclusive(|e| matches!(e, Elem::Bell(_))) {
            // The slice should contain at least one element (the bell that matched the pattern)
            assert!(!slice.is_empty());
            let wildcards_pattern = &slice[..slice.len() - 1];
            let bell_elem = slice[slice.len() - 1];
            // Normalise the wildcard pattern, and push it.  To normalise it, we move all the 'x's
            // to the start of the pattern and then add a single star (if the pattern contains any
            // stars).
            let num_wildcards = wildcards_pattern
                .iter()
                .filter(|e| matches!(e, Elem::X))
                .count();
            normalised_elems.extend(std::iter::repeat(Elem::X).take(num_wildcards));
            if wildcards_pattern.contains(&Elem::Star) {
                normalised_elems.push(Elem::Star);
            }
            // Push the bell that terminates this wildcard pattern
            normalised_elems.push(bell_elem);
        }

        // Overwrite this pattern with the normalised element list
        self.elems = normalised_elems;
        // Pop the rogue bell off the end of the pattern (we added it before the loop to make the
        // code cleaner).
        assert_eq!(self.elems.pop(), Some(Elem::Bell(Bell::TREBLE)));
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
        // unfixed bells.  These unfixed bells can either be placed in `x`s (i.e. exactly one bell
        // per `x`) or into `*`s (i.e. any non-negative number of bells per `*`).  All bells must
        // appear exactly once in the resulting row, so we must _partition_ the 'unfixed' bells
        // into the `x`s or the `*`s.
        //
        // To derive the formula for this, suppose that we have `n` unfixed bells, which must be
        // divided into `i` 'x's and `j` '*'s.  To enumerate every possibility, we must first
        // assign a bell to each of the 'x's, then (for each of these assignments) partition the
        // remaining bells between the '*'s.
        //
        // The number of assignments of `n` bells into `i` 'x's is simply `n choose i`.  Now we're
        // left with the remaining `n - i` bells to distribute into `j` '*'s.  This step is not as
        // simple, but nonetheless there is a nice way to re-frame the problem: instead of
        // distributing the bells into `j` '*'s, we choose to split the bells up by inserting
        // `j - 1` identical barriers.
        //
        // For example, if we have bells 1,2,3,4,5,6 to split into 3 '*'s, we instead insert two
        // barriers (denoted with `|`) into the sequence of bells being permuted.  So now, we are
        // looking for the number of arrangements of `123456||` (for example `62|35|41`, `|4|16532`
        // or `||654321`).  These correspond precisely to the arrangements into '*'s.
        //
        // Now to find the formula: there are `(n - i + j - 1)!` arrangements of the bells and
        // barriers, but the barriers are all identical so these permutations are over-counted by
        // `(j - 1)!` times (assuming that `(-1)! = 1`).  Putting this together, for each way of
        // assigning bells to 'x's there are `(n - i + j - 1)! / (j - 1)!` ways to distribute the
        // remaining bells into the '*'s.
        //
        // Multiplying both of these terms will give the total number of matching `Row`s

        // Compute the values of `n` (num_unfixed), `i` (num_wildcards) and `j` (num_stars) by
        // iterating over the pattern and counting.
        let mut num_unfixed = self.stage.num_bells(); // `n`, decremented for each fixed bell
        let mut num_wildcards = 0usize; // `i`
        let mut num_stars = 0usize; // `j`
        for e in &self.elems {
            match e {
                Elem::Bell(_) => num_unfixed -= 1,
                Elem::X => num_wildcards += 1,
                Elem::Star => num_stars += 1,
            }
        }

        let ways_to_fill_wildcards = utils::choice(num_unfixed, num_wildcards)?;

        // TODO: Compute the fraction directly to limit potential for overflow
        let num_bells_in_stars = num_unfixed - num_wildcards;
        let ways_to_fill_stars_numerator =
            (num_bells_in_stars + num_wildcards.saturating_sub(1)).checked_factorial()?;
        let ways_to_fill_stars_denominator = num_stars.saturating_sub(1).checked_factorial()?;
        let ways_to_fill_stars = ways_to_fill_stars_numerator / ways_to_fill_stars_denominator;

        Some(ways_to_fill_wildcards * ways_to_fill_stars)
    }

    /// Returns `true` if a given [`Row`] satisfies this `Pattern`, and `false` otherwise.  If the
    /// normalised `Pattern` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    pub fn matches(&self, r: &Row) -> bool {
        // Run a match, ignoring the indices which are a match
        self.run_match(r, |_i| ())
    }

    /// Returns the **0-indexed** places of the [`Bell`]s which match this `Pattern` (or `None` if
    /// the [`Row`] doesn't match).  The places will always be sorted in ascending order.  If the
    /// [`Row`] isn't matched, `None` is returned.
    pub fn match_pattern(&self, r: &Row) -> Option<Vec<usize>> {
        // Run the matching algorithm, storing where the bells match
        let mut matches = Vec::<usize>::new();
        let is_match = self.run_match(r, |i| matches.push(i));

        if is_match {
            // If the row did match, then return the places
            Some(matches)
        } else {
            // If the row turns out not to match the row, return an empty match
            None
        }
    }

    /// Helper function to test a match between a [`Row`] and this `Pattern`, calling some function
    /// every time a [`Bell`] is matched.
    ///
    /// If the normalised `Pattern` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    #[inline(always)]
    fn run_match(&self, r: &Row, mut match_fn: impl FnMut(usize)) -> bool {
        /* Note: This algorithm relies on the uniqueness of bells within rows, so cannot be used
         * on arbitrary iterators of bells */

        let mut bells = r.bell_iter().enumerate();
        let mut pattern_elems = self.elems.iter().copied();

        // This loop must terminate because `pattern_elems` gets shorter every iteration
        loop {
            // If we haven't run out of pattern elements, then test the patterns
            match pattern_elems.next() {
                // If the next element is a specific bell, then the next bell must be the same
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
                                return false;
                            }
                        }
                        // If the pattern expected a bell but the row finished, then the row doesn't
                        // match
                        None => return false,
                    }
                }
                // If the pattern is 'Any', then the row matches if it has more bells left
                Some(Elem::X) => {
                    if bells.next().is_none() {
                        return false;
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
                                None => return false,
                            }
                        }
                    }
                    // If the pattern ends with a star, then any extension of the row matches
                    None => return true,
                    // Because of normalisation, a star cannot be followed by anything other than a
                    // bell (or the end of the pattern).  This branch should only be reachable with
                    // `unsafe`
                    _ => unreachable!(),
                },
                // If we've run out of pattern elements, then the row matches iff we also run out of
                // bells at the same time
                None => return bells.next().is_none(),
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

#[cfg(test)]
mod tests {
    use crate::{RowBuf, Stage};

    use super::Pattern;

    #[test]
    fn pattern_normalise() {
        #[track_caller]
        fn check(inp_str: &str, exp_str: &str) {
            println!("Testing {:?}", inp_str);
            assert_eq!(Pattern::parse(inp_str, Stage::MAJOR).to_string(), exp_str);
        }

        check("", "");
        check("*", "*");
        check("**", "*");
        check("***", "*");
        check("****", "*");
        check("x", "x");
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
    fn pattern_is_match() {
        #[track_caller]
        fn check(pattern_str: &str, row_str: &str, expected_match: bool) {
            println!("Testing row {} against pattern {}", row_str, pattern_str);

            let row = &RowBuf::parse(row_str).unwrap();
            let pattern = Pattern::parse(pattern_str, row.stage());
            let is_match = pattern.matches(&row);

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
        check("xxxx", "4321", true);
        check("xxxx", "54321", false);
        // Pattern with only 'x's and numbers
        check("x1xx", "2134", true);
        check("x1xx", "2134", true);
        check("x1xx", "4321", false);
        check("x1", "21345", false);
        // Pattern with stars
        check("*1234", "1234", true);
        check("*1234", "51234", true);
        check("*1234", "123", false);
        check("*6578", "12346578", true);
        check("*65*78", "12346578", true);
        check("*65*78", "12653478", true);
        check("*65*78", "12657834", false);
        // Pattern with everything
        check("x*12*34", "61257834", true);
        check("x*12*34", "12657834", false);
        check("x*12*34", "51234", true);
        check("x*12*34", "1234", false);
        check("x*12*34", "12534", false);
    }

    #[test]
    fn pattern_match_pattern() {
        #[track_caller]
        fn check(pattern_str: &str, row_str: &str, expected_pattern: Option<Vec<usize>>) {
            println!("Testing row {} against pattern {}", row_str, pattern_str);

            let row = &RowBuf::parse(row_str).unwrap();
            let pattern = Pattern::parse(pattern_str, row.stage());
            assert_eq!(pattern.match_pattern(&row), expected_pattern);
        }

        // Patterns with only 'x's
        check("xxxx", "3412", Some(vec![]));
        check("xxXx", "4321", Some(vec![]));
        check("XXXX", "54321", None);
        // Patterns with only 'x's and numbers
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "4321", None);
        check("x1", "21345", None);
        // Patterns with stars
        check("*1234", "1234", Some(vec![0, 1, 2, 3]));
        check("*1234", "51234", Some(vec![1, 2, 3, 4]));
        check("*1234", "123", None);
        check("*6578", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12653478", Some(vec![2, 3, 6, 7]));
        check("*65*78", "12657834", None);
        // Patterns with everything
        check("x*12*34", "61257834", Some(vec![1, 2, 6, 7]));
        check("x*12*34", "12657834", None);
        check("x*12*34", "51234", Some(vec![1, 2, 3, 4]));
        check("x*12*34", "1234", None);
        check("x*12*34", "12534", None);
        check("*1x2x3x4*", "18273645", Some(vec![0, 2, 4, 6]));
        check("*1x2x3x4*", "091E273645T8", Some(vec![2, 4, 6, 8]));
    }
}
