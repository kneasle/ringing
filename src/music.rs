use std::{
    fmt::{Display, Formatter},
    iter::once,
};

use itertools::Itertools;

use crate::{Bell, Row, Stage};

/// A single element of a [`Regex`] expression.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum RegexElem {
    /// One single specific [`Bell`].  Only matched by that exact [`Bell`].
    Bell(Bell),
    /// A single wild-card usually written as `x` or `.`.  Matches one of any [`Bell`].
    Any,
    /// A wild-card usually written as `*`.  Matches any number of any [`Bell`]s.
    Glob,
}

impl RegexElem {
    /// If this is a [`RegexElem::Bell`], this returns the contained [`Bell`], otherwise `None`
    #[inline]
    pub fn bell(self) -> Option<Bell> {
        match self {
            Self::Bell(b) => Some(b),
            _ => None,
        }
    }
}

impl Display for RegexElem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bell(b) => write!(f, "{}", b),
            Self::Any => write!(f, "x"),
            Self::Glob => write!(f, "*"),
        }
    }
}

/// A regex over sequences of [`Bell`]s (i.e. [`Row`]s).
///
/// Wild-cards in `Regex`es are normalised according to two rules:
/// - Consecutive globs are reduced to a single glob (because `**` is equivalent to `*`)
/// - If there is a sequence of consecutive 'any's and globs (e.g. `*x*x***`) then the 'any's
///   will always come first, followed by the globs (in this example `xx*****`).
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Regex {
    /// The pattern which represents this `Regex`.  This is normalised with the following
    /// invariants:
    /// - There are never more than two globs in a row (because `**` is equivalent to `*`)
    /// - If there is a sequence of consecutive 'any's and globs (e.g. `*x*`) then the 'any's
    ///   will always come first, followed by the globs.
    ///
    /// These two rules feed into each other, so `*x*x` would be normalised to `xx**` (second rule:
    /// reorder 'any's and globs) and then to `xx*` (first rule: compress consecutive globs).
    ///
    /// Therefore, after a glob we can only have either a bell or the end of the regex.
    elems: Vec<RegexElem>,
}

impl Regex {
    /// Parses a `Regex` from a string.  A [`Bell`] name matches only that [`Bell`], `'x'` matches
    /// precisely one of any [`Bell`] whereas `'*'` matches zero or more of any [`Bell`].  For
    /// example, `"xxxx5678" and `"*5678"` both correspond to 56s on 8 bells.
    pub fn parse(s: &str) -> Self {
        Self::from_elems(s.chars().filter_map(|c| match c {
            'x' | '.' => Some(RegexElem::Any),
            '*' => Some(RegexElem::Glob),
            _ => Bell::from_name(c).map(RegexElem::Bell),
        }))
    }

    /// Creates a normalised `Regex` from an [`Iterator`] of [`RegexElem`]s
    pub fn from_elems(iter: impl IntoIterator<Item = RegexElem>) -> Self {
        Self::from_vec(iter.into_iter().collect_vec())
    }

    /// Creates a `Regex` from an [`Iterator`] of [`RegexElem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// This is safe if the incoming [`Iterator`] satisfies the wildcard normalisation rules.
    pub unsafe fn from_elems_unchecked(iter: impl IntoIterator<Item = RegexElem>) -> Self {
        Self::from_vec_unchecked(iter.into_iter().collect_vec())
    }

    /// Creates a normalised `Regex` from a [`Vec`] of [`RegexElem`]s.
    #[inline(always)]
    pub fn from_vec(elems: Vec<RegexElem>) -> Self {
        // This unsafety is OK because the only operation we perform on the un-normalised Regex is
        // to normalise it (thus making it safe for other operations).
        let mut r = unsafe { Self::from_vec_unchecked(elems) };
        r.normalise();
        r
    }

    /// Creates a `Regex` from a [`Vec`] of [`RegexElem`]s, without normalising.
    ///
    /// # Safety
    ///
    /// This is safe if the [`Vec`] satisfies the wildcard normalisation rules.
    #[inline(always)]
    pub unsafe fn from_vec_unchecked(elems: Vec<RegexElem>) -> Self {
        Self { elems }
    }

    /// Normalises wildcards and globs in a `Regex`.  This should only in combination with `unsafe`
    /// - if only safe code is used, `self` will already be normalised.  This is the only operation
    /// on `Regex`es which doesn't rely on the input being normalised.
    pub fn normalise(&mut self) {
        let mut normalised_elems = Vec::<RegexElem>::with_capacity(self.elems.len());

        // Insert a rogue bell to the end `self.elems` so that all runs of consecutive
        // wildcards/globs end in a bell
        self.elems.push(RegexElem::Bell(Bell::TREBLE));
        // Split the elements into chunks of 0 or more wildcards/globs, each of which finishes with
        // a bell.
        for slice in self
            .elems
            .split_inclusive(|e| matches!(e, RegexElem::Bell(_)))
        {
            // The slice should contain at least one element (the bell that matched the pattern)
            assert!(!slice.is_empty());
            let wildcards_pattern = &slice[..slice.len() - 1];
            let bell_elem = slice[slice.len() - 1];
            // Normalise the wildcard pattern, and push it.  To normalise it, we move all the 'x's
            // to the start of the pattern and then add a single glob (if the pattern contains any
            // globs).
            let num_wildcards = wildcards_pattern
                .iter()
                .filter(|e| matches!(e, RegexElem::Any))
                .count();
            normalised_elems.extend(std::iter::repeat(RegexElem::Any).take(num_wildcards));
            if wildcards_pattern.contains(&RegexElem::Glob) {
                normalised_elems.push(RegexElem::Glob);
            }
            // Push the bell that terminates this wildcard pattern
            normalised_elems.push(bell_elem);
        }

        // Overwrite this regex with the normalised element list
        self.elems = normalised_elems;
        // Pop the rogue bell off the end of the pattern (we added it before the loop to make the
        // code cleaner).
        assert_eq!(self.elems.pop(), Some(RegexElem::Bell(Bell::TREBLE)));
    }

    /// Creates a set of `Regex`es which match runs of a given length off the front or back of a
    /// [`Row`].  If the run length is longer than the stage, then no `Regex`es are returned.
    pub fn runs_front_or_back(stage: Stage, len: usize) -> Vec<Self> {
        let num_bells = stage.as_usize();

        let mut runs = Vec::with_capacity(num_bells.saturating_sub(3) * 2);
        // Iterate over every bell which could start a run
        for i in 0..=num_bells - len {
            // An iterator that yields the bells forming this run in ascending order
            let run_iterator = (i..i + len).map(Bell::from_index).map(RegexElem::Bell);

            // This unsafety is OK because all of the input patterns must be normalised because
            // they contain only one wildcard
            unsafe {
                // Descending runs on the back (e.g. `*1234`)
                runs.push(Self::from_elems_unchecked(
                    once(RegexElem::Glob).chain(run_iterator.clone()),
                ));
                // Ascending runs on the back (e.g. `*4321`)
                runs.push(Self::from_elems_unchecked(
                    once(RegexElem::Glob).chain(run_iterator.clone().rev()),
                ));
                // Descending runs off the front (e.g. `1234*`)
                runs.push(Self::from_elems_unchecked(
                    run_iterator.clone().chain(once(RegexElem::Glob)),
                ));
                // Ascending runs on the back (e.g. `4321*`)
                runs.push(Self::from_elems_unchecked(
                    run_iterator.rev().chain(once(RegexElem::Glob)),
                ));
            }
        }

        runs
    }

    /// Returns `true` if a given [`Row`] satisfies this `Regex`, and `false` otherwise.  If the
    /// normalised `Regex` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    pub fn matches(&self, r: &Row) -> bool {
        // Run a match, ignoring the indices which are a match
        self.run_match(r, |_i| ())
    }

    /// Returns the **0-indexed** places of the [`Bell`]s which match this `Regex` (or `None` if
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

    /// Helper function to test a match between a [`Row`] and this `Regex`, calling some function
    /// every time a [`Bell`] is matched.
    ///
    /// If the normalised `Regex` contains `n` elements and the [`Row`] contains `m` [`Bell`]s, the
    /// runtime is `O(n + m)`.
    #[inline(always)]
    fn run_match(&self, r: &Row, mut match_fn: impl FnMut(usize)) -> bool {
        /* Note: This algorithm relies on the uniqueness of bells within rows, so cannot be used
         * on arbitrary iterators of bells */

        let mut bells = r.bell_iter().enumerate();
        let mut regex_elems = self.elems.iter().copied();

        // This loop must terminate because `regex_elems` gets shorter every iteration
        loop {
            // If we haven't run out of regex elements, then test the patterns
            match regex_elems.next() {
                // If the next element is a specific bell, then the next bell must be the same
                Some(RegexElem::Bell(exp_bell)) => {
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
                        // If the regex expected a bell but the row finished, then the row doesn't
                        // match
                        None => return false,
                    }
                }
                // If the regex is 'Any', then the row matches if it has more bells left
                Some(RegexElem::Any) => {
                    if bells.next().is_none() {
                        return false;
                    }
                }
                // If the next element is a glob, then we need to look ahead at the next expected
                // element
                Some(RegexElem::Glob) => match regex_elems.next() {
                    // We've encountered `<...>*b<...>` where b is a bell name.  To match this, we
                    // repeatedly consume bells until either:
                    // - we find a matching bell, in which case continue with the regex
                    // - we run out of bells, in which case the regex does not match because
                    Some(RegexElem::Bell(exp_bell)) => {
                        loop {
                            match bells.next() {
                                // If we find a matching bell, then report the match and continue
                                // reading the regex
                                Some((i, b)) => {
                                    if b == exp_bell {
                                        match_fn(i);
                                        break;
                                    }
                                }
                                // If we consume the entire row without finding the wanted bell,
                                // the regex doesn't match
                                None => return false,
                            }
                        }
                    }
                    // If the regex ends with a glob, then any extension of the row matches
                    None => return true,
                    // Because of normalisation, a glob cannot be followed by anything other than a
                    // bell (or the end of the regex).  This branch should only be reachable with
                    // `unsafe`
                    _ => unreachable!(),
                },
                // If we've run out of regex elements, then the row matches iff we also run out of
                // bells at the same time
                None => return bells.next().is_none(),
            }
        }
    }
}

impl Display for Regex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for elem in &self.elems {
            write!(f, "{}", elem)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::RowBuf;

    use super::Regex;

    #[test]
    fn regex_normalise() {
        #[track_caller]
        fn check(inp_str: &str, exp_str: &str) {
            println!("Testing {:?}", inp_str);
            assert_eq!(Regex::parse(inp_str).to_string(), exp_str);
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
    fn regex_is_match() {
        #[track_caller]
        fn check(regex_str: &str, row_str: &str, expected_match: bool) {
            println!("Testing row {} against regex {}", row_str, regex_str);

            let regex = Regex::parse(regex_str);
            let row = &RowBuf::parse(row_str).unwrap();
            let is_match = regex.matches(&row);

            match (expected_match, is_match) {
                (true, false) => {
                    panic!("Expected '{}' to match '{}', but it did not.", row, regex);
                }
                (false, true) => {
                    panic!("Expected '{}' not to match '{}', but it did.", row, regex);
                }
                // If the matches lined up, then the test passes
                _ => {}
            }
        }

        // Zero-sized row
        check("", "", true);
        check("*", "", true);
        check("123", "", false);
        check("x", "", false);
        check("x*", "", false);
        // Regex with only 'x's
        check("xxxx", "3412", true);
        check("xxxx", "4321", true);
        check("xxxx", "54321", false);
        // Regex with only 'x's and numbers
        check("x1xx", "2134", true);
        check("x1xx", "2134", true);
        check("x1xx", "4321", false);
        check("x1", "21345", false);
        // Regex with globs
        check("*1234", "1234", true);
        check("*1234", "51234", true);
        check("*1234", "123", false);
        check("*6578", "12346578", true);
        check("*65*78", "12346578", true);
        check("*65*78", "12653478", true);
        check("*65*78", "12657834", false);
        // Regex with everything
        check("x*12*34", "61257834", true);
        check("x*12*34", "12657834", false);
        check("x*12*34", "51234", true);
        check("x*12*34", "1234", false);
        check("x*12*34", "12534", false);
    }

    #[test]
    fn regex_match_pattern() {
        #[track_caller]
        fn check(regex_str: &str, row_str: &str, expected_pattern: Option<Vec<usize>>) {
            println!("Testing row {} against regex {}", row_str, regex_str);

            let regex = Regex::parse(regex_str);
            let row = &RowBuf::parse(row_str).unwrap();
            assert_eq!(regex.match_pattern(&row), expected_pattern);
        }

        // Zero-sized row
        check("", "", Some(vec![]));
        check("*", "", Some(vec![]));
        check("123", "", None);
        check("x", "", None);
        check("x*", "", None);
        // Regex with only 'x's
        check("xxxx", "3412", Some(vec![]));
        check("xxxx", "4321", Some(vec![]));
        check("xxxx", "54321", None);
        // Regex with only 'x's and numbers
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "2134", Some(vec![1]));
        check("x1xx", "4321", None);
        check("x1", "21345", None);
        // Regex with globs
        check("*1234", "1234", Some(vec![0, 1, 2, 3]));
        check("*1234", "51234", Some(vec![1, 2, 3, 4]));
        check("*1234", "123", None);
        check("*6578", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12346578", Some(vec![4, 5, 6, 7]));
        check("*65*78", "12653478", Some(vec![2, 3, 6, 7]));
        check("*65*78", "12657834", None);
        // Regex with everything
        check("x*12*34", "61257834", Some(vec![1, 2, 6, 7]));
        check("x*12*34", "12657834", None);
        check("x*12*34", "51234", Some(vec![1, 2, 3, 4]));
        check("x*12*34", "1234", None);
        check("x*12*34", "12534", None);
        check("*1x2x3x4*", "18273645", Some(vec![0, 2, 4, 6]));
        check("*1x2x3x4*", "091E273645T8", Some(vec![2, 4, 6, 8]));
    }
}
