//! This module will eventually make its way into [`bellframe`]

use std::{
    fmt::{Debug, Display, Formatter},
    ops::Mul,
};

use bellframe::{
    music::{Regex, RegexElem},
    Bell, Row, RowBuf, Stage,
};
use itertools::Itertools;

/// A mask which fixes the location of some [`Bell`]s.  Unfilled positions are usually denoted by
/// `'x'` (`X` is not a valid [`Bell`] name).
///
/// This can also be thought of as a [`Regex`] with no `*`s.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Mask {
    bells: Vec<Option<Bell>>,
}

impl Mask {
    pub fn parse(s: &str) -> Self {
        Self {
            bells: s
                .chars()
                .filter_map(|c| match c {
                    'x' | 'X' | '.' => Some(None),
                    // Return `Some(Some(Bell))` if `other_char` is a bell name, otherwise `None`
                    // to ignore random chars
                    other_char => Bell::from_name(other_char).map(Some),
                })
                .collect_vec(),
        }

        // TODO: Check validity
    }

    /// Creates a `Mask` that fully specifies a given [`Row`]
    pub fn full_row(row: &Row) -> Self {
        Self {
            bells: row.bell_iter().map(Some).collect_vec(),
        }
    }

    /// Creates a `Mask` that fixes the given [`Bell`]s into their corresponding 'home' place.
    pub fn fix_bells(stage: Stage, fixed_bells: impl IntoIterator<Item = Bell>) -> Self {
        let mut bells: Vec<Option<Bell>> = vec![None; stage.as_usize()];
        for b in fixed_bells {
            bells[b.index()] = Some(b);
        }
        Self { bells }
    }

    /// Returns the [`Stage`] of [`Row`] that this `Mask` matches
    #[inline(always)]
    pub fn stage(&self) -> Stage {
        Stage::from(self.bells.len())
    }

    /// Tests whether or not a [`Row`] satisfies this `Mask`.
    pub fn matches(&self, row: &Row) -> bool {
        // Rows can't match masks of different stages
        if self.stage() != row.stage() {
            return false;
        }

        for (&expected_bell, real_bell) in self.bells.iter().zip_eq(row.bell_iter()) {
            if let Some(b) = expected_bell {
                if b != real_bell {
                    // If the mask specifically requested a different bell in this location, then
                    // the rows doesn't match
                    return false;
                }
            }
        }
        true
    }

    /// If this mask matches exactly one [`Row`], then return that [`Row`] (otherwise `None`).
    pub fn as_row(&self) -> Option<RowBuf> {
        if self.bells.iter().all(Option::is_some) {
            // This unsafety is OK because we assert an invariant that masks are subsets of rows
            // (and thus a complete mask satisfies all the invariants of rows).
            Some(unsafe { RowBuf::from_bell_iter_unchecked(self.bells.iter().map(|b| b.unwrap())) })
        } else {
            None
        }
    }

    /// Check if there exist any [`Row`]s which can satisfy both `Mask`s.  `a.is_compatible_with(b)`
    /// equivalent to (but faster than) `a.combine(b).is_some()`.
    pub fn is_compatible_with(&self, other: &Mask) -> bool {
        // Masks of different stages are always incompatible
        if self.stage() != other.stage() {
            return false;
        }

        // Now iterate over `other`'s bells and make sure that, for each specified bell
        // 1. `self` doesn't require a different bell to be in that place
        // 2. `self` doesn't require that bell to be in a different place
        for (i, (&maybe_bell_other, &maybe_bell_self)) in
            other.bells.iter().zip_eq(&self.bells).enumerate()
        {
            if let Some(b_other) = maybe_bell_other {
                // Check that `self` doesn't requires a different bell in this place
                if !maybe_bell_self.map_or(true, |b_self| b_self == b_other) {
                    return false;
                }
                // Check that `self` doesn't require this bell in a different place
                if !self
                    .bells
                    .iter()
                    .position(|&b| b == Some(b_other))
                    .map_or(true, |idx_self| i == idx_self)
                {
                    return false;
                }
            }
        }

        // If no disagreement was found, the masks are compatible
        true
    }

    /// Creates a new `Mask` which matches precisely the [`Row`]s matched by both `self` _and_
    /// `other`.  If `self` and `other` aren't [compatible](Self::is_compatible_with), then such a
    /// `Mask` cannot exist and `None` is returned.
    pub fn combine(&self, other: &Mask) -> Option<Mask> {
        if !self.is_compatible_with(other) {
            return None;
        }

        Some(Self {
            bells: self
                .bells
                .iter()
                .zip_eq(&other.bells)
                .map(|maybe_bells| match maybe_bells {
                    (Some(b1), Some(b2)) => {
                        assert_eq!(b1, b2);
                        Some(*b1)
                    }
                    (Some(b1), None) => Some(*b1),
                    (None, maybe_bell) => *maybe_bell,
                })
                .collect_vec(),
        })
    }
}

/* ===== FORMATTING ===== */

impl Debug for Mask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Mask({})", self)
    }
}

impl Display for Mask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for maybe_bell in &self.bells {
            match maybe_bell {
                Some(b) => write!(f, "{}", b)?,
                None => write!(f, "x")?,
            }
        }
        Ok(())
    }
}

/* ===== ARITHMETIC ===== */

impl Mul<&Row> for &Mask {
    type Output = Mask;

    /// Use a [`Row`] to permute the required [`Bell`]s in a [`Mask`].  Mathematically, if `r` is a
    /// [`Row`] and `m` is a [`Mask`] and `m` matches some [`Row`] `s`, then `m * r` matches `s *
    /// r`.
    ///
    /// # Panics
    ///
    /// Panics if the [`Stage`]s of the [`Row`] and [`Mask`] don't match.
    fn mul(self, rhs: &Row) -> Self::Output {
        assert_eq!(self.stage(), rhs.stage());
        Mask {
            bells: rhs.bell_iter().map(|b| self.bells[b.index()]).collect_vec(),
        }
    }
}

impl Mul<&Mask> for &Row {
    type Output = Mask;

    /// Use a [`Row`] to transfigure the required [`Bell`]s in a [`Mask`].  Mathematically, if `r`
    /// is a [`Row`] and `m` is a [`Mask`] and `m` matches some [`Row`] `s`, then `r * m` matches
    /// `r * s`.
    ///
    /// # Panics
    ///
    /// Panics if the [`Stage`]s of the [`Row`] and [`Mask`] don't match.
    fn mul(self, rhs: &Mask) -> Self::Output {
        assert_eq!(self.stage(), rhs.stage());
        Mask {
            bells: rhs
                .bells
                .iter()
                .map(|maybe_bell| maybe_bell.map(|b| self[b.index()]))
                .collect_vec(),
        }
    }
}

/* ===== CONVERSIONS ===== */

impl From<Mask> for Regex {
    fn from(mask: Mask) -> Regex {
        Regex::from_elems(
            mask.bells
                .iter()
                .map(|b| b.map_or(RegexElem::Any, RegexElem::Bell)),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches() {
        fn check(mask: &str, row: &str, exp_match: bool) {
            let is_match = Mask::parse(mask).matches(&RowBuf::parse(row).unwrap());
            match (is_match, exp_match) {
                (true, false) => panic!("'{}' unexpectedly matched '{}'", mask, row),
                (false, true) => panic!("'{}' unexpectedly didn't match '{}'", row, mask),
                _ => {}
            }
        }

        check("1xx45", "12345", true);
        check("x", "1", true);
        check("1", "1", true);
        check("123456", "123456", true);
        check("123456", "123465", false);
        check("123456", "1234567", false);
        check("x1xx56", "123456", false);
        check("x1xx56", "214356", true);
        check("x1xx56", "241356", false);
    }

    #[test]
    fn row_mul_mask() {
        fn check_ok(row: &str, mask: &str, exp_mask_str: &str) {
            let new_mask = RowBuf::parse(row).unwrap().as_row() * &Mask::parse(mask);
            let exp_mask = Mask::parse(exp_mask_str);
            assert_eq!(
                new_mask, exp_mask,
                "{} * {} gave {} (expected {})",
                row, mask, new_mask, exp_mask_str
            );
        }

        check_ok("12345", "1xx45", "1xx45");
        check_ok("32154", "1xx45", "3xx54");
        check_ok("67812345", "xxxx6578", "xxxx3245");
    }

    #[test]
    fn mask_mul_row() {
        fn check_ok(mask: &str, row: &str, exp_mask_str: &str) {
            let new_mask = Mask::parse(mask).mul(&RowBuf::parse(row).unwrap());
            let exp_mask = Mask::parse(exp_mask_str);
            assert_eq!(
                new_mask, exp_mask,
                "{} * {} gave {} (expected {})",
                mask, row, new_mask, exp_mask_str
            );
        }

        check_ok("1xx45", "12345", "1xx45");
        check_ok("1xx45", "32154", "xx154");
        check_ok("xxxx6578", "67812345", "578xxxx6");
    }
}
