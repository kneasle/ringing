use std::fmt::{Debug, Display, Formatter};

use bellframe::{Bell, Row, Stage};
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
}

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
