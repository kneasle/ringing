use std::ops::{Add, AddAssign, Index, IndexMut, Mul, RangeInclusive};

use itertools::Itertools;

use super::TotalLength;

/// A collection of counts of something, usually instances of music types or rows of a given
/// method.  Addition/subtraction is performed element-wise.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Counts(Vec<usize>);

impl Counts {
    /// Creates a set of `Counts` where every count is 0.
    pub fn zeros(len: usize) -> Self {
        Self(vec![0; len])
    }

    /// Creates a set of `Counts` where every count is 0, except for one.
    pub fn single_count(count: usize, idx: usize, num_counts: usize) -> Self {
        let mut cnts = Self::zeros(num_counts);
        cnts[idx] += count;
        cnts
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the underlying count of slices
    pub fn as_slice(&self) -> &[usize] {
        &self.0
    }

    pub fn iter(&self) -> std::slice::Iter<usize> {
        self.0.iter()
    }

    pub fn saturating_sub(&self, other: &Self) -> Self {
        Self(
            self.0
                .iter()
                .zip_eq(&other.0)
                .map(|(a, b)| a.saturating_sub(*b))
                .collect_vec(),
        )
    }

    pub fn saturating_sub_assign(&mut self, other: &Self) {
        for (v, dec) in self.0.iter_mut().zip_eq(other.0.iter()) {
            *v -= *dec;
        }
    }

    /// Determine the feasibility of getting every count within `target_range`, whilst distributing
    /// at most `max_count_left` counts.  This is used for pruning on method splices.
    pub(crate) fn is_feasible(
        &self,
        max_count_left: usize,
        target_ranges: &[RangeInclusive<TotalLength>],
    ) -> bool {
        let mut rows_required = 0;
        for (&c, range) in self.0.iter().zip_eq(target_ranges) {
            // If one of the counts is already too large then we'll never get all the counts to be
            // contained in the target range
            if c > range.end().as_usize() {
                return false;
            }
            rows_required += range.start().as_usize().saturating_sub(c);
        }
        rows_required <= max_count_left
    }
}

impl Add for &Counts {
    type Output = Counts;

    fn add(self, other: &Counts) -> Counts {
        Counts(
            self.0
                .iter()
                .zip_eq(&other.0)
                .map(|(x, y)| x + y)
                .collect_vec(),
        )
    }
}

impl AddAssign<&Counts> for Counts {
    fn add_assign(&mut self, rhs: &Counts) {
        self.0
            .iter_mut()
            .zip_eq(&rhs.0)
            .for_each(|(lhs, inc)| *lhs += *inc);
    }
}

impl Mul<usize> for &Counts {
    type Output = Counts;

    fn mul(self, multiplier: usize) -> Self::Output {
        Counts(self.0.iter().map(|&count| count * multiplier).collect_vec())
    }
}

impl Index<usize> for Counts {
    type Output = usize;

    fn index(&self, idx: usize) -> &usize {
        &self.0[idx]
    }
}

impl IndexMut<usize> for Counts {
    fn index_mut(&mut self, idx: usize) -> &mut usize {
        &mut self.0[idx]
    }
}

impl From<Vec<usize>> for Counts {
    fn from(vs: Vec<usize>) -> Self {
        Counts(vs)
    }
}
