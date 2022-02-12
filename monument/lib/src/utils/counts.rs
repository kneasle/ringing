use std::ops::{Add, AddAssign, Index, IndexMut, Mul, Range};

use itertools::Itertools;

/// A collection of counts of something, usually instances of music types or rows of a given
/// method.  Addition/subtraction is performed element-wise.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Counts(Vec<usize>);

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

    /// Returns the underlying count of slices
    pub fn counts(&self) -> &[usize] {
        &self.0
    }

    /// Determine the feasibility of getting every count within `target_range`, whilst distributing
    /// at most `max_count_left` counts.  This is used for pruning on method splices.
    pub fn is_feasible(&self, max_count_left: usize, target_range: Range<usize>) -> bool {
        let mut rows_required = 0;
        for &c in &self.0 {
            // If one of the counts is already too large then we'll never get all the counts to be
            // contained in the target range
            if c >= target_range.end {
                return false;
            }
            rows_required += target_range.start.saturating_sub(c);
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
