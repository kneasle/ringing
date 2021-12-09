use std::ops::{Add, AddAssign, Mul, Range};

use itertools::Itertools;

/// A collection of counts of something (usually rows of each method in spliced).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RowCounts {
    counts: Vec<usize>,
}

impl RowCounts {
    pub fn zero(num_counts: usize) -> Self {
        RowCounts {
            counts: vec![0; num_counts],
        }
    }

    pub fn single_count(count: usize, idx: usize, num_counts: usize) -> Self {
        let mut cnts = Self::zero(num_counts);
        cnts.counts[idx] += count;
        cnts
    }

    pub fn counts(&self) -> &[usize] {
        &self.counts
    }

    /// `true` if it's possible to get every count into `target_range` within at most
    /// `max_rows_left` rows.
    pub fn is_feasible(&self, max_rows_left: usize, target_range: Range<usize>) -> bool {
        let mut rows_required = 0;
        for &c in &self.counts {
            // If one of the counts is already too large then we'll never get all the counts to be
            // contained in the target range
            if c >= target_range.end {
                return false;
            }
            rows_required += target_range.start.saturating_sub(c);
        }
        rows_required <= max_rows_left
    }
}

impl Add for &RowCounts {
    type Output = RowCounts;

    fn add(self, other: &RowCounts) -> RowCounts {
        RowCounts {
            counts: self
                .counts
                .iter()
                .zip_eq(&other.counts)
                .map(|(x, y)| x + y)
                .collect_vec(),
        }
    }
}

impl AddAssign<&RowCounts> for RowCounts {
    fn add_assign(&mut self, rhs: &RowCounts) {
        self.counts
            .iter_mut()
            .zip_eq(&rhs.counts)
            .for_each(|(lhs, inc)| *lhs += *inc);
    }
}

impl Mul<usize> for &RowCounts {
    type Output = RowCounts;

    fn mul(self, multiplier: usize) -> Self::Output {
        RowCounts {
            counts: self
                .counts
                .iter()
                .map(|&count| count * multiplier)
                .collect_vec(),
        }
    }
}
