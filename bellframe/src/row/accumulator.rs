use std::ops::MulAssign;

use crate::{IncompatibleStages, Row, RowBuf, Stage};

/// A struct which lets you easily multiply many rows together, whilst reusing the same
/// allocations.
#[derive(Debug, Clone)]
pub struct RowAccumulator {
    total: RowBuf,
    temp_row: RowBuf,
}

impl RowAccumulator {
    /// Creates a `RowAccumulator` with an accumulated value of [rounds](RowBuf::rounds)
    #[inline]
    pub fn rounds(stage: Stage) -> Self {
        Self::new(RowBuf::rounds(stage))
    }

    /// Creates a `RowAccumulator` with a given accumulated value
    #[inline]
    pub fn new(total: RowBuf) -> Self {
        Self {
            temp_row: RowBuf::rounds(total.stage()),
            total,
        }
    }

    /// Performs `self = self * row`
    #[inline]
    pub fn accumulate(&mut self, row: &Row) -> Result<(), IncompatibleStages> {
        self.total.mul_into(row, &mut self.temp_row)?; // Multiply `total` into `temp_row`
        std::mem::swap(&mut self.total, &mut self.temp_row); // Swap the result back into `total`
        Ok(())
    }

    /// Performs `self = row * self`
    #[inline]
    pub fn pre_accumulate(&mut self, row: &Row) -> Result<(), IncompatibleStages> {
        row.mul_into(&self.total, &mut self.temp_row)?; // Multiply `total` into `temp_row`
        std::mem::swap(&mut self.total, &mut self.temp_row); // Swap the result back into `total`
        Ok(())
    }

    /// Sets the accumulated value of `self`
    #[inline]
    pub fn set(&mut self, row: &Row) {
        row.copy_into(&mut self.total);
    }

    /// Gets the accumulated value of `self`
    #[inline]
    pub fn total(&self) -> &Row {
        &self.total
    }

    /// Gets the accumulated value, consuming `self`
    #[inline]
    pub fn into_total(self) -> RowBuf {
        self.total
    }
}

impl MulAssign<&Row> for RowAccumulator {
    #[inline]
    fn mul_assign(&mut self, rhs: &Row) {
        self.accumulate(rhs).unwrap()
    }
}

impl MulAssign<&RowBuf> for RowAccumulator {
    #[inline]
    fn mul_assign(&mut self, rhs: &RowBuf) {
        self.accumulate(rhs).unwrap()
    }
}
