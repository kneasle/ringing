use std::{
    fmt::{Debug, Display, Formatter},
    iter::Sum,
    ops::{Add, AddAssign, Div, Mul, MulAssign},
    sync::atomic::{AtomicIsize, Ordering},
};

/// Bit index of the radix point in a [`Score`].  This is exactly half-way through the number,
/// trading off evenly between resolution (how close can two [`Score`]s get be before they are
/// indistinguishable) and range (how big can [`Score`]s get before becoming unrepresentable).
const RADIX_POINT_IDX: usize = std::mem::size_of::<isize>() * 8 / 2;

/// The representation of music score used by Monument.  These are fixed point numbers, meaning
/// that operations compile to integer arithmetic but Monument can still provide fractional scores
/// (which are good for user experience).  Also, unlike floating point numbers, these can soundly
/// implement [`Ord`], [`Eq`] and [`Hash`].
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct Score {
    value: isize,
}

impl Score {
    pub const ZERO: Score = Self::from_isize_truncating(0);
    pub const ONE: Score = Self::from_isize_truncating(1);

    pub const MIN: Score = Score { value: isize::MIN };
    pub const MAX: Score = Score { value: isize::MAX };

    /// Creates a new [`Score`] from an [`isize`], truncating it if it doesn't fit.
    #[inline(always)]
    const fn from_isize_truncating(v: isize) -> Self {
        Self {
            value: v << RADIX_POINT_IDX,
        }
    }

    /// Creates a `Score` from an integer representing a fraction over (2^32).
    pub fn from_numerator(value: isize) -> Self {
        Self { value }
    }

    /// Converts a `Score` to an integer representing a fraction over (2^32).
    pub fn to_radix(self) -> isize {
        self.value
    }
}

/* ===== FORMATTING ===== */

impl Debug for Score {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Score({})", self)
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", f32::from(*self))
    }
}

/* ===== CONVERSIONS ===== */

impl From<f64> for Score {
    #[inline(always)]
    fn from(v: f64) -> Self {
        Self::from_numerator((Self::ONE.value as f64 * v) as isize)
    }
}

impl From<Score> for f64 {
    #[inline(always)]
    fn from(v: Score) -> f64 {
        v.value as f64 / Score::ONE.value as f64
    }
}

impl From<f32> for Score {
    #[inline(always)]
    fn from(v: f32) -> Self {
        Self::from_numerator((Self::ONE.value as f32 * v) as isize)
    }
}

impl From<Score> for f32 {
    #[inline(always)]
    fn from(v: Score) -> f32 {
        v.value as f32 / Score::ONE.value as f32
    }
}

impl From<isize> for Score {
    #[inline(always)]
    fn from(v: isize) -> Self {
        Self::from_isize_truncating(v)
    }
}

/* ===== ARITHMETIC ===== */

impl Add for Score {
    type Output = Score;

    /// Add two scores together
    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Self::from_numerator(self.to_radix() + rhs.to_radix())
    }
}

impl AddAssign for Score {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.value += rhs.value;
    }
}

impl Mul<usize> for Score {
    type Output = Score;

    #[inline(always)]
    fn mul(self, rhs: usize) -> Self::Output {
        Self::from_numerator(self.to_radix() * rhs as isize)
    }
}

impl Mul<Score> for usize {
    type Output = Score;

    #[inline(always)]
    fn mul(self, rhs: Score) -> Self::Output {
        Score::from_numerator(rhs.to_radix() * self as isize)
    }
}

impl MulAssign<usize> for Score {
    #[inline(always)]
    fn mul_assign(&mut self, rhs: usize) {
        self.value *= rhs as isize;
    }
}

impl Div<usize> for Score {
    type Output = Score;

    #[inline(always)]
    fn div(self, rhs: usize) -> Self::Output {
        Score::from_numerator(self.to_radix() / rhs as isize)
    }
}

impl Sum for Score {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut accum = Self::ZERO;
        for score in iter {
            accum += score;
        }
        accum
    }
}

/// An atomic variable containing a single [`Score`] value which always uses
/// [`Relaxed`](Ordering::Relaxed) memory ordering.
#[derive(Debug)]
pub struct AtomicScore {
    value: AtomicIsize,
}

impl AtomicScore {
    /// Creates a new `AtomicScore` containing a given [`Score`]
    pub fn new(score: Score) -> Self {
        AtomicScore {
            value: AtomicIsize::new(score.value),
        }
    }

    /// Sets a value which will eventually be seen by the other threads
    pub fn set(&self, score: Score) {
        self.value.store(score.value, Ordering::Relaxed)
    }

    /// Sets a value which will eventually be seen by the other threads
    pub fn get(&self) -> Score {
        Score::from_numerator(self.value.load(Ordering::Relaxed))
    }
}

#[cfg(test)]
mod tests {
    use super::Score;

    #[test]
    fn conversions() {
        assert_eq!(f32::from(Score::from(0f32)), 0f32);
        assert_eq!(f32::from(Score::from(1f32)), 1f32);
    }
}
