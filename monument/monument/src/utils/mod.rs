use gcd::Gcd;
use serde::Deserialize;
use std::cmp::Ordering;

mod row_counts;

pub use row_counts::RowCounts;

/// Returns a bitmap where there's a `1` for every number that's co-prime to `n`
pub fn coprime_bitmap(n: u16) -> u64 {
    assert!(n <= 64);
    assert!(n > 0);
    if n == 1 {
        return 1; // Finishing a part in rounds is only allowed in a 1-part
    }
    let mut mask = 0;
    for i in 1..n {
        if i.gcd(n) == 1 {
            mask |= 1 << i;
        }
    }
    mask
}

/// A container type which sorts its contents according to some given [`Distance`] metric
#[derive(Debug, Clone)]
pub struct FrontierItem<T> {
    pub item: T,
    pub distance: usize,
}

impl<T> FrontierItem<T> {
    pub fn new(item: T, distance: usize) -> Self {
        Self { item, distance }
    }
}

impl<T> PartialEq for FrontierItem<T> {
    fn eq(&self, other: &Self) -> bool {
        self.distance == other.distance
    }
}

impl<T> Eq for FrontierItem<T> {}

impl<T> PartialOrd for FrontierItem<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for FrontierItem<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.distance.cmp(&other.distance)
    }
}

/// An inclusive range where each side is optionally bounded.  This is essentially a combination of
/// [`RangeInclusive`](std::ops::RangeInclusive) (`min..=max`),
/// [`RangeToInclusive`](std::ops::RangeToInclusive) (`..=max`),
/// [`RangeFrom`](std::ops::RangeFrom) (`min..`) and
/// [`RangeFull`](std::ops::RangeFull) (`..`).
#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OptRange {
    pub min: Option<usize>,
    pub max: Option<usize>,
}

impl OptRange {
    /// Returns `true` if at least one of `min` or `max` is set
    pub fn is_set(&self) -> bool {
        self.min.is_some() || self.max.is_some()
    }
}
