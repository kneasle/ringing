use gcd::Gcd;
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
    pub fn new(item: T) -> Self {
        Self { item, distance: 0 }
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
