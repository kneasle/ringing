use std::cmp::Ordering;

/// A container type which sorts its contents according to some given [`Distance`] metric
#[derive(Debug, Clone)]
pub struct Frontier<T> {
    pub item: T,
    pub distance: usize,
}

impl<T> PartialEq for Frontier<T> {
    fn eq(&self, other: &Self) -> bool {
        self.distance == other.distance
    }
}

impl<T> Eq for Frontier<T> {}

impl<T> PartialOrd for Frontier<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Frontier<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.distance.cmp(&other.distance)
    }
}
