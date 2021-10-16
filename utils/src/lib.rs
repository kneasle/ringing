use std::cmp::Ordering;

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
