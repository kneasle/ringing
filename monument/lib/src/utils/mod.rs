use std::{
    marker::PhantomData,
    sync::atomic::{AtomicU32, Ordering},
};

pub(crate) mod counts;
pub(crate) mod lengths;

pub use lengths::{PerPartLength, TotalLength};

/// A container type which sorts its contents according to some given distance metric
#[derive(Debug, Clone)]
pub(crate) struct FrontierItem<Item, Dist> {
    pub item: Item,
    pub distance: Dist,
}

impl<Item, Dist> FrontierItem<Item, Dist> {
    pub fn new(item: Item, distance: Dist) -> Self {
        Self { item, distance }
    }
}

impl<Item, Dist: PartialEq> PartialEq for FrontierItem<Item, Dist> {
    fn eq(&self, other: &Self) -> bool {
        self.distance == other.distance
    }
}

impl<Item, Dist: Eq> Eq for FrontierItem<Item, Dist> {}

impl<Item, Dist: Ord> PartialOrd for FrontierItem<Item, Dist> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Item, Dist: Ord> Ord for FrontierItem<Item, Dist> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.distance.cmp(&other.distance)
    }
}

/// Struct which generates unique `Id`s.  Can be used with any of
/// [`MethodId`](crate::parameters::MethodId) or [`CallId`](crate::parameters::CallId).
#[derive(Debug)]
pub struct IdGenerator<Id> {
    next_id: AtomicU32,
    _id: PhantomData<Id>,
}

impl<Id: From<u32> + Into<u32>> IdGenerator<Id> {
    pub fn starting_at_zero() -> Self {
        Self {
            next_id: AtomicU32::new(0),
            _id: PhantomData,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&self) -> Id {
        Id::from(self.next_id.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Boundary {
    Start,
    End,
}

/// Integer division, but rounding up instead of down
pub(crate) fn div_rounding_up(lhs: usize, rhs: usize) -> usize {
    (lhs + rhs - 1) / rhs
}
