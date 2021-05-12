use std::hash::Hash;

pub trait Set<T: PartialEq> {
    /// Produce an empty [`Set`]
    fn empty() -> Self;

    /// Does this [`Set`] contain a given value
    fn contains(&self, v: &T) -> bool;

    /// Add a new value to this [`Set`]
    fn add(&mut self, v: T);

    /// Removes a value from the set if it exists
    fn remove_last(&mut self, v: &T);
}

#[derive(Debug, Clone)]
pub struct VecSet<T> {
    vec: Vec<T>,
}

impl<T: PartialEq> Set<T> for VecSet<T> {
    #[inline(always)]
    fn empty() -> Self {
        VecSet { vec: Vec::new() }
    }

    #[inline(always)]
    fn contains(&self, v: &T) -> bool {
        self.vec.contains(v)
    }

    #[inline(always)]
    fn add(&mut self, v: T) {
        self.vec.push(v);
    }

    #[inline(always)]
    fn remove_last(&mut self, _v: &T) {
        self.vec.pop();
    }
}

impl<'a, T> IntoIterator for &'a VecSet<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

#[derive(Debug, Clone)]
pub struct HashSet<T> {
    set: std::collections::HashSet<T>,
}

impl<T: Eq + Hash> Set<T> for HashSet<T> {
    #[inline(always)]
    fn empty() -> Self {
        HashSet {
            set: std::collections::HashSet::new(),
        }
    }

    #[inline(always)]
    fn contains(&self, v: &T) -> bool {
        self.set.contains(v)
    }

    #[inline(always)]
    fn add(&mut self, v: T) {
        self.set.insert(v);
    }

    #[inline(always)]
    fn remove_last(&mut self, v: &T) {
        self.set.remove(v);
    }
}

impl<'a, T> IntoIterator for &'a HashSet<T> {
    type Item = &'a T;
    type IntoIter = std::collections::hash_set::Iter<'a, T>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.set.iter()
    }
}
