use std::{hash::Hash, marker::PhantomData};

use proj_core::RowTrait;

use crate::engine::{Node, Section};

/// A trait for a generic set of nodes.  Used to easily switch out implementations to compare
/// performance.
pub trait NodeSet<R, S> {
    /// Produce an empty [`Set`], given the number of different section types
    fn empty(num_sections: usize) -> Self;

    /// Does this [`Set`] contain a given value
    fn contains(&self, v: &Node<R, S>) -> bool;

    /// Add a new value to this [`Set`]
    fn add(&mut self, v: Node<R, S>);

    /// Removes a value from the set if it exists
    fn remove_last(&mut self, v: &Node<R, S>);
}

/// A [`NodeSet`] which stores its values in a one unsorted [`Vec`] per [`Section`].  This has
/// linear asymptotic complexity in the number of nodes of each [`Section`], but that's OK because
/// we don't usually store many nodes.  Therefore, we care much more about the constant term than
/// the overall complexity.  It also relies on the sections being mappable into a fixed range of
/// integers.
#[derive(Debug, Clone)]
pub struct SplitVecSet<R, S> {
    rows_by_segment: Vec<Vec<R>>,
    _phantom: PhantomData<S>,
}

impl<R: RowTrait, S: Section<R> + Into<usize>> NodeSet<R, S> for SplitVecSet<R, S> {
    #[inline(always)]
    fn empty(num_sections: usize) -> Self {
        SplitVecSet {
            rows_by_segment: vec![Vec::new(); num_sections],
            _phantom: PhantomData,
        }
    }

    #[inline(always)]
    fn contains(&self, node: &Node<R, S>) -> bool {
        self.rows_by_segment[node.section.into()].contains(&node.row)
    }

    #[inline(always)]
    fn add(&mut self, node: Node<R, S>) {
        self.rows_by_segment[node.section.into()].push(node.row);
    }

    #[inline(always)]
    fn remove_last(&mut self, v: &Node<R, S>) {
        self.rows_by_segment[v.section.into()].pop();
    }
}

impl<'a, R, S> IntoIterator for &'a SplitVecSet<R, S> {
    type Item = &'a Node<R, S>;
    type IntoIter = std::slice::Iter<'a, Node<R, S>>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        unimplemented!();
    }
}

/// A [`NodeSet`] which stores its values in an unsorted [`Vec`].  This has linear asymptotic
/// complexity, but that's OK because we don't usually store many nodes.  Therefore, we care much
/// more about the constant term than the overall complexity.
#[derive(Debug, Clone)]
pub struct VecSet<R, S> {
    vec: Vec<Node<R, S>>,
}

impl<R, S> NodeSet<R, S> for VecSet<R, S>
where
    R: Eq,
    S: Eq,
{
    #[inline(always)]
    fn empty(_num_sections: usize) -> Self {
        VecSet { vec: Vec::new() }
    }

    #[inline(always)]
    fn contains(&self, v: &Node<R, S>) -> bool {
        self.vec.contains(v)
    }

    #[inline(always)]
    fn add(&mut self, v: Node<R, S>) {
        self.vec.push(v);
    }

    #[inline(always)]
    fn remove_last(&mut self, _v: &Node<R, S>) {
        self.vec.pop();
    }
}

impl<'a, R, S> IntoIterator for &'a VecSet<R, S> {
    type Item = &'a Node<R, S>;
    type IntoIter = std::slice::Iter<'a, Node<R, S>>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

/// A [`NodeSet`] which stores its values in the standard library's
/// [`HashSet`](std::collections::HashSet).  This is sometimes faster than using [`Vec`]s.
#[derive(Debug, Clone)]
pub struct HashSet<R, S> {
    set: std::collections::HashSet<Node<R, S>>,
}

impl<R, S> NodeSet<R, S> for HashSet<R, S>
where
    R: Eq + Hash,
    S: Eq + Hash,
{
    #[inline(always)]
    fn empty(_num_sections: usize) -> Self {
        HashSet {
            set: std::collections::HashSet::new(),
        }
    }

    #[inline(always)]
    fn contains(&self, v: &Node<R, S>) -> bool {
        self.set.contains(v)
    }

    #[inline(always)]
    fn add(&mut self, v: Node<R, S>) {
        self.set.insert(v);
    }

    #[inline(always)]
    fn remove_last(&mut self, v: &Node<R, S>) {
        self.set.remove(v);
    }
}

impl<'a, R, S> IntoIterator for &'a HashSet<R, S> {
    type Item = &'a Node<R, S>;
    type IntoIter = std::collections::hash_set::Iter<'a, Node<R, S>>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.set.iter()
    }
}
