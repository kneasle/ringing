use std::hash::Hash;

use proj_core::{Row, Stage};

pub struct Node<S> {
    pub row: Row,
    pub section: S,
}

impl<S> Node<S> {
    pub fn new(row: Row, section: S) -> Self {
        Node { row, section }
    }
}

pub trait Section: Copy + Clone + Eq + Hash {
    type Table;

    /// The [`Stage`] of this `Section` type.  All the instances must share the same [`Stage`], so
    /// this has no `self` parameter.
    fn stage() -> Stage;

    /// The first `Section` of any composition
    fn start() -> Self;

    /// The first [`Node`] of any composition
    fn start_node() -> Node<Self> {
        Node::new(Row::rounds(Self::stage()), Self::start())
    }

    /// Returns the number of [`Row`]s in a given `Section`
    fn length(&self) -> usize;

    /// Which other `Section`s are false against `(Row::rounds(_), self)`
    fn falseness<'t>(&self, tables: &'t Self::Table) -> &'t [(Row, Self)];
}
