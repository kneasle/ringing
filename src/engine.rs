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

pub trait Section: Copy + Eq + Hash {
    type Table;

    /// The [`Stage`] of this `Section` type.  All the instances must share the same [`Stage`], so
    /// this has no `self` parameter.
    fn stage(table: &Self::Table) -> Stage;

    /// The first `Section` of any composition
    fn start() -> Self;

    /// The first [`Node`] of any composition
    fn start_node(table: &Self::Table) -> Node<Self> {
        Node::new(Row::rounds(Self::stage(table)), Self::start())
    }

    /// Returns the number of [`Row`]s in a given `Section`
    fn length(self, table: &Self::Table) -> usize;

    /// Which other `Section`s are false against `(Row::rounds(_), self)`
    fn falseness(self, table: &Self::Table) -> &[(Row, Self)];

    /// Which `Section`s and transpositions are directly reachable from a given `Section`
    fn expand(self, table: &Self::Table) -> &[(String, Row, Self)];
}
