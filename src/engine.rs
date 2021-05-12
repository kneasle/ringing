use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    ops::Range,
};

use crate::set::Set;
use itertools::Itertools;
use proj_core::{Row, Stage};

/// They type of [`Set`] that will be used by [`Engine`].  Generally [`Vec`] outperforms a
/// [`HashSet`] when the sets are small (since we care more about the constant than the asymptotic
/// performance).  However, this is left as a simple switch in order to get the best performance
/// for all cases.
type _Set<T> = crate::set::VecSet<T>;

/* DEBUG PRINT SETTINGS */

const DBG_PRINT: bool = false;
const DBG_NODE_TABLE: bool = false;

/// Acts the same as `println!` if `DBG_PRINT = true`, but otherwise gets removed by the compiler
macro_rules! dbg_println {
    ($( $args: tt )*) => {
        if DBG_PRINT {
            println!($( $args )*);
        }
    };
}

/// Acts the same as `print!` if `DBG_PRINT = true`, but otherwise gets removed by the compiler
macro_rules! dbg_print {
    ($( $args: tt )*) => {
        if DBG_PRINT {
            print!($( $args )*);
        }
    };
}

/// Trait that describes the smallest atomic chunk of a composition.  This trait is used by the
/// [`Engine`] to customise generic tree search.
pub trait Section: Display + Debug + Copy + Eq + Hash {
    type Table: Debug;
    type Call: Copy + Debug + Display;

    /// The [`Stage`] of this `Section` type.  All the instances must share the same [`Stage`], so
    /// this has no `self` parameter.
    fn stage(table: &Self::Table) -> Stage;

    /// The first `Section` of any composition
    fn start() -> Self;

    /// Returns `true` if this section is the end of a composition
    fn is_end(node: &Node<Self>) -> bool;

    /// The first [`Node`] of any composition
    fn start_node(table: &Self::Table) -> Node<Self> {
        Node::new(Row::rounds(Self::stage(table)), Self::start())
    }

    /// Returns the number of [`Row`]s in a given `Section`
    fn length(self, table: &Self::Table) -> usize;

    /// Which other `Section`s are false against `(Row::rounds(_), self)`
    fn falseness(self, table: &Self::Table) -> &[(Row, Self)];

    /// Which `Section`s and transpositions are directly reachable from a given `Section`
    fn expand(self, table: &Self::Table) -> &[(Self::Call, Row, Self)];

    /// Build a composition out of these `Section`s
    fn compose(table: &Self::Table, desired_len: Range<usize>) {
        Engine::<Self>::new(table, desired_len).compose()
    }

    /// Write a list of calls in a human-readable format
    fn comp_string(calls: &[Self::Call]) -> String;
}

/// A single node of the composition - this is a [`Section`] (usually some part of the plain
/// course), along with a [`Row`] which describes which course the [`Section`] refers to.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Node<S> {
    pub row: Row,
    pub section: S,
}

impl<S> Node<S> {
    pub fn new(row: Row, section: S) -> Self {
        Node { row, section }
    }
}

impl<S: Section> Display for Node<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}|{})", self.section, self.row)
    }
}

/// All the persistent data required to generate a composition
#[derive(Debug, Clone)]
pub struct Engine<'t, S: Section> {
    nodes: _Set<Node<S>>,
    table: &'t S::Table,
    desired_len: Range<usize>,
    calls: Vec<S::Call>,
    nodes_considered: usize,
}

impl<'t, S: Section> Engine<'t, S> {
    fn new(table: &'t S::Table, desired_len: Range<usize>) -> Self {
        Engine {
            nodes: _Set::empty(),
            table,
            desired_len,
            calls: Vec::new(),
            nodes_considered: 0,
        }
    }

    fn compose(&mut self) {
        self.recursive_compose(S::start_node(&self.table), 0, 0);

        println!("{} nodes considered", self.nodes_considered);
    }

    fn recursive_compose(&mut self, node: Node<S>, len: usize, depth: usize) {
        dbg_print!(
            "Considering {}...",
            self.calls.iter().map(S::Call::to_string).join("")
        );

        self.nodes_considered += 1;

        /* CHECK THAT THE NEW NODE IS VALID */

        // Check if we've found a valid composition
        if S::is_end(&node) && self.desired_len.contains(&len) {
            println!("FOUND COMP! (len {}): {}", len, S::comp_string(&self.calls));
            return;
        }

        // Check if we've gone on too long
        if len >= self.desired_len.end {
            dbg_println!("Comp is too long!");
            return;
        }

        // Check whether this node is false against anything we've already rung
        for (r, section) in node.section.falseness(&self.table).iter() {
            if self
                .nodes
                .contains(&Node::new(unsafe { node.row.mul_unchecked(r) }, *section))
            {
                dbg_println!("False against {}: {:?}", r, section);
                return;
            }
        }

        dbg_println!("It isn't false!");

        // Print out the node table
        if DBG_NODE_TABLE {
            for n in &self.nodes {
                println!("{}|{}", n.section, n.row);
            }
        }

        /* IF THE NEW NODE IS VALID, ADD IT TO THE COMPOSITION AND EXPAND ITS BRANCH */

        // Now add this to the engine's comp
        self.nodes.add(node.clone());

        // Expand this in all possible ways
        for (name, transposition, section) in node.section.expand(&self.table) {
            // Add the new name to the composition string
            self.calls.push(*name);

            self.recursive_compose(
                Node::new(unsafe { node.row.mul_unchecked(transposition) }, *section),
                len + node.section.length(&self.table),
                depth + 1,
            );

            // Pop the call that we've explored
            self.calls.pop();
        }

        // Return the engine to the state before this node was added
        self.nodes.remove_last(&node);
    }
}
