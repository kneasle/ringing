use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    ops::Range,
};

use proj_core::{Row, Stage};

const DBG_PRINT: bool = false;
const DBG_NODE_TABLE: bool = false;

pub trait Section: Display + Debug + Copy + Eq + Hash {
    type Table: Debug;

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
    fn expand(self, table: &Self::Table) -> &[(String, Row, Self)];

    fn compose(table: &Self::Table, desired_len: Range<usize>) {
        Engine::<Self>::new(table, desired_len).compose()
    }
}

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

#[derive(Debug, Clone)]
pub struct Engine<'t, S: Section> {
    nodes: Vec<Node<S>>,
    table: &'t S::Table,
    desired_len: Range<usize>,
    comp_string: String,
    nodes_expanded: usize,
}

impl<'t, S: Section> Engine<'t, S> {
    fn new(table: &'t S::Table, desired_len: Range<usize>) -> Self {
        Engine {
            nodes: Vec::new(),
            table,
            desired_len,
            comp_string: String::new(),
            nodes_expanded: 0,
        }
    }

    fn compose(&mut self) {
        self.recursive_compose(S::start_node(&self.table), 0, 0);

        println!("{} nodes expanded", self.nodes_expanded);
    }

    fn recursive_compose(&mut self, node: Node<S>, len: usize, depth: usize) {
        macro_rules! dbg_println {
            ($( $args: tt )*) => {
                if DBG_PRINT {
                    println!($( $args )*);
                }
            };
        }

        macro_rules! dbg_print {
            ($( $args: tt )*) => {
                if DBG_PRINT {
                    print!($( $args )*);
                }
            };
        }

        dbg_print!("Considering {}...", self.comp_string);

        self.nodes_expanded += 1;

        // Check if we've found a valid composition
        if S::is_end(&node) && self.desired_len.contains(&len) {
            println!("FOUND COMP! (len {}): {}", len, self.comp_string);
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

        // Now add this to the engine's comp
        self.nodes.push(node.clone());

        // Expand this in all possible ways
        for (name, transposition, section) in node.section.expand(&self.table) {
            // Add the new name to the composition string
            self.comp_string.push_str(name);

            self.recursive_compose(
                Node::new(unsafe { node.row.mul_unchecked(transposition) }, *section),
                len + node.section.length(&self.table),
                depth + 1,
            );

            // Revert the composition string
            self.comp_string
                .truncate(self.comp_string.len() - name.len());
        }

        // Return the engine to the state before this node was added
        self.nodes.pop();
    }
}
