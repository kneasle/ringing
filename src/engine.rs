use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    ops::{Range, RangeInclusive},
};

use crate::set::NodeSet;
use itertools::Itertools;
use proj_core::{Row, RowTrait, SimdRow, Stage};
use shortlist::Shortlist;

/// They type of [`Set`] that will be used by [`Engine`].  Generally [`Vec`] outperforms a
/// [`HashSet`] when the sets are small (since we care more about the constant than the asymptotic
/// performance).  However, this is left as a simple switch in order to get the best performance
/// for all cases.
type _Set<R, S> = crate::set::SplitVecSet<R, S>;

/* DEBUG PRINT SETTINGS */

const DBG_PRINT: bool = false;
const DBG_NODE_TABLE: bool = false;
const PRINT_COMPS: bool = false;

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

/// Trait bound for what types can be used as course heads in a composing engine.  It is
/// implemented for [`Row`] and [`SimdRow`].
pub trait CompRow: RowTrait {
    /// Pack bytes representing the [`Bell`]s of this `Row` into a 128 bit number, panicking if the
    /// [`Stage`] as greater than 16.
    fn pack_u128(&self) -> u128;
}

impl CompRow for Row {
    fn pack_u128(&self) -> u128 {
        let mut bs = 0u128;
        for (i, b) in self.bell_iter().enumerate() {
            bs |= (b.index() as u128) << (i * 8);
        }
        bs
    }
}

impl CompRow for SimdRow {
    #[inline(always)]
    fn pack_u128(&self) -> u128 {
        u128::from(self.to_m128i())
    }
}

/// Trait that describes the smallest atomic chunk of a composition.  This trait is used by the
/// [`Engine`] to customise generic tree search.
pub trait Table<R: CompRow>: Debug {
    type Section: Into<usize> + Display + Debug + Copy + Eq + Hash;
    type Call: Copy + Debug + Display;

    /* STATIC METHODS */

    /// The first `Section` of any composition
    fn start() -> Self::Section;

    /// Returns `true` if this section is the end of a composition
    fn is_end(node: &Node<R, Self::Section>) -> bool;

    /// Write a list of calls in a human-readable format
    fn comp_string(&self, calls: &[Self::Call]) -> String;

    /* COMPOSING METHODS */

    /// The number of different section values.  If `Self : Into<usize>`, then we require that the
    /// `self` always maps to a value in the range `0..S::num_sections(table)`.
    fn num_sections(&self) -> usize;

    /// The [`Stage`] of all compositions generated from this `Table`.  All the instances must
    /// share the same [`Stage`], so this has no `self` parameter.
    fn stage(&self) -> Stage;

    /// Returns the number of [`Row`]s in a given `Section`
    fn length(&self, section: Self::Section) -> usize;

    /// Which other `Section`s are false against `(Row::rounds(_), self)`
    fn falseness(&self, section: Self::Section) -> &[(R, Self::Section)];

    /// Which `Section`s and transpositions are directly reachable from a given `Section`
    fn expand(&self, section: Self::Section) -> &[(Self::Call, R, Self::Section)];

    /// Tests a certain node for musicality
    fn music(&self, node: &Node<R, Self::Section>) -> f32;

    /// Generate an upper bound for how much music can be generated in a given number of rows.
    /// This **must** be an upper bound, or optimality is no longer guaranteed.
    fn music_upper_bound(&self, num_rows: usize) -> f32;

    /* PROVIDED METHODS */

    /// The first [`Node`] of any composition specified by this `Table`
    fn start_node(&self) -> Node<R, Self::Section> {
        Node::new(R::rounds(self.stage()), Self::start())
    }

    /// Generate compositions according to this `Table`
    fn compose(&self, desired_len: RangeInclusive<usize>, shortlist_size: usize) -> Results<R, Self>
    where
        Self: Sized,
    {
        let half_open_range = *desired_len.start()..*desired_len.end() + 1;
        Engine::<R, Self>::new(self, half_open_range, shortlist_size).compose()
    }
}

/// A single node of the composition - this is a [`Section`] (usually some part of the plain
/// course), along with a [`Row`] which describes which course the [`Section`] refers to.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Node<R, S> {
    pub row: R,
    pub section: S,
}

impl<R, S> Node<R, S> {
    pub fn new(row: R, section: S) -> Self {
        Node { row, section }
    }
}

impl<R: CompRow, S: Debug> Display for Node<R, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}|{})", self.section, self.row)
    }
}

#[derive(Debug, Clone)]
pub struct Comp<R: CompRow, T: Table<R>> {
    pub calls: Vec<T::Call>,
    pub length: usize,
    pub score: f32,
}

impl<R: CompRow, T: Table<R>> Comp<R, T> {
    pub fn to_string(&self, table: &T) -> String {
        format!(
            "{} rows, music {}: {}",
            self.length,
            self.score,
            table.comp_string(&self.calls)
        )
    }
}

impl<R: CompRow, T: Table<R>> PartialOrd for Comp<R, T> {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.score.partial_cmp(&other.score)
    }
}

impl<R: CompRow, T: Table<R>> Ord for Comp<R, T> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl<R: CompRow, T: Table<R>> PartialEq for Comp<R, T> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score
    }
}

impl<R: CompRow, T: Table<R>> Eq for Comp<R, T> {}

/// The results of a composing run
#[derive(Debug, Clone)]
pub struct Results<R: CompRow, T: Table<R>> {
    pub comps: Shortlist<Comp<R, T>>,
    pub nodes_expanded: usize,
    pub comps_found: usize,
}

impl<R: CompRow, T: Table<R>> Results<R, T> {
    fn new(shortlist_size: usize) -> Self {
        Results {
            comps: Shortlist::new(shortlist_size),
            nodes_expanded: 0,
            comps_found: 0,
        }
    }
}

/// All the persistent data required to generate a composition
#[derive(Debug, Clone)]
pub struct Engine<'t, R: CompRow, T: Table<R>> {
    // Static data
    table: &'t T,
    desired_len: Range<usize>,
    // Dynamic data (history of the current comp)
    nodes: _Set<R, T::Section>,
    calls: Vec<T::Call>,
    accumulated_music: f32,
    // Dynamic data (stats or best comps)
    results: Results<R, T>,
}

impl<'t, R: CompRow, T: Table<R>> Engine<'t, R, T> {
    fn new(table: &'t T, desired_len: Range<usize>, shortlist_size: usize) -> Self {
        Engine {
            table,
            desired_len,
            nodes: _Set::empty(T::num_sections(table)),
            calls: Vec::new(),
            accumulated_music: 0.0,
            results: Results::new(shortlist_size),
        }
    }

    fn compose(mut self) -> Results<R, T> {
        // Run the composing algorithm
        self.recursive_compose(self.table.start_node(), 0, 0);
        // Return the results
        self.results
    }

    #[cold]
    #[inline(never)]
    fn save_comp(&mut self, len: usize) {
        if PRINT_COMPS {
            println!(
                "FOUND COMP! (len {}, music {}): {}",
                len,
                self.accumulated_music,
                self.table.comp_string(&self.calls)
            );
        }

        self.results.comps_found += 1;
        self.results.comps.push(Comp {
            calls: self.calls.clone(),
            length: len,
            score: self.accumulated_music,
        });
    }

    fn recursive_compose(&mut self, node: Node<R, T::Section>, len: usize, depth: usize) {
        // PERF: Move these checks into the expansion loop, since we're always doing an unnecessary
        // function call before the checks
        dbg_print!(
            "Considering {}...",
            self.calls.iter().map(T::Call::to_string).join("")
        );

        self.results.nodes_expanded += 1;

        /* CHECK THAT THE NEW NODE IS VALID */

        // Check if we've found a valid composition
        if T::is_end(&node) && self.desired_len.contains(&len) {
            self.save_comp(len);
            return;
        }

        // Check if we've gone on too long
        if len >= self.desired_len.end {
            dbg_println!("Comp is too long!");
            return;
        }

        // Check whether this node is false against anything we've already rung
        for (fch, false_section) in self.table.falseness(node.section).iter() {
            if self.nodes.contains(&Node::new(
                unsafe { node.row.mul_unchecked(fch) },
                *false_section,
            )) {
                dbg_println!("False against {}: {:?}", fch, false_section);
                return;
            }
        }

        /* Compute the music for this table */

        // We cache the music because adding and subtracting the music score isn't guaranteed to
        // give the same value when using floats
        let last_music_score = self.accumulated_music;
        self.accumulated_music += self.table.music(&node);

        dbg_println!("It isn't false!");

        // Print out the node table
        if DBG_NODE_TABLE {
            for n in &self.nodes {
                println!("{}|{}", n.section, n.row);
            }
        }

        /* IF THE NEW NODE IS VALID, EXPAND IT AND EXPLORE FURTHER */

        // Now add this to the engine's comp
        self.nodes.add(node.clone());

        // Expand this in all possible ways
        for (name, transposition, section) in self.table.expand(node.section) {
            // Add the new name to the composition string
            self.calls.push(*name);

            self.recursive_compose(
                Node::new(unsafe { node.row.mul_unchecked(transposition) }, *section),
                len + self.table.length(node.section),
                depth + 1,
            );

            // Pop the call that we've explored
            self.calls.pop();
        }

        // Return the engine to the state before this node was added
        self.accumulated_music = last_music_score;
        self.nodes.remove_last(&node);
    }
}
