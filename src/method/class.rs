use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{Display, Formatter, Write},
};

use itertools::Itertools;

use crate::{AnnotBlock, Bell, PlaceNot, Stage};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Method;

/// The full class of a [`Method`], including flags for little and differential
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FullClass {
    is_jump: bool,
    is_little: bool,
    is_differential: bool,
    class: Class,
}

impl FullClass {
    pub fn new(is_jump: bool, is_little: bool, is_differential: bool, class: Class) -> Self {
        Self {
            is_jump,
            is_little,
            is_differential,
            class,
        }
    }

    /// Compute the [`FullClass`] of a [`Method`], given an [`AnnotBlock`] representing its first lead.
    pub fn classify<A>(first_lead: &AnnotBlock<A>) -> FullClass {
        classify(first_lead) // Delegate to the helper function
    }

    /// Returns `true` if this represents a 'Jump' method
    #[inline]
    pub fn is_jump(self) -> bool {
        self.is_jump
    }

    /// Returns `true` if this represents a 'Little' method
    #[inline]
    pub fn is_little(self) -> bool {
        self.is_little
    }

    /// Returns `true` if this represents a 'Differential' method
    #[inline]
    pub fn is_differential(self) -> bool {
        self.is_differential
    }

    /// Gets the main [`Class`]
    #[inline]
    pub fn class(self) -> Class {
        self.class
    }

    /// Format the name of this class, either for a title (which won't display Principles and
    /// Hybrids) or for display (which will display all classes differently).
    pub(super) fn fmt_name(&self, f: &mut impl Write, is_for_title: bool) -> std::fmt::Result {
        // The `add_space` macro always generates a write to `is_first_segment`, but the value
        // written by the last call is never used.  The compiler will almost certainly optimise
        // that away, and we just don't need the error.
        #![allow(unused_assignments)]

        let mut is_first_segment = true;

        /// Adds a space between two parts of a name (but doesn't place an erroneous space at the
        /// start of a class).
        macro_rules! add_space {
            () => {
                if !is_first_segment {
                    write!(f, " ")?;
                }
                is_first_segment = false;
            };
        }

        // Write optional classes
        if self.is_jump {
            add_space!();
            write!(f, "Jump")?;
        }
        if self.is_differential {
            add_space!();
            write!(f, "Differential")?;
        }
        // Write 'Little' in the main class name
        if is_for_title {
            // In method titles, 'Little' should only be used for symmetric hunt methods
            if let Some(name) = self.class.name_in_title() {
                if self.is_little {
                    add_space!();
                    write!(f, "Little")?;
                }

                add_space!();
                write!(f, "{}", name)?;
            }
        } else {
            if self.is_little {
                add_space!();
                write!(f, "Little")?;
            }

            add_space!();
            write!(f, "{}", self.class)?;
        }

        Ok(())
    }
}

impl Display for FullClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_name(f, false)
    }
}

/// The class of a hunt bell path, ordered by dominance (i.e. if a method has two hunt bell paths,
/// the first class specified in this enum is used for the method - so for example, given an option
/// between Plain and Alliance, the method will be named 'Plain').
#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum HuntBellClass {
    Plain,
    TrebleDodging,
    TreblePlace,
    Alliance,
    Hybrid,
}

/// The `Class` of a [`Method`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum Class {
    /// A method with no hunt bells
    Principle,

    /* Plain Methods */
    /// A plain method where every change in direction has places made
    Place,
    /// A plain method that isn't a `Place` method
    Bob,

    /* Treble Dodging */
    /// A treble dodging method in which internal places are **never** made between dodges
    TrebleBob,
    /// A treble dodging method in which internal places are **sometimes** made between dodges
    Delight,
    /// A treble dodging method in which internal places are **always** made between dodges
    Surprise,

    /* Other hunter classes */
    /// A method where the hunt bell makes places more than twice per lead
    TreblePlace,
    /// A method where the hunt bell makes a symmetric but not well-formed path
    Alliance,
    /// A method where the hunt bell makes an asymmetric path
    Hybrid,
}

impl Class {
    /// Returns `true` if `self` is either [`Class::Place`] or [`Class::Bob`].
    #[inline]
    pub fn is_plain(self) -> bool {
        matches!(self, Self::Place | Self::Bob)
    }

    /// Returns `true` if `self` is either [`Class::TrebleBob`], [`Class::Delight`] or
    /// [`Class::Surprise`].
    #[inline]
    pub fn is_treble_dodging(self) -> bool {
        matches!(self, Self::TrebleBob | Self::Delight | Self::Surprise)
    }

    /// Returns `true` if `self` denotes a method who's dominant hunt bell follows a symmetric
    /// path.
    #[inline]
    pub fn is_symmetric_hunter(self) -> bool {
        self.is_plain()
            || self.is_treble_dodging()
            || matches!(self, Self::TreblePlace | Self::Alliance)
    }

    /// Returns the [`HuntBellClass`] of the dominant hunt bell of a [`Method`] with this `Class`
    pub fn hunt_bell_class(self) -> Option<HuntBellClass> {
        Some(match self {
            Class::Bob | Class::Place => HuntBellClass::Plain,
            Class::TrebleBob | Class::Delight | Class::Surprise => HuntBellClass::TrebleDodging,
            Class::TreblePlace => HuntBellClass::TreblePlace,
            Class::Alliance => HuntBellClass::Alliance,
            Class::Hybrid => HuntBellClass::Hybrid,
            // Principles have no hunt bells, and therefore no valid `HuntBellClass`
            Class::Principle => return None,
        })
    }

    /// Returns the human-friendly name of this `Class` as used in [`Method`] titles (i.e. where no
    /// name is given for `Principle` or `Hybrid`
    pub fn name_in_title(self) -> Option<&'static str> {
        Some(match self {
            Class::Principle => return None,

            Class::Place => "Place",
            Class::Bob => "Bob",

            Class::TrebleBob => "Treble Bob",
            Class::Delight => "Delight",
            Class::Surprise => "Surprise",

            Class::TreblePlace => "Treble Place",
            Class::Alliance => "Alliance",
            Class::Hybrid => return None,
        })
    }

    /// Returns the human-friendly name of this `Class`, including names for `Principle` and
    /// `Hybrid`.
    pub fn name(self) -> &'static str {
        match self {
            Class::Principle => "Principle",

            Class::Place => "Place",
            Class::Bob => "Bob",

            Class::TrebleBob => "Treble Bob",
            Class::Delight => "Delight",
            Class::Surprise => "Surprise",

            Class::TreblePlace => "Treble Place",
            Class::Alliance => "Alliance",
            Class::Hybrid => "Hybrid",
        }
    }
}

impl Display for Class {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/////////////////////////
// CLASSIFICATION CODE //
/////////////////////////

/// Classify a method, given its first lead
fn classify<A>(first_lead: &AnnotBlock<A>) -> FullClass {
    let stage = first_lead.stage();

    // Generate hunt & working bell cycles, and their paths
    let cycles = Cycle::cycles_from_lead(first_lead);
    let (hunt_cycles, working_cycles) = Cycle::partition_cycles(cycles);

    // The method is differential if not all the working cycles have the same length
    let all_working_cycles_equal_length = working_cycles
        .iter()
        .tuple_windows()
        .all(|(set1, set2)| set1.num_leads() == set2.num_leads());
    let is_differential = !all_working_cycles_equal_length;

    // If there are no hunt bells, then the method is a Principle
    if hunt_cycles.is_empty() {
        return FullClass {
            is_jump: false,
            is_little: false, // principles can't be little
            is_differential,
            class: Class::Principle,
        };
    }

    // Classify all the hunt bell paths, and use the dominant class for the method
    let (mut best_is_little, mut best_hunt_bell_class) =
        classify_hunt_cycle(&hunt_cycles[0], stage);
    let mut hunt_cycles_in_best_class = vec![&hunt_cycles[0]];
    for cycle in &hunt_cycles[1..] {
        let (is_little, class) = classify_hunt_cycle(cycle, stage);

        match class.cmp(&best_hunt_bell_class) {
            // If this class is strictly better than the last one, then overwrite
            // best_hunt_bell_class directly
            Ordering::Less => {
                best_is_little = is_little;
                best_hunt_bell_class = class;
                hunt_cycles_in_best_class.clear();
                hunt_cycles_in_best_class.push(cycle);
            }
            // If this hunt bell is equal to the current best class, then we only name the
            // method 'Little' if **all** its best-classed hunt bells are Little
            Ordering::Equal => {
                best_is_little &= is_little;
                hunt_cycles_in_best_class.push(cycle);
            }
            // If this class is strictly worse than the current best, then it can't contribute
            // to the Method's class
            Ordering::Greater => {}
        }
    }

    let hunt_bells_in_best_class = hunt_cycles_in_best_class
        .iter()
        .map(|cycle| cycle.as_hunt_bell().unwrap())
        .collect_vec();

    // Convert the HuntBellClass into a `Class` for the whole method (sub-classifying if
    // necessary)
    let class = match best_hunt_bell_class {
        HuntBellClass::Plain => {
            // All paths except the dominant hunt bell count towards making a method `Bob` or
            // `Place` (including other hunt bells), so we have to add in the non-dominant hunt
            // bells to the cycles checked by `sub_classify_plain`
            let mut cycles = working_cycles.iter().collect_vec();
            cycles.extend(hunt_cycles.iter().filter(|cycle| {
                !hunt_bells_in_best_class.contains(&cycle.as_hunt_bell().unwrap())
            }));
            sub_classify_plain(cycles)
        }
        HuntBellClass::TrebleDodging => {
            sub_classify_treble_dodging(&first_lead, &hunt_cycles_in_best_class)
        }
        HuntBellClass::TreblePlace => Class::TreblePlace,
        HuntBellClass::Alliance => Class::Alliance,
        HuntBellClass::Hybrid => Class::Hybrid,
    };

    // Build a `FullClass` for this method
    FullClass {
        is_jump: false,
        is_little: best_is_little,
        is_differential,
        class,
    }
}

/// Classify a hunt bell path (and whether or not the method is Little)
fn classify_hunt_cycle(cycle: &Cycle, stage: Stage) -> (bool, HuntBellClass) {
    // Count the number of rows spent in each place (we ignore the first element of the path
    // because the `path` includes the leftover row and therefore counts the first place twice).
    let mut num_rows_in_each_place = vec![0usize; stage.num_bells()];
    for p in &cycle.full_path {
        num_rows_in_each_place[*p] += 1;
    }

    // A method is Little if some places are not visited by this hunt bell
    let is_little = num_rows_in_each_place.contains(&0);
    // A method is stationary if exactly one place is visited
    let is_stationary = num_rows_in_each_place.iter().filter(|v| **v > 0).count() == 1;
    let are_places_visited_exactly_twice =
        num_rows_in_each_place.iter().all(|&n| matches!(n, 0 | 2));

    // If a hunt bell path is stationary, then it classifies as Treble Place
    // (from Framework):
    // ... or
    // a) The Hunt Bell is a Stationary Bell
    // b) The Method does not use Jump Changes.
    if is_stationary {
        return (is_little, HuntBellClass::TreblePlace);
    }

    // A method is Plain iff:
    // a) The Hunt Bell rings exactly twice in each Place of the Path during a Plain Lead
    //    [condition of the if statement]
    // b) The Hunt Bell is not a Stationary Bell [early return]
    // c) The Method does not use Jump Changes [pre-check for jump methods]
    if are_places_visited_exactly_twice {
        return (is_little, HuntBellClass::Plain);
    }

    let is_path_palindromic = is_palindromic(&cycle.full_path);
    // Places are made when two consecutive places are equal (wrapping around the lead end)
    let num_places_made = cycle
        .full_path
        .iter()
        .circular_tuple_windows()
        .filter(|(a, b)| a == b)
        .count();
    // Places are equally covered if all non-zero counts in `num_rows_in_each_place` are the same
    let are_all_places_equally_covered = num_rows_in_each_place
        .iter()
        .filter(|v| **v != 0)
        .tuple_windows()
        .all(|(a, b)| a == b);

    let class = match (
        is_path_palindromic,
        are_all_places_equally_covered,
        num_places_made.cmp(&2),
    ) {
        // A hunt bell is Treble Place if:
        // a) the path is palindromic
        // b) the path covers all places equally
        // c) places are made more than twice
        (true, true, Ordering::Greater) => HuntBellClass::TreblePlace,
        // If places are made precisely twice, then this method is either Plain or Treble Dodging.
        // However, if it were plain, then we would have returned earlier in the function so, by
        // the process of elimination, this path must be treble dodging.
        (true, true, Ordering::Equal) => HuntBellClass::TrebleDodging,
        // Any palindromic path which covers all places equally but <2 places are made is hybrid
        // (it must just be dodging)
        (true, true, Ordering::Less) => HuntBellClass::Hybrid,
        // Palindromic paths where places aren't evenly covered are Alliance
        (true, false, _) => HuntBellClass::Alliance,
        // Non-palindromic paths are always Hybrid
        (false, _, _) => HuntBellClass::Hybrid,
    };

    (is_little, class)
}

/// Given a Treble Bob method, sub-classify it into either Surprise, Delight or Treble Bob
fn sub_classify_treble_dodging<A>(first_lead: &AnnotBlock<A>, hunt_cycles: &[&Cycle]) -> Class {
    let mut cross_indices = HashSet::<usize>::new();
    // Use the hunt bell paths to figure out in which places the hunt bells hunt between dodges
    for &cycle in hunt_cycles {
        let min_place = *cycle.full_path.iter().min().unwrap();
        // Find the 'cross sections' - i.e. the positions where this hunt bell hunts between
        // calling positions
        cross_indices.extend(
            cycle
                .full_path
                .iter()
                .circular_tuple_windows()
                .positions(|(a, b)| (a - min_place) / 2 != (b - min_place) / 2),
        );
    }

    // Check for internal places at each of the cross locations
    let mut all_internal_places = true;
    let mut all_no_internal_places = true;
    for i in cross_indices {
        let r1 = first_lead.get_row(i).unwrap();
        let r2 = first_lead.get_row(i + 1).unwrap();
        let has_internal_places = PlaceNot::pn_between(r1, r2).unwrap().has_internal_places();
        if has_internal_places {
            all_no_internal_places = false;
        } else {
            all_internal_places = false;
        }
    }

    match (all_internal_places, all_no_internal_places) {
        (false, true) => Class::TrebleBob,
        (false, false) => Class::Delight,
        (true, false) => Class::Surprise,
        // In this case, there must be no cross points (i.e. the hunt bell is confined to two
        // places) and in this case the method defaults to Treble Bob
        (true, true) => Class::TrebleBob,
    }
}

/// Given a Plain method, sub-classify it into either Bob or Place
fn sub_classify_plain(working_bell_cycles: Vec<&Cycle>) -> Class {
    // Follow each cycle of working bells, and generate the full path of that cycle for detecting
    // points
    for cycle in working_bell_cycles {
        // Check for a change in direction without an intervening place.  I.e. we're looking for:
        //     x                x
        //       x     or     x
        //     x                x
        // If this happens, we know that the method is a Bob, not a Place
        for (&a, &b, &c) in cycle.full_path.iter().circular_tuple_windows() {
            if a == c && (b as isize == a as isize - 1 || b == a + 1) {
                return Class::Bob;
            }
        }
    }
    // If no 'dodges' were found, then the method is `Place`
    Class::Place
}

/// Determines if a sequence of places is the same forwards as it is backwards.  This currently
/// uses a fairly naive algorithm of generating the reverse path and then repeatedly rotating it,
/// giving a roughly quadratic algorithm.  This should be fine, but if someone knows of or wants to
/// implement a faster algorithm then that'd be great :).
fn is_palindromic(path: &[usize]) -> bool {
    // Try rotating `reversed_path` by each amount, and test if the result is equal to
    // `is_palindromic`
    for i in 0..path.len() {
        let rotated_reversed_path = path.iter().rev().cycle().skip(i);
        if rotated_reversed_path.zip(path).all(|(p1, p2)| p1 == p2) {
            // If the reversed & rotated path is equal to the forward path, then this path is
            // palindromic
            return true;
        }
    }
    // If all the rotations of the reversed path are non-equal to the forward path, then the path
    // isn't palindromic
    false
}

/// A subset of the place bells of a method which all take the same path.
#[derive(Debug, Clone)]
struct Cycle {
    place_bells: Vec<Bell>,
    full_path: Vec<usize>,
}

impl Cycle {
    /// Returns all the cycles generated by repeating a given `lead`
    fn cycles_from_lead<A>(first_lead: &AnnotBlock<A>) -> Vec<Self> {
        // Check that the lead starts from rounds
        assert!(first_lead.first_annot_row().unwrap().row().is_rounds());

        let mut cycles = Vec::<Cycle>::new();
        let mut bells_left = vec![true; first_lead.stage().num_bells()];

        // Repeatedly follow cycles until every bell is in a cycle that we've already explored
        while let Some(starting_bell_idx) = bells_left.iter().position(|v| *v) {
            let starting_bell = Bell::from_index(starting_bell_idx);
            let mut place_bells_in_cycle = Vec::new();
            let mut full_path = Vec::new();
            // Follow the cycle which contains `starting_bell_idx`, marking those bells as
            // found
            let mut place_bell = starting_bell;
            loop {
                // Mark that we've covered this bell
                bells_left[place_bell.index()] = false;
                // Track this bell's path through the next lead
                let (path, next_place_bell) = first_lead.path_of(place_bell).unwrap();
                // Add path & place_bell to this cycle
                place_bells_in_cycle.push(place_bell);
                full_path.extend(path);
                // Move to the next place bell, and
                place_bell = Bell::from_index(next_place_bell);
                if place_bell == starting_bell {
                    break;
                }
            }
            cycles.push(Cycle {
                place_bells: place_bells_in_cycle,
                full_path,
            });
        }

        cycles
    }

    fn is_hunt(&self) -> bool {
        self.place_bells.len() == 1
    }

    /// If `self.is_hunt()`, this returns the hunt bell for `self`.
    fn as_hunt_bell(&self) -> Option<Bell> {
        match self.place_bells.len() {
            1 => Some(self.place_bells[0]),
            _ => None,
        }
    }

    fn num_leads(&self) -> usize {
        self.place_bells.len()
    }

    /// Classify and partition [`Bell`]s into to (hunt, working).
    fn partition_cycles(cycles: Vec<Cycle>) -> (Vec<Cycle>, Vec<Cycle>) {
        let mut hunt_cycles = Vec::new();
        let mut working_cycles = Vec::new();

        for cycle in cycles {
            if cycle.is_hunt() {
                // If this bell is at its home position at the lead head, then it must be a hunt bell
                hunt_cycles.push(cycle);
            } else {
                working_cycles.push(cycle);
            }
        }

        (hunt_cycles, working_cycles)
    }
}
#[cfg(test)]
mod tests {
    use crate::{method::FullClass, Block, MethodLib};

    #[test]
    fn classification() {
        let mut num_misclassifications = 0usize;
        for (name, pn, class) in MethodLib::cc_lib().unwrap().all_pns_and_classes() {
            let plain_lead: Block = pn.to_block_from_rounds();
            let computed_class = FullClass::classify(&plain_lead);
            if computed_class != class {
                println!(
                    "Misclassified {} as '{}' (should be '{}')",
                    name, computed_class, class
                );
                num_misclassifications += 1;
            }
        }
        println!("{} methods misclassified", num_misclassifications);

        if num_misclassifications > 0 {
            panic!();
        }
    }
}
