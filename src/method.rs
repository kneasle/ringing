use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{Display, Formatter, Write},
};

use itertools::Itertools;

use crate::{
    block::AnnotRowIter, place_not::PnBlockParseError, AnnotBlock, AnnotRow, Bell, PlaceNot,
    PnBlock, Row, RowBuf, Stage,
};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

/// A standard label name used for denoting the 'lead end' of a method
pub const LABEL_LEAD_END: &str = "LE";

/// The definition of a 'method' within Change Ringing.  This struct follows quite a loose
/// definition, which allows the `Method` struct to represent things that may not count as methods
/// (as determined by the Framework).  Essentially, a `Method` consists of a [`Block`] which is
/// intended to be rung as a repeating unit (usually a 'lead'), along with names for specific
/// locations within this [`Block`].  Calls can then be attached to these locations (by name), and
/// thus the single lead can be modified to determine the effect of calls in a general way.  This
/// follows how complib.org's composition input works.
#[derive(Debug, Clone)]
pub struct Method {
    title: String,
    name: String,
    class: FullClass,
    first_lead: AnnotBlock<Option<String>>,
}

impl Method {
    /// Creates a new `Method` from its raw parts
    pub fn new(
        title: String,
        name: String,
        class: FullClass,
        first_lead: AnnotBlock<Option<String>>,
    ) -> Self {
        Self {
            title,
            name,
            class,
            first_lead,
        }
    }

    /// Create and classify a new `Method`, given its name and first lead
    pub fn with_name(name: String, first_lead: AnnotBlock<Option<String>>) -> Self {
        let class = FullClass::classify(&first_lead);
        Self {
            title: generate_title(&name, class, first_lead.stage()),
            name,
            class,
            first_lead,
        }
    }

    /// Parses a place notation string and creates a `Method` with that place notation and no lead
    /// locations.
    pub fn from_place_not_string(
        name: String,
        stage: Stage,
        place_notation: &str,
    ) -> Result<Self, PnBlockParseError> {
        Ok(Self::with_name(
            name,
            PnBlock::parse(place_notation, stage)?.to_block(),
        ))
    }

    /// Creates a new `Method` from some place notation, adding a lead end annotation.
    pub fn with_lead_end(name: String, block: &PnBlock) -> Self {
        let mut first_lead: AnnotBlock<Option<String>> = block.to_block();
        *first_lead.get_annot_mut(first_lead.len() - 1).unwrap() = Some(LABEL_LEAD_END.to_owned());
        Self::with_name(name, first_lead)
    }

    /// Returns an `AnnotBlock` of the first lead of this `Method`
    #[inline]
    pub fn lead(&self) -> &AnnotBlock<Option<String>> {
        &self.first_lead
    }

    /// The overall transposing effect of one lead of this `Method`.
    #[inline]
    pub fn lead_head(&self) -> &Row {
        self.first_lead.leftover_row()
    }

    /// How many [`Row`]s are in a single lead of this `Method`?
    #[inline]
    pub fn lead_len(&self) -> usize {
        self.first_lead.len()
    }

    /// Gets the [`Stage`] of this `Method`
    #[inline]
    pub fn stage(&self) -> Stage {
        self.first_lead.stage()
    }

    /// Gets the name of this `Method`
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Generates a new [`CourseIter`] which generates an infinite course of this [`Method`],
    /// starting at a given `starting_row`.
    #[inline]
    pub fn course_iter(&self, starting_row: RowBuf) -> CourseIter<'_> {
        CourseIter::new(self, starting_row)
    }

    /// Returns an [`AnnotBlock`] representing the plain course of this method
    pub fn plain_course(&self) -> AnnotBlock<(usize, Option<&str>)> {
        // TODO: If we prevent labelling of leftover rows, then this turns into a one-line iterator
        // chain
        let mut annot_rows = Vec::new();
        let mut is_first_row = true;
        for (i, label, row) in self.plain_course_iter() {
            let is_rounds_at_lead_end = i == 0 && row.is_rounds();
            annot_rows.push(AnnotRow::new(row, (i, label)));
            if is_rounds_at_lead_end && !is_first_row {
                break;
            }
            is_first_row = false;
        }
        // This unsafety is OK because:
        // - There must be at most one copy of `self.lead`, which must have length at least 2
        // - [`CourseIter`] guarutees that all returned rows have the same [`Stage`]
        unsafe { AnnotBlock::from_annot_rows_unchecked(annot_rows) }
    }

    /// Generates a new [`CourseIter`] which generates the plain course of this [`Method`] forever.
    #[inline]
    pub fn plain_course_iter(&self) -> CourseIter<'_> {
        CourseIter::new(self, RowBuf::rounds(self.stage()))
    }

    /// Sets or clears the label at a given index, panicking if the index is out of range
    pub fn set_label(&mut self, index: usize, label: Option<String>) {
        *self.first_lead.get_annot_mut(index).unwrap() = label;
    }

    /// Returns the label at a given index, panicking if the index is out of range
    pub fn get_label(&self, index: usize) -> Option<&str> {
        self.first_lead
            .get_annot(index)
            .unwrap()
            .as_ref()
            .map(String::as_str)
    }
}

/// Type alias used for brevity in [`CourseIter`]
type _InternalIter<'m> =
    std::iter::Peekable<std::iter::Enumerate<AnnotRowIter<'m, Option<String>>>>;

/// An iterator that generates repeating leads of a given [`Method`].  **This iterator never
/// returns.**
#[derive(Clone, Debug)]
pub struct CourseIter<'m> {
    method: &'m Method,
    current_iter: _InternalIter<'m>,
    // PERF: We could replace this with an accumulator to stop needless allocations
    current_lead_head: RowBuf,
}

impl<'m> CourseIter<'m> {
    /// Creates a new `CourseIter` which generates a given [`Method`], beginning at some inital
    /// [`Row`].
    fn new(method: &'m Method, first_lead_head: RowBuf) -> Self {
        CourseIter {
            method,
            current_lead_head: first_lead_head,
            current_iter: Self::get_iter(method),
        }
    }

    /// Gets a new [`_InternalIter`] from a [`Method`]
    fn get_iter(method: &'m Method) -> _InternalIter<'m> {
        // TODO: If leftover rows can't be annotated, then this slice is unnecessary
        method.first_lead.annot_rows()[..method.lead_len()]
            .iter()
            .enumerate()
            .peekable()
    }
}

// PERF: We should implement more of the iterator methods like `skip`, which are used extensively
// but generate very bad code by default
impl<'m> Iterator for CourseIter<'m> {
    type Item = (usize, Option<&'m str>, RowBuf);

    fn next(&mut self) -> Option<Self::Item> {
        // If the iterator is about to finish, then move on by a lead and create a new iterator
        if self.current_iter.peek().is_none() {
            self.current_iter = Self::get_iter(self.method);
            // This unsafety is OK because the rows all originate from the same `AnnotBlock`
            // which guarutees that its rows have the same stage
            self.current_lead_head = unsafe {
                self.current_lead_head
                    .mul_unchecked(&self.method.lead_head())
            };
        }
        // Now, generate the next item to return.  Unwrapping here is fine, because
        // `self.current_iter` must generate at least one Row (because methods can never have a
        // 0-length lead)
        let (sub_lead_index, annot_r) = self.current_iter.next().unwrap();
        Some((
            sub_lead_index,
            annot_r.annot().as_deref(),
            // This unsafety is OK because the rows all originate from the same `AnnotBlock` which
            // guarutees that its rows have the same stage
            unsafe { self.current_lead_head.mul_unchecked(annot_r.row()) },
        ))
    }
}

/// Generate the (standard) title of a [`Method`] from its parts, according to the Framework's
/// rules.  Some methods (e.g. Grandsire and Union) do not follow this convention, and therefore
/// their titles must be stored separately.
pub fn generate_title(name: &str, class: FullClass, stage: Stage) -> String {
    let mut s = String::new();

    // Push the name, followed by a space (if the name is non-empty)
    s.push_str(name);
    if !name.is_empty() {
        s.push(' ');
    }
    // Push the classification, and add another space if the classification wasn't the empty string
    // (which we check indirectly by measuring the length of `s` before and after adding the
    // classification string)
    let len_before_class = s.len();
    write!(s, "{}", class).unwrap();
    if s.len() > len_before_class {
        // If the class made the string longer, we need another space before the stage
        s.push(' ');
    }
    // Always push the stage
    s.push_str(stage.name().expect("Stage was too big to generate a name"));

    s
}

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
        let lead_head = first_lead.leftover_row();
        let stage = first_lead.stage();

        // Partition bells into hunts & working bells
        let (hunt_bells, working_bells) = partition_bells(lead_head);

        // A method is differential iff the working bell cycles don't have the same length
        let cycles = {
            let mut cycles = Vec::<Vec<Bell>>::new();
            let mut working_bell_left = vec![false; stage.as_usize()];
            for b in &working_bells {
                working_bell_left[b.index()] = true;
            }

            // Repeatedly follow cycles until all working bells have been found
            while let Some(starting_bell_idx) = working_bell_left.iter().position(|v| *v) {
                let starting_bell = Bell::from_index(starting_bell_idx);
                let mut bells_in_cycle = Vec::new();
                // Follow the cycle which contains `starting_bell_idx`, marking those bells as
                // found
                let mut bell = starting_bell;
                loop {
                    // Mark that we've covered this bell
                    working_bell_left[bell.index()] = false;
                    // Move a step down the cycle, and check if we've returned to the start bell
                    bell = Bell::from_index(lead_head.place_of(bell).unwrap());
                    bells_in_cycle.push(bell);
                    if bell == starting_bell {
                        break;
                    }
                }
                cycles.push(bells_in_cycle);
            }

            cycles
        };

        // If all the cycles have the same lengths, then the method is not differential
        let is_differential = !cycles
            .iter()
            .tuple_windows()
            .all(|(set1, set2)| set1.len() == set2.len());

        // If there are no hunt bells, then the method is a Principle
        if hunt_bells.is_empty() {
            return FullClass {
                is_jump: false,
                is_little: false, // principles can't be little
                is_differential,
                class: Class::Principle,
            };
        }

        // Classify all the hunt bell paths, and use the dominant class for the method
        let (mut best_is_little, mut best_hunt_bell_class) =
            classify_hunt_bell_path(first_lead, hunt_bells[0]);
        let mut hunt_bells_in_best_class = vec![hunt_bells[0]];
        for &b in &hunt_bells[1..] {
            let (is_little, class) = classify_hunt_bell_path(first_lead, b);

            match class.cmp(&best_hunt_bell_class) {
                // If this class is strictly better than the last one, then overwrite
                // best_hunt_bell_class directly
                Ordering::Less => {
                    best_is_little = is_little;
                    best_hunt_bell_class = class;
                    hunt_bells_in_best_class.clear();
                    hunt_bells_in_best_class.push(b);
                }
                // If this hunt bell is equal to the current best class, then we only name the
                // method 'Little' if **all** its best-classed hunt bells are Little
                Ordering::Equal => {
                    best_is_little &= is_little;
                    hunt_bells_in_best_class.push(b);
                }
                // If this class is strictly worse than the current best, then it can't contribute
                // to the Method's class
                Ordering::Greater => {}
            }
        }

        // Convert the HuntBellClass into a `Class` for the whole method (sub-classifying if
        // necessary)
        let class = match best_hunt_bell_class {
            HuntBellClass::Plain => {
                // All paths except the dominant hunt bell count towards making a method `Bob` or
                // `Place`, so we have to add in the non-dominant hunt bells to the cycles checked
                // by `sub_classify_plain`
                let mut cycles = cycles;
                cycles.extend(
                    hunt_bells
                        .iter()
                        .filter(|b| !hunt_bells_in_best_class.contains(b))
                        .map(|b| vec![*b]),
                );
                sub_classify_plain(first_lead, &cycles)
            }
            HuntBellClass::TrebleDodging => {
                sub_classify_treble_dodging(&first_lead, &hunt_bells_in_best_class)
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
}

impl Display for FullClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // The `add_space` macro generates writes to `is_first_segment`, but this value is never
        // used for the last call
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
        // 'Little' should only be used for symmetric hunt methods
        if self.class.is_symmetric_hunter() {
            if self.is_little {
                add_space!();
                write!(f, "Little")?;
            }

            // Write the main class
            if let Some(name) = self.class.name() {
                add_space!();
                write!(f, "{}", name)?;
            }
        }

        Ok(())
    }
}

/* CLASSIFICATION HELPER FUNCTIONS */

/// Classify and partition [`Bell`]s according to (hunt, working).
fn partition_bells(lead_head: &Row) -> (Vec<Bell>, Vec<Bell>) {
    let mut hunt_bells = Vec::new();
    let mut working_bells = Vec::new();

    for (i, b) in lead_head.bell_iter().enumerate() {
        if i == b.index() {
            // If this bell is at its home position at the lead head, then it must be a hunt bell
            hunt_bells.push(b);
        } else {
            working_bells.push(b);
        }
    }

    (hunt_bells, working_bells)
}

/// Classify a hunt bell path (and whether or not the method is Little)
fn classify_hunt_bell_path<A>(first_lead: &AnnotBlock<A>, bell: Bell) -> (bool, HuntBellClass) {
    let path = first_lead.path_of(bell).unwrap();
    let stage = first_lead.stage();

    // Count the number of rows spent in each place (we ignore the first element of the path
    // because the `path` includes the leftover row and therefore counts the first place twice).
    let mut num_rows_in_each_place = vec![0usize; stage.as_usize()];
    for p in &path {
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

    let is_path_palindromic = is_palindromic(&path);
    let num_places_made = path
        .iter()
        .circular_tuple_windows()
        .filter(|(a, b)| a == b)
        .count();
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
        (true, true, Ordering::Greater) => HuntBellClass::TreblePlace,
        (true, true, Ordering::Equal) => HuntBellClass::TrebleDodging,
        (true, true, Ordering::Less) => HuntBellClass::Hybrid,
        // Paths where places aren't evenly covered are also Alliance
        (true, false, _) => HuntBellClass::Alliance,
        // Non-palindromic paths are always Hybrid
        (false, _, _) => HuntBellClass::Hybrid,
    };

    (is_little, class)
}

/// Given a Treble Bob method, sub-classify it into either Surprise, Delight or Treble Bob
fn sub_classify_treble_dodging<A>(first_lead: &AnnotBlock<A>, hunt_bells: &[Bell]) -> Class {
    let mut cross_indices = HashSet::<usize>::new();
    // Use the hunt bell paths to figure out in which places the hunt bells hunt between dodges
    for &h in hunt_bells {
        let path = first_lead.path_of(h).unwrap();
        let min_place = *path.iter().min().unwrap();
        // Find the cross sections
        cross_indices.extend(
            path.iter()
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
        (false, false) => Class::Delight,
        (false, true) => Class::TrebleBob,
        (true, false) => Class::Surprise,
        // In this case, there must be no cross points and in this case the method defaults to
        // Treble Bob
        (true, true) => Class::TrebleBob,
    }
}

/// Given a Plain method, sub-classify it into either Bob or Place
fn sub_classify_plain<A>(first_lead: &AnnotBlock<A>, working_bell_cycles: &[Vec<Bell>]) -> Class {
    // Follow each cycle of working bells, and generate the full path of that cycle for detecting
    // points
    for bells_in_cycle in working_bell_cycles {
        // Generate the full path of this cycle
        let mut path = Vec::<usize>::with_capacity(bells_in_cycle.len() * first_lead.len());
        for b in bells_in_cycle {
            path.extend(first_lead.path_of(*b).unwrap());
        }
        // Check for a change in direction without an intervening place.  I.e. we're looking for:
        // b          b
        //   b  or  b
        // b          b
        // If this happens, we know that the method is a Bob, not a Place
        for (a, b, c) in path.into_iter().circular_tuple_windows() {
            if a == c && (b as isize == a as isize - 1 || b == a + 1) {
                return Class::Bob;
            }
        }
    }

    Class::Place
}

/// Determines if a sequence of places is the same forwards as it is backwards.  This currently
/// uses a fairly naive algorithm of generating the reverse path and then repeatedly rotating it,
/// giving a roughly quadratic algorithm.  This should be fine, but if someone knows of or wants to
/// implement a faster algorithm then that'd be great :).
fn is_palindromic(path: &[usize]) -> bool {
    true
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

    /// Returns the printable name of this `Class`, or `None` if `self` is [`Class::Principle`].
    pub fn name(self) -> Option<&'static str> {
        Some(match self {
            Class::Principle => return None,

            Class::Place => "Place",
            Class::Bob => "Bob",

            Class::TrebleBob => "Treble Bob",
            Class::Delight => "Delight",
            Class::Surprise => "Surprise",

            Class::TreblePlace => "Treble Place",
            Class::Alliance => "Alliance",
            Class::Hybrid => "Hybrid",
        })
    }
}

impl Display for Class {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name().unwrap_or(""))
    }
}

#[cfg(test)]
mod tests {
    use crate::{method::FullClass, Block, MethodLib};

    #[test]
    fn classification() {
        let mut num_misclassifications = 0usize;
        for (name, pn, class) in MethodLib::cc_lib().unwrap().all_pns_and_classes() {
            let plain_lead: Block = pn.to_block();
            let computed_class = FullClass::classify(&plain_lead);
            if computed_class != class {
                println!("Misclassified {} as '{}'", name, computed_class);
                num_misclassifications += 1;
            }
        }
        println!("{} methods misclassified", num_misclassifications);

        if num_misclassifications > 0 {
            panic!();
        }
    }
}
