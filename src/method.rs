use std::fmt::{Display, Formatter, Write};

use crate::{
    block::AnnotRowIter, place_not::PnBlockParseError, AnnotBlock, AnnotRow, PnBlock, Row, RowBuf,
    Stage,
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
        todo!();
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
