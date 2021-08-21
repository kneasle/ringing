use crate::{place_not::PnBlockParseError, AnnotBlock, PnBlock, Row, Stage};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

use self::class::FullClass;

pub mod class;

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
            PnBlock::parse(place_notation, stage)?.to_block_from_rounds(),
        ))
    }

    /// Creates a new `Method` from some place notation, adding a lead end annotation.
    pub fn with_lead_end(name: String, block: &PnBlock) -> Self {
        let mut first_lead: AnnotBlock<Option<String>> = block.to_block_from_rounds();
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

    /// Returns an [`AnnotBlock`] representing the plain course of this method
    pub fn plain_course(&self) -> AnnotBlock<(usize, Option<&str>)> {
        // Create a copy of `self.first_lead` where each row is also annotated by its index within
        // this lead
        let first_lead_with_indices = self
            .first_lead
            // We use `as_deref` to convert `&Option<String>` to `Option<&str>`
            .clone_map_annots_with_index(|i, label| (i, label.as_deref()));

        // Start with the first lead, and repeatedly add leads until we get back to rounds
        let mut plain_course = first_lead_with_indices;
        while !plain_course.leftover_row().is_rounds() {
            plain_course.extend_from_self(0..self.lead_len());
        }
        plain_course
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

    /// Returns an [`AnnotBlock`] of the first lead of this [`Method`], along with the lead
    /// location labels.
    pub fn first_lead(&self) -> &AnnotBlock<Option<String>> {
        &self.first_lead
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
    class.fmt_name(&mut s, true).unwrap();
    if s.len() > len_before_class {
        // If the class made the string longer, we need another space before the stage
        s.push(' ');
    }
    // Always push the stage
    s.push_str(stage.name().expect("Stage was too big to generate a name"));

    s
}
