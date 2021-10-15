use itertools::Itertools;

use crate::{place_not::PnBlockParseError, AnnotBlock, PnBlock, Row, RowBuf, Stage};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

use self::class::FullClass;

pub mod class;

/// A standard label name used for denoting the 'lead end' of a method
pub const LABEL_LEAD_END: &str = "LE";

/// The definition of a 'method' within Change Ringing.  Essentially, a `Method` consists of a
/// [`Block`] which is intended to be rung as a repeating unit (usually a 'lead'), along with names
/// for specific locations within this [`Block`].  Calls can then be attached to these locations
/// (by name), and thus the single lead can be modified to determine the effect of calls in a
/// general way.  This follows how complib.org's composition input works.
#[derive(Debug, Clone)]
pub struct Method {
    title: String,
    name: String,
    class: FullClass,
    first_lead: AnnotBlock<Option<String>>,
}

impl Method {
    //////////////////
    // CONSTRUCTORS //
    //////////////////

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

    /////////////
    // GETTERS //
    /////////////

    /// Returns an `AnnotBlock` of the first lead of this `Method`
    #[deprecated(note = "Use `Method::first_lead` instead`")]
    #[inline]
    pub fn lead(&self) -> &AnnotBlock<Option<String>> {
        &self.first_lead
    }

    /// Returns an [`AnnotBlock`] of the first lead of this [`Method`], along with the lead
    /// location labels.
    #[inline]
    pub fn first_lead(&self) -> &AnnotBlock<Option<String>> {
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

    //////////////////////////////
    // BLOCK-RELATED OPERATIONS //
    //////////////////////////////

    /// Returns the [`Row`] at some index in the plain lead of this `Method`.
    ///
    /// # Panics
    ///
    /// Panics if the sub-lead index is larger than the first lead
    pub fn row_in_plain_lead(&self, idx: usize) -> &Row {
        &self.first_lead.row_vec()[idx]
    }

    /// Returns the [`Row`] at some index in the infinite plain course of this `Method`.
    pub fn row_in_plain_course(&self, idx: usize) -> RowBuf {
        let num_leads = idx / self.lead_len();
        let sub_lead_idx = idx % self.lead_len();

        let lead_head = self.lead_head().pow_u(num_leads);
        let row_within_lead = self.row_in_plain_lead(sub_lead_idx);
        lead_head.as_row() * row_within_lead
    }

    /// Returns an [`AnnotBlock`] representing the plain course of this method
    pub fn plain_course(&self) -> AnnotBlock<RowAnnot> {
        // Create a copy of `self.first_lead` where each row is also annotated by its index within
        // this lead
        let first_lead_with_indices = self
            .first_lead
            // We use `as_deref` to convert `&Option<String>` to `Option<&str>`
            .clone_map_annots_with_index(|i, label| RowAnnot {
                sub_lead_idx: i,
                label: label.as_deref(),
            });

        // Start with the first lead, and repeatedly add leads until we get back to rounds
        let mut plain_course = first_lead_with_indices;
        while !plain_course.leftover_row().is_rounds() {
            plain_course.extend_from_within(0..self.lead_len());
        }
        plain_course
    }

    //////////////////////
    // LABEL OPERATIONS //
    //////////////////////

    /// Sets or clears the label at a given index, panicking if the index is out of range
    pub fn set_label(&mut self, index: usize, label: Option<String>) {
        *self.first_lead.get_annot_mut(index).unwrap() = label;
    }

    /// Same as `self.set_label(0, Some(LABEL_LEAD_END.to_owned()))`
    pub fn set_lead_end_label(&mut self) {
        self.set_label(0, Some(LABEL_LEAD_END.to_owned()))
    }

    /// Returns the label at a given index, panicking if the index is out of range
    // TODO: Make this not panic
    pub fn get_label(&self, index: usize) -> Option<&str> {
        self.first_lead
            .get_annot(index)
            .unwrap()
            .as_ref()
            .map(String::as_str)
    }

    /// An [`Iterator`] over the sub-lead indices of a particular lead label.
    pub fn label_indices<'s>(&'s self, label: &'s str) -> impl Iterator<Item = usize> + 's {
        self.first_lead
            .annots()
            .positions(move |l| l.as_ref().map(String::as_str) == Some(label))
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

/// The source of a [`Row`] within a [`Method`]
#[derive(Debug, Clone)]
pub struct RowAnnot<'meth> {
    sub_lead_idx: usize,
    label: Option<&'meth str>,
}

impl<'meth> RowAnnot<'meth> {
    pub fn sub_lead_idx(&self) -> usize {
        self.sub_lead_idx
    }

    pub fn label(&self) -> Option<&'meth str> {
        self.label
    }
}
