use itertools::Itertools;

use crate::{place_not::PnBlockParseError, Block, PnBlock, Row, RowBuf, Stage};

use self::class::FullClass;

pub mod class;

/// A standard label name used for denoting the 'lead end' of a method
pub const LABEL_LEAD_END: &str = "LE";

/// The definition of a 'method' within Change Ringing.  Essentially, a `Method` consists of a
/// [`Block`] which is intended to be rung as a repeating unit (usually a 'lead'), along with
/// 'labels' for specific locations within this [`Block`].  Calls can then be attached to these
/// locations (by their label), and thus the single lead can be modified to determine the effect of
/// calls in a general way.  This follows how [CompLib](https://complib.org)'s composition input
/// works.
#[derive(Debug, Clone)]
pub struct Method {
    title: String,
    name: String,
    class: FullClass,
    /// The first lead of this [`Method`], where each row can be given any number of arbitrary
    /// labels.
    // TODO: Use a `HashMap<(usize, String)>` to store lead labels?
    first_lead: Block<Vec<String>>,
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
        first_lead: Block<Vec<String>>,
    ) -> Self {
        Self {
            title,
            name,
            class,
            first_lead,
        }
    }

    /// Create and classify a new `Method`, given its name and first lead
    pub fn with_name(name: String, first_lead: Block<Vec<String>>) -> Self {
        let class = FullClass::classify(&first_lead);
        Self {
            title: generate_title(&name, class, first_lead.stage()),
            name,
            class,
            first_lead,
        }
    }

    /// Parses a place notation string and creates a `Method` with that place notation and no
    /// labels.
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

    /// Creates a new `Method` from some place notation, adding a [`LABEL_LEAD_END`] on the first
    /// row.
    pub fn with_lead_end(name: String, block: &PnBlock) -> Self {
        let mut first_lead: Block<Vec<String>> = block.to_block_from_rounds();
        first_lead
            .get_annot_mut(0)
            .unwrap()
            .push(LABEL_LEAD_END.to_owned());
        Self::with_name(name, first_lead)
    }

    /////////////
    // GETTERS //
    /////////////

    /// Returns an [`Block`] of the first lead of this [`Method`], along with the labels applied to
    /// each [`Row`].
    #[inline]
    pub fn first_lead(&self) -> &Block<Vec<String>> {
        &self.first_lead
    }

    /// The last [`Row`] of the [first lead](Self::first_lead).  Don't confuse this with the
    /// **[lead head](Self::lead_head)**: 'lead **end**' refers to the treble's _handstroke_ lead,
    /// whereas 'lead **head**' refers to the treble's _backstroke_ lead.
    ///
    /// # Panics
    ///
    /// Panics if this method has a 0-length lead.
    #[inline]
    #[track_caller]
    pub fn lead_end(&self) -> &Row {
        self.first_lead
            .rows()
            .last()
            .expect("`Method::lead_end` called on a method with a 0-length lead")
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

    #[inline]
    pub fn class(&self) -> FullClass {
        self.class
    }

    /// Gets the **name** of this `Method` - i.e. the [`title`](Self::title) without the
    /// classification or [`Stage`].  Take Bristol Major as an example: its name is
    /// `"Bristol"` but its title is `"Bristol Surprise Major"`.
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the **title** of this `Method` - i.e. including the classification or [`Stage`].
    /// Take Bristol Major as an example: its name is `"Bristol"` but its title is `"Bristol
    /// Surprise Major"`.
    pub fn title(&self) -> &str {
        self.title.as_ref()
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

    /// Returns an [`Block`] representing the plain course of this method
    pub fn plain_course(&self) -> Block<RowAnnot> {
        // Create a copy of `self.first_lead` where each row is also annotated by its index within
        // this lead
        let first_lead_with_indices = self
            .first_lead
            .clone_map_annots_with_index(|i, labels| RowAnnot::new(i, labels));

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
    pub fn add_label(&mut self, index: usize, label: String) {
        let labels = self.first_lead.get_annot_mut(index).unwrap();
        if !labels.contains(&label) {
            labels.push(label);
        }
    }

    /// Same as `self.set_label(0, Some(LABEL_LEAD_END.to_owned()))`
    pub fn set_lead_end_label(&mut self) {
        self.add_label(0, LABEL_LEAD_END.to_owned())
    }

    /// Returns the label at a given index, panicking if the index is out of range
    // TODO: Make this not panic
    pub fn get_labels(&self, index: usize) -> &[String] {
        self.first_lead.get_annot(index).unwrap()
    }

    /// An [`Iterator`] over the sub-lead indices of a particular lead label.
    pub fn label_indices<'s>(&'s self, label: &'s str) -> impl Iterator<Item = usize> + 's {
        self.first_lead
            .annots()
            .positions(move |labels| labels.iter().any(|l| l == label))
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
    pub sub_lead_idx: usize,
    pub labels: &'meth [String],
}

impl<'meth> RowAnnot<'meth> {
    fn new(sub_lead_idx: usize, labels: &'meth [String]) -> Self {
        Self {
            sub_lead_idx,
            labels,
        }
    }
}
