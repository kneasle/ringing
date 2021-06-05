use crate::{method::LABEL_LEAD_END, Block, PnBlock, Row};

pub const NOTATION_BOB: char = '-';
pub const NOTATION_SINGLE: char = 's';

#[derive(Debug, Clone)]
pub struct Call {
    notation: char,
    location: String,
    covers: usize,
    block: Block,
}

// `Call`s can't have size 0 because it is backed by a `Block`, which enforces a non-zero size
// invariant.
#[allow(clippy::len_without_is_empty)]
impl Call {
    /// Creates a new `Call` from its parts
    #[inline]
    pub fn new(notation: char, location: String, covers: usize, block: Block) -> Self {
        Call {
            notation,
            location,
            covers,
            block,
        }
    }

    /// Creates a call with notation `'-'`, which covers only the lead end
    pub fn le_bob(pn_block: PnBlock) -> Self {
        Self::new(
            NOTATION_BOB,
            LABEL_LEAD_END.to_owned(),
            pn_block.len(),
            pn_block.to_block(),
        )
    }

    /// Creates a call with notation `'s'`, which covers only the lead end
    pub fn le_single(pn_block: PnBlock) -> Self {
        Self::new(
            NOTATION_SINGLE,
            LABEL_LEAD_END.to_owned(),
            pn_block.len(),
            pn_block.to_block(),
        )
    }

    /// Gets an [`Iterator`] over the rows in this `Call`
    #[inline]
    pub fn rows(&self) -> impl Iterator<Item = &Row> {
        self.block.rows()
    }

    /// Gets the number of [`Row`]s that will be covered by this `Call`
    #[inline]
    pub fn cover_len(&self) -> usize {
        self.covers
    }

    /// Gets the number of [`Row`]s that will be **generated** by this `Call`
    #[inline]
    pub fn len(&self) -> usize {
        self.block.len()
    }

    /// Gets the [`char`] that represents this `Call`.
    #[inline]
    pub fn notation(&self) -> char {
        self.notation
    }

    /// Gets the lead location that this `Call` occupies
    #[inline]
    pub fn location(&self) -> &str {
        &self.location
    }

    /// Returns a [`Row`] representing the overall transposition of this `Call` (i.e. the
    /// permutation which maps the [`Row`] where this `Call` starts to the [`Row`] where it ends).
    /// This is useful for generating things such as calling positions.
    #[inline]
    pub fn transposition(&self) -> &Row {
        self.block.leftover_row()
    }
}
