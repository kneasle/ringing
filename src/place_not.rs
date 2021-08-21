//! Module for parsing and handling place notation

use crate::{AnnotBlock, Bell, IncompatibleStages, Row, RowBuf, SameStageVec, Stage};
use itertools::Itertools;
use std::{
    fmt::{Debug, Display, Formatter},
    ops::Range,
};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ParseError {
    PlaceOutOfStage { place: usize, stage: Stage },
    AmbiguousPlacesBetween { p: usize, q: usize },
    OddStageCross { stage: Stage },
    NoPlacesGiven,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::OddStageCross { stage } => {
                write!(f, "Cross notation for odd stage {}", stage)
            }
            ParseError::PlaceOutOfStage { place, stage } => {
                write!(
                    f,
                    "Place '{}' is out stage {}",
                    Bell::from_index(*place),
                    stage
                )
            }
            ParseError::AmbiguousPlacesBetween { p, q } => write!(
                f,
                "Ambiguous gap of {} bells between places '{}' and '{}'.",
                q - p - 1,
                Bell::from_index(*p),
                Bell::from_index(*q)
            ),
            ParseError::NoPlacesGiven => {
                write!(f, "No places given.  Use 'x' or '-' for a cross.")
            }
        }
    }
}

/// A single piece of place notation on any [`Stage`].
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct PlaceNot {
    /// A **0-indexed** list of which places are made during this `PlaceNot`.  We maintain the
    /// following invariants:
    /// - The places are fully expanded and unambiguous.  This means that every place made is
    ///   explicitly stored, with no implicit or ambiguous places.
    ///
    ///   For example, suppose the [`Stage`] is [`MAJOR`](Stage::MAJOR).
    ///    - The place notation "4" has an implicit place made at lead and so would be stored as
    ///      `vec![0, 3]`.
    ///    - The place notation "146" has an implicit place made in 5ths, so would be
    ///      stored as `vec![0, 3, 4, 5]`.
    /// - The places are stored **in ascending order**.  So "4817" would be stored as
    ///   `vec![0, 3, 6, 7]`.
    ///
    /// Enforcing these invariants improves the the speed of permutation and equality tests at the
    /// cost of (slightly) slower parsing, but I think this trade-off is justified since all
    /// `PlaceNot`s are parsed only once but permuting/equality tests happen many many times.
    places: Vec<usize>,
    /// The [`Stage`] that this `PlaceNot` is intended to be used for.
    stage: Stage,
}

impl PlaceNot {
    /// Parse a string, interpreting it as a single `PlaceNot` of a given [`Stage`].  Like
    /// [`RowBuf::parse_with_stage`], this ignores chars that don't correspond to valid [`Bell`]
    /// names, including `&`, `.`, `,` and `+` which have reserved meanings in blocks of place
    /// notation.  This will expand implicit places (even between two written places) but will fail
    /// if there is any kind of ambiguity, returning a [`ParseError`] describing the problem.  This
    /// also runs in `O(n)` time except for sorting the places which takes `O(n log n)` time.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Stage, PlaceNot, place_not::ParseError};
    ///
    /// // Parsing a valid place notation is OK
    /// assert_eq!(PlaceNot::parse("14", Stage::MAJOR)?.to_string(), "14");
    /// // Ordering and rogue chars don't matter
    /// assert_eq!(PlaceNot::parse("  4|7~18", Stage::ROYAL)?.to_string(), "1478");
    /// // Implicit places will be expanded
    /// assert_eq!(PlaceNot::parse("467", Stage::MAXIMUS)?.to_string(), "14567T");
    ///
    /// // Parsing invalid or ambiguous PN is not OK, and warns you of the problem
    /// assert_eq!(
    ///     PlaceNot::parse("14T", Stage::MAJOR).unwrap_err().to_string(),
    ///     "Place 'T' is out stage Major"
    /// );
    /// assert_eq!(
    ///     PlaceNot::parse("15", Stage::MAJOR).unwrap_err().to_string(),
    ///     "Ambiguous gap of 3 bells between places '1' and '5'."
    /// );
    /// # Ok::<(), ParseError>(())
    /// ```
    pub fn parse(s: &str, stage: Stage) -> Result<Self, ParseError> {
        // If the string is any one of the cross strings, then return CROSS
        if s.len() == 1 && s.chars().next().map(CharMeaning::from) == Some(CharMeaning::Cross) {
            return Self::cross(stage).ok_or(ParseError::OddStageCross { stage });
        }
        // Parse the string into bell indices, ignoring any invalid characters
        let mut parsed_places: Vec<usize> = s
            .chars()
            .filter_map(Bell::from_name)
            .map(Bell::index)
            .collect();
        // Sort the places into ascending order (unstable sort doesn't matter for usizes)
        parsed_places.sort_unstable();
        // Convert this unsorted slice into a PlaceNot, or return an error.  The unsafety is OK
        // because we have just sorted the slice
        unsafe { Self::from_sorted_slice(&parsed_places, stage) }
    }

    /// Creates a new `PlaceNot` from a sorted slice of places, performing bounds checks and
    /// returning errors if necessary.
    ///
    /// # Safety
    ///
    /// This function is safe if `parsed_places` is sorted into increasing order
    pub unsafe fn from_sorted_slice(
        parsed_places: &[usize],
        stage: Stage,
    ) -> Result<Self, ParseError> {
        // Check if we were given no places (I'm making this an error because '-' should be used
        // instead)
        parsed_places.last().ok_or(ParseError::NoPlacesGiven)?;
        // Check if any of the bells are out of range
        if let Some(out_of_range_place) = parsed_places.last().filter(|p| **p >= stage.num_bells())
        {
            return Err(ParseError::PlaceOutOfStage {
                place: *out_of_range_place,
                stage,
            });
        }

        // Rebuild to a new Vec when adding places to avoid quadratic behaviour
        let mut places = Vec::with_capacity(parsed_places.len() + 5);
        // Add implicit place in lead
        if parsed_places.first().filter(|p| *p % 2 == 1).is_some() {
            places.push(0)
        }
        // Copy the contents of `parsed_places`, inserting implicit places where necessary
        for (p, q) in parsed_places.iter().copied().tuple_windows() {
            // Add `p` to `places`
            places.push(p);
            // Check if there is an implicit place made between these, or if the place notation is
            // ambiguous
            let num_intermediate_places = q - p - 1;
            if num_intermediate_places == 1 {
                places.push(p + 1);
            } else if num_intermediate_places % 2 == 1 {
                // Any other even number causes an error
                return Err(ParseError::AmbiguousPlacesBetween { p, q });
            }
            // `q` will be pushed in the next loop iteration
        }
        // Copy the last element from `places`.  This is a special case, because `tuple_windows`
        // won't return the last element as the first element of a tuple window (because there's
        // nothing to pair it with)
        if let Some(p) = parsed_places.last() {
            places.push(*p)
        }
        // Add implicit place at the back if necessary
        if parsed_places
            .last()
            .filter(|p| (stage.num_bells() - *p) % 2 == 0)
            .is_some()
        {
            places.push(stage.num_bells() - 1)
        }
        // Create struct and return.  We don't need to sort `places`, because we only pushed to it
        // in ascending order.
        Ok(PlaceNot { places, stage })
    }

    /// Returns a new `PlaceNot` representing the 'cross' notation on a given stage.  This will
    /// fail if `stage` doesn't have an even number of bells.
    ///
    /// # Example
    /// ```
    /// use bellframe::{PlaceNot, Stage};
    ///
    /// // These are crosses
    /// assert_eq!(
    ///     PlaceNot::cross(Stage::MAJOR).unwrap(),
    ///     PlaceNot::parse("x", Stage::MAJOR)?
    /// );
    /// # Ok::<(), bellframe::place_not::ParseError>(())
    /// ```
    pub fn cross(stage: Stage) -> Option<Self> {
        if stage.num_bells() % 2 == 0 {
            Some(PlaceNot {
                places: Vec::new(),
                stage,
            })
        } else {
            None
        }
    }

    /// Checks if this `PlaceNot` corresponds to the 'cross' notation.
    ///
    /// # Example
    /// ```
    /// use bellframe::{PlaceNot, Stage};
    ///
    /// // These are crosses
    /// assert!(PlaceNot::cross(Stage::MAJOR).unwrap().is_cross());
    /// assert!(PlaceNot::parse("x", Stage::MAJOR)?.is_cross());
    /// // These are not
    /// assert!(!PlaceNot::parse("14", Stage::MAJOR)?.is_cross());
    /// assert!(!PlaceNot::parse("3", Stage::TRIPLES)?.is_cross());
    /// # Ok::<(), bellframe::place_not::ParseError>(())
    /// ```
    #[inline(always)]
    pub fn is_cross(&self) -> bool {
        self.places.is_empty()
    }

    /// Checks whether a given `place` is made in this `PlaceNot`.
    #[inline(always)]
    pub fn contains(&self, place: usize) -> bool {
        self.places.contains(&place)
    }

    /// Checks whether internal places are made in this `PlaceNot`
    #[inline(always)]
    pub fn has_internal_places(&self) -> bool {
        let has_lead = self.places.first() == Some(&0);
        let has_lie = self.places.last() == Some(&(self.stage.num_bells() - 1));
        let num_external_places = if has_lead { 1 } else { 0 } + if has_lie { 1 } else { 0 };
        self.places.len() > num_external_places
    }

    /// Returns the [`Stage`] of this `PlaceNot`
    #[inline(always)]
    pub fn stage(&self) -> Stage {
        self.stage
    }

    /// Returns the [`PlaceNot`] that goes between two [`Row`]s, or `None` if the [`Row`]s are not
    /// adjacent.
    pub fn pn_between(r1: &Row, r2: &Row) -> Option<PlaceNot> {
        if r1.stage() != r2.stage() {
            return None;
        }

        let mut places = Vec::<usize>::new();
        let mut bell_pair_iter = r1.bell_iter().zip_eq(r2.bell_iter()).enumerate();
        while let Some((place, (b1, b2))) = bell_pair_iter.next() {
            // If b1 and b2 are the same, then a place must be made
            if b1 == b2 {
                places.push(place);
            } else {
                // Otherwise, we expect b1 and b2 to swap round in the next place:
                // ... b1  b2 ...
                //      (to)
                // ... b2  b1 ...
                if Some((place + 1, (b2, b1))) != bell_pair_iter.next() {
                    return None;
                }
            }
        }

        Some(Self {
            places,
            stage: r1.stage(),
        })
    }

    /// Returns a [`RowBuf`] representing the same transposition as this `PlaceNot`.
    pub fn transposition(&self) -> RowBuf {
        let mut row = RowBuf::rounds(self.stage());
        // This unsafety is OK because the `row` has the same stage as `self`
        unsafe {
            self.permute_unchecked(&mut row);
        }
        row
    }

    /// Uses this `PlaceNot` to perform an in-place permutation of a given [`Row`].  If you want to
    /// to preserve the old [`Row`], then use [`permute_new`](Self::permute_new).
    pub fn permute(&self, row: &mut Row) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(row.stage(), self.stage)?;
        unsafe { self.permute_unchecked(row) };
        Ok(())
    }

    /// Uses this `PlaceNot` to perform an in-place permutation of a given [`Row`], **without**
    /// checking that the [`Stage`]s match.  If you want to to preserve the old [`Row`], then use
    /// [`permute_new_unchecked`](Self::permute_new).
    ///
    /// # Safety
    ///
    /// This function is safe to use only when `self.stage() == row.stage()`.
    pub unsafe fn permute_unchecked(&self, row: &mut Row) {
        let mut places = self.places.iter().copied().peekable();
        let mut i = 0;
        while i < self.stage.num_bells() {
            if places.peek() == Some(&i) {
                // If this PN contains a place at this index, then no modification is necessary but
                // we do need to consume the place so we move on the iterator
                places.next();
                i += 1;
            } else {
                // If this isn't a place, then we know by invariant that i + 1 is also not a place
                // (or out of range), so we perform a swap and move on by two bells
                row.swap(i, i + 1);
                i += 2;
            }
        }
    }

    /// Uses this `PlaceNot` to permute a given [`Row`], preserving the old copy and returning a
    /// new [`Row`].  This checks that the [`Stage`]s are equal, and is therefore safe.
    pub fn permute_new(&self, row: &Row) -> Result<RowBuf, IncompatibleStages> {
        IncompatibleStages::test_err(row.stage(), self.stage)?;
        Ok(unsafe { self.permute_new_unchecked(row) })
    }

    /// Uses this `PlaceNot` to permute a given [`Row`], preserving the old copy and returning a
    /// new [`Row`].
    ///
    /// # Safety
    ///
    /// This function is safe to use only when `self.stage() == row.stage()`.
    pub unsafe fn permute_new_unchecked(&self, row: &Row) -> RowBuf {
        let mut new_row = row.to_owned();
        self.permute_unchecked(&mut new_row);
        new_row
    }
}

impl Debug for PlaceNot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "PlaceNot({})", self)
    }
}

impl Display for PlaceNot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_cross() {
            // Always display cross notation as '-' to avoid confusion with bell names
            write!(f, "-")
        } else {
            // Otherwise concatenate all the bell names together
            for p in &self.places {
                write!(f, "{}", Bell::from_index(*p))?;
            }
            Ok(())
        }
    }
}

/// The possible ways that parsing a block of place notations could fail
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum PnBlockParseError {
    /// A '+' was found somewhere other than the start of a block (e.g. in `16x16+16,12`).  The
    /// argument refers to the byte index of the location of the '+' within the parse string.
    PlusNotAtBlockStart(usize),
    /// One of the pieces of place notation was invalid.  The [`Range`] points to the byte range
    /// within the input string where the invalid place notation string was found, whereas the
    /// [`ParseError`] describes the problem.
    PnError(Range<usize>, ParseError),
    /// The string represents a block with no place notations.  This would violate the invariants
    /// of [`PnBlock`], so is an error.
    EmptyBlock,
}

impl Display for PnBlockParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PnBlockParseError::EmptyBlock => write!(f, "`PnBlock`s can't be empty."),
            PnBlockParseError::PnError(range, err) => {
                write!(f, "Error parsing PN at index {}: {}", range.start, err)
            }
            PnBlockParseError::PlusNotAtBlockStart(index) => {
                write!(f, "'+' at index {} is not at the start of a block.", index)
            }
        }
    }
}

impl std::error::Error for PnBlockParseError {}

/// The different ways a `PnBlock` could be found to be invalid
#[derive(Clone, Debug, Copy, Eq, PartialEq, Hash)]
pub enum InvalidPnBlockError {
    IncompatibleStages(usize, IncompatibleStages),
    EmptyBlock,
}

impl Display for InvalidPnBlockError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidPnBlockError::IncompatibleStages(ind, e) => write!(
                f,
                "Incompatible stages: First PN has stage {}, whereas the {}th has {}",
                e.lhs_stage, ind, e.rhs_stage
            ),
            InvalidPnBlockError::EmptyBlock => write!(f, "`PnBlock`s can't be empty."),
        }
    }
}

impl std::error::Error for InvalidPnBlockError {}

/// A contiguous block of [`PlaceNot`]s.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PnBlock {
    /// The underlying [`PlaceNot`]s that make up this block.  This has to satisfy the following
    /// invariants:
    /// - `pns` cannot be empty, since that would correspond to a zero-length [`Block`], which is
    ///   not allowed
    /// - All the [`PlaceNot`]s must have the same [`Stage`].
    pns: Vec<PlaceNot>,
}

// PnBlocks can't have zero length, so `is_empty` is unnecessary
#[allow(clippy::len_without_is_empty)]
impl PnBlock {
    /// Parse a string slice into a `PnBlock`, checking for ambiguity and correctness.  This also
    /// expands symmetric blocks and implicit places.
    pub fn parse(s: &str, stage: Stage) -> Result<Self, PnBlockParseError> {
        let address_of_start_of_s = s.as_ptr() as usize;
        let mut pns: Vec<PlaceNot> = Vec::new();
        // A re-usuable chunk of memory used to store the unexpanded version of a symblock before
        // copying it into `pns`.
        let mut sym_block_buf: Vec<PlaceNot> = Vec::new();
        let is_single_block = !s.contains(',');
        // Split `s` into symmetric blocks, which are delimited by `,`
        for sym_block in s.split(',') {
            // Calculate the index of the start of this block within `s`, so that errors can be
            // pinpointed accurately
            let byte_offset = sym_block.as_ptr() as usize - address_of_start_of_s;
            // Parse this symblock as an asymmetric block into `sym_block_buf`
            let is_asymmetric =
                Self::parse_asym_block(sym_block, byte_offset, stage, &mut sym_block_buf)?;

            // Handle the output of parsing the current block
            if is_single_block || is_asymmetric {
                pns.extend(sym_block_buf.drain(..));
            } else {
                // Clone sym_block_buf into `pns` in order
                pns.extend_from_slice(&sym_block_buf);
                // **Move** pns except the last one from sym_block_buf in reverse order
                pns.extend(sym_block_buf.drain(..).rev().skip(1));
            }
        }
        // Return an error if pns is empty, otherwise construct the block
        if pns.is_empty() {
            Err(PnBlockParseError::EmptyBlock)
        } else {
            Ok(PnBlock { pns })
        }
    }

    fn parse_asym_block(
        block: &str,
        byte_offset: usize,
        stage: Stage,
        buf: &mut Vec<PlaceNot>,
    ) -> Result<bool, PnBlockParseError> {
        // Check that the buffer is empty -- it should be, because this will only be used in
        // `Self::parse`
        debug_assert!(buf.is_empty());
        // Create an iterator over the chars in block that we will then read from left to right and
        // parse
        let mut tok_indices = block
            .char_indices()
            .map(|(i, c)| (i + byte_offset, CharMeaning::from(c)))
            // Insert a 'fake' delimiter at the end, to make sure that the last chunk of place
            // notation is not ignored
            .chain(std::iter::once((
                byte_offset + block.len(),
                CharMeaning::Delimiter,
            )))
            // We need one a lookahead of one char to make parsing easier
            .peekable();

        /* Step 1: Skip meaningless chars at the left of the string */
        loop {
            if let Some((_i, c)) = tok_indices.peek() {
                if matches!(c, CharMeaning::Delimiter | CharMeaning::Unknown) {
                    tok_indices.next();
                    continue;
                }
            }
            break;
        }

        /* Step 2: If the first non-delimiter char we see represents an asymmetric block, then
         * consume it and log the asymmetricness of the block */
        let is_asymmetric = matches!(tok_indices.peek(), Some((_i, CharMeaning::Asym)));
        if is_asymmetric {
            // Consume the asymmetric-block char, and any rogue delimiters after it
            tok_indices.next();
        }

        // A buffer used to accumulate the block of places that are currently being parsed
        let mut places: Vec<usize> = Vec::new();
        // Tracks the index of the first byte in the chunk of PN currently being read.  This is
        // used so that we can return a byte range in the case of an error
        let mut current_pn_start_index = 0;
        for (i, m) in tok_indices {
            let index = i + byte_offset;
            match m {
                // If the char is a bell name, then add it to the places
                CharMeaning::Bell(b) => {
                    if places.is_empty() {
                        // If this was the first place of the pn chunk, then we store its index as
                        // the start of this pn block
                        current_pn_start_index = index;
                    }
                    places.push(b.index());
                }
                // If it's a cross notation or a delimiter, then we create a new PlaceNot out of
                // the places we've collected so far and push it to `buf`
                CharMeaning::Cross | CharMeaning::Delimiter => {
                    if !places.is_empty() {
                        // Create a new place notation from `places`, reporting the error if
                        // necessary
                        places.sort_unstable();
                        // This unsafety is OK, because we have just sorted the slice
                        let new_pn = unsafe {
                            PlaceNot::from_sorted_slice(&places, stage).map_err(|e| {
                                PnBlockParseError::PnError(current_pn_start_index..index + 1, e)
                            })
                        }?;
                        places.clear();
                        // Push the new place notation to the buffer
                        buf.push(new_pn);
                    }
                }
                // A '+' (for asymmetric block) not at the start of a block is an error
                CharMeaning::Asym => return Err(PnBlockParseError::PlusNotAtBlockStart(index)),
                // Unknown characters are ignored
                CharMeaning::Unknown => continue,
            }
            // Push a cross notation if we see it, making sure to any errors
            if m == CharMeaning::Cross {
                buf.push(
                    PlaceNot::cross(stage)
                        .ok_or(ParseError::OddStageCross { stage })
                        .map_err(|e| PnBlockParseError::PnError(index..index + 1, e))?,
                );
            }
        }

        Ok(is_asymmetric)
    }

    /// Creates a new `PnBlock` from an [`Iterator`] of [`PlaceNot`]s, checking that the resulting
    /// `PnBlock` is valid (i.e. all the stages match and the `PnBlock` contains at least one
    /// [`PlaceNot`]).
    // This function returns a `Result`, so we can't use the `FromIterator` trait.  Anyway, I don't
    // think this is too confusing because we won't implement `FromIterator` on `PnBlock` anyway.
    #[allow(clippy::should_implement_trait)]
    pub fn from_iter(
        iter: impl IntoIterator<Item = PlaceNot>,
    ) -> Result<Self, InvalidPnBlockError> {
        // PERF: We could possibly avoid allocating if `iter` turns out to be invalid
        Self::from_vec(iter.into_iter().collect())
    }

    /// Creates a new `PnBlock` from a [`Vec`] of [`PlaceNot`]s, checking that the resulting
    /// `PnBlock` is valid (i.e. all the stages match and the `PnBlock` contains at least one
    /// [`PlaceNot`]).
    pub fn from_vec(pns: Vec<PlaceNot>) -> Result<Self, InvalidPnBlockError> {
        // Get the first stage and check that the block isn't empty
        let first_stage = pns.first().ok_or(InvalidPnBlockError::EmptyBlock)?.stage;
        // Check that all of the stages match
        for (i, pn) in pns.iter().enumerate().skip(1) {
            IncompatibleStages::test_err(first_stage, pn.stage)
                .map_err(|e| InvalidPnBlockError::IncompatibleStages(i, e))?;
        }
        // If all these checks pass, create a new block
        Ok(unsafe { Self::from_vec_unchecked(pns) })
    }

    /// Creates a new `PnBlock` from a [`Vec`] of [`PlaceNot`]s, **without** checking that the resulting
    /// `PnBlock` is valid.
    ///
    /// # Safety
    ///
    /// This is only safe if:
    /// - All the [`Stage`]s of the [`PlaceNot`]s match
    /// - The [`Vec`] is non-empty
    pub unsafe fn from_vec_unchecked(pns: Vec<PlaceNot>) -> Self {
        PnBlock { pns }
    }

    /// Returns an iterator over the [`PlaceNot`]s contained in this `PnBlock`
    #[inline]
    pub fn place_nots(&self) -> std::slice::Iter<PlaceNot> {
        self.pns.iter()
    }

    /// The [`Stage`] of this `PnBlock`.
    #[inline]
    pub fn stage(&self) -> Stage {
        // This index cannot fail, because we maintain an invariant that `self.pns` always has at
        // least one element.
        self.pns[0].stage
    }

    /// The number of [`PlaceNot`]s in this `PnBlock`.  This is also the `len` of any [`Block`]
    /// generated by applying this `PnBlock` to some [`Row`].
    #[inline]
    pub fn len(&self) -> usize {
        self.pns.len()
    }

    /// Generates the [`Row`]s which follow from applying `self` to a given [`Row`].  The resulting
    /// [`SameStageVec`] has length one greater than that of `self`, because it starts with
    /// `start_row` and then adds one new [`Row`] per [`PlaceNot`] in `self`.
    pub fn to_rows(&self, start_row: RowBuf) -> Result<SameStageVec, IncompatibleStages> {
        IncompatibleStages::test_err(start_row.stage(), self.stage())?;

        let mut rows = SameStageVec::from_row_buf(start_row.clone());
        let mut current_row = start_row;
        for pn in &self.pns {
            // This unsafety is OK because:
            // - all PlaceNots in `self` have the same stage (by invariant)
            // - we return with an error if `start_row` has a different `stage` to this `PnBlock`
            // => `pn.stage() == current_row.stage()`
            unsafe { pn.permute_unchecked(&mut current_row) };
            rows.push(&current_row).unwrap(); // Unwrap is fine, because we've already checked that
                                              // the stages match
        }
        Ok(rows)
    }

    /// Generates the [`AnnotBlock`] of this `PnBlock`, starting at some `start_row`.  All the
    /// annotations are generated by `A::default()`.
    pub fn to_block<A>(&self, start_row: RowBuf) -> Result<AnnotBlock<A>, IncompatibleStages>
    where
        A: Default,
    {
        let rows = self.to_rows(start_row)?;
        Ok(AnnotBlock::with_default_annots(rows))
    }

    /// Generates the [`AnnotBlock`] of this `PnBlock`, starting at [rounds](RowBuf::rounds).  All
    /// the annotations are generated by `A::default()`.
    pub fn to_block_from_rounds<A>(&self) -> AnnotBlock<A>
    where
        A: Default,
    {
        // Unwrapping here is fine, because we provide a `RowBuf` that has the same `Stage` as
        // `self`
        self.to_block(RowBuf::rounds(self.stage())).unwrap()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum CharMeaning {
    Bell(Bell),
    Delimiter,
    Cross,
    Asym,
    Unknown,
}

impl From<char> for CharMeaning {
    fn from(c: char) -> Self {
        if let Some(b) = Bell::from_name(c) {
            CharMeaning::Bell(b)
        } else {
            match c {
                '+' => CharMeaning::Asym,
                ' ' | '.' => CharMeaning::Delimiter,
                'x' | 'X' | '-' => CharMeaning::Cross,
                _ => CharMeaning::Unknown,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ParseError;
    use crate::{Block, PlaceNot, PnBlock, RowBuf, Stage};

    #[test]
    fn parse_ok() {
        for (inp_string, stage, exp_places) in &[
            // No implict places
            ("14", Stage::MAJOR, vec![0, 3]),
            ("1256", Stage::MINOR, vec![0, 1, 4, 5]),
            ("16", Stage::MAXIMUS, vec![0, 5]),
            ("127", Stage::TRIPLES, vec![0, 1, 6]),
            ("3", Stage::CATERS, vec![2]),
            // Implicit places in lead
            ("4", Stage::MINIMUS, vec![0, 3]),
            ("234", Stage::MINOR, vec![0, 1, 2, 3]),
            ("478", Stage::MAJOR, vec![0, 3, 6, 7]),
            ("470", Stage::ROYAL, vec![0, 3, 6, 9]),
            ("2", Stage::TWO, vec![0, 1]),
            // Implicit places in lie
            ("3", Stage::MAJOR, vec![2, 7]),
            ("12", Stage::DOUBLES, vec![0, 1, 4]),
            ("1", Stage::TWO, vec![0, 1]),
            ("14", Stage::CINQUES, vec![0, 3, 10]),
            // Implicit places between two other places
            ("146", Stage::MAJOR, vec![0, 3, 4, 5]),
            ("13", Stage::SINGLES, vec![0, 1, 2]),
            ("13", Stage::TRIPLES, vec![0, 1, 2]),
            ("135", Stage::TRIPLES, vec![0, 1, 2, 3, 4]),
            // Implicit places in multiple places
            ("23", Stage::MAJOR, vec![0, 1, 2, 7]),
            ("4", Stage::TRIPLES, vec![0, 3, 6]),
            ("45", Stage::MINOR, vec![0, 3, 4, 5]),
            ("46", Stage::CATERS, vec![0, 3, 4, 5, 8]),
            // Out of order places
            ("6152", Stage::MINOR, vec![0, 1, 4, 5]),
            ("342", Stage::MINOR, vec![0, 1, 2, 3]),
            ("32", Stage::MAJOR, vec![0, 1, 2, 7]),
            ("21", Stage::DOUBLES, vec![0, 1, 4]),
            // Misc characters
            ("2\t  1 |", Stage::DOUBLES, vec![0, 1, 4]),
            ("   6\n?15&2!", Stage::MINOR, vec![0, 1, 4, 5]),
        ] {
            let pn = PlaceNot::parse(inp_string, *stage).unwrap();
            assert_eq!(pn.stage, *stage);
            assert_eq!(pn.places, *exp_places);
        }
    }

    #[test]
    fn parse_err_odd_bell_cross() {
        for &stage in &[
            Stage::SINGLES,
            Stage::DOUBLES,
            Stage::TRIPLES,
            Stage::CATERS,
            Stage::CINQUES,
            Stage::SEPTUPLES,
            Stage::SEXTUPLES,
        ] {
            for cross_not in &["x", "X", "-"] {
                assert_eq!(
                    PlaceNot::parse(*cross_not, stage),
                    Err(ParseError::OddStageCross { stage })
                );
            }
        }
    }

    #[test]
    fn parse_err_place_out_of_stage() {
        for &(inp_string, stage, place) in &[
            ("148", Stage::MINIMUS, 7),
            ("91562", Stage::MINOR, 8),
            ("  3", Stage::TWO, 2),
        ] {
            assert_eq!(
                PlaceNot::parse(inp_string, stage),
                Err(ParseError::PlaceOutOfStage { stage, place })
            );
        }
    }

    #[test]
    fn parse_err_no_places_given() {
        for stage in 0..12 {
            assert_eq!(
                PlaceNot::parse("", Stage::from(stage)),
                Err(ParseError::NoPlacesGiven)
            );
        }
    }

    #[test]
    fn parse_err_ambiguous_gap() {
        for &(inp_string, stage, exp_p, exp_q) in &[
            // No implict places
            ("15", Stage::MAJOR, 0, 4),
            ("39", Stage::ROYAL, 2, 8),
            ("1925", Stage::MAXIMUS, 4, 8),
            ("1026", Stage::ROYAL, 1, 5),
        ] {
            assert_eq!(
                PlaceNot::parse(inp_string, stage),
                Err(ParseError::AmbiguousPlacesBetween { p: exp_p, q: exp_q })
            );
        }
    }

    #[test]
    fn parse_block_ok() {
        let equal_blocks = [
            (Stage::SINGLES, "1.3", "1   .  3", 2),
            (Stage::MINIMUS, "-4-3-1-..2", "x14x34x14x12", 8),
            (Stage::MINIMUS, "-4-3-1-..2", "-14x34-14x12", 8),
            (Stage::MINIMUS, "x14x14,12", "-14-14-14-12", 8),
            (Stage::TRIPLES, "2.3", "2,3", 2),
            (Stage::MAJOR, "x1,1x,x1,1x,x1,2", "-18-18-18-18,12", 16),
            (Stage::MAJOR, "+x4x1,", "x14x18", 4),
            (Stage::MAXIMUS, "x4x1,", "x14x1Tx14x", 7),
            (Stage::MAXIMUS, "xxx1", "---1T", 4),
            (Stage::MAXIMUS, "x   -\tx1", "---1T", 4),
        ];

        for &(stage, s1, s2, exp_len) in &equal_blocks {
            println!("Parsing {} vs {}", s1, s2);
            let b1 = PnBlock::parse(s1, stage).unwrap();
            let b2 = PnBlock::parse(s2, stage).unwrap();
            assert_eq!(b1, b2);
            assert_eq!(b1.len(), exp_len);
        }
    }

    #[test]
    fn pn_to_block() {
        let alnick_lead = "156342
516324
153642
513462
531426
354162
351426
534162
354612
345621
436512
463521
643251
634215
362451
326415
236145
321654
326145
231654
213645
123465
214356
124365
123456";
        let plain_bob_major_lead = "12345678
21436587
24163857
42618375
46281735
64827153
68472513
86745231
87654321
78563412
75836142
57381624
53718264
35172846
31527486
13254768
13527486";

        let equal_blocks = [
            (Stage::MINOR, "34-36.14-12-36.14-14.36,12", alnick_lead), // Alnwick Surprise Minor
            (Stage::MINOR, "34-3.4-2-3.4-4.3,+2", alnick_lead),        // Alnwick Surprise Minor
            (Stage::MAJOR, "x18x18x18x18,12", plain_bob_major_lead),   // Plain Bob Major
        ];

        for &(stage, pn, block_str) in &equal_blocks {
            println!("Parsing {}", pn);
            let expected_block = Block::parse(block_str).unwrap();
            let b1: Block = PnBlock::parse(pn, stage)
                .unwrap()
                .to_block(expected_block.first_annot_row().unwrap().row().to_owned())
                .unwrap();
            assert_eq!(b1, expected_block);
        }
    }

    #[test]
    fn pn_between_rows() {
        #[track_caller]
        fn check_ok(r1: &str, r2: &str, exp_pn: &str) {
            let row1 = RowBuf::parse(r1).unwrap();
            let row2 = RowBuf::parse(r2).unwrap();
            let exp_pn = PlaceNot::parse(exp_pn, row1.stage()).unwrap();
            assert_eq!(PlaceNot::pn_between(&row1, &row2), Some(exp_pn));
        }

        check_ok("12345678", "13245678", "145678");
        check_ok("12345678", "13246587", "14");
        check_ok("18247653", "81246753", "3478");
    }
}
