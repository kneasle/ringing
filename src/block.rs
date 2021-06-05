//! A representation of a [`Block`] of ringing; i.e. a sort of 'multi-permutation' which takes a
//! starting [`Row`] and yields a sequence of permuted [`Row`]s.

use std::{
    fmt::{Display, Formatter},
    ops::Index,
};

use crate::{IncompatibleStages, InvalidRowError, Row, Stage};

/// All the possible ways that parsing a [`Block`] could fail
#[derive(Debug, Clone)]
pub enum ParseError {
    ZeroLengthBlock,
    InvalidRow {
        line: usize,
        err: InvalidRowError,
    },
    IncompatibleStages {
        line: usize,
        first_stage: Stage,
        different_stage: Stage,
    },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ZeroLengthBlock => write!(f, "Blocks can't have length 0"),
            ParseError::InvalidRow { line, err } => {
                write!(f, "Error parsing line {}: {}", line, err)
            }
            ParseError::IncompatibleStages {
                line,
                first_stage,
                different_stage,
            } => {
                write!(
                    f,
                    "Row on line {} has different stage ({}) to the first stage ({})",
                    line, different_stage, first_stage
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnotRow<A> {
    row: Row,
    annot: A,
}

impl<A> AnnotRow<A> {
    /// Creates a new `AnnotRow` from its parts
    #[inline]
    pub fn new(row: Row, annot: A) -> Self {
        AnnotRow { row, annot }
    }

    /// Creates a new `Row` with the default annotations
    #[inline]
    pub fn with_default(row: Row) -> Self
    where
        A: Default,
    {
        Self::new(row, A::default())
    }

    /// Gets the [`Row`] contained in this `AnnotRow`
    #[inline]
    pub fn row(&self) -> &Row {
        &self.row
    }

    /// Gets the [`Stage`] of this `AnnotRow`
    #[inline]
    pub fn stage(&self) -> Stage {
        self.row.stage()
    }

    /// Separates this `AnnotRow` into its raw parts
    #[inline]
    pub fn into_raw_parts(self) -> (Row, A) {
        (self.row, self.annot)
    }

    /// Sets the [`Row`] contained within this `AnnotRow`, without checking that the [`Stage`]s
    /// match.
    ///
    /// # Safety
    ///
    /// This is safe to call, so long as the [`Stage`] of `row` is equal to that of `self`
    #[inline]
    pub unsafe fn set_row_unchecked(&mut self, row: Row) {
        self.row = row;
    }

    /// Gets an immutable reference to the annotation of this `AnnotRow`
    #[inline]
    pub fn annot(&self) -> &A {
        &self.annot
    }

    /// Gets a mutable reference to the annotation of this `AnnotRow`
    #[inline]
    pub fn annot_mut(&mut self) -> &mut A {
        &mut self.annot
    }

    /// Applies a function to the annotation contained within this `AnnotRow`
    #[inline]
    pub fn map_annot<B>(self, f: impl Fn(A) -> B) -> AnnotRow<B> {
        AnnotRow::new(self.row, f(self.annot))
    }

    /// Clones the [`Row`] of `self`, applying a function to the annotation contained within this
    /// `AnnotRow`
    #[inline]
    pub fn clone_map_annot<B>(&self, f: impl Fn(&A) -> B) -> AnnotRow<B> {
        AnnotRow::new(self.row.clone(), f(&self.annot))
    }
}

/// An `AnnotBlock` with no annotations.
pub type Block = AnnotBlock<()>;

pub type AnnotRowIter<'b, A> = std::slice::Iter<'b, AnnotRow<A>>;

/// An `AnnotBlock` is in essence a multi-permutation: it describes the transposition of a single
/// start [`Row`] into many [`Row`]s, the first of which is always the one supplied.  The last
/// [`Row`] of an `AnnotBlock` is considered 'left-over', and represents the first [`Row`] that
/// should be rung after this `AnnotBlock`.
///
/// A few things to note about `Block`s:
/// - All `Block`s must have non-zero length.  Zero-length blocks cannot be created with `safe`
///   code, and will cause undefined behaviour or `panic!`s.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnotBlock<A> {
    /// The [`Row`]s making up this `Block`.
    ///
    /// A few important implementation details to note:
    /// 1. The last [`Row`] in `Block::rows` is 'left-over' - i.e. it shouldn't be used for truth
    ///    checking, and is used to generate the starting [`Row`] for the next `Block` that would
    ///    be rung after this one.
    ///
    /// We also enforce the following invariants:
    /// 1. `Block::rows` contains at least two [`Row`]s.  Zero-length `Block`s cannot be created
    ///    using `safe` code.
    /// 2. All the [`Row`]s in `Block::rows` must have the same [`Stage`].
    /// 3. The first [`Row`] should always equal `rounds`
    rows: Vec<AnnotRow<A>>,
}

// We don't need `is_empty`, because the length is guaruteed to be at least 1
#[allow(clippy::len_without_is_empty)]
impl<A> AnnotBlock<A> {
    /// Parse a multi-line [`str`]ing into an unannotated `Block`.  The last [`Row`] parsed will be
    /// considered 'left over' - i.e. it isn't directly part of this `Block` but rather will be the
    /// first [`Row`] of any `Block` which gets appended to this one.  Each [`Row`] has the default
    /// annotation (thus requiring that `A` implements [`Default`]).
    pub fn parse(s: &str) -> Result<Self, ParseError>
    where
        A: Default,
    {
        // We store the _inverse_ of the first Row, because for each row R we are solving the
        // equation `FX = R` where F is the first Row.  The solution to this is `X = F^-1 * R`, so
        // it makes sense to invert F once and then use that in all subsequent calculations.
        let mut inv_first_row: Option<Row> = None;
        let mut annot_rows: Vec<AnnotRow<A>> = Vec::new();
        for (i, l) in s.lines().enumerate() {
            // Parse the line into a Row, and fail if its either invalid or doesn't match the stage
            let parsed_row =
                Row::parse(l).map_err(|err| ParseError::InvalidRow { line: i, err })?;
            if let Some(inv_first_row) = &inv_first_row {
                if inv_first_row.stage() != parsed_row.stage() {
                    return Err(ParseError::IncompatibleStages {
                        line: i,
                        first_stage: inv_first_row.stage(),
                        different_stage: parsed_row.stage(),
                    });
                }
                // If all the checks passed, push the row
                annot_rows.push(AnnotRow::with_default(unsafe {
                    inv_first_row.mul_unchecked(&parsed_row)
                }));
            } else {
                // If this is the first Row, then push rounds and set the inverse first row
                inv_first_row = Some(!&parsed_row);
                annot_rows.push(AnnotRow::with_default(Row::rounds(parsed_row.stage())));
            }
        }
        // Return an error if the rows would form a zero-length block
        if annot_rows.len() <= 1 {
            return Err(ParseError::ZeroLengthBlock);
        }
        // Create a block from the newly parsed [`Row`]s.  This unsafety is OK, because we have
        // verified all the invariants
        Ok(unsafe { Self::from_annot_rows_unchecked(annot_rows) })
    }

    /// Creates a new `AnnotBlock` from a [`Vec`] of annotated [`Row`]s, checking that the result
    /// is valid.
    pub fn from_annot_rows(annot_rows: Vec<AnnotRow<A>>) -> Result<Self, ParseError> {
        assert!(annot_rows[0].row.is_rounds());
        if annot_rows.len() <= 1 {
            return Err(ParseError::ZeroLengthBlock);
        }
        let first_stage = annot_rows[0].row.stage();
        for (i, annot_row) in annot_rows.iter().enumerate().skip(1) {
            if annot_row.row.stage() != first_stage {
                return Err(ParseError::IncompatibleStages {
                    line: i,
                    first_stage,
                    different_stage: annot_row.row.stage(),
                });
            }
        }
        // This unsafety is OK because we've checked all the required invariants
        Ok(unsafe { Self::from_annot_rows_unchecked(annot_rows) })
    }

    /// Creates a new `AnnotBlock` from a [`Vec`] of annotated [`Row`]s, without performing any
    /// safety checks.
    ///
    /// # Safety
    ///
    /// This is safe when the following properties hold:
    /// - `rows` has length at least 2.  This is so that there is at least one [`Row`] in the
    ///   `AnnotBlock`, plus one leftover [`Row`].
    /// - All the `rows` have the same [`Stage`].
    pub unsafe fn from_annot_rows_unchecked(rows: Vec<AnnotRow<A>>) -> Self {
        AnnotBlock { rows }
    }

    /// Creates an empty `AnnotBlock` on a given [`Stage`] (i.e. an `AnnotBlock` containing only
    /// rounds as the leftover row).  **This function is wildly unsafe**; it should only be used if
    /// you are going to extend the block before passing it out of the `unsafe` boundary.
    ///
    /// # Safety
    ///
    /// This function is never safe on its own.  In order to make the result safe, you have to
    /// push more [`AnnotRow`]s onto the `AnnotBlock` (using [`AnnotBlock::extend_with`],
    /// [`AnnotBlock::extend_from_iter_transposed`], etc.).
    #[inline]
    pub unsafe fn empty(stage: Stage) -> Self
    where
        A: Default,
    {
        AnnotBlock {
            rows: vec![AnnotRow::new(Row::rounds(stage), A::default())],
        }
    }

    /// Gets the [`Stage`] of this `Block`.
    #[inline]
    pub fn stage(&self) -> Stage {
        self.rows[0].row.stage()
    }

    /// Gets the [`Row`] at a given index, along with its annotation.
    #[inline]
    pub fn get_row(&self, index: usize) -> Option<&Row> {
        self.get_annot_row(index).map(AnnotRow::row)
    }

    /// Gets an immutable reference to the annotation of the [`Row`] at a given index, if it
    /// exists.
    #[inline]
    pub fn get_annot(&self, index: usize) -> Option<&A> {
        self.get_annot_row(index).map(AnnotRow::annot)
    }

    /// Gets an mutable reference to the annotation of the [`Row`] at a given index, if it
    /// exists.
    #[inline]
    pub fn get_annot_mut(&mut self, index: usize) -> Option<&mut A> {
        self.rows.get_mut(index).map(AnnotRow::annot_mut)
    }

    /// Gets the [`Row`] at a given index, along with its annotation.
    #[inline]
    pub fn get_annot_row(&self, index: usize) -> Option<&AnnotRow<A>> {
        self.rows.get(index)
    }

    /// Gets the first [`Row`] of this `AnnotBlock`, along with its annotation.
    #[inline]
    pub fn first_annot_row(&self) -> &AnnotRow<A> {
        // This can't panic, because of the invariant disallowing zero-sized `AnnotBlock`s
        &self.rows[0]
    }

    /// Gets the length of this `Block` (excluding the left-over [`Row`]).  This is guarunteed to
    /// be at least 1.
    #[inline]
    pub fn len(&self) -> usize {
        self.rows.len() - 1
    }

    /// Returns an [`Iterator`] over all the [`Row`]s in this `AnnotBlock`, along with their
    /// annotations.
    #[inline]
    pub fn iter(&self) -> AnnotRowIter<'_, A> {
        self.rows.iter()
    }

    /// Returns an immutable reference to the slice of annotated [`Row`]s making up this [`Block`]
    #[inline]
    pub fn annot_rows(&self) -> &[AnnotRow<A>] {
        self.rows.as_slice()
    }

    /// Returns an [`Iterator`] over all the [`Row`]s in this `Block`, without their annotations.
    #[inline]
    pub fn rows(&self) -> impl Iterator<Item = &Row> + '_ {
        self.iter().map(AnnotRow::row)
    }

    /// Returns an [`Iterator`] over all the annotations in this `Block`.
    #[inline]
    pub fn annots(&self) -> impl Iterator<Item = &A> + '_ {
        self.iter().map(AnnotRow::annot)
    }

    /// Returns an [`Iterator`] yielding mutable references to the annotations in this `Block`.
    #[inline]
    pub fn annots_mut(&mut self) -> impl Iterator<Item = &mut A> + '_ {
        self.rows.iter_mut().map(AnnotRow::annot_mut)
    }

    /// Pre-multiplies every [`Row`] in this `Block` by another [`Row`].  The resulting `Block` is
    /// equivalent to `self` (inasmuch as the relations between the [`Row`]s are identical), but it
    /// will start from a different [`Row`].
    pub fn pre_mul(&mut self, perm_row: &Row) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(perm_row.stage(), self.stage())?;
        let mut row_buf = Row::empty();
        self.rows.iter_mut().for_each(|AnnotRow { row, .. }| {
            // Do in-place pre-multiplication using `row_buf` as a temporary buffer
            row_buf.clone_from(row);
            *row = unsafe { perm_row.mul_unchecked(&row_buf) };
        });
        Ok(())
    }

    /// Returns the 'left-over' [`Row`] of this `Block`, along with its annotation.  This [`Row`]
    /// represents the overall transposition of the `Block`, and should not be used when generating
    /// rows for truth checking.
    #[inline]
    pub fn leftover_annot_row(&self) -> &AnnotRow<A> {
        // We can safely unwrap here, because we enforce an invariant that `self.rows.len() > 0`
        self.rows.last().unwrap()
    }

    /// Returns the 'left-over' [`Row`] of this `Block`.  This [`Row`] represents the overall
    /// transposition of the `Block`, and should not be used when generating rows for truth
    /// checking.
    #[inline]
    pub fn leftover_row(&self) -> &Row {
        &self.leftover_annot_row().row
    }

    /// Returns a mutable reference to the annotation of the 'left-over' [`Row`] of this `Block`.
    #[inline]
    pub fn leftover_annot_mut(&mut self) -> &mut A {
        // We can safely unwrap here, because we enforce an invariant that `self.rows.len() > 0`
        &mut self.rows.last_mut().unwrap().annot
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function.
    pub fn map_annots<B>(self, f: impl Fn(A) -> B) -> AnnotBlock<B> {
        unsafe {
            AnnotBlock::from_annot_rows_unchecked(
                self.rows.into_iter().map(|a| a.map_annot(&f)).collect(),
            )
        }
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function.
    pub fn into_rows(self) -> Vec<Row> {
        self.rows
            .into_iter()
            .map(|annot_row| annot_row.row)
            .collect()
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function.
    pub fn prefix(&self, limit: usize) -> Self
    where
        A: Clone,
    {
        assert!(limit > 0);
        unsafe {
            AnnotBlock::from_annot_rows_unchecked(
                self.rows.iter().cloned().take(limit + 1).collect(),
            )
        }
    }

    /// Splits this `AnnotBlock` into two separate `AnnotBlock`s at a specified index.  This makes
    /// `self` shorter, whilst returning the remainder as a new `AnnotBlock` (along with its first
    /// [`Row`]).  This returns `None` without mutation if either of the blocks would have zero
    /// length.  During the course of this, the row at the split point will be used twice so the
    /// annotation is moved to the second block (and the leftover row of `self` is annotated with
    /// a default value).
    #[must_use]
    pub fn split(&mut self, index: usize) -> Option<(Row, Self)>
    where
        A: Default,
    {
        // Early return
        if index == 0 || index >= self.len() - 1 {
            return None;
        }
        // Firstly, record the first Row of the 2nd block and cache its inverse to avoid
        // recalculation
        let other_block_first_row = self.rows[index].row.clone();
        let inv_first_row = !&other_block_first_row;
        // Now, drain the rows out of `self`, transpose them and collect them in a new `Vec` to be
        // turned into the new `AnnotBlock`
        let new_rows: Vec<AnnotRow<A>> = self
            .rows
            .drain(index..)
            .map(|AnnotRow { row, annot }| AnnotRow::new(&inv_first_row * &row, annot))
            .collect();
        // The drain will have left `self` without a leftover Row, so we add it back in by cloning
        // `other_block_first_row`
        self.rows
            .push(AnnotRow::with_default(other_block_first_row.clone()));
        // Finally, construct the new Block.  The unsafety here is OK because:
        // - The new block has length >= 2, which is checked by the early return
        // - All the `Row`s have the same stage:
        //       all Rows in `self` have the same stage (by invariant)
        //    => all permuted copies of rows in `self` have the same stage (because permuting a Row
        //       can't change its stage)
        //    => all rows in `new_rows` must have the same stage
        // - new_rows[0] must start in rounds, because the first row is multiplied it by its own
        //   inverse, which always generates rounds
        Some((other_block_first_row, unsafe {
            Self::from_annot_rows_unchecked(new_rows)
        }))
    }

    /// Extend this `AnnotBlock` with [`AnnotRow`]s generated by a given [`Iterator`], transposing
    /// them so that the first new [`Row`] matches `self.leftover_row()`.
    pub fn extend_from_iter_transposed(
        &mut self,
        annot_rows: impl IntoIterator<Item = AnnotRow<A>>,
    ) -> Result<(), IncompatibleStages> {
        let mut transposition: Option<Row> = None;
        for annot_r in annot_rows {
            // Return error if the stages don't match
            IncompatibleStages::test_err(self.stage(), annot_r.row.stage())?;
            match &transposition {
                // If we're not on the first row, then push the transposed version of this row onto
                // self.rows
                Some(t) => self.rows.push(AnnotRow::new(
                    unsafe { t.mul_unchecked(annot_r.row()) },
                    annot_r.annot,
                )),
                // If we **are** on the first row, we calculate the transposition and then
                // overwrite the current leftover row in-place (bypassing the transposition
                // because we know that it will have no effect)
                None => {
                    transposition = Some(unsafe {
                        annot_r.row().tranposition_to_unchecked(self.leftover_row())
                    });
                    self.rows.last_mut().unwrap().annot = annot_r.annot;
                }
            };
        }
        Ok(())
    }

    /// Extend this `AnnotBlock` with the contents of another `AnnotBlock`.  This modifies `self`
    /// to have the effect of ringing `self` then `other`.  Note that this overwrites the
    /// leftover [`Row`] of `self`, replacing its annotation with that of `other`'s first [`Row`].
    pub fn extend_with(&mut self, other: Self) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(self.stage(), other.stage())?;
        // Remove the leftover row
        let leftover_row = self.rows.pop().unwrap().row;
        self.rows
            .extend(other.rows.into_iter().map(|AnnotRow { row, annot }| {
                AnnotRow::new(unsafe { leftover_row.mul_unchecked(&row) }, annot)
            }));
        Ok(())
    }

    /// Extend this `AnnotBlock` with the contents of another `AnnotBlock`, cloning the
    /// annotations.  This modifies `self` to have the effect of ringing `self` then `other`.  Note
    /// that this overwrites the leftover [`Row`] of `self`, replacing its annotation with that of
    /// `other`'s first [`Row`].
    pub fn extend_with_cloned(&mut self, other: &Self) -> Result<(), IncompatibleStages>
    where
        A: Clone,
    {
        IncompatibleStages::test_err(self.stage(), other.stage())?;
        // Remove the leftover row
        let leftover_row = self.rows.pop().unwrap().row;
        self.rows
            .extend(other.rows.iter().map(|AnnotRow { row, annot }| {
                AnnotRow::new(unsafe { leftover_row.mul_unchecked(row) }, annot.clone())
            }));
        Ok(())
    }
}

impl<A> Index<usize> for AnnotBlock<A> {
    type Output = AnnotRow<A>;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.rows[index]
    }
}
