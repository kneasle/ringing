//! A representation of a [`Block`] of ringing; i.e. a sort of 'multi-permutation' which takes a
//! starting [`Row`] and yields a sequence of permuted [`Row`]s.

use std::{
    fmt::{Display, Formatter},
    iter::repeat_with,
    ops::Range,
};

use itertools::Itertools;

use crate::{
    row::same_stage_vec, utils::split_vec, Bell, IncompatibleStages, InvalidRowError, Row, RowBuf,
    SameStageVec, Stage,
};

/// All the possible ways that parsing a [`Block`] could fail
#[derive(Debug, Clone)]
pub enum ParseError {
    ZeroLengthBlock,
    InvalidRow {
        line: usize,
        err: InvalidRowError,
    },
    IncompatibleStages {
        line_index: usize,
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
                line_index: line,
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct AnnotRow<'b, A> {
    row: &'b Row,
    annot: &'b A,
}

impl<'b, A> AnnotRow<'b, A> {
    pub fn row(self) -> &'b Row {
        self.row
    }

    pub fn annot(self) -> &'b A {
        self.annot
    }
}

/// An `AnnotBlock` with no annotations.
pub type Block = AnnotBlock<()>;

/// A block of [`Row`], each of which can be given an annotation of any type.  Blocks can start
/// from any [`Row`], and can be empty.
///
/// All blocks must finish with a 'left-over' [`Row`].  This [`Row`] denotes the first [`Row`] of
/// any block rung **after** this one.  This is not considered part of the `AnnotBlock`, and
/// therefore cannot be annotated.  However, it is necessary - for example, if we create a `Block`
/// for the first lead of Cambridge and Primrose Surprise Minor then they would be identical except
/// for their 'left-over' row.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnotBlock<A> {
    /// The [`Row`]s making up this `Block`.
    ///
    /// **Invariant**: `row.len() >= 1`
    row_buffer: SameStageVec,
    /// The annotations on each [`Row`] in this `AnnotBlock`.
    ///
    /// **Invariant**: `rows.len() = annots.len() + 1`, because the 'left-over' row cannot be annotated.
    annots: Vec<A>,
}

impl<A> AnnotBlock<A> {
    //////////////////
    // CONSTRUCTORS //
    //////////////////

    /// Parse a multi-line [`str`]ing into an `AnnotBlock`, where each row is given the annotation
    /// created by `A::default()`.  Each line in the string is interpreted as a [`Row`], with the
    /// last row being 'left-over'.
    pub fn parse(s: &str) -> Result<Self, ParseError>
    where
        A: Default,
    {
        let mut line_iter = s.lines().enumerate();

        // Parse the first line before creating the `SameStageVec` (since we need to know upfront
        // what the stage is going to be).
        let (_zero, first_line) = line_iter.next().ok_or(ParseError::ZeroLengthBlock)?;
        let first_row =
            RowBuf::parse(first_line).map_err(|err| ParseError::InvalidRow { line: 0, err })?;
        // Create a `SameStageVec` containing just `first_row`
        let mut row_buffer = SameStageVec::from_row_buf(first_row);

        // Now parse the rest of the lines into `SameStageVec`
        for (i, line) in line_iter {
            // Parse the line into a Row, and fail if its either invalid or doesn't match the stage
            let parsed_row =
                RowBuf::parse(line).map_err(|err| ParseError::InvalidRow { line: i, err })?;
            row_buffer.push(&parsed_row).map_err(
                |IncompatibleStages {
                     lhs_stage,
                     rhs_stage,
                 }| ParseError::IncompatibleStages {
                    line_index: i,
                    first_stage: lhs_stage,
                    different_stage: rhs_stage,
                },
            )?;
        }

        Ok(Self::from_vec_with_default_annots(row_buffer))
    }

    /// Creates a new `AnnotBlock` from a [`SameStageVec`], where every annotation is
    /// `A::default()`.
    ///
    /// # Panics
    ///
    /// This panics if the [`SameStageVec`] provided is empty.
    pub fn from_vec_with_default_annots(rows: SameStageVec) -> Self
    where
        A: Default,
    {
        assert!(!rows.is_empty());
        Self {
            annots: repeat_with(A::default).take(rows.len() - 1).collect_vec(),
            row_buffer: rows,
        }
    }

    /////////////////
    // STAGE & LEN //
    /////////////////

    /// Gets the [`Stage`] of this `Block`.
    #[inline]
    pub fn stage(&self) -> Stage {
        self.row_buffer.stage()
    }

    /// Gets the effective [`Stage`] of this `AnnotBlock` - i.e. the smallest [`Stage`] that this
    /// `AnnotBlock` can be reduced to without producing invalid [`Row`]s.  See
    /// [`Row::effective_stage`] for more info and examples.
    pub fn effective_stage(&self) -> Stage {
        self.rows()
            .map(Row::effective_stage)
            .max()
            // Unwrapping here is safe, because blocks must contain at least one Row
            .unwrap()
    }

    /// Shorthand for `self.len() == 0`
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.annots.is_empty()
    }

    /// Gets the length of this `Block` (excluding the left-over [`Row`]).
    #[inline]
    pub fn len(&self) -> usize {
        self.annots.len()
    }

    /////////////
    // GETTERS //
    /////////////

    /// Gets the [`Row`] at a given index, along with its annotation.
    #[inline]
    pub fn get_row(&self, index: usize) -> Option<&Row> {
        self.row_buffer.get(index)
    }

    /// Gets an immutable reference to the annotation of the [`Row`] at a given index, if it
    /// exists.
    #[inline]
    pub fn get_annot(&self, index: usize) -> Option<&A> {
        self.annots.get(index)
    }

    /// Gets an mutable reference to the annotation of the [`Row`] at a given index, if it
    /// exists.
    #[inline]
    pub fn get_annot_mut(&mut self, index: usize) -> Option<&mut A> {
        self.annots.get_mut(index)
    }

    /// Gets the [`Row`] at a given index, along with its annotation.
    #[inline]
    pub fn get_annot_row(&self, index: usize) -> Option<AnnotRow<A>> {
        let row = self.get_row(index)?;
        let annot = self.get_annot(index)?;
        Some(AnnotRow { row, annot })
    }

    /// Gets the first [`Row`] of this `AnnotBlock`, along with its annotation.
    #[inline]
    pub fn first_annot_row(&self) -> Option<AnnotRow<A>> {
        self.get_annot_row(0)
    }

    /// Returns the 'left-over' [`Row`] of this `Block`.  This [`Row`] represents the overall
    /// transposition of the `Block`, and should not be used when generating rows for truth
    /// checking.
    #[inline]
    pub fn leftover_row(&self) -> &Row {
        &self.row_buffer.last().unwrap()
    }

    //////////////////////////////
    // ITERATORS / PATH GETTERS //
    //////////////////////////////

    /// Returns an [`Iterator`] which yields the [`Row`]s which are directly part of this
    /// `AnnotBlock`.  This does not include the 'left-over' row; if you want to include the
    /// left-over [`Row`], use [`AnnotBlock::all_rows`] instead.
    #[inline]
    pub fn rows(&self) -> same_stage_vec::Iter {
        self.row_buffer.iter()
    }

    /// Returns the places of a given [`Bell`] in each [`Row`] of this `AnnotBlock`.  Also returns
    /// the place of `bell` in the leftover row.
    pub fn path_of(&self, bell: Bell) -> Option<(Vec<usize>, usize)> {
        let mut full_path = self.full_path_of(bell)?;
        let place_in_leftover_row = full_path.pop().unwrap();
        Some((full_path, place_in_leftover_row))
    }

    /// Returns the places of a given [`Bell`] in each [`Row`] of this `AnnotBlock`, **including**
    /// the leftover row.
    pub fn full_path_of(&self, bell: Bell) -> Option<Vec<usize>> {
        self.row_buffer.path_of(bell) // Delegate to `SameStageVec`
    }

    /////////////////////////
    // IN-PLACE OPERATIONS //
    /////////////////////////

    /// Pre-multiplies every [`Row`] in this `Block` in-place by another [`Row`], whilst preserving
    /// the annotations.
    pub fn permute(&mut self, lhs_row: &Row) -> Result<(), IncompatibleStages> {
        self.row_buffer.permute(lhs_row) // Delegate to `SameStageVec`
    }

    /// Extends `self` with a chunk of itself, transposed to start with `self.leftover_row()`.
    pub fn extend_from_self(&mut self, range: Range<usize>)
    where
        A: Clone,
    {
        // Remove the leftover row from the row buffer, so that the new rows can be inserted in its
        // place
        let leftover_row = self.row_buffer.pop().unwrap(); // OK because `row_buffer` can't be empty
        let first_row_of_chunk = self.get_row(range.start).unwrap();
        // This unwrap is fine, because both rows were taken from the same `SameStageVec`
        let transposition = Row::solve_xa_equals_b(first_row_of_chunk, &leftover_row).unwrap();

        self.row_buffer
            .extend_transposed_from_within(range.clone(), &transposition) // Extend the rows
            .unwrap(); // Unwrapping is fine because `transposition` comes from `self.row_buffer`
        self.annots.extend_from_within(range); // Extend the annots
    }

    ///////////////////////////////////
    // OPERATIONS WHICH CONSUME SELF //
    ///////////////////////////////////

    /// Consumes this `AnnotBlock`, and returns a [`SameStageVec`] containing the same [`Row`]s,
    /// **including** the left-over row.
    pub fn into_row_buffer(self) -> SameStageVec {
        self.row_buffer
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function.
    pub fn map_annots<B>(self, f: impl Fn(A) -> B) -> AnnotBlock<B> {
        AnnotBlock {
            row_buffer: self.row_buffer, // Don't modify the rows
            annots: self.annots.into_iter().map(f).collect_vec(),
        }
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function (along with its index within the
    /// `AnnotBlock`).
    pub fn map_annots_with_index<B>(self, f: impl Fn(usize, A) -> B) -> AnnotBlock<B> {
        AnnotBlock {
            row_buffer: self.row_buffer, // Don't modify the rows
            annots: self
                .annots
                .into_iter()
                .enumerate()
                .map(|(i, annot)| f(i, annot))
                .collect_vec(),
        }
    }

    /// Convert this `AnnotBlock` into another `AnnotBlock` with identical [`Row`]s, but where each
    /// annotation is passed through the given function (along with its index within the
    /// `AnnotBlock`).
    pub fn clone_map_annots_with_index<'s, B>(
        &'s self,
        f: impl Fn(usize, &'s A) -> B,
    ) -> AnnotBlock<B> {
        AnnotBlock {
            row_buffer: self.row_buffer.to_owned(), // Don't modify the rows
            annots: self
                .annots
                .iter()
                .enumerate()
                .map(|(i, annot)| f(i, annot))
                .collect_vec(),
        }
    }

    /// Splits this `AnnotBlock` into two separate `AnnotBlock`s at a specified index.  This is
    /// defined such the first `AnnotBlock` has length `index`.  This returns `None` if the second
    /// `AnnotBlock` would have negative length.
    pub fn split(self, index: usize) -> Option<(Self, Self)> {
        let (first_annots, second_annots) = split_vec(self.annots, index)?;
        let (mut first_rows, second_rows) = self.row_buffer.split(index)?;
        // Copy the first row of `second_rows` back into `first_rows` so it becomes the leftover
        // row of the first block
        let first_row_of_second = second_rows.first().unwrap(); // Unwrap is safe because
                                                                // `self.row_buffer.len() > index + 1`
        first_rows.push(first_row_of_second).unwrap(); // Unwrap is safe because both rows came
                                                       // from `self.row_buffer`

        // Construct the new pair of blocks
        let first_block = Self {
            row_buffer: first_rows,
            annots: first_annots,
        };
        let second_block = Self {
            row_buffer: second_rows,
            annots: second_annots,
        };
        Some((first_block, second_block))
    }
}
