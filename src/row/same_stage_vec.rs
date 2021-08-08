use std::ops::{Index, Range};

use itertools::Itertools;

use crate::{utils::split_vec, Bell, IncompatibleStages, Row, RowBuf, Stage};

/// A heap-allocated buffer of [`Row`]s which are required to have the same [`Stage`].  Collecting
/// [`Row`]s of the same [`Stage`] is nearly always what we want, and having a type to enforce this
/// is convenient.
///
/// Requiring matching [`Stage`]s has useful performance benefits too.  These stem from the fact
/// that the [`Row`]s can simply be stored sequentially in one [`Vec`] without any indirection or
/// extra allocations.  This is both more cache-friendly and uses less memory.
///
/// Additionally, having a linear memory layout makes `SameStageVec`s extremely amenable to SIMD
/// optimisations (which haven't yet been implemented).  This would allow us to do common
/// operations (like permuting all the [`Row`]s or finding the path of a [`Bell`]) extremely
/// quickly.  For example, SIMD can perform permutation at 16 bells per CPU cycle (or 32 on very
/// new CPUs).
///
/// # Example
///
/// ```
/// use bellframe::{RowBuf, SameStageVec, Stage};
///
/// // Create a new `SameStageVec` for 6-bell rows
/// let mut new_buffer = SameStageVec::new(Stage::MINOR);
/// // Add some nice rows
/// new_buffer.push(&RowBuf::rounds(Stage::MINOR));
/// new_buffer.push(&RowBuf::queens(Stage::MINOR));
/// new_buffer.push(&RowBuf::backrounds(Stage::MINOR));
/// // `new_buffer` now contains 3 rows
/// assert_eq!(new_buffer.len(), 3);
/// // The 3rd row is backrounds
/// assert!(new_buffer[2].is_backrounds());
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SameStageVec {
    /// A contiguous chunk of [`Bell`]s, containing all the [`Row`]s concatenated together.  For
    /// example, the start of a plain course of Cambridge Surprise Minor would be stored as
    /// `123456214365124635216453261435624153621435...`
    /// or, with spaces inserted between rows:
    /// `123456 214365 124635 216453 261435 624153 621435 ...`
    ///
    /// **Invariant**: Each [`Stage`]-aligned segment of `bells` must form a valid [`Row`]
    /// according to the Framework (see [`Row`]'s docs).  A sub-slice is [`Stage`]-aligned if its
    /// length is `self.stage` and its start index is an integer multiple of `self.stage`.
    ///
    /// **Invariant**: `self.bells.len()` is an integer multiple of `self.stage`.
    bells: Vec<Bell>,
    /// The [`Stage`] of all the [`Row`]s in this buffer.
    ///
    /// **Invariant**: This cannot be [`Stage::ZERO`] (because the number of rows is computed with
    /// `bells.len() / stage.as_usize()`, which would divide by zero).
    stage: Stage,
}

impl SameStageVec {
    //////////////////
    // CONSTRUCTORS //
    //////////////////

    /// Creates a new `SameStageVec`, where all the [`Row`]s are expected to have a given
    /// [`Stage`].
    ///
    /// # Panics
    ///
    /// Panics if `Stage::ZERO` is passed.
    #[inline]
    pub fn new(stage: Stage) -> Self {
        assert!(stage > Stage::ZERO);
        Self {
            bells: Vec::new(),
            stage,
        }
    }

    /// Creates a new `SameStageVec` that can hold `capacity` [`Row`]s without re-allocating, and
    /// all the [`Row`]s are expected to have a given [`Stage`].
    ///
    /// # Panics
    ///
    /// Panics if `Stage::ZERO` is passed.
    #[inline]
    pub fn with_capacity(stage: Stage, capacity: usize) -> Self {
        assert!(stage > Stage::ZERO);
        Self {
            bells: Vec::with_capacity(capacity * stage.as_usize()),
            stage,
        }
    }

    /// Creates a `SameStageVec` containing exactly one row, reusing the allocation from an
    /// existing [`RowBuf`].
    #[inline]
    pub fn from_row_buf(row: RowBuf) -> Self {
        let stage = row.stage();
        // This unsafety is OK because `row.to_bell_vec()` generates a Vec containing only one
        // `Row`, which is required to be valid by `RowBuf`'s invariants
        unsafe { Self::from_bell_vec_unchecked(row.into_bell_vec(), stage) }
    }

    /// Creates a `SameStageVec` from a [`Vec`] of [`Bell`]s (containing the [`Row`]s concatenated
    /// together), without checking that they correspond to valid [`Row`]s.
    ///
    /// # Safety
    ///
    /// This function is safe if `bells` is formed of valid [`Row`]s (each of the provided
    /// [`Stage`]) concatenated together.
    #[inline]
    pub unsafe fn from_bell_vec_unchecked(bells: Vec<Bell>, stage: Stage) -> Self {
        Self { bells, stage }
    }

    //////////////////
    // SIZE GETTERS //
    //////////////////

    /// The [`Stage`] of every [`Row`] in this `SameStageVec`.
    #[inline]
    pub fn stage(&self) -> Stage {
        self.stage
    }

    /// Gets the number of [`Row`]s contained in this `SameStageVec`
    #[inline]
    pub fn len(&self) -> usize {
        // In debug builds, check that `bells` contains a whole number of rows
        debug_assert!(self.bells.len() % self.stage.as_usize() == 0);
        // Calculate the length
        self.bells.len() / self.stage.as_usize()
    }

    /// Returns `true` if this `SameStageVec` contains no [`Row`]s.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bells.is_empty()
    }

    /////////////////
    // ROW GETTERS //
    /////////////////

    /// Gets a reference to the [`Row`] at a given `index`, or `None` if `index >= self.len()`.
    pub fn get(&self, index: usize) -> Option<&Row> {
        let bell_slice = self.bells.get(self.get_range_of_row(index))?;
        // This unsafety is OK, because we uphold an invariant that each stage-aligned segment of
        // `self.bells` is a valid `Row`
        Some(unsafe { Row::from_slice_unchecked(bell_slice) })
    }

    /// Gets a mutable reference to the [`Row`] at a given `index`, or `None` if
    /// `index >= self.len()`.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Row> {
        let range = self.get_range_of_row(index);
        let bell_slice = self.bells.get_mut(range)?;
        // This unsafety is OK, because we uphold an invariant that each stage-aligned segment of
        // `self.bells` is a valid `Row`
        Some(unsafe { Row::from_mut_slice_unchecked(bell_slice) })
    }

    /// Gets a reference to the first [`Row`], if it exists.
    pub fn first(&self) -> Option<&Row> {
        self.get(0)
    }

    /// Gets a mutable reference to the first [`Row`], if it exists.
    pub fn first_mut(&mut self) -> Option<&mut Row> {
        self.get_mut(0)
    }

    /// Gets a reference to the last [`Row`], if it exists.
    pub fn last(&self) -> Option<&Row> {
        self.get(self.len().checked_sub(1)?)
    }

    /// Gets a mutable reference to the last [`Row`], if it exists.
    pub fn last_mut(&mut self) -> Option<&mut Row> {
        self.get_mut(self.len().checked_sub(1)?)
    }

    /// Returns an [`Iterator`] over the [`Row`]s in this buffer.
    #[inline]
    pub fn iter(&self) -> Iter {
        Iter {
            // The invariant on `bells_left` follows because there is an identical invariant on
            // `SameStageVec.bells`
            bells_left: &self.bells,
            stage: self.stage,
        }
    }

    /// Returns a [`Vec`] containing the place of a [`Bell`] in each [`Row`] in this
    /// `SameStageVec`.  Returns `None` if the [`Bell`] exceeds the [`Stage`] of `self`.
    pub fn path_of(&self, bell: Bell) -> Option<Vec<usize>> {
        if bell.number() > self.stage().as_usize() {
            return None;
        }
        // TODO: Write a vectorised routine for this
        Some(self.iter().map(|r| r.place_of(bell).unwrap()).collect_vec())
    }

    ////////////////
    // OPERATIONS //
    ////////////////

    /// Adds a new [`Row`] to the end of the buffer, checking that its [`Stage`] is as expected.
    #[inline]
    pub fn push(&mut self, row: &Row) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(self.stage, row.stage())?;
        self.bells.extend(row.bell_iter());
        Ok(())
    }

    /// Removes the last [`Row`] from `self` and returning if it.  Returns `None` if `self`
    /// contains no elements.
    #[inline]
    pub fn pop(&mut self) -> Option<RowBuf> {
        // Compute the index of the first `Bell` in the last `Row`, and return `None` if that index
        // would be negative
        let start_index = self.bells.len().checked_sub(self.stage.as_usize())?;
        debug_assert_eq!(start_index % self.stage.as_usize(), 0);
        // This unsafety is OK because we enforce an invariant that every stage-aligned segment of
        // `self.bells` is a valid `Row`.
        Some(unsafe { RowBuf::from_bell_iter_unchecked(self.bells.drain(start_index..)) })
    }

    /// Extends this buffer with the [`Row`]s yielded by an [`Iterator`].  If any of the [`Row`]s
    /// cause a [`Stage`] mismatch, an error is returned and the buffer is left unchanged.
    pub fn extend<T: AsRef<Row>>(
        &mut self,
        row_iter: impl IntoIterator<Item = T>,
    ) -> Result<(), IncompatibleStages> {
        let num_bells_before_extend = self.bells.len(); // Used to revert the `Bell` slice
        for r in row_iter.into_iter() {
            let row = r.as_ref();
            if let Err(e) = IncompatibleStages::test_err(self.stage, row.stage()) {
                self.bells.drain(num_bells_before_extend..); // Remove the extra bells
                return Err(e); // End the iteration
            }
            self.bells.extend(row.bell_iter());
        }
        Ok(())
    }

    /// Extend `self` with the [`Row`]s from `other`, returning an error if the [`Stage`]s don't
    /// match.  `x.extend_from_buf(y)` has the same effect as `x.extend(y)` (because
    /// `&SameStageVec` implements [`IntoIterator`]), but this version is faster since the
    /// [`Stage`] comparison is only performed once.
    #[inline]
    pub fn extend_from_buf(&mut self, other: &Self) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(self.stage, other.stage)?;
        self.bells.extend(other.bells.iter().copied()); // Copy bells across in one go
        Ok(())
    }

    /// Take a range of `self`, pre-multiply all the `Row`s and extend `self` with them.
    #[inline]
    pub fn extend_transposed_from_within(
        &mut self,
        range: Range<usize>,
        transposition: &Row,
    ) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(self.stage, transposition.stage())?;
        // Copy the bells one-by-one, because otherwise we'd have to borrow `self.bells` twice and
        // the borrow checker doesn't like that.
        for i in range {
            let transposed_bell = transposition[self.bells[i].index()];
            self.bells.push(transposed_bell);
        }
        Ok(())
    }

    /// Pre-multiplies every [`Row`] in this `Block` in-place by another [`Row`].
    pub fn permute(&mut self, lhs_row: &Row) -> Result<(), IncompatibleStages> {
        IncompatibleStages::test_err(lhs_row.stage(), self.stage())?;
        // TODO: Write vectorised routine for this
        for b in &mut self.bells {
            // This function is safe because:
            // - We know that `self.stage() == lhs_row.stage()`
            // - Because of the `Row` invariants, `b.index() < self.stage()` for all `b` in
            //   `self.bells`
            // => `b.index() < lhs_row.stage()` for every `b`
            // => calling `get_bell_unchecked` is safe
            *b = unsafe { lhs_row.get_bell_unchecked(b.index()) };
        }
        Ok(())
    }

    /// Splits `self` into two `SameStageVec`s (`a` and `b`) where:
    /// - `a.len() == index`, and
    /// - `self == a.extend_from_buf(&b)` (i.e. no rows are created or destroyed)
    pub fn split(self, index: usize) -> Option<(Self, Self)> {
        let (left_bells, right_bells) = split_vec(self.bells, index * self.stage.as_usize())?;
        Some((
            // Both of these are safe because we split `self.bells` at an integer multiple of
            // `self.stage`, thus preserving the row boundaries and upholding the invariants
            unsafe { Self::from_bell_vec_unchecked(left_bells, self.stage) },
            unsafe { Self::from_bell_vec_unchecked(right_bells, self.stage) },
        ))
    }

    //////////////////////
    // HELPER FUNCTIONS //
    //////////////////////

    /// Gets the [`Range`] of `self.bells` which would contain the [`Row`] at a given `index`.
    fn get_range_of_row(&self, index: usize) -> Range<usize> {
        let s = self.stage.as_usize();
        index * s..(index + 1) * s
    }
}

impl Index<usize> for SameStageVec {
    type Output = Row;

    fn index(&self, index: usize) -> &Self::Output {
        // This unsafety is fine, because we require that every stage-aligned chunk of bells forms
        // a valid row according to the Framework
        unsafe { Row::from_slice_unchecked(&self.bells[self.get_range_of_row(index)]) }
    }
}

impl<'v> IntoIterator for &'v SameStageVec {
    type Item = &'v Row;

    type IntoIter = Iter<'v>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An [`Iterator`] which yields [`Row`]s from a [`SameStageVec`]
#[derive(Debug, Clone)]
pub struct Iter<'v> {
    /// Invariant: Just like [`SameStageVec`], every [`Stage`]-aligned segment of `bells_left` must
    /// form a valid row according to the Framework.
    bells_left: &'v [Bell],
    stage: Stage,
}

impl<'v> Iterator for Iter<'v> {
    type Item = &'v Row;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bells_left.is_empty() {
            return None;
        }
        // Remove the first `self.stage` elements from `self.bells_left`.  Since we've removed
        // `self.stage` items, this means that the stage-aligned segments haven't changed, so the
        // invariant on `self.bells_left` is still satisfied.
        let (next_row, future_rows) = self.bells_left.split_at(self.stage.as_usize());
        self.bells_left = future_rows;
        // This unsafety is OK because the invariant on `self.bells_left` requires that `next_row`
        // (a stage-aligned segment of `self.bells_left`) forms a valid row according to the
        // Framework.
        Some(unsafe { Row::from_slice_unchecked(next_row) })
    }
}
