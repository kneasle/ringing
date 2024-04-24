use std::{
    cmp::Ordering,
    collections::{HashSet, VecDeque},
    fmt::{Debug, Display, Formatter},
    ops::{Index, Mul, MulAssign, Not},
    rc::Rc,
    sync::Arc,
};

use itertools::Itertools;

use crate::{Bell, IncompatibleStages, Parity, RowBuf, SameStageVec, Stage};

use super::{MulIntoError, RowAccumulator};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

pub type BellIter<'a> = std::iter::Cloned<std::slice::Iter<'a, Bell>>;

/// A borrowed `Row` of [`Bell`]s.
///
/// This can be viewed as a permutation of [rounds](RowBuf::rounds) on a given [`Stage`].
///
/// `Row`s and [`RowBuf`]s must always be valid according to
/// [the Framework](https://cccbr.github.io/method_ringing_framework/fundamentals.html) - i.e., it
/// must contain every [`Bell`] up to its [`Stage`] once and precisely once.  This is only checked
/// in the constructors and then used as assumed knowledge to avoid further checks.  This is
/// similar to how [`&str`](str) and [`String`] are required to be valid UTF-8.
///
/// # Example
/// ```
/// use bellframe::{Bell, RowBuf, Stage, InvalidRowError};
///
/// // Create rounds on 8 bells.  Rounds is always valid on any `Stage`
/// let rounds_on_8 = RowBuf::rounds(Stage::MAJOR);
/// assert_eq!(rounds_on_8.stage(), Stage::MAJOR);
/// assert_eq!(rounds_on_8.to_string(), "12345678");
///
/// // Parse a generic (valid) change from a string.  Note how invalid
/// // `char`s are skipped.  This could fail if the resulting `Row` is
/// // invalid, so we use ? to propogate that error out of the current
/// // function.
/// let queens = RowBuf::parse("13579 | 24680")?;
/// assert_eq!(queens.stage(), Stage::ROYAL);
/// assert_eq!(queens.to_string(), "1357924680");
///
/// // If we try to parse an invalid `Row`, we get an error.  This means
/// // that we can assume that all `Row`s satisfy the Framework's definition
/// assert_eq!(
///     RowBuf::parse("112345"),
///     Err(InvalidRowError::DuplicateBell(Bell::from_name('1').unwrap()))
/// );
/// #
/// # Ok::<(), InvalidRowError>(())
/// ```
#[derive(Eq, PartialEq, PartialOrd, Ord, Hash)]
#[repr(transparent)] // Required so we can safely cast between &[Bell] and &Row
pub struct Row {
    /// The [`Bell`]s in the order that they would be rung.  Because of the 'valid row' invariant,
    /// this can't contain duplicate [`Bell`]s or any [`Bell`]s with number greater than the
    /// [`Stage`] of this `Row`.
    bell_slice: [Bell],
}

impl Row {
    /// Returns the [`Stage`] of this `Row`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // Rounds on a given `Stage` should have that `Stage`
    /// assert_eq!(RowBuf::rounds(Stage::MINIMUS).stage(), Stage::MINIMUS);
    /// assert_eq!(RowBuf::rounds(Stage::SEPTUPLES).stage(), Stage::SEPTUPLES);
    ///
    /// assert_eq!(RowBuf::parse("41325")?.stage(), Stage::DOUBLES);
    /// assert_eq!(RowBuf::parse("321 654 987 0")?.stage(), Stage::ROYAL);
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    #[inline]
    pub fn stage(&self) -> Stage {
        Stage::new(self.bell_slice.len() as u8)
    }

    /// Returns the underlying slice of [`Bell`]s.
    pub fn bells(&self) -> &[Bell] {
        &self.bell_slice
    }

    /// Returns an iterator over the [`Bell`]s in this `Row`.
    #[inline]
    pub fn bell_iter(&self) -> BellIter {
        self.bell_slice.iter().cloned()
    }

    /// Returns an immutable reference to the underlying slice of [`Bell`]s that makes up this
    /// `Row`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf};
    ///
    /// let tittums = RowBuf::parse("15263748")?;
    /// assert_eq!(tittums.slice()[3], Bell::from_name('6').unwrap());
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    #[inline]
    pub fn slice(&self) -> &[Bell] {
        &self.bell_slice
    }

    /// Gets the **0-indexed** place at which a given [`Bell`] appears in this `Row`, returning
    /// `None` if the [`Bell`] is out of the stage.  This performs a linear search of the `Row`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf};
    ///
    /// # fn test() -> Option<()> {
    /// let tittums = RowBuf::parse("15263748").unwrap();
    /// // The treble is leading in position 0
    /// assert_eq!(tittums.place_of(Bell::from_name('1')?), 0);
    /// // The '5' is at index `1`, because indices always start from zero
    /// assert_eq!(tittums.place_of(Bell::from_name('5')?), 1);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub fn place_of(&self, bell: Bell) -> u8 {
        self.bell_iter().position(|b| b == bell).unwrap() as u8
    }

    /// Gets the [`Bell`] at a given place in this [`Row`], **without** bounds checks.
    ///
    /// # Safety
    ///
    /// This function is safe if `place < self.stage.as_usize()`.
    #[inline]
    pub unsafe fn get_bell_unchecked(&self, place: usize) -> Bell {
        *self.bell_slice.get_unchecked(place)
    }

    /// Returns the [`Parity`] of this [`Row`].  In order to avoid allocations, this function uses
    /// mutates `self` - specifically `self` is left as rounds, but this is an implementation
    /// detail and should not be relied upon.
    pub fn parity(&mut self) -> Parity {
        // Invariants: self.bells[..first_non_rounds_bell] is sorted
        let mut num_swaps = 0;
        let mut first_non_rounds_bell = 0;
        while first_non_rounds_bell < self.stage().num_bells() {
            let cur_bell = self.bell_slice[first_non_rounds_bell];
            if cur_bell == Bell::from_index(first_non_rounds_bell as u8) {
                // Check if the current bell is sorted, then just move on to the next one
                first_non_rounds_bell += 1;
            } else {
                // If this bell isn't in its right place, then swap it and keep going
                self.swap(first_non_rounds_bell, cur_bell.index());
                num_swaps += 1;
            }
        }
        Parity::from_number(num_swaps)
    }

    /// Perform an in-place check that this `Row` is equal to rounds.  `x.is_rounds()` is an
    /// optimised version of `x == RowBuf::rounds(x.stage())`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // Rounds is ... rounds (DOH)
    /// assert!(RowBuf::rounds(Stage::MAXIMUS).is_rounds());
    /// // This is not rounds
    /// assert!(!RowBuf::parse("18423756")?.is_rounds());
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn is_rounds(&self) -> bool {
        self.bell_iter().enumerate().all(|(i, b)| b.index() == i)
    }

    /// Perform an in-place check that this `Row` is equal to [backrounds](RowBuf::backrounds).
    /// `x.is_rounds()` is an optimised version of `x == RowBuf::backrounds(x.stage())`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // Backrounds is ... backrounds (DOH)
    /// assert!(RowBuf::backrounds(Stage::MAXIMUS).is_backrounds());
    /// // This is not backrounds
    /// assert!(!RowBuf::parse("18423756")?.is_backrounds());
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn is_backrounds(&self) -> bool {
        self.bell_iter()
            .rev() // Assert that the bells are in reverse order
            .enumerate()
            .all(|(i, b)| b.index() == i)
    }

    /// Return the [`Stage`] of the shortest prefix of `self` that is still a valid `Row`.  This is
    /// the smallest [`Stage`] that this `Row` can be safely reduced to.
    /// [`Rounds`](RowBuf::rounds) on any [`Stage`] has an `effective_stage` of [`Stage::ONE`],
    /// since zero-bell [`Stage`]s can't exist.
    ///
    /// # Example
    ///
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // This is essentially a Triples row, because it has the 8 covering
    /// assert_eq!(RowBuf::parse("14237568")?.effective_stage(), Stage::TRIPLES);
    /// // This row has no cover bells, so can't be reduced
    /// assert_eq!(RowBuf::parse("18423756")?.effective_stage(), Stage::MAJOR);
    /// // Rounds always has an effective `Stage` of one
    /// assert_eq!(RowBuf::rounds(Stage::MAXIMUS).effective_stage(), Stage::ONE);
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn effective_stage(&self) -> Stage {
        // Iterate backwards over the bells looking for the first bell which isn't in its place.
        // This bell defines the effective_stage of the row.
        for (i, b) in self.bell_slice.iter().enumerate().rev() {
            if b.index() != i {
                // The `+ 1` is needed because `i` is 0-indexed
                return Stage::new(i as u8 + 1);
            }
        }
        // If the loop reached the front of the row, then the effective stage is 1
        Stage::ONE
    }

    /// Swap two [`Bell`]s round in this `Row`, panicking if either of the indices point out of
    /// bounds.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// let mut rounds = RowBuf::rounds(Stage::MAJOR);
    /// assert_eq!(rounds.to_string(), "12345678");
    /// rounds.swap(0, 1); // Note we are using 0-indexing
    /// assert_eq!(rounds.to_string(), "21345678");
    /// rounds.swap(2, 5); // Note we are using 0-indexing
    /// assert_eq!(rounds.to_string(), "21645378");
    /// ```
    #[inline]
    pub fn swap(&mut self, a: usize, b: usize) {
        self.bell_slice.swap(a, b);
    }

    /// Overwrite `self` in-place with the contents of another [`Row`].  Returns an
    /// [`IncompatibleStages`] error if `other` has a different stage.
    #[track_caller]
    pub fn copy_from(&mut self, other: &Row) {
        self.check_stage(other);
        self.bell_slice.copy_from_slice(&other.bell_slice);
    }

    /* PERMUTATION ARITHMETIC */

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), checking that the [`Stage`]s are
    /// compatible.  This is like using the * operator, except that this returns a [`Result`]
    /// instead of [`panic!`]ing when the [`Stage`]s don't match.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf};
    ///
    /// // Multiplying two Rows of the same Stage is fine
    /// assert_eq!(
    ///     RowBuf::parse("13425678")?.try_mul(&RowBuf::parse("43217568")?),
    ///     Ok(RowBuf::parse("24317568")?)
    /// );
    /// // Multiplying two Rows of different Stages causes an error but no
    /// // undefined behaviour
    /// assert_eq!(
    ///     &RowBuf::parse("13425678")?
    ///         .try_mul(&RowBuf::parse("4321")?)
    ///         .unwrap_err()
    ///         .to_string(),
    ///     "Incompatible stages: Major (lhs), Minimus (rhs)"
    /// );
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn try_mul(&self, rhs: &Self) -> Result<RowBuf, IncompatibleStages> {
        IncompatibleStages::test_err(self.stage(), rhs.stage())?;
        // This unsafety is OK because the `self` and `rhs` are both assumed to be valid, and we
        // have already checked that their stages are equal
        Ok(unsafe { self.mul_unchecked(rhs) })
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), but without checking that the
    /// [`Stage`]s are compatible.  This is slightly faster than using `*` or [`Row::try_mul`],
    /// but could cause undefined behaviour.
    ///
    /// # Safety
    ///
    /// This is safe if the two `Row`s have the same [`Stage`] (which is often an invariant
    /// enforced by other data types, such as [`Block`]).
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf, Stage, IncompatibleStages};
    ///
    /// // Multiplying two Rows of the same Stage is OK, but still unsafe
    /// assert_eq!(
    ///     unsafe {
    ///         RowBuf::parse("13425678")?.mul_unchecked(&RowBuf::parse("43217568")?)
    ///     },
    ///     RowBuf::parse("24317568")?
    /// );
    /// // Multiplying two Rows of different Stages is not OK, and creates an invalid Row.
    /// let bad_row = unsafe {
    ///     RowBuf::parse("13475628")?.mul_unchecked(&RowBuf::parse("4321")?)
    /// };
    /// assert_eq!(bad_row.to_string(), "7431"); // NOT A VALID ROW!
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub unsafe fn mul_unchecked(&self, rhs: &Row) -> RowBuf {
        // We bypass the validity check because if two Rows are valid, then so is their product.
        // However, this function is also unsafe because permuting two rows of different Stages
        // causes undefined behaviour
        RowBuf::from_bell_iter_unchecked(rhs.bell_iter().map(|b| self.bell_slice[b.index()]))
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), storing the result in an
    /// existing [`RowBuf`].  This will change the [`Stage`] of the output [`RowBuf`] if needed.
    #[track_caller]
    pub fn mul_into(&self, rhs: &Row, out: &mut RowBuf) {
        self.check_stage(rhs);
        // This unsafety is OK because we've just checked that the stages match
        unsafe { self.mul_into_unchecked(rhs, out) }
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), storing the result in an
    /// existing [`RowBuf`].  This will change the [`Stage`] of the output [`RowBuf`] if needed.
    ///
    /// # Safety
    ///
    /// This function is safe if `self` and `rhs` have the same stage
    pub unsafe fn mul_into_unchecked(&self, rhs: &Row, out: &mut RowBuf) {
        // Replace `out.bell_vec` with `self * rhs`
        out.bell_vec.clear();
        out.bell_vec
            .extend(rhs.bell_iter().map(|b| self.bell_slice[b.index()]));
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), storing the result in an
    /// existing `Row`.  If any of the [`Stage`]s don't match, then a [`MulIntoError`] specifying
    /// the mismatch is returned.
    pub fn mul_into_row(&self, rhs: &Row, out: &mut Row) -> Result<(), MulIntoError> {
        // Test that all 3 stages match
        IncompatibleStages::test_err(self.stage(), rhs.stage()).map_err(MulIntoError::RhsStage)?;
        IncompatibleStages::test_err(self.stage(), out.stage()).map_err(MulIntoError::IntoStage)?;
        // This unsafety is OK because we've just checked that the stages match
        unsafe { self.mul_into_row_unchecked(rhs, out) }
        Ok(())
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), storing the result in an
    /// existing `Row`.
    ///
    /// # Safety
    ///
    /// This is safe if `self`, `rhs` and `out` all share the same [`Stage`].
    pub unsafe fn mul_into_row_unchecked(&self, rhs: &Row, out: &mut Row) {
        // We bypass the validity check because if two Rows are valid, then so is their product.
        // However, this function is also unsafe because permuting two rows of different Stages
        // causes undefined behaviour
        for (out_bell, rhs_bell) in out.bell_slice.iter_mut().zip_eq(rhs.bell_slice.iter()) {
            *out_bell = self.bell_slice[rhs_bell.index()];
        }
    }

    /// Find the inverse of a `Row`.  If `X` is the input `Row`, and `Y = X.inv()`, then `XY = YX =
    /// I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This operation cannot
    /// fail, since all valid `Row`s have an inverse.  This is equivalent to using the `!` operator.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // The inverse of Queens is Tittums
    /// assert_eq!(!&*RowBuf::parse("135246")?, RowBuf::parse("142536")?);
    /// // Backrounds is self-inverse
    /// assert_eq!(!&*RowBuf::backrounds(Stage::MAJOR), RowBuf::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// assert_eq!(!&*RowBuf::parse("1342")?, RowBuf::parse("1423")?);
    /// #
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn inv(&self) -> RowBuf {
        let mut inv_bells = vec![Bell::TREBLE; self.stage().num_bells()];
        for (i, b) in self.bell_slice.iter().enumerate() {
            inv_bells[b.index()] = Bell::from_index(i as u8);
        }
        // This unsafety is OK because Rows form a group and by the closure of groups under
        // inversion, if `self` is in the group of permutations, then so is `!self`.
        unsafe { RowBuf::from_vec_unchecked(inv_bells) }
    }

    /// Calculate the inverse of this `Row`, storing the result in an existing `Row`.  If the
    /// [`Stage`]s don't match, an [`IncompatibleStages`] error is returned and the output is not
    /// modified.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // The inverse of Queens is Tittums
    /// let mut row_buf = RowBuf::rounds(Stage::MINOR);
    /// RowBuf::parse("135246")?.inv_into(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::parse("142536")?);
    ///
    /// // Backrounds is self-inverse
    /// let mut row_buf = RowBuf::rounds(Stage::MAJOR);
    /// RowBuf::backrounds(Stage::MAJOR).inv_into(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::backrounds(Stage::MAJOR));
    ///
    /// // `1324` inverts to `1423`
    /// let mut row_buf = RowBuf::rounds(Stage::MINIMUS);
    /// RowBuf::parse("1342")?.inv_into(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::parse("1423")?);
    /// #
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    // TODO: Rename this `inv_into_row`
    #[track_caller]
    pub fn inv_into(&self, out: &mut Row) {
        self.check_stage(out);
        // Now perform the inversion
        for (i, b) in self.bell_iter().enumerate() {
            // PERF: If this ever becomes a bottleneck, this is a good place to remove the bounds
            // checks
            out.bell_slice[b.index()] = Bell::from_index(i as u8);
        }
    }

    /// Calculate the inverse of this `Row`, storing the result in an existing `RowBuf` (thus
    /// making use of its allocation).  This resizes `out` to make it the right [`Stage`] to take
    /// the output value.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // Create a new row that will be overwritten to avoid reallocations
    /// let mut row_buf = RowBuf::rounds(Stage::ONE);
    /// // The inverse of Queens is Tittums
    /// RowBuf::parse("135246")?.inv_into_buf(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::parse("142536")?);
    /// // Backrounds is self-inverse
    /// RowBuf::backrounds(Stage::MAJOR).inv_into_buf(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// RowBuf::parse("1342")?.inv_into_buf(&mut row_buf);
    /// assert_eq!(row_buf, RowBuf::parse("1423")?);
    /// #
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    // TODO: Rename this `inv_into`
    pub fn inv_into_buf(&self, out: &mut RowBuf) {
        // Make sure that `out` has the right stage
        match out.stage().cmp(&self.stage()) {
            Ordering::Less => {
                out.bell_vec.extend(
                    std::iter::repeat(Bell::TREBLE)
                        .take(self.bell_slice.len() - out.bell_vec.len()),
                );
            }
            Ordering::Greater => {
                out.bell_vec.drain(self.bell_slice.len()..);
            }
            Ordering::Equal => {}
        }
        debug_assert_eq!(out.stage(), self.stage());
        // Now perform the inversion
        for (i, b) in self.bell_iter().enumerate() {
            out.bell_vec[b.index()] = Bell::from_index(i as u8);
        }
    }

    /// Raises `self` to some (possibly negative) `exponent`
    pub fn pow_i(&self, exponent: isize) -> RowBuf {
        if exponent < 0 {
            self.inv().pow_u((-exponent) as usize)
        } else {
            self.pow_u(exponent as usize)
        }
    }

    /// Raises `self` to some positive `exponent`
    pub fn pow_u(&self, exponent: usize) -> RowBuf {
        // TODO/PERF: Implement this using repeated squaring
        let mut accumulator = RowAccumulator::rounds(self.stage());
        for _ in 0..exponent {
            // Unwrap is safe because the accumulator has the same stage as `self`
            accumulator.post_accumulate(self);
        }
        accumulator.into_total()
    }

    /// Computes the value of `x` which satisfies `a * x = b` - i.e. the `Row` which
    /// post-transposes `a` to `b`.
    #[inline]
    pub fn solve_ax_equals_b(a: &Self, b: &Self) -> RowBuf {
        !a * b
    }

    /// Computes the value of `x` which satisfies `x * a = b` - i.e. the `Row` which
    /// pre-multiplies `a` to `b`.  This is equivalent to `a.transposition_to(b)`.
    #[inline]
    pub fn solve_xa_equals_b(a: &Self, b: &Self) -> RowBuf {
        b * !a
    }

    /* MISC FUNCTIONS */

    pub fn is_fixed(&self, bell: Bell) -> bool {
        self.bell_slice.get(bell.index()) == Some(&bell)
    }

    /// Returns an [`Iterator`] over the [`Bell`]s in this `Row` which are fixed in their home
    /// positions (i.e. they're fixed by the transposition represented by this row).
    pub fn fixed_bells(&self) -> impl Iterator<Item = Bell> + '_ {
        self.bell_slice
            .iter()
            .enumerate()
            .filter(|&(idx, bell)| idx == bell.index())
            .map(|(_idx, bell)| *bell)
    }

    /// Copies the bells of `self` into `other`
    #[inline]
    pub fn copy_into(&self, other: &mut RowBuf) {
        other.bell_vec.clear();
        other.bell_vec.extend_from_slice(&self.bell_slice);
    }

    /// Creates an `Rc<Row>` containing the same [`Bell`]s as `self`
    #[inline]
    pub fn to_rc(&self) -> Rc<Row> {
        let rc_of_bells: Rc<[Bell]> = self.bell_slice.to_vec().into();
        let ptr_to_bells = Rc::into_raw(rc_of_bells);
        // Ptr cast is OK because `Row` and `[Bell]` have identical memory layouts
        let ptr_to_row = ptr_to_bells as *const Row;
        // Unsafety is OK because `ptr_to_row` came (indirectly) from `Rc::into_raw`
        unsafe { Rc::from_raw(ptr_to_row) }
    }

    /// Creates an `Rc<Row>` containing the same [`Bell`]s as `self`
    #[inline]
    pub fn to_arc(&self) -> Arc<Row> {
        let arc_of_bells: Arc<[Bell]> = self.bell_slice.to_vec().into();
        let ptr_to_bells = Arc::into_raw(arc_of_bells);
        // Ptr cast is OK because `Row` and `[Bell]` have identical memory layouts
        let ptr_to_row = ptr_to_bells as *const Row;
        // Unsafety is OK because `ptr_to_row` came (indirectly) from `Arc::into_raw`
        unsafe { Arc::from_raw(ptr_to_row) }
    }

    /// Gets the smallest non-zero number `n` such that `self.pow_u(n).is_rounds()`.
    #[inline]
    pub fn order(&self) -> usize {
        let mut accum = RowAccumulator::new(self.to_owned());
        let mut count = 1;
        while !accum.total().is_rounds() {
            // Safe, because `RowAccumulator` is initialised with `self` and its stage never
            // changes (so `accum.stage() == self.stage()` for all loop iterations).
            unsafe { accum.post_accumulate_unchecked(self) };
            count += 1;
        }
        count
    }

    /// Generate all the `Row`s formed by repeatedly permuting a given `Row`.  The first item
    /// returned will always be the input `Row`, and the last will always be `rounds`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf};
    ///
    /// // The closure of "18234567" are all the fixed-treble cyclic part heads.
    /// assert_eq!(
    ///     RowBuf::parse("18234567")?.closure(),
    ///     vec![
    ///         RowBuf::parse("18234567")?,
    ///         RowBuf::parse("17823456")?,
    ///         RowBuf::parse("16782345")?,
    ///         RowBuf::parse("15678234")?,
    ///         RowBuf::parse("14567823")?,
    ///         RowBuf::parse("13456782")?,
    ///         RowBuf::parse("12345678")?,
    ///     ]
    /// );
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn closure(&self) -> Vec<RowBuf> {
        let mut closure = vec![self.to_owned()];
        loop {
            let last_row = closure.last().unwrap();
            if last_row.is_rounds() {
                return closure;
            }
            // This unsafety is OK, because `row` and `self` have the same Stage
            let next_row = unsafe { last_row.mul_unchecked(self) };
            closure.push(next_row);
        }
    }

    /// Generates all the `Row`s formed by repeatedly permuting a given `Row`, but the first `Row`
    /// returned will always be [rounds](RowBuf::rounds), rather than `self`.  This is useful for
    /// situations like generating part heads, where it's more intuitive for the closure to start
    /// at rounds.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf};
    ///
    /// // The closure of "18234567" are all the fixed-treble cyclic part heads.
    /// // Note how rounds is the first Row/part head generated
    /// assert_eq!(
    ///     RowBuf::parse("18234567")?.closure_from_rounds(),
    ///     vec![
    ///         RowBuf::parse("12345678")?,
    ///         RowBuf::parse("18234567")?,
    ///         RowBuf::parse("17823456")?,
    ///         RowBuf::parse("16782345")?,
    ///         RowBuf::parse("15678234")?,
    ///         RowBuf::parse("14567823")?,
    ///         RowBuf::parse("13456782")?,
    ///     ]
    /// );
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    pub fn closure_from_rounds(&self) -> Vec<RowBuf> {
        let mut closure = vec![RowBuf::rounds(self.stage())];
        loop {
            let last_row = closure.last().unwrap();
            // This unsafety is OK, because `row` and `self` have the same Stage
            let next_row = unsafe { last_row.mul_unchecked(self) };
            if next_row.is_rounds() {
                return closure;
            }
            closure.push(next_row);
        }
    }

    /// Takes a sequence of sets of `Row`s (`[X_1, X_2, ..., X_n]`) and computes every product
    /// `x_1 * x_2 * ... * x_n` where `x_i` comes from `X_i` for all `i`.
    pub fn multi_cartesian_product(
        row_sets: impl IntoIterator<Item = impl IntoIterator<Item = impl AsRef<Self>>>,
        stage: Stage,
    ) -> SameStageVec {
        let mut set_iter = row_sets.into_iter();
        // We will always be transposing the contents of `transpose_from` with the new values,
        // putting the results into `transpose_to`.  At the end of every loop iteration these are
        // swapped round
        // PERF: Use same stage buffers here for linear layout and massive reduction in allocations
        let mut transpose_from = SameStageVec::new(stage);
        let mut transpose_to = SameStageVec::new(stage);
        // Consume the first set as a special case:
        match set_iter.next() {
            // If it doesn't exist, no things are being CPed together so we return the empty Vec
            None => return SameStageVec::new(stage),
            // If it does exist, then populate the `transpose_from` buffer with it and initialise
            // the stage
            Some(set) => {
                for r in set.into_iter() {
                    let r = r.as_ref();
                    transpose_from.push(r);
                }
            }
        }
        // Now, treat all subsequent sets identically
        for set in set_iter {
            // First up, check if `transpose_from` is empty, in which case the output will be empty
            if transpose_from.is_empty() {
                return SameStageVec::new(stage);
            }
            // Unwrap the stage once, since it has either been set by now or `transpose_from` is
            // empty
            // Now, transpose every item in `transpose_from` with every item from the new set and
            // push into `transpose_to`
            transpose_to.clear();
            for r2 in set {
                let r2 = r2.as_ref();
                for r1 in &transpose_from {
                    // SAFETY: We checked that `r2` and all of `transpose_from` have stage `s`
                    transpose_to.push(&(r1 * r2));
                }
            }
            // Finally, swap the buffers so that we read from the newly transposed rows
            std::mem::swap(&mut transpose_to, &mut transpose_from);
        }
        // Note: we return `transpose_from` here (rather than `transpose_to`) because the two
        // buffers have just been swapped at the end of the loop iteration
        transpose_from
    }

    /// Generates the least group containing a given set of `Row`s, returning the result in a
    /// [`HashSet`] (therefore, the result is unordered).  The current algorithm is quite slow; if
    /// anyone knows of a better one, then please let me know...
    pub fn least_group_containing<'a>(
        rows: impl IntoIterator<Item = &'a Self> + Clone,
    ) -> HashSet<RowBuf>
    where
        Self: 'a,
    {
        // Check that stages match
        for (r1, r2) in rows.clone().into_iter().tuple_windows() {
            assert_eq!(r1.stage(), r2.stage());
        }
        // The algorithm used here is to expand every possible way of expanding the input elements,
        // in depth first order.
        let mut set = HashSet::<RowBuf>::new();
        let mut frontier = VecDeque::<RowBuf>::new();
        // We seed the frontier and `set` manually the first time round to avoid checking the
        // stages all the time (if the input rows are all compatible, then so will any finite
        // product of them).
        for r in rows.clone().into_iter() {
            if set.insert(r.to_owned()) {
                frontier.push_back(r.to_owned());
            }
        }
        // Now, we repeatedly pop the last item of the frontier and post-multiply it by every row
        // in the input set.  We check each of these for inclusion *before* pushing it back to the
        // frontier (thus avoiding causing allocations if we need to).  This loop must terminate,
        // because at each iteration the number of unexpanded nodes in the tree decreases and that
        // number is bounded by the size of the resulting group (which is finite).
        while let Some(r) = frontier.pop_front() {
            for r2 in rows.clone().into_iter() {
                // SAFETY: we checked that all Rows in `rows` have stage `s`
                let new_row = unsafe { r.mul_unchecked(r2) };
                if !set.contains(&new_row) {
                    frontier.push_back(new_row.clone());
                    set.insert(new_row);
                }
            }
        }
        set
    }

    /// Determines if the given set of [`Row`]s forms a group.  This performs `n^2` transpositions
    /// and `n` inversions where `n` is the number of unique elements yeilded by `rows`.  See [this
    /// Wikipedia page](https://en.wikipedia.org/wiki/Subgroup_test) for the algorithm used.
    pub fn is_group<'a>(rows: impl IntoIterator<Item = &'a Row>) -> bool
    where
        Self: 'a,
    {
        // Build a hash set with the contents of `rows`
        let row_set: HashSet<&Row> = rows.into_iter().collect();
        // Check that stages match
        for (r1, r2) in row_set.iter().tuple_windows() {
            assert_eq!(r1.stage(), r2.stage());
        }
        // We early return here because if the set is empty then this cannot be a group but all the
        // checks will be vacuously satisfied
        if row_set.is_empty() {
            return false;
        }
        // Check that stages match
        // Now perform the group check by verifying that `a * !b` is in the set for all a, b in
        // `row_set`.
        // PERF: We're multiplying every row by its inverse, which always gives rounds and
        // therefore we can replace those checks with an in-place rounds check on the incoming rows
        // and thus gain performance
        // The buffers `b_inv` and `a_mul_b_inv` are reused in each loop iteration to avoid
        // performing `n(n + 1)` allocations.
        let mut b_inv = RowBuf::rounds(Stage::ONE);
        let mut a_mul_b_inv = RowBuf::rounds(Stage::ONE);
        for &b in &row_set {
            b.inv_into_buf(&mut b_inv);
            for &a in &row_set {
                // SAFETY: we checked that all the stages match at the start of this function
                unsafe { a.mul_into_unchecked(&b_inv, &mut a_mul_b_inv) }
                // If `a * !b` is not in `row_set`, then this can't be a group so we return false
                if !row_set.contains(&*a_mul_b_inv) {
                    return false;
                }
            }
        }
        // If all of the checks passed, then the set is a group
        true
    }

    /// A very collision-resistant hash function.  It is guaranteed to be perfectly
    /// collision-resistant on the following [`Stage`]s:
    /// - 16-bit machines: Up to 6 bells
    /// - 32-bit machines: Up to 9 bells
    /// - 64-bit machines: Up to 16 bells
    ///
    /// This hashing algorithm works by reading the row as a number using the stage as a base, thus
    /// guaranteeing that (ignoring overflow), two [`Row`]s will only be hashed to the same value
    /// if they are in fact the same.  This is ludicrously inefficient in terms of hash density,
    /// but it is fast and perfect and in most cases will suffice.
    pub fn fast_hash(&self) -> usize {
        let mut accum = 0;
        let mut multiplier = 1;
        for b in self.bell_iter() {
            accum += b.index() * multiplier;
            multiplier *= self.stage().num_bells();
        }
        accum
    }

    /// Creates a `&Row` from a `&[Bell]`, **without** checking that that slice forms a valid
    /// [`Row`].
    ///
    /// # Safety
    ///
    /// This is safe if the [`Bell`]s in `slice` are a valid [`Row`] according to the Framework.
    /// See [`Row`]'s docs for more information about this invariant.
    #[inline]
    pub unsafe fn from_slice_unchecked(slice: &[Bell]) -> &Row {
        // The unsafe pointer cast here is OK, because Row is a `#[repr(transparent)]` wrapper
        // around slices of `Bell`s and the pointer cast doesn't change the lifetime of the
        // underlying data.
        &*(slice as *const [Bell] as *const Row)
    }

    /// Creates a `&mut Row` from a `&mut [Bell]`, **without** checking that the slice forms a
    /// valid [`Row`].
    ///
    /// # Safety
    ///
    /// This is safe if the [`Bell`]s in `slice` are a valid [`Row`] according to the Framework.
    /// See [`Row`]'s docs for more information about this invariant.
    #[inline]
    pub unsafe fn from_mut_slice_unchecked(slice: &mut [Bell]) -> &mut Row {
        // The unsafe pointer cast here is OK, because Row a `#[repr(transparent)]` wrapper around
        // slices of `Bell`s and the pointer cast doesn't change the lifetime of the underlying
        // data.
        &mut *(slice as *mut [Bell] as *mut Row)
    }

    #[track_caller]
    fn check_stage(&self, row: &Row) {
        assert_eq!(
            self.stage(),
            row.stage(),
            "Stage mismatch: LHS has stage {:?} but RHS has stage {:?}",
            self.stage(),
            row.stage(),
        );
    }
}

impl Index<usize> for Row {
    type Output = Bell;

    /// `row[n]` gets the `n`th bell in `row` (0-indexed).
    ///
    /// # Panics
    ///
    /// Panics if `n >= row.stage().num_bells()`
    fn index(&self, index: usize) -> &Bell {
        &self.bell_slice[index]
    }
}

impl Not for &RowBuf {
    type Output = RowBuf;

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaruteed to have an inverse.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// // The inverse of Queens is Tittums
    /// assert_eq!(!&*RowBuf::parse("135246")?, RowBuf::parse("142536")?);
    /// // Backrounds is self-inverse
    /// assert_eq!(!&*RowBuf::backrounds(Stage::MAJOR), RowBuf::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// assert_eq!(!&*RowBuf::parse("1342")?, RowBuf::parse("1423")?);
    /// #
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    fn not(self) -> Self::Output {
        self.inv()
    }
}

impl Not for &Row {
    type Output = RowBuf;

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaruteed to have an inverse.
    fn not(self) -> Self::Output {
        self.inv()
    }
}

impl Not for RowBuf {
    type Output = RowBuf;

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaruteed to have an inverse.
    fn not(self) -> Self::Output {
        self.inv()
    }
}

impl Mul for &Row {
    type Output = RowBuf;

    /// Uses the RHS to permute the LHS without consuming either argument.
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf};
    ///
    /// // Multiplying two Rows of the same Stage just returns a new RowBuf
    /// assert_eq!(
    ///     &RowBuf::parse("13425678")? * &RowBuf::parse("43217568")?,
    ///     RowBuf::parse("24317568")?
    /// );
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    ///
    /// ```should_panic
    /// use bellframe::{RowBuf};
    ///
    /// // Multiplying two Rows of different Stages will panic rather than
    /// // produce undefined behaviour
    /// let _unrow = &*RowBuf::parse("13425678")? * &*RowBuf::parse("4321")?;
    /// # Ok::<(), bellframe::InvalidRowError>(())
    /// ```
    #[inline]
    fn mul(self, rhs: &Row) -> Self::Output {
        self.try_mul(rhs).unwrap()
    }
}

macro_rules! mul_impl {
    ($lhs: ty, $rhs: ty) => {
        impl Mul<$rhs> for $lhs {
            type Output = RowBuf;

            /// Uses the RHS to permute the LHS without consuming either argument.
            #[inline]
            fn mul(self, rhs: $rhs) -> Self::Output {
                self.try_mul(&rhs).unwrap()
            }
        }
    };
}

// Add impl for every pair within `RowBuf`, `&RowBuf` and `&Row` (except `&Row * &Row`, which we've
// explicitly implemented)
mul_impl!(RowBuf, RowBuf);
mul_impl!(RowBuf, &RowBuf);
mul_impl!(RowBuf, &Row);
mul_impl!(&RowBuf, RowBuf);
mul_impl!(&RowBuf, &RowBuf);
mul_impl!(&RowBuf, &Row);
mul_impl!(&Row, RowBuf);
mul_impl!(&Row, &RowBuf);
// NOTE: Intentionally commented, because `&Row, &Row` is the one explicit implementation
// mul_impl!(&Row, &Row);

impl MulAssign<&Row> for RowBuf {
    fn mul_assign(&mut self, rhs: &Row) {
        *self = &*self * rhs;
    }
}

impl MulAssign<&RowBuf> for RowBuf {
    fn mul_assign(&mut self, rhs: &RowBuf) {
        *self *= rhs.as_row();
    }
}

impl MulAssign<RowBuf> for RowBuf {
    fn mul_assign(&mut self, rhs: RowBuf) {
        *self *= rhs.as_row();
    }
}

impl<'row> IntoIterator for &'row Row {
    type Item = Bell;
    type IntoIter = BellIter<'row>;

    fn into_iter(self) -> Self::IntoIter {
        self.bell_iter()
    }
}

////////////////
// FORMATTING //
////////////////

impl Debug for Row {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Row({})", self)
    }
}

impl Display for Row {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for b in self.bell_iter() {
            write!(f, "{}", b)?;
        }
        Ok(())
    }
}

/// New-type of a [`Row`] which displays that [`Row`], but with all the fixed 'cover bells'
/// removed.  This will have the length of the [`effective_stage`](Row::effective_stage) of the
/// [`Row`].
pub struct ShortRow<'r>(pub &'r Row);

impl Display for ShortRow<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let length = self.0.effective_stage().num_bells();
        for b in self.0.bell_iter().take(length) {
            write!(f, "{}", b)?;
        }
        Ok(())
    }
}

/// A new-type over a [`Row`] which just displays the [`Bell`]s as its [`Debug`] implementation
/// (i.e. the [`Debug`] impl of [`DbgRow`] is identical to the [`Display`] impl of [`Row`]).
#[derive(Clone)]
pub struct DbgRow<'r>(pub &'r Row);

impl Debug for DbgRow<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::{Row, RowBuf};

    #[test]
    fn is_group() {
        #[track_caller]
        fn check(rows: &[&str]) {
            let rows: Vec<RowBuf> = rows.iter().map(|s| RowBuf::parse(s).unwrap()).collect();
            println!("Is {:?} a group?", rows);
            assert!(Row::is_group(rows.iter().map(|r| r.deref())));
        }

        check(&["1234", "1342", "1423"]);
        check(&["1"]);
        check(&["1234", "1324"]);
        check(&["1234", "1234", "1234", "1324"]);
        check(&["1234", "4123", "3412", "2341"]);
        check(&["123456", "134256", "142356", "132456", "124356", "143256"]);
        #[rustfmt::skip]
        check(&[
            "123456", "134562", "145623", "156234", "162345",
            "165432", "126543", "132654", "143265", "154326",
        ]);
        check(&["123456", "234561", "345612", "456123", "561234", "612345"]);
        #[rustfmt::skip]
        check(&[
            "123456", "234561", "345612", "456123", "561234", "612345",
            "654321", "165432", "216543", "321654", "432165", "543216",
        ]);
    }

    #[test]
    fn is_non_group() {
        #[track_caller]
        fn check(groups: &[&str]) {
            let rows: Vec<RowBuf> = groups.iter().map(|s| RowBuf::parse(s).unwrap()).collect();
            println!("Is {:?} not a group?", groups);
            assert!(!Row::is_group(rows.iter().map(|r| r.deref())));
        }

        check(&["21"]);
        check(&["123456", "134256", "142356", "132456", "124356"]); // 143256 is missing
        check(&[]); // The empty set doesn't contain an identity element
        check(&[
            "123456", "134256", "142356", "132456", "124356", "143256", "213456",
        ]);
    }

    #[test]
    fn order() {
        #[track_caller]
        fn check(row: &str, exp_order: usize) {
            assert_eq!(RowBuf::parse(row).unwrap().order(), exp_order);
        }

        check("1", 1);
        check("1234", 1);
        check("123456789", 1);
        check("21", 2);
        check("2134", 2);
        check("2143", 2);
        check("23145", 3);
        check("23451", 5);
        check("23154", 6);
        check("231564", 3);
        check("1452367890", 2);
    }
}
