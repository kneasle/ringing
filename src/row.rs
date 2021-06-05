//! A heap-allocated row of [`Bell`]s.  This is also used as a permutation.

use std::{
    cmp::Ordering,
    collections::{HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use crate::{Bell, IncompatibleStages, Parity, Stage};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

/// All the possible ways that a [`Row`] could be invalid.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidRowError {
    /// A [`Bell`] would appear twice in the new [`Row`] (for example in `113456` or `4152357`)
    DuplicateBell(Bell),
    /// A [`Bell`] is not within the range of the [`Stage`] of the new [`Row`] (for example `7` in
    /// `12745` or `5` in `5432`).
    BellOutOfStage(Bell, Stage),
    /// A given Bell would be missing from the [`Row`].  Note that this is only generated if we
    /// already know the [`Stage`] of the new [`Row`], otherwise the other two variants are
    /// sufficient for every case.
    MissingBell(Bell),
}

impl std::fmt::Display for InvalidRowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidRowError::DuplicateBell(bell) => {
                write!(f, "Bell '{}' appears twice.", bell)
            }
            InvalidRowError::BellOutOfStage(bell, stage) => {
                write!(f, "Bell '{}' is not within the stage {}", bell, stage)
            }
            InvalidRowError::MissingBell(bell) => {
                write!(f, "Bell '{}' is missing", bell)
            }
        }
    }
}

impl std::error::Error for InvalidRowError {}

/// A single `Row` of [`Bell`]s.
///
/// This can be viewed as a permutation of [rounds](Row::rounds) on a given [`Stage`].
///
/// A `Row` must always be valid according to
/// [the Framework](https://cccbr.github.io/method_ringing_framework/fundamentals.html) - i.e., it
/// must contain every [`Bell`] up to its [`Stage`] once and precisely once.  This is only checked
/// in the constructors and then used as assumed knowledge to avoid further checks.  This is
/// similar to how [`&str`](str) and [`String`] are required to be valid UTF-8.
///
/// # Example
/// ```
/// use proj_core::{Bell, Row, Stage, InvalidRowError};
///
/// // Create rounds on 8 bells.  Rounds is always valid on any `Stage`
/// let rounds_on_8 = Row::rounds(Stage::MAJOR);
/// assert_eq!(rounds_on_8.stage(), Stage::MAJOR);
/// assert_eq!(rounds_on_8.to_string(), "12345678");
///
/// // Parse a generic (valid) change from a string.  Note how invalid
/// // `char`s are skipped.  This could fail if the resulting `Row` is
/// // invalid, so we use ? to propogate that error out of the current
/// // function.
/// let queens = Row::parse("13579 | 24680")?;
/// assert_eq!(queens.stage(), Stage::ROYAL);
/// assert_eq!(queens.to_string(), "1357924680");
///
/// // If we try to parse an invalid `Row`, we get an error.  This means
/// // that we can assume that all `Row`s satisfy the Framework's definition
/// assert_eq!(
///     Row::parse("112345"),
///     Err(InvalidRowError::DuplicateBell(Bell::from_name('1').unwrap()))
/// );
/// #
/// # Ok::<(), InvalidRowError>(())
/// ```
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Row {
    /// The [`Bell`]s in the order that they would be rung.  Because of the 'valid row' invariant,
    /// this can't contain duplicate [`Bell`]s or any [`Bell`]s with number greater than the
    /// [`Stage`] of this [`Row`].
    bells: Vec<Bell>,
}

impl Row {
    /// Creates a `Row` from a [`Vec`] of [`Bell`]s, checking that the the resulting `Row` is valid.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, InvalidRowError, Row};
    ///
    /// // Converting a `Row` from a valid `Vec` of `Bell`s is fine
    /// assert_eq!(
    ///     Row::from_vec(vec![
    ///         Bell::from_name('4').unwrap(),
    ///         Bell::from_name('2').unwrap(),
    ///         Bell::from_name('1').unwrap(),
    ///         Bell::from_name('3').unwrap(),
    ///     ])?.to_string(),
    ///     "4213"
    /// );
    /// // Converting a `Row` from an invalid `Vec` of `Bell`s is not so fine
    /// assert_eq!(
    ///     Row::from_vec(vec![
    ///         Bell::from_name('4').unwrap(),
    ///         Bell::from_name('2').unwrap(),
    ///         Bell::from_name('1').unwrap(),
    ///         Bell::from_name('4').unwrap(),
    ///     ]),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_name('4').unwrap()))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn from_vec(bells: Vec<Bell>) -> Result<Row, InvalidRowError> {
        // This unsafety is OK because the resulting row is never used for anything other than a
        // validity check
        unsafe { Self::from_vec_unchecked(bells) }.check_validity()
    }

    /// Creates a `Row` from a [`Vec`] of [`Bell`]s, **without** checking that the the resulting
    /// `Row` is valid.  Only use this if you're certain that the input is valid, since performing
    /// invalid operations on `Row`s is undefined behaviour.
    ///
    /// # Safety
    ///
    /// This function is safe if `bells` corresponds to a valid `Row` according to the CC's
    /// Framework.  This means that each [`Bell`] is unique, and has [`index`](Bell::index) smaller
    /// than the `bells.len()`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, InvalidRowError, Row};
    ///
    /// # fn test() -> Option<()> {
    /// // Converting a `Row` from a valid `Vec` of `Bell`s is fine, but still unsafe
    /// assert_eq!(
    ///     unsafe {
    ///         Row::from_vec_unchecked(vec![
    ///             Bell::from_name('4')?,
    ///             Bell::from_name('2')?,
    ///             Bell::from_name('1')?,
    ///             Bell::from_name('3')?,
    ///         ])
    ///     }.to_string(),
    ///     "4213"
    /// );
    /// // Converting a `Row` from an invalid `Vec` of `Bell`s compiles and runs,
    /// // but silently creates an invalid `Row`
    /// assert_eq!(
    ///     unsafe {
    ///         Row::from_vec_unchecked(vec![
    ///             Bell::from_name('4')?,
    ///             Bell::from_name('2')?,
    ///             Bell::from_name('1')?,
    ///             Bell::from_name('4')?,
    ///         ])
    ///     }.to_string(),
    ///     "4214"
    /// );
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub unsafe fn from_vec_unchecked(bells: Vec<Bell>) -> Row {
        Row { bells }
    }

    /// Utility function that creates a `Row` from an iterator of [`Bell`]s, **without** performing
    /// the validity check.  This function is `unsafe`; only use it if you can guarantee that the
    /// resulting `Row` is valid.
    ///
    /// # Safety
    ///
    /// This function is safe if `iter` yields a valid `Row` according to the CC's Framework.  This
    /// means that each [`Bell`] is unique, and has [`index`](Bell::index) smaller than the number
    /// of items yeilded by `iter`.
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row, Stage, InvalidRowError};
    ///
    /// // Create a valid row from an iterator over `Bell`s
    /// let iter = [0, 3, 4, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = unsafe { Row::from_iter_unchecked(iter) };
    /// assert_eq!(row.to_string(), "14532");
    /// // Create an invalid row from an iterator over `Bell`s.  We get no error,
    /// // but doing anything with the resulting `Row` is undefined behaviour
    /// let iter = [0, 3, 7, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = unsafe { Row::from_iter_unchecked(iter) };
    /// assert_eq!(row.to_string(), "14832");
    /// ```
    pub unsafe fn from_iter_unchecked(iter: impl Iterator<Item = Bell>) -> Self {
        Self::from_vec_unchecked(iter.collect())
    }

    /// Returns the [`Stage`] of this `Row`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// // Rounds on a given `Stage` should have that `Stage`
    /// assert_eq!(Row::rounds(Stage::MINIMUS).stage(), Stage::MINIMUS);
    /// assert_eq!(Row::rounds(Stage::SEPTUPLES).stage(), Stage::SEPTUPLES);
    ///
    /// assert_eq!(Row::parse("41325")?.stage(), Stage::DOUBLES);
    /// assert_eq!(Row::parse("321 654 987 0")?.stage(), Stage::ROYAL);
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    #[inline(always)]
    pub fn stage(&self) -> Stage {
        self.bells.len().into()
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), but without checking that the
    /// [`Stage`]s are compatible.  This is slighlty faster than using `*` or [`Row::mul`], but is
    /// `unsafe`: the output is not guaruteed to be valid unless both inputs have the same
    /// [`Stage`].
    ///
    /// # Safety
    ///
    /// This is safe if the two `Row`s have the same [`Stage`] (which is often an invariant
    /// enforced by other datatypes, such as [`Block`]).
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row, Stage, IncompatibleStages};
    ///
    /// // Multiplying two Rows of the same Stage is OK, but still unsafe
    /// assert_eq!(
    ///     unsafe {
    ///         Row::parse("13425678")?.mul_unchecked(&Row::parse("43217568")?)
    ///     },
    ///     Row::parse("24317568")?
    /// );
    /// // Multiplying two Rows of different Stages is not OK, and creates an invalid Row.
    /// // Note how both sides of the `assert_eq` have to use unsafe to create an invalid Row.
    /// assert_eq!(
    ///     unsafe { Row::parse("13475628")?.mul_unchecked(&Row::parse("4321")?) },
    ///     unsafe {Row::from_vec_unchecked(
    ///         [7, 4, 3, 1].iter().map(|&x| Bell::from_number(x).unwrap()).collect()
    ///     )}
    /// );
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub unsafe fn mul_unchecked(&self, rhs: &Self) -> Self {
        // We bypass the validity check because if two Rows are valid, then so is their product.
        // However, this function is also unsafe because permuting two rows of different Stages
        // causes undefined behaviour
        Row::from_vec_unchecked(rhs.bell_iter().map(|b| self[b.index()]).collect())
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), storing the result in an
    /// existing `Row` (thus making use of its allocation).
    ///
    /// # Safety
    ///
    /// This is safe if `self` and `rhs` both have the same [`Stage`].
    pub unsafe fn mul_into_unchecked(&self, rhs: &Self, out: &mut Self) {
        // We bypass the validity check because if two Rows are valid, then so is their product.
        // However, this function is also unsafe because permuting two rows of different Stages
        // causes undefined behaviour
        out.bells.clear();
        out.bells.extend(rhs.bell_iter().map(|b| self[b.index()]));
    }

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaruteed to have an inverse.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// // The inverse of Queens is Tittums
    /// assert_eq!(!Row::parse("135246")?, Row::parse("142536")?);
    /// // Backrounds is self-inverse
    /// assert_eq!(!Row::backrounds(Stage::MAJOR), Row::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// assert_eq!(!Row::parse("1342")?, Row::parse("1423")?);
    /// #
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn inv(&self) -> Self {
        let mut inv_bells = vec![Bell::TREBLE; self.stage().as_usize()];
        for (i, b) in self.bells.iter().enumerate() {
            inv_bells[b.index()] = Bell::from_index(i);
        }
        // This unsafety is OK because Rows form a group and by the closure of groups under
        // inversion, if `self` is in the group of permutations, then so is `!self`.
        unsafe { Row::from_vec_unchecked(inv_bells) }
    }

    /// Calculate the inverse of this `Row`, storing the result in an existing `Row` (thus making
    /// use of its allocation).  This resizes `out` to make it the right [`Stage`] to take the
    /// output value.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// // Create a new row that will be overwritten to avoid reallocations
    /// let mut row_buf = Row::empty();
    /// // The inverse of Queens is Tittums
    /// Row::parse("135246")?.inv_into(&mut row_buf);
    /// assert_eq!(row_buf, Row::parse("142536")?);
    /// // Backrounds is self-inverse
    /// Row::backrounds(Stage::MAJOR).inv_into(&mut row_buf);
    /// assert_eq!(row_buf, Row::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// Row::parse("1342")?.inv_into(&mut row_buf);
    /// assert_eq!(row_buf, Row::parse("1423")?);
    /// #
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn inv_into(&self, out: &mut Self) {
        // Make sure that `out` has the right stage
        match out.stage().cmp(&self.stage()) {
            Ordering::Less => {
                out.bells.extend(
                    std::iter::repeat(Bell::TREBLE).take(self.bells.len() - out.bells.len()),
                );
            }
            Ordering::Greater => {
                out.bells.drain(self.bells.len()..);
            }
            Ordering::Equal => {}
        }
        debug_assert_eq!(out.stage(), self.stage());
        // Now perform the inversion
        for (i, b) in self.bell_iter().enumerate() {
            out.bells[b.index()] = Bell::from_index(i);
        }
    }

    /// Swap two [`Bell`]s round in this `Row`, panicking if either of the indices point out of
    /// bounds.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// let mut rounds = Row::rounds(Stage::MAJOR);
    /// assert_eq!(rounds.to_string(), "12345678");
    /// rounds.swap(0, 1); // Note we are using 0-indexing
    /// assert_eq!(rounds.to_string(), "21345678");
    /// rounds.swap(2, 5); // Note we are using 0-indexing
    /// assert_eq!(rounds.to_string(), "21645378");
    /// ```
    #[inline(always)]
    pub fn swap(&mut self, a: usize, b: usize) {
        self.bells.swap(a, b);
    }

    /// Extend this `Row` in-place with cover bells so that it has a given [`Stage`]
    pub fn extend_to_stage(&mut self, stage: Stage) {
        self.bells
            .extend((self.bells.len()..stage.as_usize()).map(Bell::from_index));
    }

    /// Parse a string into a `Row`, skipping any [`char`]s that aren't valid bell names.  This
    /// returns `Err(`[`InvalidRowError`]`)` if the `Row` would be invalid.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row, Stage, InvalidRowError};
    ///
    /// // Parsing a valid Row is fine
    /// assert_eq!(Row::parse("12543")?.to_string(), "12543");
    /// // Parsing valid rows with invalid characters is also fine
    /// assert_eq!(Row::parse("4321\t[65 78]")?.to_string(), "43216578");
    /// assert_eq!(Row::parse("3|2|1  6|5|4  9|8|7")?.to_string(), "321654987");
    /// // Parsing an invalid `Row` returns an error describing the problem
    /// assert_eq!(
    ///     Row::parse("112345"),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_number(1).unwrap()))
    /// );
    /// assert_eq!(
    ///     Row::parse("12745"),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_number(7).unwrap(),
    ///         Stage::DOUBLES
    ///     ))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn parse(s: &str) -> Result<Self, InvalidRowError> {
        Self::from_iter(s.chars().filter_map(Bell::from_name))
    }

    /// Parse a string into a `Row`, extending to the given [`Stage`] if required and skipping any
    /// [`char`]s that aren't valid bell names.  This returns `Err(`[`InvalidRowError`]`)` if the
    /// `Row` would be invalid, and this will produce better error messages than [`Row::parse`]
    /// because of the extra information provided by the [`Stage`].
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row, Stage, InvalidRowError};
    ///
    /// // Parsing a valid Row is fine
    /// assert_eq!(Row::parse("12543")?.to_string(), "12543");
    /// // Parsing valid rows with invalid characters is also fine
    /// assert_eq!(Row::parse("4321\t[65 78]")?.to_string(), "43216578");
    /// assert_eq!(Row::parse("3|2|1  6|5|4  9|8|7")?.to_string(), "321654987");
    /// // Parsing an invalid `Row` returns an error describing the problem
    /// assert_eq!(
    ///     Row::parse("112345"),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_number(1).unwrap()))
    /// );
    /// assert_eq!(
    ///     Row::parse("12745"),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_name('7').unwrap(),
    ///         Stage::DOUBLES
    ///     ))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn parse_with_stage(s: &str, stage: Stage) -> Result<Self, InvalidRowError> {
        // This unsafety is OK because the resulting row is never used for anything other than a
        // validity check
        unsafe { Self::from_iter_unchecked(s.chars().filter_map(Bell::from_name)) }
            .check_validity_with_stage(stage)
    }

    /// Creates rounds on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// assert_eq!(Row::rounds(Stage::MINIMUS).to_string(), "1234");
    /// assert_eq!(Row::rounds(Stage::CATERS).to_string(), "123456789");
    /// ```
    pub fn rounds(stage: Stage) -> Self {
        // This unsafety is OK, because rounds is always a valid `Row`
        unsafe { Self::from_iter_unchecked((0..stage.as_usize()).map(Bell::from_index)) }
    }

    /// Creates backrounds on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// assert_eq!(Row::backrounds(Stage::MINIMUS).to_string(), "4321");
    /// assert_eq!(Row::backrounds(Stage::CATERS).to_string(), "987654321");
    /// ```
    pub fn backrounds(stage: Stage) -> Self {
        // This unsafety is OK, because backrounds is always a valid `Row`
        unsafe { Self::from_iter_unchecked((0..stage.as_usize()).rev().map(Bell::from_index)) }
    }

    /// Creates Queens on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// assert_eq!(Row::queens(Stage::MINIMUS).to_string(), "1324");
    /// assert_eq!(Row::queens(Stage::CATERS).to_string(), "135792468");
    /// ```
    pub fn queens(stage: Stage) -> Self {
        // This unsafety is OK, because Queens is always a valid `Row`
        unsafe {
            Self::from_iter_unchecked(
                (0..stage.as_usize())
                    .step_by(2)
                    .chain((1..stage.as_usize()).step_by(2))
                    .map(Bell::from_index),
            )
        }
    }

    /// Creates a `Row` containing no [`Bell`]s, but without causing any allocations.  This is
    /// useful for initialising temporary `Row`s.
    pub fn empty() -> Self {
        // This unsafety is OK, because 0-length rows are always valid (albeit useless in most
        // cases)
        unsafe { Self::from_iter_unchecked(std::iter::empty()) }
    }

    /// Utility function that creates a `Row` from an iterator of [`Bell`]s, performing the
    /// validity check.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row, Stage, InvalidRowError};
    ///
    /// // Create a valid row from an iterator over `Bell`s
    /// let iter = [0, 3, 4, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = Row::from_iter(iter)?;
    /// assert_eq!(row.to_string(), "14532");
    /// // Attempt to create an invalid row from an iterator over `Bell`s
    /// // (we get an error)
    /// let iter = [0, 3, 7, 2, 1].iter().copied().map(Bell::from_index);
    /// assert_eq!(
    ///     Row::from_iter(iter),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_name('8').unwrap(),
    ///         Stage::DOUBLES,
    ///     ))
    /// );
    ///
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    #[allow(clippy::should_implement_trait)]
    pub fn from_iter(iter: impl Iterator<Item = Bell>) -> Result<Self, InvalidRowError> {
        // This unsafety is OK because the resulting row is never used for anything other than a
        // validity check
        unsafe { Self::from_iter_unchecked(iter) }.check_validity()
    }

    /// All the `Row`s formed by repeatedly permuting a given `Row`.  The first item returned will
    /// always be the input `Row`, and the last will always be `rounds`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row};
    ///
    /// // The closure of "18234567" are all the fixed-treble cyclic part heads.
    /// assert_eq!(
    ///     Row::parse("18234567")?.closure(),
    ///     vec![
    ///         Row::parse("18234567")?,
    ///         Row::parse("17823456")?,
    ///         Row::parse("16782345")?,
    ///         Row::parse("15678234")?,
    ///         Row::parse("14567823")?,
    ///         Row::parse("13456782")?,
    ///         Row::parse("12345678")?,
    ///     ]
    /// );
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn closure(&self) -> Vec<Self> {
        let mut closure = Vec::new();
        let mut row = self.clone();
        loop {
            closure.push(row.clone());
            if row.is_rounds() {
                return closure;
            }
            // This unsafety is OK, because `self` is a valid Row and `row` and `self` will always
            // have the same Stage
            row = unsafe { row.mul_unchecked(self) };
        }
    }

    /// Returns the [`Parity`] of this [`Row`].
    pub fn parity(mut self) -> Parity {
        // Invariants: self.bells[..first_non_rounds_bell] is sorted
        let mut num_swaps = 0;
        let mut first_non_rounds_bell = 0;
        while first_non_rounds_bell < self.stage().as_usize() {
            let cur_bell = self.bells[first_non_rounds_bell];
            if cur_bell == Bell::from_index(first_non_rounds_bell) {
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

    /// All the `Row`s formed by repeatedly permuting a given `Row`, but the first `Row` returned
    /// will always be [rounds](Row::rounds), rather than `self`.  This is useful for situations
    /// like generating part heads, where it's more intutive for the closure to start at rounds.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row};
    ///
    /// // The closure of "18234567" are all the fixed-treble cyclic part heads.
    /// // Note how rounds is the first Row/part head generated
    /// assert_eq!(
    ///     Row::parse("18234567")?.closure_from_rounds(),
    ///     vec![
    ///         Row::parse("12345678")?,
    ///         Row::parse("18234567")?,
    ///         Row::parse("17823456")?,
    ///         Row::parse("16782345")?,
    ///         Row::parse("15678234")?,
    ///         Row::parse("14567823")?,
    ///         Row::parse("13456782")?,
    ///     ]
    /// );
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn closure_from_rounds(&self) -> Vec<Self> {
        let mut closure = vec![Self::rounds(self.stage())];
        let mut row = self.clone();
        while !row.is_rounds() {
            closure.push(row.clone());
            // This unsafety is OK, because `self` is a valid Row and `row` and `self` will always
            // have the same Stage
            row = unsafe { row.mul_unchecked(self) };
        }
        closure
    }

    /// Takes a sequence of sets of `Row`s (`[X_1, X_2, ..., X_n]`) and computes every product
    /// `x_1 * x_2 * ... * x_n` where `x_i` comes from `X_i` for all `i`.
    pub fn multi_cartesian_product(
        row_sets: impl IntoIterator<Item = Vec<Self>>,
    ) -> Result<Vec<Self>, IncompatibleStages> {
        let mut set_iter = row_sets.into_iter();
        let mut stage: Option<Stage> = None;
        // We will always be transposing the contents of `transpose_from` with the new values,
        // putting the results into `transpose_to`.  At the end of every loop iteration these are
        // swapped round
        // PERF: Use same stage buffers here for linear layout and massive reduction in allocations
        let mut transpose_from: Vec<Self> = Vec::new();
        let mut transpose_to: Vec<Self> = Vec::new();
        // Consume the first set as a special case:
        match set_iter.next() {
            // If it doesn't exist, no things are being CPed together so we return the empty Vec
            None => return Ok(Vec::new()),
            // If it does exist, then populate the `transpose_from` buffer with it and initialise
            // the stage
            Some(set) => {
                for r in set.into_iter() {
                    IncompatibleStages::test_err_opt(&mut stage, r.stage())?;
                    transpose_from.push(r.clone());
                }
            }
        }
        // Now, treat all subsequent sets identically
        for set in set_iter {
            // First up, check if `transpose_from` is empty, in which case the output will be empty
            if transpose_from.is_empty() {
                return Ok(Vec::new());
            }
            // Unwrap the stage once, since it has either been set by now or `transpose_from` is
            // empty
            let s = stage.unwrap();
            // Now, transpose every item in `transpose_from` with every item from the new set and
            // push into `transpose_to`
            transpose_to.clear();
            for r2 in &set {
                IncompatibleStages::test_err(s, r2.stage())?;
                for r1 in &transpose_from {
                    transpose_to.push(unsafe { r1.mul_unchecked(r2) });
                }
            }
            // Finally, swap the buffers so that we read from the newly transposed rows
            std::mem::swap(&mut transpose_to, &mut transpose_from);
        }
        // Note: we return `transpose_from` here (rather than `transpose_to`) because the two
        // buffers have just been swapped at the end of the loop iteration
        Ok(transpose_from)
    }

    /// Generates the least group containing a given set of `Row`s, returning the result in a
    /// [`HashSet`] (therefore, the result is unordered).  The current algorithm is quite slow; if
    /// anyone knows of a better one, then please let me know...
    pub fn least_group_containing<'a>(
        rows: impl IntoIterator<Item = &'a Self> + Clone,
    ) -> Result<HashSet<Self>, IncompatibleStages>
    where
        Self: 'a,
    {
        // The algorithm used here is to expand every possible way of expanding the input elements,
        // in depth first order.
        let mut set = HashSet::<Self>::new();
        let mut stage: Option<Stage> = None;
        let mut frontier = VecDeque::<Self>::new();
        // We seed the frontier and `set` manually the first time round to avoid checking the
        // stages all the time (if the input rows are all compatible, then so will any finite
        // product of them).
        for r in rows.clone().into_iter() {
            IncompatibleStages::test_err_opt(&mut stage, r.stage())?;
            if set.insert(r.clone()) {
                frontier.push_back(r.clone());
            }
        }
        // Now, we repeatedly pop the last item of the frontier and post-multiply it by every row
        // in the input set.  We check each of these for inclusion *before* pushing it back to the
        // frontier (thus avoiding causing allocations if we need to).  This loop must terminate,
        // because at each iteration the number of unexpanded nodes in the tree decreases and that
        // number is bounded by the size of the resulting group (which is finite).
        while let Some(r) = frontier.pop_front() {
            for r2 in rows.clone().into_iter() {
                // This unsafety is OK because we checked that all Rows in `rows` have equal stages
                let new_row = unsafe { r.mul_unchecked(r2) };
                if !set.contains(&new_row) {
                    frontier.push_back(new_row.clone());
                    set.insert(new_row);
                }
            }
        }
        Ok(set)
    }

    /// Determines if the given set of [`Row`]s forms a group.  This performs `n^2` transpositions
    /// and `n` inversions where `n` is the number of unique elements yeilded by `rows`.  See [this
    /// Wikipedia page](https://en.wikipedia.org/wiki/Subgroup_test) for the algorithm used.
    pub fn is_group<'a>(
        rows: impl IntoIterator<Item = &'a Self>,
    ) -> Result<bool, IncompatibleStages>
    where
        Self: 'a,
    {
        // Build a hash set with the contents of `rows`
        let row_set: HashSet<&Self> = rows.into_iter().collect();
        // We early return here because if the set is empty then this cannot be a group but all the
        // checks will be vacuously satisfied
        if row_set.is_empty() {
            return Ok(false);
        }
        // Check that stages match
        let mut first_stage: Option<Stage> = None;
        for r in &row_set {
            if let Some(fs) = first_stage {
                IncompatibleStages::test_err(fs, r.stage())?;
            } else {
                first_stage = Some(r.stage());
            }
        }
        // Now perform the group check by verifying that `a * !b` is in the set for all a, b in
        // `row_set`.
        // PERF: We're multiplying every row by its inverse, which always gives rounds and
        // therefore we can replace those checks with an in-place rounds check on the incoming rows
        // and thus gain performance
        // The buffers `b_inv` and `a_mul_b_inv` are reused in each loop iteration to avoid
        // performing `n(n + 1)` allocations.
        let mut b_inv = Self::empty();
        let mut a_mul_b_inv = Self::empty();
        for &b in &row_set {
            b.inv_into(&mut b_inv);
            for &a in &row_set {
                // This unsafety is OK because we checked that all the stages match at the start of
                // this function
                unsafe { a.mul_into_unchecked(&b_inv, &mut a_mul_b_inv) }
                // If `a * !b` is not in `row_set`, then this can't be a group so we return false
                if !row_set.contains(&a_mul_b_inv) {
                    return Ok(false);
                }
            }
        }
        // If all of the checks passed, then the set is a group
        Ok(true)
    }

    /* Once GATs are possible, we will be able to make default implementations for these */

    /// Checks the validity of a potential `Row`, returning it if valid and returning an
    /// [`InvalidRowError`] otherwise (consuming the potential `Row` so it can't be used).
    pub fn check_validity(self) -> Result<Self, InvalidRowError> {
        check_validity(self.stage(), self.bell_iter())?;
        Ok(self)
    }

    /// Checks the validity of a potential `Row`, extending it to the given [`Stage`] if valid and
    /// returning an [`InvalidRowError`] otherwise (consuming the potential `Row` so it can't be
    /// used).  This will provide nicer errors than [`Row::check_validity`] since this has extra
    /// information about the desired [`Stage`] of the potential `Row`.
    pub fn check_validity_with_stage(mut self, stage: Stage) -> Result<Self, InvalidRowError> {
        check_validity_with_stage(stage, self.bell_iter())?;
        // If no errors were generated so far, then extend the row and return
        self.extend_to_stage(stage);
        Ok(self)
    }

    /// Gets the **0-indexed** place at which a given [`Bell`] appears in this `Row`, returning
    /// `None` if the [`Bell`] is out of the stage.  This performs a linear search of the `Row`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row};
    ///
    /// # fn test() -> Option<()> {
    /// let tittums = Row::parse("15263748").unwrap();
    /// // The treble is leading in position 0
    /// assert_eq!(tittums.place_of(Bell::from_name('1')?)?, 0);
    /// // The '5' is at index `1`, because indices always start from zero
    /// assert_eq!(tittums.place_of(Bell::from_name('5')?)?, 1);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub fn place_of(&self, bell: Bell) -> Option<usize> {
        self.bells.iter().position(|b| *b == bell)
    }

    /// Perform an in-place check that this `Row` is equal to rounds.  `x.is_rounds()` is an
    /// optimised version of `x == Row::rounds(x.stage())`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// // Rounds is ... rounds (DOH)
    /// assert!(Row::rounds(Stage::MAXIMUS).is_rounds());
    /// // This is not rounds
    /// assert!(!Row::parse("18423756")?.is_rounds());
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn is_rounds(&self) -> bool {
        self.bell_iter().enumerate().all(|(i, b)| b.index() == i)
    }

    /// Multiply two `Row`s (i.e. use the RHS to permute the LHS), checking that the [`Stage`]s are
    /// compatible.  This is like using [`*`](<Row as Mul>::mul), except that this returns a
    /// [`Result`] instead of [`panic!`]ing.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row};
    ///
    /// // Multiplying two Rows of the same Stage is fine
    /// assert_eq!(
    ///     Row::parse("13425678")?.mul(&Row::parse("43217568")?),
    ///     Ok(Row::parse("24317568")?)
    /// );
    /// // Multiplying two Rows of different Stages causes an error but no
    /// // undefined behaviour
    /// assert_eq!(
    ///     &Row::parse("13425678")?
    ///         .mul(&Row::parse("4321")?)
    ///         .unwrap_err()
    ///         .to_string(),
    ///     "Incompatible stages: Major (lhs), Minimus (rhs)"
    /// );
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn mul(&self, rhs: &Self) -> Result<Self, IncompatibleStages> {
        IncompatibleStages::test_err(self.stage(), rhs.stage())?;
        // This unsafety is OK because the `self` and `rhs` are both assumed to be valid, and we
        // have already checked that their stages are equal
        Ok(unsafe { self.mul_unchecked(rhs) })
    }

    /// Computes the value of `r` which satisfies `r * self = other` - i.e. the `Row` which
    /// pre-transposes `self` to `other`.
    #[inline]
    pub fn tranposition_to(&self, other: &Self) -> Result<Self, IncompatibleStages> {
        other.mul(&self.inv())
    }

    /// Computes the value of `r` which satisfies `r * self = other` - i.e. the `Row` which
    /// pre-transposes `self` to `other`, bypassing the same-[`Stage`] check.
    ///
    /// # Safety
    ///
    /// This is safe if `self` and `other` have the same [`Stage`].
    #[inline]
    pub unsafe fn tranposition_to_unchecked(&self, other: &Self) -> Self {
        other.mul_unchecked(&self.inv())
    }

    /// Returns an iterator over the [`Bell`]s in this [`Row`]
    #[inline]
    pub fn bell_iter(&self) -> std::iter::Cloned<std::slice::Iter<'_, Bell>> {
        self.slice().iter().cloned()
    }

    /// Returns an immutable reference to the underlying slice of [`Bell`]s that makes up this
    /// `Row`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Bell, Row};
    ///
    /// let tittums = Row::parse("15263748")?;
    /// assert_eq!(tittums.slice()[3], Bell::from_name('6').unwrap());
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    #[inline]
    pub fn slice(&self) -> &[Bell] {
        self.bells.as_slice()
    }

    /// Concatenates the names of the [`Bell`]s in this `Row` to the end of a [`String`].  Using
    /// `row.to_string()` will behave the same as this but will return an newly allocated
    /// [`String`].
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row};
    ///
    /// let waterfall = Row::parse("6543217890")?;
    /// let mut string = "Waterfall is: ".to_owned();
    /// waterfall.push_to_string(&mut string);
    /// assert_eq!(string, "Waterfall is: 6543217890");
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    pub fn push_to_string(&self, string: &mut String) {
        for b in &self.bells {
            string.push_str(&b.name());
        }
    }

    /// A very collision-resistant hash function.  It is guarunteed to be perfectly
    /// collision-resistant on the following [`Stage`]s:
    /// - 16-bit machines: Up to 6 bells
    /// - 32-bit machines: Up to 9 bells
    /// - 64-bit machines: Up to 16 bells
    ///
    /// This hashing algorithm works by reading the row as a number using the stage as a base, thus
    /// guarunteeing that (ignoring overflow), two [`Row`]s will only be hashed to the same value
    /// if they are in fact the same.  This is ludicrously inefficient in terms of hash density,
    /// but it is fast and perfect and in most cases will suffice.
    pub fn fast_hash(&self) -> usize {
        let mut accum = 0;
        let mut multiplier = 1;
        for b in self.bells.iter() {
            accum += b.index() * multiplier;
            multiplier *= self.stage().as_usize();
        }
        accum
    }
}

impl std::fmt::Debug for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Row({})", self.to_string())
    }
}

impl std::fmt::Display for Row {
    /// Returns a [`String`] representing this `Row`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// assert_eq!(Row::rounds(Stage::MAJOR).to_string(), "12345678");
    /// assert_eq!(Row::parse("146235")?.to_string(), "146235");
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::with_capacity(self.stage().as_usize());
        self.push_to_string(&mut s);
        write!(f, "{}", s)
    }
}

impl std::ops::Index<usize> for Row {
    type Output = Bell;

    fn index(&self, index: usize) -> &Bell {
        &self.slice()[index]
    }
}

impl std::ops::Mul for Row {
    type Output = Row;

    /// See [`&Row * &Row`](<&Row as std::ops::Mul>::mul) for docs.
    fn mul(self, rhs: Row) -> Self::Output {
        // Delegate to the borrowed version
        &self * &rhs
    }
}

impl std::ops::Mul for &Row {
    type Output = Row;

    /// Uses the RHS to permute the LHS without consuming either argument.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row};
    ///
    /// // Multiplying two Rows of the same Stage just returns a new Row
    /// assert_eq!(
    ///     &Row::parse("13425678")? * &Row::parse("43217568")?,
    ///     Row::parse("24317568")?
    /// );
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    ///
    /// ```should_panic
    /// use proj_core::{Row};
    ///
    /// // Multiplying two Rows of different Stages panics rather than
    /// // producing undefined behaviour
    /// let _unrow = &Row::parse("13425678")? * &Row::parse("4321")?;
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    fn mul(self, rhs: &Row) -> Self::Output {
        assert_eq!(self.stage(), rhs.stage());
        // This unsafety is OK because the product of two valid Rows of the same Stage is always
        // valid (because groups are closed under their binary operation).
        unsafe { self.mul_unchecked(rhs) }
    }
}

impl std::ops::Not for Row {
    type Output = Row;

    /// See [`!&Row`](<&Row as std::ops::Not>::not) for docs.
    #[inline]
    fn not(self) -> Self::Output {
        // Delegate to the borrowed version
        !&self
    }
}

impl std::ops::Not for &Row {
    type Output = Row;

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaruteed to have an inverse.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, Stage};
    ///
    /// // The inverse of Queens is Tittums
    /// assert_eq!(!Row::parse("135246")?, Row::parse("142536")?);
    /// // Backrounds is self-inverse
    /// assert_eq!(!Row::backrounds(Stage::MAJOR), Row::backrounds(Stage::MAJOR));
    /// // `1324` inverts to `1423`
    /// assert_eq!(!Row::parse("1342")?, Row::parse("1423")?);
    /// #
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    fn not(self) -> Self::Output {
        self.inv()
    }
}

fn check_validity(stage: Stage, bells: impl Iterator<Item = Bell>) -> Result<(), InvalidRowError> {
    // We check validity by keeping a checklist of which `Bell`s we've seen, and checking off
    // each bell as we go.
    let mut checklist = vec![false; stage.as_usize()];
    // Loop over all the bells to check them off in the checklist.  We do not need to check for
    // empty spaces in the checklist once we've done because (by the Pigeon Hole Principle),
    // fitting `n` bells into `n` slots with some gaps will always require that a bell is
    // either out of range or two bells share a slot.
    for b in bells {
        match checklist.get_mut(b.index()) {
            // If the `Bell` is out of range of the checklist, it can't belong within the `Stage`
            // of this `Row`
            None => return Err(InvalidRowError::BellOutOfStage(b, stage)),
            // If the `Bell` has already been seen before, then it must be a duplicate
            Some(&mut true) => return Err(InvalidRowError::DuplicateBell(b)),
            // If the `Bell` has not been seen before, check off the checklist entry and continue
            Some(x) => *x = true,
        }
    }
    // If none of the `Bell`s caused errors, the row must be valid
    Ok(())
}

fn check_validity_with_stage(
    stage: Stage,
    bells: impl Iterator<Item = Bell>,
) -> Result<(), InvalidRowError> {
    // We check validity by keeping a checklist of which `Bell`s we've seen, and checking off
    // each bell as we go.
    let mut checklist = vec![false; stage.as_usize()];
    // It's OK to initialise this with the `TREBLE` (and not handle the case where there are no
    // bells),
    let mut biggest_bell_found = Bell::TREBLE;
    // Loop over all the bells to check them off in the checklist
    for b in bells {
        match checklist.get_mut(b.index()) {
            // If the `Bell` is out of range of the checklist, it can't belong within the `Stage`
            // of this `Row`
            None => return Err(InvalidRowError::BellOutOfStage(b, stage)),
            // If the `Bell` has already been seen before, then it must be a duplicate
            Some(&mut true) => return Err(InvalidRowError::DuplicateBell(b)),
            // If the `Bell` has not been seen before, check off the checklist entry and continue
            Some(x) => *x = true,
        }
        biggest_bell_found = b.max(biggest_bell_found);
    }
    // The Pigeon Hole Principle argument from `check_validity` doesn't apply here, because
    // there could be fewer `Bell`s than the `stage` specified.  However, this does allow us to
    // accurately say when bells are missing so we do another pass over the `checklist` to
    // check for missing bells.  If this check also passes, then `self` must be a valid `Row`
    // of some stage <= `stage`.
    //
    // The iterator chain runs a linear search the first instance of `false` up to
    // `biggest_bell_found`, which is the index of our missing bell.  There looks like there is
    // an off-by-one error here since we skip checking `biggest_bell_found` which is
    // technically within the specified range, but this is OK because (by definition) we know
    // that a bell of `biggest_bell_found` has been found, so it cannot be missing.
    if let Some((index, _)) = checklist[..biggest_bell_found.index()]
        .iter()
        .enumerate()
        .find(|(_i, x)| !**x)
    {
        return Err(InvalidRowError::MissingBell(Bell::from_index(index)));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{Bell, InvalidRowError, Row, Stage};

    #[test]
    fn parse_with_stage_ok() {
        for (inp_str, stage, exp_row) in &[
            ("321", Stage::SINGLES, "321"),
            ("321", Stage::MINOR, "321456"),
            ("1342", Stage::MAJOR, "13425678"),
            ("123564", Stage::ROYAL, "1235647890"),
            ("21", Stage::DOUBLES, "21345"),
            ("", Stage::MINIMUS, "1234"),
        ] {
            assert_eq!(
                Row::parse_with_stage(inp_str, *stage).unwrap(),
                Row::parse(exp_row).unwrap()
            );
        }
    }

    #[test]
    fn parse_with_stage_err() {
        // Input rows with duplicated bells
        for (inp_str, stage, dup_bell) in &[
            ("322", Stage::SINGLES, '2'),
            ("11", Stage::MAXIMUS, '1'),
            ("512435", Stage::MINOR, '5'),
            ("331212", Stage::MINOR, '3'),
        ] {
            assert_eq!(
                Row::parse_with_stage(inp_str, *stage),
                Err(InvalidRowError::DuplicateBell(
                    Bell::from_name(*dup_bell).unwrap()
                ))
            );
        }
        // Input rows which contain bells that don't fit into the specified stage
        for (inp_str, stage, bell_out_of_range) in &[
            ("0", Stage::SINGLES, '0'),
            ("3218", Stage::MINOR, '8'),
            ("12345678", Stage::SINGLES, '4'),
        ] {
            assert_eq!(
                Row::parse_with_stage(inp_str, *stage),
                Err(InvalidRowError::BellOutOfStage(
                    Bell::from_name(*bell_out_of_range).unwrap(),
                    *stage
                ))
            );
        }
        // Input rows with missing bells
        for (inp_str, stage, missing_bell) in &[
            ("13", Stage::SINGLES, '2'),
            ("14", Stage::MINOR, '2'),
            ("14567892", Stage::CATERS, '3'),
        ] {
            assert_eq!(
                Row::parse_with_stage(inp_str, *stage),
                Err(InvalidRowError::MissingBell(
                    Bell::from_name(*missing_bell).unwrap(),
                ))
            );
        }
    }

    #[test]
    fn is_group() {
        #[rustfmt::skip]
        let groups = [
            vec!["1234", "1342", "1423"],
            vec!["1"],
            vec!["1234", "1324"],
            vec!["1234", "1234", "1234", "1324"],
            vec!["1234", "4123", "3412", "2341"],
            vec!["123456", "134256", "142356", "132456", "124356", "143256"],
            vec![
                "123456", "134562", "145623", "156234", "162345",
                "165432", "126543", "132654", "143265", "154326",
            ],
            vec!["123456", "234561", "345612", "456123", "561234", "612345"],
            vec![
                "123456", "234561", "345612", "456123", "561234", "612345",
                "654321", "165432", "216543", "321654", "432165", "543216",
            ],
        ];
        let non_groups = [
            vec!["21"],
            vec!["123456", "134256", "142356", "132456", "124356"], // 143256 is missing
            vec![], // The empty set doesn't contain an identity element
            vec![
                "123456", "134256", "142356", "132456", "124356", "143256", "213456",
            ],
        ];

        for g in &groups {
            let rows: Vec<Row> = g.iter().map(|s| Row::parse(s).unwrap()).collect();
            println!("Is {:?} a group?", g);
            assert!(Row::is_group(rows.iter()).unwrap());
        }
        for g in &non_groups {
            let rows: Vec<Row> = g.iter().map(|s| Row::parse(s).unwrap()).collect();
            println!("Is {:?} not a group?", g);
            assert!(!Row::is_group(rows.iter()).unwrap());
        }
    }
}
