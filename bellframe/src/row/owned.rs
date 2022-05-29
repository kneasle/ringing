use std::{
    borrow::{Borrow, BorrowMut},
    convert::TryFrom,
    fmt::{Debug, Display, Formatter},
    ops::{Deref, DerefMut},
    str::FromStr,
};

use itertools::Itertools;

use crate::{Bell, InvalidRowError, Stage};

// Imports used solely for doc comments
#[allow(unused_imports)]
use crate::Block;

use super::{borrowed::Row, BellIter};

/// An owned row.
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct RowBuf {
    /// The [`Bell`]s in the order that they would be rung.  Because of the 'valid row' invariant,
    /// this can't contain duplicate [`Bell`]s or any [`Bell`]s with number greater than the
    /// [`Stage`] of this `RowBuf`.
    // This is `pub(super)` so that `super::borrowed::Row` can access it/
    pub(super) bell_vec: Vec<Bell>,
}

impl RowBuf {
    /* CONSTRUCTORS */

    /// Parse a string into a `RowBuf`, skipping any [`char`]s that aren't valid [`Bell`] names.
    /// This returns a [`InvalidRowError`] if the `RowBuf` would be invalid.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf, Stage, InvalidRowError};
    ///
    /// // Parsing a valid Row is fine
    /// assert_eq!(RowBuf::parse("12543")?.to_string(), "12543");
    /// // Parsing valid rows with invalid characters is also fine
    /// assert_eq!(RowBuf::parse("4321\t[65 78]")?.to_string(), "43216578");
    /// assert_eq!(RowBuf::parse("3|2|1  6|5|4  9|8|7")?.to_string(), "321654987");
    /// // Parsing an invalid `Row` returns an error describing the problem
    /// assert_eq!(
    ///     RowBuf::parse("112345"),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_number(1).unwrap()))
    /// );
    /// assert_eq!(
    ///     RowBuf::parse("12745"),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_number(7).unwrap(),
    ///         Stage::DOUBLES
    ///     ))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn parse(s: &str) -> Result<Self, InvalidRowError> {
        Self::from_bell_iter(s.chars().filter_map(Bell::from_name))
    }

    /// Parse a string into a `RowBuf`, extending to the given [`Stage`] if required and skipping
    /// any [`char`]s that aren't valid [`Bell`] names.  This returns [`InvalidRowError`] if the
    /// `RowBuf` would be invalid, and this will produce better error messages than
    /// [`RowBuf::parse`] because of the extra information provided by the [`Stage`].
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf, Stage, InvalidRowError};
    ///
    /// // Parsing a valid Row is fine
    /// assert_eq!(RowBuf::parse("12543")?.to_string(), "12543");
    /// // Parsing valid rows with invalid characters is also fine
    /// assert_eq!(RowBuf::parse("4321\t[65 78]")?.to_string(), "43216578");
    /// assert_eq!(RowBuf::parse("3|2|1  6|5|4  9|8|7")?.to_string(), "321654987");
    /// // Parsing an invalid `Row` returns an error describing the problem
    /// assert_eq!(
    ///     RowBuf::parse("112345"),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_number(1).unwrap()))
    /// );
    /// assert_eq!(
    ///     RowBuf::parse("12745"),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_name('7').unwrap(),
    ///         Stage::DOUBLES
    ///     ))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn parse_with_stage(s: &str, stage: Stage) -> Result<Self, InvalidRowError> {
        Self::from_vec_with_stage(s.chars().filter_map(Bell::from_name).collect_vec(), stage)
    }

    /// Creates rounds on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// assert_eq!(RowBuf::rounds(Stage::MINIMUS).to_string(), "1234");
    /// assert_eq!(RowBuf::rounds(Stage::CATERS).to_string(), "123456789");
    /// ```
    pub fn rounds(stage: Stage) -> Self {
        // This unsafety is OK, because rounds is always a valid `Row`
        unsafe { Self::from_bell_iter_unchecked(stage.bells()) }
    }

    /// Creates backrounds on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// assert_eq!(RowBuf::backrounds(Stage::MINIMUS).to_string(), "4321");
    /// assert_eq!(RowBuf::backrounds(Stage::CATERS).to_string(), "987654321");
    /// ```
    pub fn backrounds(stage: Stage) -> Self {
        // This unsafety is OK, because backrounds is always a valid `Row`
        unsafe { Self::from_bell_iter_unchecked(stage.bells().rev()) }
    }

    /// Creates Queens on a given [`Stage`].
    ///
    /// # Example
    /// ```
    /// use bellframe::{RowBuf, Stage};
    ///
    /// assert_eq!(RowBuf::queens(Stage::MINIMUS).to_string(), "1324");
    /// assert_eq!(RowBuf::queens(Stage::CATERS).to_string(), "135792468");
    /// ```
    pub fn queens(stage: Stage) -> Self {
        let odds = stage.bells().step_by(2);
        let evens = stage.bells().skip(1).step_by(2);
        // This unsafety is OK, because Queens is always a valid `Row`
        unsafe { Self::from_bell_iter_unchecked(odds.chain(evens)) }
    }

    /* UTILITY CONSTRUCTORS */

    /// Creates a `RowBuf` from a [`Vec`] of [`Bell`]s, checking that the resulting `RowBuf` is
    /// valid.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, InvalidRowError, RowBuf};
    ///
    /// // Converting a `Row` from a valid `Vec` of `Bell`s is fine
    /// assert_eq!(
    ///     RowBuf::from_vec(vec![
    ///         Bell::from_name('4').unwrap(),
    ///         Bell::from_name('2').unwrap(),
    ///         Bell::from_name('1').unwrap(),
    ///         Bell::from_name('3').unwrap(),
    ///     ])?.to_string(),
    ///     "4213"
    /// );
    /// // Converting a `Row` from an invalid `Vec` of `Bell`s is not so fine
    /// assert_eq!(
    ///     RowBuf::from_vec(vec![
    ///         Bell::from_name('4').unwrap(),
    ///         Bell::from_name('2').unwrap(),
    ///         Bell::from_name('1').unwrap(),
    ///         Bell::from_name('4').unwrap(),
    ///     ]),
    ///     Err(InvalidRowError::DuplicateBell(Bell::from_name('4').unwrap()))
    /// );
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn from_vec(bells: Vec<Bell>) -> Result<RowBuf, InvalidRowError> {
        let stage = Stage::try_from(bells.len() as u8)?;
        crate::utils::check_duplicate_or_out_of_stage(bells.iter().copied(), stage)?;
        Ok(Self { bell_vec: bells })
    }

    /// Creates a `RowBuf` from a [`Vec`] of [`Bell`]s, **without** checking that the resulting
    /// `RowBuf` is valid.  This is the unsafe version of [`RowBuf::from_vec`].
    ///
    /// # Safety
    ///
    /// This function is safe if `bells` corresponds to a valid `Row` according to the CC's
    /// Framework.  This means that each [`Bell`] is unique, and has [`index`](Bell::index) smaller
    /// than the `bells.len()`.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, InvalidRowError, RowBuf};
    ///
    /// # fn test() -> Option<()> {
    /// // Converting a `RowBuf` from a valid `Vec` of `Bell`s is fine, but still unsafe
    /// assert_eq!(
    ///     unsafe {
    ///         RowBuf::from_vec_unchecked(vec![
    ///             Bell::from_name('4')?,
    ///             Bell::from_name('2')?,
    ///             Bell::from_name('1')?,
    ///             Bell::from_name('3')?,
    ///         ])
    ///     }.to_string(),
    ///     "4213"
    /// );
    /// // Converting a `Row` from an invalid `Vec` of `Bell`s compiles and runs,
    /// // but silently creates an invalid `Row` and, by extension, silently causes
    /// // undefined behaviour
    /// assert_eq!(
    ///     unsafe {
    ///         RowBuf::from_vec_unchecked(vec![
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
    pub unsafe fn from_vec_unchecked(bells: Vec<Bell>) -> RowBuf {
        RowBuf { bell_vec: bells }
    }

    /// Utility function that creates a `RowBuf` from an [`Iterator`] of [`Bell`]s, checking that
    /// the resulting `RowBuf` is valid.
    ///
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf, Stage, InvalidRowError};
    ///
    /// // Create a valid row from an iterator over `Bell`s
    /// let iter = [0, 3, 4, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = RowBuf::from_bell_iter(iter)?;
    /// assert_eq!(row.to_string(), "14532");
    /// // Attempt to create an invalid row from an iterator over `Bell`s
    /// // (we get an error)
    /// let iter = [0, 3, 7, 2, 1].iter().copied().map(Bell::from_index);
    /// assert_eq!(
    ///     RowBuf::from_bell_iter(iter),
    ///     Err(InvalidRowError::BellOutOfStage(
    ///         Bell::from_name('8').unwrap(),
    ///         Stage::DOUBLES,
    ///     ))
    /// );
    ///
    /// # Ok::<(), InvalidRowError>(())
    /// ```
    pub fn from_bell_iter(iter: impl Iterator<Item = Bell>) -> Result<Self, InvalidRowError> {
        Self::from_vec(iter.collect_vec())
    }

    /// Creates a `RowBuf` from a [`Vec`] of [`Bell`]s, **without** checking that the resulting
    /// `RowBuf` is valid.  This is the unsafe version of [`RowBuf::from_bell_iter`].
    ///
    /// # Safety
    ///
    /// This function is safe if `iter` yields a valid `Row` according to the CC's Framework.  This
    /// means that each [`Bell`] is unique, and has [`index`](Bell::index) smaller than the number
    /// of items yeilded by `iter`.
    /// # Example
    /// ```
    /// use bellframe::{Bell, RowBuf, Stage, InvalidRowError};
    ///
    /// // Create a valid row from an iterator over `Bell`s
    /// let iter = [0, 3, 4, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = unsafe { RowBuf::from_bell_iter_unchecked(iter) };
    /// assert_eq!(row.to_string(), "14532");
    /// // Create an invalid row from an iterator over `Bell`s.  We get no error,
    /// // but doing anything with the resulting `Row` is undefined behaviour
    /// let iter = [0, 3, 7, 2, 1].iter().copied().map(Bell::from_index);
    /// let row = unsafe { RowBuf::from_bell_iter_unchecked(iter) };
    /// assert_eq!(row.to_string(), "14832");
    /// ```
    pub unsafe fn from_bell_iter_unchecked(iter: impl Iterator<Item = Bell>) -> Self {
        Self::from_vec_unchecked(iter.collect())
    }

    /// Checks the validity of a potential `RowBuf`, extending it to the given [`Stage`] if valid
    /// and returning an [`InvalidRowError`] otherwise (consuming the potential `RowBuf` so it
    /// can't be used).  This will provide nicer errors than [`RowBuf::from_vec`] since this
    /// has extra information about the desired [`Stage`] of the potential `RowBuf`.
    pub fn from_vec_with_stage(
        mut bells: Vec<Bell>,
        stage: Stage,
    ) -> Result<Self, InvalidRowError> {
        // We check validity by keeping a checklist of which `Bell`s we've seen, and checking off
        // each bell as we go.
        let mut checklist = vec![false; stage.num_bells()];
        // It's OK to initialise this with the `TREBLE` (and not handle the case where there are no
        // bells),
        let mut biggest_bell_found = Bell::TREBLE;
        // Loop over all the bells to check them off in the checklist
        for &b in &bells {
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
            .find(|&(_i, x)| !*x)
        {
            return Err(InvalidRowError::MissingBell(Bell::from_index(index as u8)));
        }

        // If no errors were generated so far, then `bells` must represent a [`Row`] (or be empty)
        // so we should extend it with 'cover' bells up to the stage.  We can't just create a `Row`
        // immediately then call `Row::extend_to_stage` because parsing `""` would temporarily
        // create a 0-length `Row`
        assert!(bells.len() <= stage.num_bells());
        let cover_bells = stage.bells().skip(bells.len());
        bells.extend(cover_bells);

        // This unsafety is OK because we have verified that `bells` corresponds to a `Row`, while
        // the no-zero-stage invariant makes sure that `bells` is non-empty
        let mut row = unsafe { Self::from_vec_unchecked(bells) };
        row.extend_to_stage(stage);
        Ok(row)
    }

    /// Consumes this `RowBuf` and returns the underlying [`Vec`] of [`Bell`]s
    #[inline]
    pub fn into_bell_vec(self) -> Vec<Bell> {
        self.bell_vec
    }

    /// Converts a [`RowBuf`] into a [`Row`].  Equivalent to `&*self`, but doesn't rely on type
    /// inference.
    #[inline]
    pub fn as_row(&self) -> &Row {
        // This unsafety is OK, because `RowBuf` requires its bells to form a valid row according
        // to the Framework
        unsafe { Row::from_slice_unchecked(&self.bell_vec) }
    }

    /// Converts a [`RowBuf`] into a [`Row`].  Equivalent to `&*self`, but doesn't rely on type
    /// inference.
    #[inline]
    pub fn as_mut_row(&mut self) -> &mut Row {
        // This unsafety is OK, because `RowBuf` requires its bells to form a valid row according
        // to the Framework
        unsafe { Row::from_mut_slice_unchecked(&mut self.bell_vec) }
    }

    /* MUTATING OPERATIONS */

    /// Extend this `RowBuf` in-place with cover bells until that it has a given [`Stage`].
    pub fn extend_to_stage(&mut self, stage: Stage) {
        assert!(self.stage() <= stage);
        self.bell_vec
            .extend(stage.bells().skip(self.bell_vec.len()));
    }

    /// Overwrites this with the contents of a [`Row`], thus reusing the allocation.
    pub fn overwrite_from(&mut self, row: &Row) {
        self.bell_vec.clear();
        self.bell_vec.extend(row.bell_iter());
    }
}

/* CONVERSIONS BETWEEN `Row` AND `RowBuf` */

impl Deref for RowBuf {
    type Target = Row;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_row()
    }
}

impl DerefMut for RowBuf {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // This unsafety is OK because the slice of `Bell`s comes from a `RowBuf`, which must
        // represent a valid row
        unsafe { Row::from_mut_slice_unchecked(&mut self.bell_vec) }
    }
}

impl Borrow<Row> for RowBuf {
    #[inline]
    fn borrow(&self) -> &Row {
        self.deref()
    }
}

impl BorrowMut<Row> for RowBuf {
    #[inline]
    fn borrow_mut(&mut self) -> &mut Row {
        self.deref_mut()
    }
}

impl AsRef<Row> for RowBuf {
    #[inline]
    fn as_ref(&self) -> &Row {
        self.as_row()
    }
}

impl AsMut<Row> for RowBuf {
    #[inline]
    fn as_mut(&mut self) -> &mut Row {
        self.as_mut_row()
    }
}

impl ToOwned for Row {
    type Owned = RowBuf;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        // We can skip the validity checks here because `Row` is valid by invariant
        unsafe { RowBuf::from_bell_iter_unchecked(self.bell_iter()) }
    }
}

/* FORMATTING */

impl Debug for RowBuf {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RowBuf({})", self)
    }
}

impl Display for RowBuf {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Delegate to `Row`'s implementation
        Display::fmt(self.deref(), f)
    }
}

/* OTHER TRAITS */

impl<'row> IntoIterator for &'row RowBuf {
    type Item = Bell;
    type IntoIter = BellIter<'row>;

    fn into_iter(self) -> Self::IntoIter {
        self.bell_iter()
    }
}

impl FromStr for RowBuf {
    type Err = InvalidRowError;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl PartialEq<Row> for RowBuf {
    fn eq(&self, other: &Row) -> bool {
        self.as_row() == other
    }
}

impl PartialEq<RowBuf> for Row {
    fn eq(&self, other: &RowBuf) -> bool {
        self == other.as_row()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn row_ref_size() {
        assert_eq!(std::mem::size_of::<&Row>(), 16);
    }

    #[test]
    fn parse_ok() {
        #[track_caller]
        fn check(inp_str: &str) {
            let row = RowBuf::parse(inp_str).unwrap();
            assert_eq!(&row.to_string(), inp_str);
        }

        check("1");
        check("12");
        check("21");
        check("164589237");
    }

    #[test]
    fn parse_err() {
        #[track_caller]
        fn check_dup_bell(inp_str: &str, dup_bell: u8) {
            let dup_bell = Bell::from_number(dup_bell).unwrap();
            assert_eq!(
                RowBuf::parse(inp_str),
                Err(InvalidRowError::DuplicateBell(dup_bell))
            );
        }

        #[track_caller]
        fn check_out_of_stage(inp_str: &str, bell: u8, stage: u8) {
            let bell = Bell::from_number(bell).unwrap();
            let stage = Stage::new(stage);
            assert_eq!(
                RowBuf::parse(inp_str),
                Err(InvalidRowError::BellOutOfStage(bell, stage))
            );
        }

        assert_eq!(RowBuf::parse(""), Err(InvalidRowError::NoBells));
        check_dup_bell("11", 1);
        check_dup_bell("124523", 2);
        check_out_of_stage("124", 4, 3);
        check_out_of_stage("5234", 5, 4);
        check_out_of_stage("21A65", 13, 5);
        // `InvalidRowError::MissingBell` isn't possible for `RowBuf::parse`
    }

    #[test]
    fn parse_with_stage_ok() {
        #[track_caller]
        fn check(inp_str: &str, stage: Stage, exp_row: &str) {
            assert_eq!(
                RowBuf::parse_with_stage(inp_str, stage).unwrap(),
                RowBuf::parse(exp_row).unwrap()
            );
        }

        check("321", Stage::SINGLES, "321");
        check("321", Stage::MINOR, "321456");
        check("1342", Stage::MAJOR, "13425678");
        check("123564", Stage::ROYAL, "1235647890");
        check("21", Stage::DOUBLES, "21345");
        check("", Stage::MINIMUS, "1234");
    }

    #[test]
    fn parse_with_stage_err_dup_bell() {
        fn check(inp_str: &str, stage: Stage, dup_bell: char) {
            assert_eq!(
                RowBuf::parse_with_stage(inp_str, stage),
                Err(InvalidRowError::DuplicateBell(
                    Bell::from_name(dup_bell).unwrap()
                ))
            );
        }

        check("322", Stage::SINGLES, '2');
        check("11", Stage::MAXIMUS, '1');
        check("512435", Stage::MINOR, '5');
        check("331212", Stage::MINOR, '3');
    }

    #[test]
    fn parse_with_stage_out_of_stage() {
        fn check(inp_str: &str, stage: Stage, bell_out_of_range: char) {
            assert_eq!(
                RowBuf::parse_with_stage(inp_str, stage),
                Err(InvalidRowError::BellOutOfStage(
                    Bell::from_name(bell_out_of_range).unwrap(),
                    stage
                ))
            );
        }

        check("0", Stage::SINGLES, '0');
        check("3218", Stage::MINOR, '8');
        check("12345678", Stage::SINGLES, '4');
    }

    #[test]
    fn parse_with_stage_missing_bell() {
        fn check(inp_str: &str, stage: Stage, missing_bell: char) {
            assert_eq!(
                RowBuf::parse_with_stage(inp_str, stage),
                Err(InvalidRowError::MissingBell(
                    Bell::from_name(missing_bell).unwrap(),
                ))
            );
        }

        check("13", Stage::SINGLES, '2');
        check("14", Stage::MINOR, '2');
        check("14567892", Stage::CATERS, '3');
    }

    #[quickcheck]
    fn parse_doesnt_panic(v: String) -> bool {
        let _ = v.parse::<RowBuf>();
        true // the only way for this test to fail is if `RowBuf::parse` panics
    }

    #[quickcheck]
    fn parse_with_stage_doesnt_panic(s: String, stage: Stage) -> bool {
        let _ = RowBuf::parse_with_stage(&s, stage);
        true // the only way for this test to fail is if `RowBuf::parse_with_stage` panics
    }
}
