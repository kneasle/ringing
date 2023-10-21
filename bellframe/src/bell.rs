//! A type-safe representation of a bell.

use std::fmt::{Debug, Display, Formatter};

use bytemuck::{Pod, Zeroable};
#[cfg(feature = "serde")]
use serde_crate::{
    de::{Error, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};

use crate::Stage;

/// A lookup string of the bell names
// Letters missing from the alphabet sequence
// - E, T stand for 11 and 12
// - I could be confused with 1
// - O could be confused with 0 and Q
// - X is not a valid bell name to avoid confusion with 'x' as place notation
const BELL_NAMES: &str = "1234567890ETABCDFGHJKLMNPQRSUVWYZ";

/// A type-safe representation of a 'bell', which adds things like conversions to and from
/// commonly-used bell names.  Each `Bell` takes a single byte in memory.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Zeroable, Pod)]
#[repr(transparent)] // Needed to force memory layout
pub struct Bell {
    /// A zero-indexed number representing the `Bell`.  I.e the treble is always
    /// `Bell { index: 0 }`, and the 12th is `Bell { index: 11 }` but would be
    /// [`Display`](std::fmt::Display)ed as `T`.
    ///
    /// `index` cannot take the value 255, since this would imply a [`Stage`] of 256 which is
    /// unrepresentable.  We can use this as a 'null' value for e.g. [`Mask`](crate::Mask) to take
    /// up exactly one byte per place.
    index: u8,
}

impl Bell {
    /// Creates a `Bell` from a [`char`] containing a bell name (e.g. `'4'` or `'T'`).  If the name
    /// is not valid, then this fails and returns [`None`].  Note that lower case [`char`]s are not
    /// considered valid bell names.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // Converting a valid name to a `Bell` and back should be the identity function
    /// assert_eq!(Bell::from_name('1')?.name(), "1");
    /// assert_eq!(Bell::from_name('5')?.name(), "5");
    /// assert_eq!(Bell::from_name('0')?.name(), "0");
    /// assert_eq!(Bell::from_name('T')?.name(), "T");
    /// // Converting a lower-case letter should return `None`, even if the upper case
    /// // version is valid
    /// assert_eq!(Bell::from_name('e'), None);
    /// assert_eq!(Bell::from_name('t'), None);
    /// // Converting any old rubbish will return `None` (no shade on Ferris)
    /// assert_eq!(Bell::from_name('\r'), None);
    /// assert_eq!(Bell::from_name('🦀'), None);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn from_name(c: char) -> Option<Bell> {
        BELL_NAMES
            .chars()
            .position(|x| x == c)
            .map(|v| v as u8)
            .map(Bell::from_index)
    }

    /// Creates a `Bell` from a 0-indexed integer.
    ///
    /// # Panics
    ///
    /// Panics if 255 is passed (a `Bell` with index 255 would imply a [`Stage`] of 256, which
    /// cannot be created).
    ///
    /// # Example
    /// ```
    /// use bellframe::Bell;
    ///
    /// // A 'Bell' with index 0 is the treble
    /// assert_eq!(Bell::from_index(0).name(), "1");
    /// // A 'Bell' with index 11 is the '12' or 'T'
    /// assert_eq!(Bell::from_index(11).name(), "T");
    /// ```
    #[inline]
    pub fn from_index(index: u8) -> Bell {
        assert_ne!(index, 255, "`Bell`s with index 255 can't be created.");
        Bell { index }
    }

    /// Creates a `Bell` from a 1-indexed integer.  This could fail if `number` is `0`, so in that
    /// case [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // The `Bell` with number '12' is the 12th and should be displayed as 'T'
    /// assert_eq!(Bell::from_number(12)?.name(), "T");
    /// // Trying to create a Bell with number `0` fails:
    /// assert_eq!(Bell::from_number(0), None);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    // TODO: Make this panic when constructing bell #0
    pub fn from_number(number: u8) -> Option<Bell> {
        number.checked_sub(1).map(Bell::from_index)
    }

    /// Creates the `Bell` representing the tenor or heaviest bell on a given [`Stage`].  This
    /// could fail if the [`Stage`] has no `Bell`s, so in that case [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::{Bell, Stage};
    ///
    /// // The **5** is the 'tenor' when ringing Doubles
    /// assert_eq!(Bell::tenor(Stage::DOUBLES), Bell::from_number(5)?);
    /// // The 12 is the tenor on maximus
    /// assert_eq!(Bell::tenor(Stage::MAXIMUS), Bell::from_number(12)?);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn tenor(stage: Stage) -> Bell {
        // Unwrapping here is safe because `Stage.num_bells()` can never be zero
        Self::from_number(stage.num_bells_u8()).unwrap()
    }

    /// A [`Bell`] representing the 'treble' on any stage.  Equivalent to
    /// `Bell::from_name('1').unwrap()`.
    ///
    /// # Example
    /// ```
    /// use bellframe::Bell;
    ///
    /// // `TREBLE` should be the bell with name '1'
    /// assert_eq!(Bell::from_name('1'), Some(Bell::TREBLE));
    /// // The `TREBLE` has index 0, and its number is 1
    /// assert_eq!(Bell::TREBLE.index(), 0);
    /// assert_eq!(Bell::TREBLE.number(), 1);
    /// // The treble should display as `"1"`
    /// assert_eq!(Bell::TREBLE.name(), "1");
    /// ```
    pub const TREBLE: Bell = Bell { index: 0 };

    /// The largest [`Bell`] that can be stored.
    pub const MAX: Bell = Bell { index: 254 };

    /// Converts this `Bell` into the [`char`] that it should be displayed as.  If the `Bell` is
    /// too big to have a corresponding name, then [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // A 'Bell' with index 0 is the treble, and therefore displays as `1`
    /// assert_eq!(Bell::from_index(0).to_char(), Some('1'));
    /// // The 11th should display as 'E'
    /// assert_eq!(Bell::from_number(11)?.to_char(), Some('E'));
    ///
    /// // Trying to display the 100th Bell fails:
    /// assert_eq!(Bell::from_number(100)?.to_char(), None);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn to_char(self) -> Option<char> {
        BELL_NAMES.as_bytes().get(self.index()).map(|x| *x as char)
    }

    /// Returns the 0-indexed representation of this `Bell`.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // Creating a `Bell` with `from_index` should return the same index passed to it
    /// assert_eq!(Bell::from_index(0).index(), 0);
    /// assert_eq!(Bell::from_index(12).index(), 12);
    ///
    /// assert_eq!(Bell::from_name('8')?.index(), 7);
    /// assert_eq!(Bell::from_name('0')?.index(), 9);
    /// assert_eq!(Bell::from_name('T')?.index(), 11);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub fn index(self) -> usize {
        self.index as usize
    }

    /// Returns the 0-indexed representation of this `Bell`, as a [`u8`].
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // Creating a `Bell` with `from_index` should return the same index passed to it
    /// assert_eq!(Bell::from_index(0).index_u8(), 0);
    /// assert_eq!(Bell::from_index(12).index_u8(), 12);
    ///
    /// assert_eq!(Bell::from_name('8')?.index_u8(), 7);
    /// assert_eq!(Bell::from_name('0')?.index_u8(), 9);
    /// assert_eq!(Bell::from_name('T')?.index_u8(), 11);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub fn index_u8(self) -> u8 {
        self.index
    }

    /// Returns the 1-indexed representation of this `Bell`.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// assert_eq!(Bell::from_index(0).number(), 1);
    /// assert_eq!(Bell::from_name('0')?.number(), 10);
    /// // Using `from_number` should return the same number that was passed to it
    /// assert_eq!(Bell::from_number(4)?.number(), 4);
    /// assert_eq!(Bell::from_number(10)?.number(), 10);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    #[inline]
    pub fn number(self) -> u8 {
        self.index + 1 // Can't overflow because `index` cannot be 255
    }

    /// Converts this `Bell` into a [`String`] that it should be displayed as.  Unlike
    /// [`to_char`](Bell::to_char), this does not fail if the `Bell` is to big to have a name.
    /// Instead, it returns the 1-indexed ['number'](Bell::number) of the `Bell` in angle brackets.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use bellframe::Bell;
    ///
    /// // Bells which are <= 9th should return their number as a `String`
    /// assert_eq!(Bell::from_number(1)?.name(), "1");
    /// assert_eq!(Bell::from_number(5)?.name(), "5");
    /// assert_eq!(Bell::from_number(9)?.name(), "9");
    /// // The 10th display as "0"
    /// assert_eq!(Bell::from_number(10)?.name(), "0");
    /// // Other bells display as their single-character names
    /// assert_eq!(Bell::from_number(12)?.name(), "T");
    /// assert_eq!(Bell::from_number(16)?.name(), "D");
    /// // Anything too big simply displays as '<{bell.number()}>'
    /// assert_eq!(Bell::from_number(100)?.name(), "<100>");
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn name(self) -> String {
        match self.to_char() {
            None => format!("<{}>", self.number()),
            Some(c) => c.to_string(),
        }
    }
}

impl Debug for Bell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Bell({})", self)
    }
}

impl Display for Bell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Bell {
    #[track_caller]
    fn from_i16_idx(idx: i16) -> Self {
        debug_assert!(
            idx >= u8::MIN as i16,
            "Integer underflow while adding to `Bell`"
        );
        debug_assert!(
            idx <= u8::MAX as i16,
            "Integer overflow while adding to `Bell`"
        );
        Self { index: idx as u8 }
    }
}

impl std::ops::Add<i16> for Bell {
    type Output = Bell;

    #[track_caller]
    fn add(self, rhs: i16) -> Self::Output {
        Self::from_i16_idx(self.index as i16 + rhs)
    }
}

impl std::ops::AddAssign<i16> for Bell {
    #[track_caller]
    fn add_assign(&mut self, rhs: i16) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub<i16> for Bell {
    type Output = Bell;

    #[track_caller]
    fn sub(self, rhs: i16) -> Self::Output {
        Self::from_i16_idx(self.index as i16 - rhs)
    }
}

impl std::ops::SubAssign<i16> for Bell {
    #[track_caller]
    fn sub_assign(&mut self, rhs: i16) {
        *self = *self - rhs;
    }
}

///////////
// SERDE //
///////////

#[cfg(feature = "serde")]
struct BellVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for BellVisitor {
    type Value = Bell;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a non-negative integer, or a bell name")
    }

    fn visit_u8<E>(self, val: u8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        if val > 0 {
            // Unwrapping here is safe, because we checked in the `if` statement that `val` is not 0.
            Ok(Bell::from_number(val).unwrap())
        } else {
            Err(E::custom(format!("invalid Bell number: {}", val)))
        }
    }

    fn visit_char<E>(self, v: char) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Bell::from_name(v).ok_or_else(|| E::custom(format!("'{}' is not a bell name", v)))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        if v.len() != 1 {
            return Err(E::custom(format!("'{}' is not a bell name", v)));
        }

        v.chars()
            .next()
            .and_then(Bell::from_name)
            .ok_or_else(|| E::custom(format!("'{}' is not a bell name", v)))
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Bell {
    fn deserialize<D>(deserializer: D) -> Result<Bell, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_u64(BellVisitor)
    }
}

// Serialise as a u64
#[cfg(feature = "serde")]
impl Serialize for Bell {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u64(self.index as u64)
    }
}

#[cfg(test)]
mod tests {
    use super::Bell;

    #[test]
    #[should_panic]
    fn from_index_panic() {
        Bell::from_index(255);
    }
}
