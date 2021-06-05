//! A type-safe representation of a bell.

use std::fmt::Formatter;

use serde_crate::{
    de::{Error, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};

use crate::Stage;

/// A lookup string of the bell names
// - E, T stand for 11 and 12 and therefore feel out of place
// - I could be confused with 1
// - O could be confused with 0 and Q
// - X is not a valid bell name to avoid confusion with 'x' as place notation
const BELL_NAMES: &str = "1234567890ETABCDFGHJKLMNPQRSUVWYZ";

/// A type-safe representation of a 'bell', which adds things like conversions to and from
/// commonly-used bell names.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Bell {
    /// A zero-indexed number representing the `Bell`.  I.e the treble is always
    /// `Bell { index: 0 }`, and the 12th is `Bell { index: 11 }` but would be
    /// [`Display`](std::fmt::Display)ed as `T`.
    index: usize,
}

impl Bell {
    /// Creates a `Bell` from a [`char`] containing a bell name (e.g. `'4'` or `'T'`).  If the name
    /// is not valid, then this fails and returns [`None`].  Note that lower case [`char`]s are not
    /// considered valid bell names.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
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
            .map(Bell::from_index)
    }

    /// Creates a `Bell` from a 0-indexed integer.
    ///
    /// # Example
    /// ```
    /// use proj_core::Bell;
    ///
    /// // A 'Bell' with index 0 is the treble
    /// assert_eq!(Bell::from_index(0).name(), "1");
    /// // A 'Bell' with index 11 is the '12' or 'T'
    /// assert_eq!(Bell::from_index(11).name(), "T");
    /// ```
    #[inline]
    pub fn from_index(index: usize) -> Bell {
        Bell { index }
    }

    /// Creates a `Bell` from a 1-indexed integer.  This could fail if `number` is `0`, so in that
    /// case [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
    ///
    /// // The `Bell` with number '12' is the 12th and should be displayed as 'T'
    /// assert_eq!(Bell::from_number(12)?.name(), "T");
    /// // Trying to create a Bell with number `0` fails:
    /// assert_eq!(Bell::from_number(0), None);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn from_number(number: usize) -> Option<Bell> {
        number.checked_sub(1).map(Bell::from_index)
    }

    /// Creates the `Bell` representing the tenor or heaviest bell on a given [`Stage`].  This
    /// could fail if the [`Stage`] has no `Bell`s, so in that case [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::{Bell, Stage};
    ///
    /// // The **5** is the 'tenor' when ringing Doubles
    /// assert_eq!(Bell::tenor(Stage::DOUBLES)?, Bell::from_number(5)?);
    /// // The 12 is the tenor on maximus
    /// assert_eq!(Bell::tenor(Stage::MAXIMUS)?, Bell::from_number(12)?);
    /// # Some(())
    /// # }
    /// # fn main() { test().unwrap() }
    /// ```
    pub fn tenor(stage: Stage) -> Option<Bell> {
        Self::from_number(stage.as_usize())
    }

    /// A [`Bell`] representing the 'treble' on any stage.  Equivalent to
    /// `Bell::from_name('1').unwrap()`.
    ///
    /// # Example
    /// ```
    /// use proj_core::Bell;
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

    /// Converts this `Bell` into the [`char`] that it should be displayed as.  If the `Bell` is
    /// too big to have a corresponding name, then [`None`] is returned.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
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
    pub fn to_char(&self) -> Option<char> {
        BELL_NAMES.as_bytes().get(self.index).map(|x| *x as char)
    }

    /// Returns the 0-indexed representation of this `Bell`.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
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
        self.index
    }

    /// Returns the 1-indexed representation of this `Bell`.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
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
    pub fn number(self) -> usize {
        self.index + 1
    }

    /// Converts this `Bell` into a [`String`] that it should be displayed as.  Unlike
    /// [`to_char`](Bell::to_char), this does not fail if the `Bell` is to big to have a name.
    /// Instead, it returns the 1-indexed ['number'](Bell::number) of the `Bell` in angle brackets.
    ///
    /// # Example
    /// ```
    /// # fn test() -> Option<()> {
    /// use proj_core::Bell;
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
    pub fn name(&self) -> String {
        match self.to_char() {
            None => format!("<{}>", self.number()),
            Some(c) => c.to_string(),
        }
    }
}

impl std::fmt::Display for Bell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[cfg(feature = "serde")]
struct BellVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for BellVisitor {
    type Value = Bell;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("a non-negative integer, or a bell name")
    }

    fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v as i64)
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_bell(v)
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Bell::from_number(v as usize).ok_or(E::custom("can't have Bell #0"))
    }

    fn visit_char<E>(self, v: char) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Bell::from_name(v).ok_or(E::custom(format!("'{}' is not a bell name", v)))
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
            .ok_or(E::custom(format!("'{}' is not a bell name", v)))
    }
}

/// Helper function to attempt to parse a [`Bell`] from a [`i64`]
#[cfg(feature = "serde")]
#[inline(always)]
fn try_parse_bell<E: Error>(val: i64) -> Result<Bell, E> {
    if val > 0 {
        // Unwrapping here is safe, because we checked in the `if` statement that `val` is not 0.
        Ok(Bell::from_number(val as usize).unwrap())
    } else {
        Err(E::custom(format!("invalid Bell number: {}", val)))
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
