//! A representation of a stage, with human-friendly `const`s and display names.

use std::fmt::{Debug, Display, Formatter};

#[cfg(feature = "serde")]
use serde_crate::{
    de::{Error, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};

// Imports used solely by doc comments
#[allow(unused_imports)]
use crate::Row;

/// A newtype over [`usize`] that represents a stage.
///
/// To create a new `Stage`, you can either create it directly by using `Stage::from(usize)` or use
/// a constant for the human name for each `Stage`:
/// ```
/// use bellframe::Stage;
///
/// // Converting from numbers is the same as using the constants
/// assert_eq!(Stage::SINGLES, Stage::from(3));
/// assert_eq!(Stage::MAJOR, Stage::from(8));
/// assert_eq!(Stage::CINQUES, Stage::from(11));
/// assert_eq!(Stage::SIXTEEN, Stage::from(16));
/// // We can use `Stage::from` to generate `Stage`s that don't have names
/// assert_eq!(Stage::from(2).as_usize(), 2);
/// assert_eq!(Stage::from(100).as_usize(), 100);
/// ```
///
/// `Stage`s with names will also be [`Display`](std::fmt::Display)ed as their names:
/// ```
/// # use bellframe::Stage;
/// #
/// assert_eq!(&format!("{}", Stage::MAXIMUS), "Maximus");
/// assert_eq!(&format!("{}", Stage::from(9)), "Caters");
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct Stage(usize);

impl Stage {
    /// Returns this `Stage` as a [`usize`].
    ///
    /// # Example
    /// ```
    /// use bellframe::Stage;
    ///
    /// assert_eq!(Stage::DOUBLES.as_usize(), 5);
    /// assert_eq!(Stage::MAXIMUS.as_usize(), 12);
    /// ```
    #[inline(always)]
    pub fn as_usize(self) -> usize {
        self.0
    }

    /// Returns true if this `Stage` has an even number of bells
    #[inline(always)]
    pub fn is_even(self) -> bool {
        self.as_usize() % 2 == 0
    }

    /// Creates a [`Stage`] from the lower case version of the human-friendly name (e.g.
    /// `"royal"`, `"triples"` or `"twenty-two"`).
    pub fn from_lower_case_name(name: &str) -> Option<Stage> {
        Some(match name {
            "zero" => Stage::ZERO,

            "one" => Stage::ONE,
            "two" => Stage::TWO,
            "singles" => Stage::SINGLES,
            "minimus" => Stage::MINIMUS,

            "doubles" => Stage::DOUBLES,
            "minor" => Stage::MINOR,
            "triples" => Stage::TRIPLES,
            "major" => Stage::MAJOR,

            "caters" => Stage::CATERS,
            "royal" => Stage::ROYAL,
            "cinques" => Stage::CINQUES,
            "maximus" => Stage::MAXIMUS,

            "sextuples" => Stage::SEXTUPLES,
            "fourteen" => Stage::FOURTEEN,
            "septuples" => Stage::SEPTUPLES,
            "sixteen" => Stage::SIXTEEN,

            "octuples" => Stage(17),
            "eighteen" => Stage(18),
            "nonuples" => Stage(19),
            "twenty" => Stage(20),

            "decuples" => Stage(21),
            "twenty-two" => Stage(22),

            _ => return None,
        })
    }

    /// Gets the human-friendly name of this [`Stage`], as would be used in [`Method`] names (or
    /// `None` if `self` is too big to have a name).
    pub fn name(self) -> Option<&'static str> {
        Some(match self.0 {
            1 => "One",
            2 => "Two",
            3 => "Singles",
            4 => "Minimus",

            5 => "Doubles",
            6 => "Minor",
            7 => "Triples",
            8 => "Major",

            9 => "Caters",
            10 => "Royal",
            11 => "Cinques",
            12 => "Maximus",

            13 => "Sextuples",
            14 => "Fourteen",
            15 => "Septuples",
            16 => "Sixteen",

            17 => "Octuples",
            18 => "Eighteen",
            19 => "Nonuples",
            20 => "Twenty",

            21 => "Decuples",
            22 => "Twenty-two",

            _ => return None,
        })
    }
}

/// User-friendly constants for commonly used `Stage`s.
///
/// # Example
/// ```
/// use bellframe::Stage;
///
/// assert_eq!(Stage::MINIMUS, Stage::from(4));
/// assert_eq!(Stage::MINOR, Stage::from(6));
/// assert_eq!(Stage::TRIPLES, Stage::from(7));
/// assert_eq!(Stage::FOURTEEN, Stage::from(14));
/// assert_eq!(Stage::SEPTUPLES, Stage::from(15));
/// ```
impl Stage {
    /// A `Stage` with no bells
    pub const ZERO: Stage = Stage(0);

    /// A `Stage` with `1` 'working' bell
    pub const ONE: Stage = Stage(1);

    /// A `Stage` with `2` working bells
    pub const TWO: Stage = Stage(2);

    /// A `Stage` with `3` working bells
    pub const SINGLES: Stage = Stage(3);

    /// A `Stage` with `4` working bells
    pub const MINIMUS: Stage = Stage(4);

    /// A `Stage` with `5` working bells
    pub const DOUBLES: Stage = Stage(5);

    /// A `Stage` with `6` working bells
    pub const MINOR: Stage = Stage(6);

    /// A `Stage` with `7` working bells
    pub const TRIPLES: Stage = Stage(7);

    /// A `Stage` with `8` working bells
    pub const MAJOR: Stage = Stage(8);

    /// A `Stage` with `9` working bells
    pub const CATERS: Stage = Stage(9);

    /// A `Stage` with `10` working bells
    pub const ROYAL: Stage = Stage(10);

    /// A `Stage` with `11` working bells
    pub const CINQUES: Stage = Stage(11);

    /// A `Stage` with `12` working bells
    pub const MAXIMUS: Stage = Stage(12);

    /// A `Stage` with `13` working bells
    pub const SEXTUPLES: Stage = Stage(13);

    /// A `Stage` with `14` working bells
    pub const FOURTEEN: Stage = Stage(14);

    /// A `Stage` with `15` working bells
    pub const SEPTUPLES: Stage = Stage(15);

    /// A `Stage` with `16` working bells
    pub const SIXTEEN: Stage = Stage(16);
}

impl Debug for Stage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let const_name = match self.0 {
            0 => "ZERO",
            1 => "ONE",
            2 => "TWO",
            3 => "SINGLES",
            4 => "MINIMUS",
            5 => "DOUBLES",
            6 => "MINOR",
            7 => "TRIPLES",
            8 => "MAJOR",
            9 => "CATERS",
            10 => "ROYAL",
            11 => "CINQUES",
            12 => "MAXIMUS",
            13 => "SEXTUPLES",
            14 => "FOURTEEN",
            15 => "SEPTUPLES",
            16 => "SIXTEEN",
            num_bells => return write!(f, "Stage({})", num_bells),
        };
        write!(f, "Stage::{}", const_name)
    }
}

impl Display for Stage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.name() {
            Some(n) => write!(f, "{}", n),
            None => write!(f, "{} bells", self.0),
        }
    }
}

impl From<usize> for Stage {
    fn from(n: usize) -> Self {
        Stage(n)
    }
}

/* Allow [`Stage`]s to be serialised and deserialised with `serde` */

// Serialise as a u64
#[cfg(feature = "serde")]
impl Serialize for Stage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u64(self.0 as u64)
    }
}

// Stage by default will deserialise from either a name (i.e. a string) or a non-negative number.
// If types are not known (e.g. when using Bincode), it will deserialise as a u64 to make sure that
// deserialisation is always an inverse of serialisation.
#[cfg(feature = "serde")]
struct StageVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for StageVisitor {
    type Value = Stage;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a non-negative integer, or a stage name")
    }

    fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_stage(v as i64)
    }

    fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Stage::from(v as usize))
    }

    fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_stage(v as i64)
    }

    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Stage::from(v as usize))
    }

    fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_stage(v as i64)
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Stage::from(v as usize))
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        try_parse_stage(v as i64)
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Stage::from(v as usize))
    }

    /// Attempt to parse a [`Stage`] from a string.  This matches standard [`Stage`] names on up to
    /// 16 bells, and is not case sensitive.
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        let lower_str = v.to_lowercase();
        Stage::from_lower_case_name(&lower_str)
            .ok_or_else(|| E::custom(format!("'{}' is not a stage name", v)))
    }
}

/// Helper function to attempt to parse a [`Stage`] from a [`i64`]
#[cfg(feature = "serde")]
#[inline(always)]
fn try_parse_stage<E: Error>(val: i64) -> Result<Stage, E> {
    if val >= 0 {
        Ok(Stage::from(val as usize))
    } else {
        Err(E::custom(format!("negative Stage: {}", val)))
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Stage {
    fn deserialize<D>(deserializer: D) -> Result<Stage, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_u64(StageVisitor)
    }
}

/// An error created when a [`Row`] was used to permute something with the wrong length
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct IncompatibleStages {
    /// The [`Stage`] of the [`Row`] that was being permuted
    pub(crate) lhs_stage: Stage,
    /// The [`Stage`] of the [`Row`] that was doing the permuting
    pub(crate) rhs_stage: Stage,
}

impl IncompatibleStages {
    /// Compares two [`Stage`]s, returning `Ok(())` if they are equal and returning the appropriate
    /// `IncompatibleStages` error if not.
    pub fn test_err(lhs_stage: Stage, rhs_stage: Stage) -> Result<(), Self> {
        if lhs_stage == rhs_stage {
            Ok(())
        } else {
            Err(IncompatibleStages {
                lhs_stage,
                rhs_stage,
            })
        }
    }

    /// Compares an `Option<Stage>` to a [`Stage`], overwriting the `Option` if it's `None` but
    /// otherwise checking the [`Stage`]s for validity.  This is useful if you have a sequence of
    /// [`Row`]s and you want to verify that all the [`Stage`]s are equal without treating the
    /// first [`Row`] as a special case.
    pub fn test_err_opt(opt: &mut Option<Stage>, stage: Stage) -> Result<(), Self> {
        match opt {
            None => {
                *opt = Some(stage);
                Ok(())
            }
            Some(s) => Self::test_err(*s, stage),
        }
    }
}

impl Display for IncompatibleStages {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Incompatible stages: {} (lhs), {} (rhs)",
            self.lhs_stage, self.rhs_stage
        )
    }
}

impl std::error::Error for IncompatibleStages {}
