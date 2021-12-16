use std::ops::Not;

use self::Stroke::{Back, Hand};

#[cfg(feature = "serde")]
use serde_crate::{Deserialize, Serialize};

/// Stroke of a row, i.e. handstroke (`Stroke::Hand`) or backstroke (`Stroke::Back`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize, Serialize)]
#[serde(crate = "serde_crate", rename_all = "snake_case")]
pub enum Stroke {
    Hand = 0,
    Back = 1,
}

impl Stroke {
    /// Returns the `Stroke` which happens `offset` [`Row`](crate::Row)s after a given `start` `Stroke`.
    ///
    /// # Example
    ///
    /// ```
    /// use bellframe::Stroke;
    ///
    /// assert_eq!(Stroke::Hand.offset(0), Stroke::Hand);
    /// // The row after a backstroke is a handstroke
    /// assert_eq!(Stroke::Back.offset(1), Stroke::Hand);
    /// assert_eq!(Stroke::Hand.offset(1235124), Stroke::Hand); // Whoa lots of ringing
    /// ```
    pub fn offset(self, offset: usize) -> Stroke {
        if offset % 2 == 0 {
            self
        } else {
            !self
        }
    }

    /// Returns the `Stroke` which happens `offset` [`Row`](crate::Row)s after (or before) `self`.
    ///
    /// # Example
    ///
    /// ```
    /// use bellframe::Stroke;
    ///
    /// assert_eq!(Stroke::Hand.offset_i(0), Stroke::Hand);
    /// // The row after a backstroke is a handstroke
    /// assert_eq!(Stroke::Back.offset_i(1), Stroke::Hand);
    /// // The row before a backstroke is also a handstroke
    /// assert_eq!(Stroke::Back.offset_i(-1), Stroke::Hand);
    /// assert_eq!(Stroke::Hand.offset_i(1235124), Stroke::Hand); // Whoa lots of ringing
    /// ```
    pub fn offset_i(self, offset: isize) -> Stroke {
        if offset % 2 == 0 {
            self
        } else {
            !self
        }
    }
}

impl Not for Stroke {
    type Output = Self;

    /// Returns the opposite `Stroke` to `self`
    ///
    /// # Example
    ///
    /// ```
    /// use bellframe::Stroke;
    ///
    /// assert_eq!(!Stroke::Hand, Stroke::Back);
    /// assert_eq!(!Stroke::Back, Stroke::Hand);
    /// ```
    fn not(self) -> Self::Output {
        match self {
            Hand => Back,
            Back => Hand,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset() {
        #[track_caller]
        fn check(start: Stroke, offset: usize, expected: Stroke) {
            assert_eq!(start.offset(offset), expected);
        }
        check(Hand, 0, Hand);
        check(Back, 0, Back);
        check(Hand, 1, Back);
        check(Back, 1, Hand);
        check(Hand, 123547, Back);
        check(Back, 123547, Hand);
        check(Hand, usize::MAX, Back); // usize::MAX is odd
        check(Back, usize::MAX, Hand);
    }

    #[test]
    fn offset_i() {
        #[track_caller]
        fn check(start: Stroke, offset: isize, expected: Stroke) {
            assert_eq!(start.offset_i(offset), expected);
        }
        check(Hand, 0, Hand);
        check(Back, 0, Back);
        check(Hand, 1, Back);
        check(Back, 1, Hand);
        check(Hand, 123547, Back);
        check(Back, 123547, Hand);
        check(Hand, isize::MAX, Back); // isize::MAX is odd
        check(Back, isize::MAX, Hand);
        check(Hand, isize::MIN, Hand); // isize::MIN is even
        check(Back, isize::MIN, Back);
    }

    #[test]
    fn not() {
        assert_eq!(!Hand, Back);
        assert_eq!(!Back, Hand);
    }
}
