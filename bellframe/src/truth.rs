// Only used for doc comments
#[allow(unused_imports)]
use crate::Row;

/// Whether [`Row`]s are repeated in some section of ringing.
///
/// This is shaped identically to `bool`, but is a different type to allow the compiler to spot
/// common mistakes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Truth {
    /// No [`Row`]s are repeated
    True,
    /// At least one [`Row`] is repeated
    False,
}

impl Truth {
    /// `true` if `self` is [`Truth::False`].
    ///
    /// This is a more explicit version of `!bool::from(self)`
    pub fn is_false(self) -> bool {
        !bool::from(self)
    }

    /// `true` if `self` is [`Truth::True`].
    ///
    /// This is a more explicit version of `bool::from(self)`
    pub fn is_true(self) -> bool {
        bool::from(self)
    }
}

impl From<bool> for Truth {
    fn from(v: bool) -> Self {
        match v {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl From<Truth> for bool {
    fn from(v: Truth) -> Self {
        match v {
            Truth::True => true,
            Truth::False => false,
        }
    }
}
