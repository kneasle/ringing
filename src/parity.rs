use std::ops::Mul;

// Imports used solely for doc tests
#[allow(unused_imports)]
use crate::Row;

/// Data type representing the parity of a [`Row`].  To generate these, you probably want to use
/// [`RowTrait::parity`].  Note that [`RowTrait::parity`] always performs a heap allocation and is
/// linear-time in the [`Stage`] of the [`Row`].  If you are using `Parity`s as optimisations
/// within hot code I would recommend computing them upfront on your input [`Row`]s and then
/// tracking them by hand, probably using the [`*` operator](Parity::mul).
///
/// Note that it would be quite cheap for [`Row`]s to track their [`Parity`] all the time, but that
/// would result in less-than ideal data layouts and doesn't fit with the 'only pay for what you
/// use' design goal of this library.
#[repr(u8)]
#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub enum Parity {
    /// A given [`Row`] requires an **even** number of [`swap`](RowTrait::swap)s to return to
    /// [`rounds`](RowTrait::rounds).  This is also often called _in course_ or _positive_.  The
    /// parity of [`rounds`](RowTrait::rounds) on every [`Stage`].
    Even = 0,
    /// A given [`Row`] requires an **odd** number of [`swap`](RowTrait::swap)s to return to
    /// [`rounds`](RowTrait::rounds).  This is also often called _out of course_ or _negative_.
    Odd = 1,
}

impl Parity {
    /// Maps `true` to [`Parity::Even`] and `false` to [`Parity::Odd`].  On most machines, this
    /// will not actually do anything and be removed by the compiler.
    #[inline(always)]
    pub fn from_is_odd(v: bool) -> Self {
        match v {
            false => Parity::Even,
            true => Parity::Odd,
        }
    }

    /// Returns the `Parity` of a given number.
    #[inline(always)]
    pub fn from_number(v: usize) -> Parity {
        Self::from_is_odd(v % 2 != 0)
    }

    /// Returns the `Parity` of a given number.
    #[inline(always)]
    pub fn zero_or_one(self) -> u8 {
        match self {
            Parity::Even => 0,
            Parity::Odd => 1,
        }
    }

    /// Returns the `Parity` of a given number.
    #[inline(always)]
    pub fn plus_or_minus(self) -> char {
        match self {
            Parity::Even => '+',
            Parity::Odd => '-',
        }
    }
}

impl Mul for Parity {
    type Output = Self;

    /// 'Multiply' two [`Parity`]s together (this corresponds to xor/uncarried addition where
    /// `Even` is `0` and `Odd` is `1`).  Also, if you have a [`Row`] `r1` with [`Parity`] `p1` and
    /// another [`Row`] `r2` with [`Parity`] `p2` then `(r1 * r2).parity()` will always equal `p1 *
    /// p2`.  Thus, this can be used to convert calls to [`RowTrait::parity`] into an extremely
    /// cheap (likely single-cycle) update function.
    #[inline(always)]
    fn mul(self, rhs: Self) -> Self::Output {
        Self::from_is_odd(self != rhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::Stage;

    // In general, it's a bad idea to import enum variants directly, but in this case it makes our
    // tests much terser
    use super::Parity::{Even, Odd};
    use super::*;

    #[test]
    fn mul() {
        assert_eq!(Even * Even, Even);
        assert_eq!(Even * Odd, Odd);
        assert_eq!(Odd * Even, Odd);
        assert_eq!(Odd * Odd, Even);
    }

    #[test]
    fn from_number() {
        assert_eq!(Parity::from_number(0), Even);
        assert_eq!(Parity::from_number(1000), Even);
        assert_eq!(Parity::from_number(1), Odd);
        assert_eq!(Parity::from_number(47), Odd);
    }

    #[test]
    fn row_parity() {
        assert_eq!(Row::rounds(Stage::from(0)).parity(), Even);
        assert_eq!(Row::rounds(Stage::MAJOR).parity(), Even);
        assert_eq!(Row::parse("13245").unwrap().parity(), Odd);
        assert_eq!(Row::parse("231546").unwrap().parity(), Odd);
        assert_eq!(Row::parse("231564").unwrap().parity(), Even);
    }
}
