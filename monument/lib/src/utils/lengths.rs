use std::{
    iter::Sum,
    ops::{Add, AddAssign, Sub, SubAssign},
};

use datasize::DataSize;

/// A length **in one part** of the composition.  This and [`TotalLength`] allow the compiler to
/// disallow mixing up the different definitions of 'length'.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct PerPartLength(u32);

/// The combined length **across all parts**.  This and [`PerPartLength`] allow the compiler to
/// disallow mixing up the different definitions of 'length'.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, DataSize)]
pub(crate) struct TotalLength(u32);

impl PerPartLength {
    pub fn as_total(self, part_heads: &crate::group::PartHeadGroup) -> TotalLength {
        TotalLength(self.0 * part_heads.size() as u32)
    }
}

macro_rules! impl_length {
    ($name: ident) => {
        impl $name {
            pub const ZERO: Self = Self(0);

            pub fn new(l: usize) -> Self {
                Self(l as u32)
            }

            pub fn as_usize(self) -> usize {
                self.0 as usize
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.0, f)
            }
        }

        impl Sub for $name {
            type Output = Self;

            #[track_caller]
            fn sub(self, rhs: Self) -> Self {
                Self(self.0 - rhs.0)
            }
        }

        impl SubAssign for $name {
            #[track_caller]
            fn sub_assign(&mut self, rhs: Self) {
                self.0 -= rhs.0
            }
        }

        impl Add for $name {
            type Output = Self;

            #[track_caller]
            fn add(self, rhs: Self) -> Self {
                Self(self.0 + rhs.0)
            }
        }

        impl AddAssign for $name {
            #[track_caller]
            fn add_assign(&mut self, rhs: Self) {
                self.0 += rhs.0
            }
        }

        impl Sum for $name {
            fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
                let mut total = Self::ZERO;
                for v in iter {
                    total += v;
                }
                total
            }
        }
    };
}

impl_length!(PerPartLength);
impl_length!(TotalLength);
