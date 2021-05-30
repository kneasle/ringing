// Enable this library even if the feature flags aren't set.  This will make sure that
// rust-analyzer is able to index it and provide autocomplete for upstream crates
// #![cfg(target_feature = "ssse3")]
// #![cfg(target_feature = "sse4.1")]
// Because of the unsafety, this module is hidden behind a feature flag
#![cfg(feature = "simd_row")]
// The SIMD intrinsics we're using are only valid on x86
#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

use std::hash::{Hash, Hasher};

use crate::{Bell, InvalidRowError, Row, RowTrait, Stage};
use itertools::Itertools;
use safe_arch::{m128i, shuffle_av_i8z_all_m128i};

use super::{check_validity, check_validity_with_stage};

const ROUNDS: u128 = 0x0f0e0d0c_0b0a0908_07060504_03020100;

/// A `Row` type which uses SIMD to perform permutation and equality each in a single clock cycle.
/// In return, the current CPU must support the `ssse3` and `sse4.1` x86 extensions and
/// [`SimdRow`]s are limited to 16 [`Bell`]s (which should cover ~99% of cases anyway).  **You have
/// to check this using [`SimdRow::are_cpu_features_enabled`] before using `SimdRow`**, since
/// running this SIMD code on processors without the required instruction extensions is instant UB.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct SimdRow {
    /// The bells contained in this [`SimdRow`], packed as individual bytes with the first [`Bell`]
    /// in the least significant byte.
    ///
    /// **Invariant:** The unused bytes **must** be set their own indices (making this always a
    /// valid `Row` on 16 bells).  This is because that bit pattern is preserved by multiplication,
    /// meaning that simple bit equality is sufficient without any extra bit masking.
    bells: m128i,
    stage: Stage,
}

impl SimdRow {
    #[inline(always)]
    pub fn to_m128i(self) -> m128i {
        self.bells
    }

    #[inline(always)]
    pub fn bell_iter(self) -> BellIter {
        BellIter {
            bells: u128::from(self.bells),
            bells_left: self.stage.as_usize(),
        }
    }

    pub fn are_cpu_features_enabled() -> bool {
        is_x86_feature_detected!("ssse3") && is_x86_feature_detected!("sse4.1")
    }
}

impl RowTrait for SimdRow {
    unsafe fn from_iter_unchecked(bell_iter: impl Iterator<Item = Bell>) -> Self {
        let mut val = 0u128;
        let mut fused_bell_iter = bell_iter.fuse();
        let mut num_bells_popped = 0;

        // We fill every byte to make sure that the unused-byte invariant is upheld
        for i in 0u8..16 {
            let new_byte = fused_bell_iter.next().map_or(i, |b| {
                num_bells_popped += 1;
                b.index() as u8
            });
            val |= (new_byte as u128) << (i * 8);
        }

        assert!(
            fused_bell_iter.next().is_none(),
            "SimdRows can only contain 16 bells",
        );

        SimdRow {
            bells: m128i::from(val),
            stage: Stage::from(num_bells_popped),
        }
    }

    #[inline(always)]
    fn bell_at(&self, place: usize) -> Bell {
        let bell = (u128::from(self.bells) >> (place * 8)) & 0xffu128;
        Bell::from_index(bell as usize)
    }

    #[inline]
    #[target_feature(enable = "ssse3")]
    unsafe fn mul_unchecked(&self, other: &Self) -> Self {
        SimdRow {
            bells: shuffle_av_i8z_all_m128i(self.bells, other.bells),
            stage: self.stage,
        }
    }

    #[inline(always)]
    fn stage(&self) -> Stage {
        self.stage
    }

    #[inline(always)]
    unsafe fn mul_into_unchecked(&self, rhs: &Self, out: &mut Self) {
        *out = self.mul_unchecked(rhs);
    }

    #[inline(always)]
    fn swap(&mut self, a: usize, b: usize) {
        // A 128 bit integer with 1s in the locations of bytes a and b
        let byte_mask = (0xffu128 << (a * 8)) | (0xffu128 << (b * 8));
        // A 128 bit integer with `b` in byte index a and `a` in byte index b
        let swap_bytes = ((b as u128) << (a * 8)) | ((a as u128) << (b * 8));
        // A 128 bit integer with each byte containing its own index except for bytes `a` and `b`,
        // which have been replaced by each other's index.  Therefore, this is the permutation
        // which swaps bells at `a` and `b`
        let perm = (ROUNDS & !byte_mask) | swap_bytes;
        // Use a SIMD byte shuffle to perform the swap
        self.bells = shuffle_av_i8z_all_m128i(self.bells, m128i::from(perm))
    }

    #[inline(always)]
    fn inv_into(&self, out: &mut Self) {
        *out = self.inv();
    }

    fn inv(&self) -> Self {
        // 128 bit integer where we'll put the lower bytes representing the inverse of `self`.  The
        // higher/unused bits will be added later.
        let mut inverted_bytes = 0u128;
        for (i, b) in self.bell_iter().enumerate() {
            inverted_bytes |= (i as u128) << (b.index() * 8);
        }
        // 128 bit integer with 0s in the `self.stage.as_usize()` lowest bytes and 1s elsewhere
        let byte_mask = (-1i128 as u128) << (self.stage.as_usize() * 8);
        // Use the bytemask to insert rounds as the unused-bytes, to satisfy our invariant.
        // `inverted_bytes` has 0s everywhere, so the `|` is fine.
        let final_bytes = (ROUNDS & byte_mask) | inverted_bytes;
        Self {
            bells: m128i::from(final_bytes),
            stage: self.stage,
        }
    }

    fn check_validity(self) -> Result<Self, InvalidRowError> {
        check_validity(self.stage, self.bell_iter())?;
        Ok(self)
    }

    fn check_validity_with_stage(self, stage: Stage) -> Result<Self, InvalidRowError> {
        check_validity_with_stage(stage, self.bell_iter())?;
        Ok(self)
    }

    #[inline(always)]
    fn place_of(&self, bell: Bell) -> Option<usize> {
        // PERF: We can almost certainly use some SIMD here to make things faster
        self.bell_iter().position(|b| b == bell)
    }

    #[inline(always)]
    fn is_rounds(&self) -> bool {
        u128::from(self.bells) == ROUNDS
    }

    #[inline(always)]
    fn extend_to_stage(&mut self, stage: Stage) {
        assert!(stage.as_usize() <= 16);
        assert!(stage >= self.stage);
        // Due to the unused-bytes invariant, we already have the cover bells in place to do the
        // extension for free
        self.stage = stage;
    }
}

impl Hash for SimdRow {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.stage.hash(state);
        safe_arch::extract_i64_imm_m128i!(self.bells, 0).hash(state);
        safe_arch::extract_i64_imm_m128i!(self.bells, 1).hash(state);
    }
}

impl std::fmt::Debug for SimdRow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Row({})", self.to_string())
    }
}

impl std::fmt::Display for SimdRow {
    /// Returns a [`String`] representing this `Row`.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, RowTrait, Stage};
    ///
    /// assert_eq!(Row::rounds(Stage::MAJOR).to_string(), "12345678");
    /// assert_eq!(Row::parse("146235")?.to_string(), "146235");
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.bell_iter().map(|b| b.to_string()).join(""))
    }
}

impl std::ops::Mul for SimdRow {
    type Output = Self;

    /// Uses the RHS to permute the LHS without consuming either argument.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, RowTrait};
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
    /// use proj_core::{Row, RowTrait};
    ///
    /// // Multiplying two Rows of different Stages panics rather than
    /// // producing undefined behaviour
    /// let _unrow = &Row::parse("13425678")? * &Row::parse("4321")?;
    /// # Ok::<(), proj_core::InvalidRowError>(())
    /// ```
    #[inline(always)]
    fn mul(self, rhs: Self) -> Self::Output {
        assert_eq!(self.stage(), rhs.stage());
        // This unsafety is OK because the product of two valid Rows of the same Stage is always
        // valid (because groups are closed under their binary operation).
        unsafe { self.mul_unchecked(&rhs) }
    }
}

impl std::ops::Not for SimdRow {
    type Output = Self;

    /// Find the inverse of a [`Row`].  If `X` is the input [`Row`], and `Y = !X`, then
    /// `XY = YX = I` where `I` is the identity on the same stage as `X` (i.e. rounds).  This
    /// operation cannot fail, since valid [`Row`]s are guaranteed to have an inverse.
    ///
    /// # Example
    /// ```
    /// use proj_core::{Row, RowTrait, Stage};
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
    #[inline]
    fn not(self) -> Self::Output {
        // Delegate to the borrowed version
        self.inv()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BellIter {
    bells: u128,
    bells_left: usize,
}

impl Iterator for BellIter {
    type Item = Bell;

    fn next(&mut self) -> Option<Self::Item> {
        // Mark that we're consuming another bell, and if the subtraction fails then it must mean
        // that the iterator has finished
        self.bells_left = self.bells_left.checked_sub(1)?;
        // Read the correct byte from the u128 as a Bell
        let bell = Bell::from_index(self.bells as usize & 0xff);
        // Shift the u128 down a byte so that the next bell is in the least significant byte
        self.bells = self.bells >> 8;
        // Return the new Bell
        Some(bell)
    }
}

impl From<Row> for SimdRow {
    fn from(r: Row) -> Self {
        unsafe { Self::from_iter_unchecked(r.bell_iter()) }
    }
}

// Test to check that transmutations are safe
#[test]
fn test_m128i_size_align() {
    assert_eq!(core::mem::size_of::<m128i>(), 16);
    assert_eq!(core::mem::align_of::<m128i>(), 16);

    assert_eq!(core::mem::size_of::<u128>(), 16);
    assert_eq!(core::mem::align_of::<u128>(), 8);
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn perm_to_16() -> BoxedStrategy<Vec<usize>> {
        (0usize..16)
            .prop_map(|n| (0..n).collect_vec())
            .prop_shuffle()
            .boxed()
    }

    proptest! {
        #[test]
        fn inv(a in perm_to_16()) {
            let r = SimdRow::from_iter(a.iter().map(|v| Bell::from_index(*v))).unwrap();
            let r_inv = r.inv();
            let rounds = SimdRow::rounds(r.stage());

            assert_eq!(r.stage(), r_inv.stage());
            assert_eq!(r.mul(&r_inv), Ok(rounds));
            assert_eq!(r_inv.mul(&r), Ok(rounds));
        }

        #[test]
        fn mul_matches_row(a in perm_to_16(), b in perm_to_16()) {
            let r1 = Row::from_iter(a.iter().map(|v| Bell::from_index(*v))).unwrap();
            let r2 = Row::from_iter(b.iter().map(|v| Bell::from_index(*v))).unwrap();

            let standard_mul = r1.mul(&r2).map(SimdRow::from);
            let simd_mul = SimdRow::from(r1).mul(&SimdRow::from(r2));

            // Converting then multiplying should give the same result as vice versa
            assert_eq!(standard_mul, simd_mul);
        }

        #[test]
        fn swap_matches_row(a in perm_to_16(), i1 in 0usize..16, i2 in 0usize..16) {
            // Swapping out of range will panic
            if i1 < a.len() && i2 < a.len() {
                let mut standard_swap = Row::from_iter(
                    a.iter().map(|v| Bell::from_index(*v))
                ).unwrap();
                let mut simd_swap = SimdRow::from(standard_swap.clone());

                standard_swap.swap(i1, i2);
                simd_swap.swap(i1, i2);

                // Converting then swapping should give the same result as vice versa
                assert_eq!(SimdRow::from(standard_swap), simd_swap);
            }
        }
    }
}
