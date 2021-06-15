// The SIMD intrinsics we're using are only valid on x86
#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

use bellframe::{Bell, Row, Stage};

const ROUNDS: u128 = 0x0f0e0d0c_0b0a0908_07060504_03020100;

/// A fast but unsafe version of [`Row`] which uses SIMD to accelerate [`Row`] operations.
///
/// This does not store a [`Stage`] (because the [`Stage`] is known externally during
/// the composing loop).  This makes equality checks faster, as well as making the memory layout
/// more efficient.  Additionally, this can only store [`Stage`] of up to 16 bells.
#[derive(Debug, Clone, Copy)]
pub struct FastRow {
    bell_bytes: __m128i,
}

impl FastRow {
    /// Return `true` if the current CPU supports the SIMD instructions.
    #[inline(always)]
    pub fn are_cpu_features_enabled() -> bool {
        is_x86_feature_detected!("ssse3") && is_x86_feature_detected!("sse2")
    }

    /// Creates a `FastRow` from an iterator of [`Bell`]s, without checking that the resulting
    /// sequence forms a valid [`Row`].
    ///
    /// # Safety
    ///
    /// This function is safe if `bell_iter` yields a valid [`Row`].
    ///
    /// # Panics
    ///
    /// This function panics if `bell_iter` yields more than 16 values.
    pub unsafe fn from_iter_unchecked(bell_iter: impl Iterator<Item = Bell>) -> (Self, Stage) {
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

        (Self::from_u128(val), Stage::from(num_bells_popped))
    }

    /// Permutes `self` by `other`.
    ///
    /// # Safety
    ///
    /// This is safe if the CPU feature `ssse3` is enabled.
    #[inline]
    #[target_feature(enable = "ssse3")]
    pub unsafe fn mul_unchecked(self, other: FastRow) -> FastRow {
        FastRow {
            bell_bytes: _mm_shuffle_epi8(self.bell_bytes, other.bell_bytes),
        }
    }

    /// Returns the `FastRow` representing rounds on any [`Stage`] up to 16
    ///
    /// # Safety
    ///
    /// This is safe if the CPU feature `sse2` is enabled.
    #[inline]
    #[target_feature(enable = "sse2")]
    pub unsafe fn rounds() -> Self {
        Self::from_u128(ROUNDS)
    }

    /// Creates a `FastRow` from a `u128` containing the [`Bell`]s as bytes.
    ///
    /// # Safety
    ///
    /// This is safe if the CPU feature `sse2` is enabled, and the row is expanded to rounds on 16.
    #[inline]
    #[target_feature(enable = "sse2")]
    pub unsafe fn from_u128(v: u128) -> Self {
        FastRow {
            bell_bytes: _mm_set_epi64x(
                // Mask out the top 64 bits
                (v >> 64) as i64,
                // Mask out the lower 64 bits
                v as i64,
            ),
        }
    }
}

impl PartialEq for FastRow {
    #[inline(always)]
    fn eq(&self, other: &FastRow) -> bool {
        u128::from(*self) == u128::from(*other)
    }
}

impl Eq for FastRow {}

impl From<FastRow> for u128 {
    fn from(fast_row: FastRow) -> u128 {
        // This unsafety is OK because the size of u128 and __m128i are the same (128 bits), but
        // the alignment requirements for `u128` (8 bytes) are less strict than those of `__m128i`
        // (16 bytes)
        unsafe { std::mem::transmute(fast_row.bell_bytes) }
    }
}

impl From<&Row> for FastRow {
    fn from(r: &Row) -> Self {
        // This unsafety is OK because `Row`s are valid by invariant
        unsafe { Self::from_iter_unchecked(r.bell_iter()).0 }
    }
}

#[cfg(test)]
mod tests {
    use super::FastRow;
    use bellframe::{Bell, RowBuf};
    use itertools::Itertools;
    use proptest::prelude::*;

    fn perm_to_16() -> BoxedStrategy<Vec<usize>> {
        (0usize..16)
            .prop_map(|n| (0..n).collect_vec())
            .prop_shuffle()
            .boxed()
    }

    proptest! {
        #[test]
        fn mul_matches_row(a in perm_to_16(), b in perm_to_16()) {
            // Fail the tests if the CPU features aren't enabled
            assert!(FastRow::are_cpu_features_enabled());

            if a.len() == b.len() {
                let r1 = RowBuf::from_bell_iter(a.iter().map(|v| Bell::from_index(*v))).unwrap();
                let r2 = RowBuf::from_bell_iter(b.iter().map(|v| Bell::from_index(*v))).unwrap();

                let standard_mul = FastRow::from(&*r1.mul_result(&r2).unwrap());
                // This unsafety is fine because we've asserted that the correct CPU flags are set
                let simd_mul = unsafe { FastRow::from(&*r1).mul_unchecked(FastRow::from(&*r2)) };

                // Converting then multiplying should give the same result as vice versa
                assert_eq!(standard_mul, simd_mul, "{:x} {:x}", u128::from(standard_mul), u128::from(simd_mul));
            }
        }
    }
}
