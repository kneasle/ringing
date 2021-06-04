// The SIMD intrinsics we're using are only valid on x86
#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

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

    /// Permutes `self` by `other`.
    #[inline]
    #[target_feature(enable = "ssse3")]
    pub unsafe fn mul_unchecked(self, other: FastRow) -> FastRow {
        FastRow {
            bell_bytes: _mm_shuffle_epi8(self.bell_bytes, other.bell_bytes),
        }
    }

    /// Returns the `FastRow` representing rounds on any [`Stage`] up to 16
    #[inline]
    #[target_feature(enable = "sse2")]
    pub unsafe fn rounds() -> Self {
        Self::from_u128(ROUNDS)
    }

    #[inline]
    #[target_feature(enable = "sse2")]
    pub unsafe fn from_u128(v: u128) -> Self {
        FastRow {
            bell_bytes: _mm_set_epi64x(
                // Mask out the top 64 bits
                (v >> 64) as i64,
                // And mask out the lower 64 bits
                (v & (1u128 << 64 - 1)) as i64,
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
