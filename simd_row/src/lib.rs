// #![cfg(target_feature = "ssse3")]

pub mod row_trait;

use proj_core::{Bell, Stage};
use safe_arch::{m128i, shuffle_av_i8z_all_m128i};

#[inline(always)]
fn rounds_rep() -> m128i {
    m128i::from(0x0f0e0d0c_0b0a0908_07060504_03020100u128)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SimdRow {
    /// The bells contained in this [`SimdRow`], packed as individual bytes with the first [`Bell`]
    /// in the least significant byte.
    ///
    /// **Invariant:** The unused bytes **must** be set their own indices (making this always a
    /// valid `Row` on 16 bells).  This is because that bitpattern is preserved by multiplication,
    /// meaning that simple bit equality is sufficient without any extra bitmasking.
    bells: m128i,
    stage: Stage,
}

impl row_trait::RowTrait for SimdRow {
    type BellIter = BellIter;

    unsafe fn _from_iter_unchecked(bell_iter: impl Iterator<Item = Bell>) -> Self {
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
            fused_bell_iter.next().is_some(),
            "SimdRows can only contain 16 bells",
        );

        SimdRow {
            bells: m128i::from(val),
            stage: Stage::from(num_bells_popped),
        }
    }

    #[inline(always)]
    unsafe fn _mul_unchecked(&self, other: &Self) -> Self {
        SimdRow {
            bells: shuffle_av_i8z_all_m128i(self.bells, other.bells),
            stage: self.stage,
        }
    }

    #[inline(always)]
    fn _stage(&self) -> Stage {
        self.stage
    }

    fn _bell_iter(&self) -> Self::BellIter {
        BellIter {
            bells: u128::from(self.bells),
            bells_left: self.stage.as_usize(),
        }
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
        //
        // Read the correct byte from the u128 as a Bell
        let bell = Bell::from_index(self.bells as usize & 0xff);
        // Shift the u128 down a byte so that the next bell is in the least significant byte
        self.bells = self.bells >> 8;
        // Return the new Bell
        Some(bell)
    }
}
