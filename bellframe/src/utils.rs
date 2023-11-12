use std::ops::{Bound, Range, RangeBounds};

use crate::{Bell, InvalidRowError, Stage};
use itertools::Itertools;

/// Helper function to calculate the length of the longest run off the start of a given
/// [`Iterator`]
pub fn run_len(iter: impl IntoIterator<Item = Bell>) -> usize {
    iter.into_iter()
        .map(|b| b.index())
        .tuple_windows::<(usize, usize)>()
        // Subtraction can't over-/under-flow because i1 and i2 are both +ve
        .take_while(|&(i1, i2)| (i1 as i8 - i2 as i8).abs() == 1)
        .count()
        + 1
}

/// Given some [`Bell`]s and a [`Stage`], simultaneously check for duplicate [`Bell`]s and any
/// [`Bell`]s which are too big for the given [`Stage`].
pub(crate) fn check_duplicate_or_out_of_stage(
    bells: impl IntoIterator<Item = Bell>,
    stage: Stage,
) -> Result<(), InvalidRowError> {
    // We check validity by keeping a checklist of which `Bell`s we've seen, and checking off
    // each bell as we go.  PERF: use a bitmap here
    let mut checklist = vec![false; stage.num_bells()];
    // Loop over all the bells to check them off in the checklist.  We do not need to check for
    // empty spaces in the checklist once we've done because (by the Pigeon Hole Principle),
    // fitting `n` bells into `n` slots with some gaps will always require that a bell is
    // either out of range or two bells share a slot.
    for b in bells {
        match checklist.get_mut(b.index()) {
            // If the `Bell` is out of range of the checklist, it can't belong within the
            // `Stage` of this `Row`
            None => return Err(InvalidRowError::BellOutOfStage(b, stage)),
            // If the `Bell` has already been seen before, then it must be a duplicate
            Some(&mut true) => return Err(InvalidRowError::DuplicateBell(b)),
            // If the `Bell` has not been seen before, check off the checklist entry and
            // continue
            Some(x) => *x = true,
        }
    }
    // If none of the `Bell`s caused errors, the row must be valid
    Ok(())
}

/// Converts any [`RangeBounds`] (i.e. `x..=y`, `..y`, `..`, etc.) into a concrete [`Range`], where
/// unbounded min/max bounds are clamped to either `0` or `length`, respectively
pub fn clamp_range(range: impl RangeBounds<usize>, length: usize) -> Range<usize> {
    let range_min_inclusive = match range.start_bound() {
        Bound::Included(v) => *v,
        Bound::Excluded(v) => *v + 1,
        Bound::Unbounded => 0,
    };
    let range_max_exclusive = match range.end_bound() {
        Bound::Included(v) => *v + 1,
        Bound::Excluded(v) => *v,
        Bound::Unbounded => length,
    };

    range_min_inclusive..range_max_exclusive
}

/// Split a [`Vec`] into two segments at a given `index`
pub fn split_vec<T>(vec: Vec<T>, index: usize) -> Option<(Vec<T>, Vec<T>)> {
    if index > vec.len() {
        return None; // Early return if the index is out of bounds
    }
    // We re-use the allocation of `vec` as the first return value, so drain the remaining elements
    // into another `Vec` to create `other`
    let mut left_vec = vec;
    let right_vec = left_vec.split_off(index);
    Some((left_vec, right_vec))
}

#[cfg(test)]
mod tests {
    use crate::{run_len as rl, RowBuf};

    #[test]
    fn run_len() {
        for &(row, run_len_f) in &[("123456", 6), ("456231", 3), ("612345", 1)] {
            assert_eq!(rl(RowBuf::parse(row).unwrap().bell_iter()), run_len_f);
        }
    }
}
