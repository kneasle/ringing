use std::cmp;

use crate::Bell;
use itertools::Itertools;

/// Helper function to calculate the length of the longest run off the start of a given
/// [`Iterator`]
pub fn run_len(iter: impl IntoIterator<Item = Bell>) -> usize {
    iter.into_iter()
        .map(|b| b.index())
        .tuple_windows::<(usize, usize)>()
        .take_while(|&(i1, i2)| (i1 as isize - i2 as isize).abs() == 1)
        .count()
        + 1
}

/// Split a [`Vec`] into two segments at a given `index`
pub fn split_vec<T>(vec: Vec<T>, index: usize) -> Option<(Vec<T>, Vec<T>)> {
    if index > vec.len() {
        return None; // Early return if the index is out of bounds
    }
    // We re-use the allocation of `vec` as the first return value, so drain the remaining elements
    // into another `Vec` to create `other`
    let mut left_vec = vec;
    let right_vec = left_vec.drain(index..).collect_vec();
    Some((left_vec, right_vec))
}

/// Computes `n choose k`, returning `None` if the computation would cause [`usize`] to overflow.
pub fn choice(n: usize, k: usize) -> Option<usize> {
    //                    n!
    // n choose k = -------------
    //              k! * (n - k)!
    //
    //                     1*2*3*...*n
    //            = ------------------------     (expanding the factorials)
    //              k! * (1*2*3*...*(n - k))
    //
    //              (n-k+1)*(n-k+2)*...*n
    //            = ---------------------     (cancelling)
    //                       k!
    //
    // This minimises the risk of overflow, since we only have to directly compute `n! / k!` and
    // `k!`, both of which are smaller than `n!` and `(n - k)! * k!`.  This lessens the risk of
    // overflow.

    // Replace `k` with `min(k, n - k)` because `n choose k = n choose (n - k)` for all n, k and we
    // want to do as few loop iterations as possible.
    let k = cmp::min(k, n - k);
    // PERF: Check for overflow upfront?
    let mut numerator = 1usize;
    let mut denominator = 1usize;
    for i in 1..=k {
        numerator = numerator.checked_mul(n - k + i)?; // computes (n-k+1) * (n-k+2) * ... * (n-k+k = n)
        denominator *= i; // computes 1 * 2 * 3 * ... * k.  We don't need to check for overflow
                          // because `numerator >= denominator`
    }
    Some(numerator / denominator)
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
