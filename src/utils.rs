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

#[cfg(test)]
mod tests {
    use crate::{run_len as rl, Row};

    #[test]
    fn run_len() {
        for &(row, run_len_f) in &[("123456", 6), ("456231", 3), ("612345", 1)] {
            assert_eq!(rl(Row::parse(row).unwrap().bell_iter()), run_len_f);
        }
    }
}
