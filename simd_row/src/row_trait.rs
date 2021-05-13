use proj_core::{Bell, IncompatibleStages, Stage};

pub trait RowTrait: Eq + Clone {
    type BellIter: Iterator<Item = Bell>;

    unsafe fn _from_iter_unchecked(iter: impl Iterator<Item = Bell>) -> Self;

    unsafe fn _mul_unchecked(&self, other: &Self) -> Self;

    fn _bell_iter(&self) -> Self::BellIter;

    fn _stage(&self) -> Stage;

    fn _rounds(stage: Stage) -> Self {
        // This unsafety is OK, because rounds is always a valid `Row`
        unsafe { Self::_from_iter_unchecked((0..stage.as_usize()).map(Bell::from_index)) }
    }

    fn _backrounds(stage: Stage) -> Self {
        // This unsafety is OK, because backrounds is always a valid `Row`
        unsafe { Self::_from_iter_unchecked((0..stage.as_usize()).rev().map(Bell::from_index)) }
    }

    fn _queens(stage: Stage) -> Self {
        // This unsafety is OK, because Queens is always a valid `Row`
        unsafe {
            Self::_from_iter_unchecked(
                (0..stage.as_usize())
                    .step_by(2)
                    .chain((1..stage.as_usize()).step_by(2))
                    .map(Bell::from_index),
            )
        }
    }

    fn _empty() -> Self {
        // This unsafety is OK, because 0-length rows are always valid (albeit useless in most
        // cases)
        unsafe { Self::_from_iter_unchecked(std::iter::empty()) }
    }

    fn _fast_hash(&self) -> usize {
        let mut accum = 0;
        let mut multiplier = 1;
        for b in self._bell_iter() {
            accum += b.index() * multiplier;
            multiplier *= self._stage().as_usize();
        }
        accum
    }

    #[inline]
    fn _place_of(&self, bell: Bell) -> Option<usize> {
        self._bell_iter().position(|b| b == bell)
    }

    fn _is_rounds(&self) -> bool {
        self._bell_iter().enumerate().all(|(i, b)| b.index() == i)
    }

    fn _mul(&self, rhs: &Self) -> Result<Self, IncompatibleStages> {
        IncompatibleStages::test_err(self._stage(), rhs._stage())?;
        // This unsafety is OK because the `self` and `rhs` are both assumed to be valid, and we
        // have already checked that their stages are equal
        Ok(unsafe { self._mul_unchecked(rhs) })
    }

    fn _closure(&self) -> Vec<Self> {
        let mut closure = Vec::new();
        let mut row = self.clone();
        loop {
            closure.push(row.clone());
            if row._is_rounds() {
                return closure;
            }
            // This unsafety is OK, because `self` is a valid Row and `row` and `self` will always
            // have the same Stage
            row = unsafe { row._mul_unchecked(self) };
        }
    }
}
