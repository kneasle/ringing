use std::ops::{Deref, Mul, Not};

use bellframe::{Bell, Row, RowBuf, Stage};
use datasize::DataSize;
use gcd::Gcd;

/// A group of [`Row`]s, used to represent part heads.  Currently limited to cyclic groups (in the
/// mathematical sense of 'cyclic').
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PartHeadGroup {
    /// The [`Row`]s which make up the `PartHeadGroup`.  `part_heads[0]` is always rounds.
    ///
    /// Invariant: there is some row `r` such that `part_heads[i] = r^i` for all `i`.
    // PERF: Make this a `SameStageVec`
    part_heads: Vec<RowBuf>,
    /// For each value in `part_heads`, the bit at index `i` is `1` iff that `part_heads[i]` is
    /// a generator for this group
    is_generator_bitmap: u64,
}

/// A compact representation of a single `PartHead` within a [`PartHeadGroup`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, DataSize)]
pub struct PartHead {
    /// The index into the owning [`PartHeadGroup`]'s `part_heads` list.
    index: u8,
}

/// A compact representation of a transformation between two `PartHead`s
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhRotation {
    /// The index into the owning [`PartHeadGroup`]'s `part_heads` list.
    rotation: u8,
    /// The number of [`PartHead`]s in the source [`PartHeadGroup`]
    num_parts: u8,
}

impl PartHeadGroup {
    /// Create a new `PartHeadGroup` containing all elements generated by a given [`Row`].
    pub fn new(row: &Row) -> Self {
        let part_heads = row.closure_from_rounds();
        Self {
            is_generator_bitmap: coprime_bitmap(part_heads.len()),
            part_heads,
        }
    }

    /// Create a new `PartHeadGroup` containing only one part.
    pub fn one_part(stage: Stage) -> Self {
        Self::new(&RowBuf::rounds(stage))
    }

    /// Return the maximum [`effective_stage`](Row::effective_stage) of every [`Row`] in this
    /// `PartHeadGroup`
    pub fn effective_stage(&self) -> Stage {
        self.part_heads.last().unwrap().effective_stage()
    }

    /// Returns the number of part heads in this `PartHeadGroup`.
    pub fn size(&self) -> usize {
        self.part_heads.len()
    }

    pub fn is_one_part(&self) -> bool {
        self.size() == 1
    }

    pub fn is_multi_part(&self) -> bool {
        !self.is_one_part()
    }

    /// Returns an [`Iterator`] over the [`Row`]s in this `PartHeadGroup`
    pub fn rows(&self) -> impl Iterator<Item = &Row> + Clone {
        self.part_heads.iter().map(Deref::deref)
    }

    /// Returns an [`Iterator`] over the [`Row`]s in this `PartHeadGroup`, along with their
    /// corresponding [`PhRotation`]s.
    pub fn rotations(&self) -> impl Iterator<Item = (&Row, PhRotation)> {
        self.part_heads.iter().enumerate().map(|(idx, row)| {
            (
                row.as_row(),
                PhRotation {
                    rotation: idx as u8,
                    num_parts: self.size() as u8,
                },
            )
        })
    }

    /* PROCESSING ELEMENTS */

    /// Return a set of groups of [`Bell`]s which share paths through the compositions.
    ///
    /// For example, a composition with `parthead = "134265"` will return
    /// `[[1], [2, 3, 4], [5, 6], [7], [8]]`.  The ordering of the [`Bell`]s and groups is
    /// non-deterministic.
    pub fn bell_cycles(&self) -> Vec<Vec<Bell>> {
        let base_part_head = self.part_heads.last().unwrap();

        let mut bells_used = vec![false; base_part_head.stage().num_bells()];
        let mut groups = Vec::new();
        // Repeatedly pick a bell that we haven't put into a group
        while let Some(group_start) = bells_used.iter().position(|u| !*u) {
            let group_start = Bell::from_index(group_start as u8);
            let mut group = Vec::new();
            // This bell is grouped with every place bell it visits in the part head
            let mut next_bell = group_start;
            loop {
                group.push(next_bell);
                bells_used[next_bell.index()] = true;
                next_bell = base_part_head[next_bell.index()];
                if next_bell == group_start {
                    break; // We've fully run through this loop
                }
            }
            groups.push(group);
        }
        groups
    }

    /// Given an abstract [`PartHead`], return the corresponding [`Row`]
    pub fn get_row(&self, element: PartHead) -> &Row {
        &self.part_heads[element.index as usize]
    }

    pub fn get_part_head(&self, row: &Row) -> Option<PartHead> {
        let index = self.part_heads.iter().position(|x| x == row)?;
        Some(PartHead { index: index as u8 })
    }

    pub fn is_row_generator(&self, row: &Row) -> bool {
        let Some(part_head) = self.get_part_head(row) else {
            return false; // Can't be a generator if it isn't in the group
        };
        self.is_generator(part_head)
    }

    /// Returns `true` iff every [`PartHead`] in this group is a power of the given `part_head`
    pub fn is_generator(&self, part_head: PartHead) -> bool {
        self.is_generator_bitmap & (1 << part_head.index) != 0
    }
}

impl PartHead {
    /// Return the compact [`PartHead`] representing [`rounds`](RowBuf::rounds).
    pub fn rounds() -> Self {
        // Rounds is always the 0th element by the invariant on `PartHeadGroup.part_heads`
        PartHead { index: 0 }
    }
}

impl PhRotation {
    pub fn is_identity(self) -> bool {
        self.rotation == 0
    }
}

impl Not for PhRotation {
    type Output = Self;

    /// Return the [`PhRotation`] which undoes the effect of `self`
    fn not(self) -> Self {
        PhRotation {
            rotation: (self.num_parts - self.rotation) % self.num_parts,
            num_parts: self.num_parts,
        }
    }
}

impl Mul<PhRotation> for PartHead {
    type Output = Self;

    /// Applies some [`PhRotation`] to a given [`PartHead`]
    fn mul(self, rot: PhRotation) -> Self {
        PartHead {
            index: (self.index + rot.rotation) % rot.num_parts,
        }
    }
}

/// Returns a bitmap where there's a `1` for every number that's co-prime to `n`
fn coprime_bitmap(n: usize) -> u64 {
    assert!(n <= 64);
    assert!(n > 0);
    if n == 1 {
        return 1; // Finishing a part in rounds is only allowed in a 1-part
    }
    let mut mask = 0;
    for i in 1..n {
        if i.gcd(n) == 1 {
            mask |= 1 << i;
        }
    }
    mask
}
