use datasize::DataSize;

use super::graph::{StartIdx, SuccIdx};

/// A container of prefix paths, stored as a linked-list style tree such that common prefixes are
/// only stored once.
#[derive(Debug)]
pub(super) struct Paths {
    /// Flat vector of [`PathNode`]s, stored compactly as 64-bit numbers.  The bits are used as
    /// follows:
    ///
    /// |   Bits   | [`PathNode::Start`] |  [`PathNode::Cons`]  | [`PathNode::Empty`] |
    /// |----------|---------------------|----------------------|---------------------|
    /// | ` 0..32` |        zeros        | `last` ([`PathId`])  | `next` ([`PathId`]) |
    /// | `32..63` |     [`StartIdx`]    | `succ` ([`SuccIdx`]) |        zeros        |
    /// | `63..64` |        zero         |         zero         |         one         |
    nodes: index_vec::IndexVec<PathId, u64>,
    /// The first `num_starts` elements of `nodes` are all [`PathNode::Start`]s
    num_starts: usize,
    /// The index of an [`PathNode::Empty`] node.  The empty nodes form a linked list, of which
    /// this is the head.
    first_empty: PathId,

    /// Number of non-[`PathNode::Empty`] nodes
    size: usize,
}

#[derive(Debug)]
enum PathNode {
    /// This node has no predecessors and is the start of a path
    Start(StartIdx),
    /// This node extends the `PathNode` at the `usize` index by taking the [`SuccIdx`]th successor
    /// of the currently reached node.
    Cons { last: PathId, succ: SuccIdx },
    /// This slot is empty and the next empty slot is at `next`
    Empty { next: PathId },
}

impl Paths {
    /// Crates an empty set of [`Paths`].
    pub(super) fn new() -> Self {
        Self {
            nodes: index_vec::IndexVec::new(),
            num_starts: 0,
            first_empty: NULL_NODE_IDX, // No empty nodes in the linked list
            size: 0,
        }
    }

    /// Add a new [`PathNode::Start`].
    ///
    /// # Panics
    ///
    /// Panics if any non-start nodes have been `add`ed
    pub(super) fn add_start(&mut self, start_idx: StartIdx) -> PathId {
        assert_eq!(
            self.nodes.len(),
            self.num_starts,
            "`add_start` called after other nodes were `add`ed."
        );
        self.size += 1;
        self.num_starts += 1;
        self.nodes.push(as_high_31_bits(start_idx.raw()))
    }

    /// Adds a new [`PathNode::Cons`] to this set of paths, returning its [`PathId`].
    pub(super) fn add(&mut self, last: PathId, succ: SuccIdx) -> PathId {
        // Generate the node data
        let mut node_data = last.raw() as u64; // Put `last` into the low 32 bits
        node_data |= as_high_31_bits(succ.raw()); // Put `succ` into the high 31 bits.  Note that
                                                  // this also leaves the 'empty' bit still as `0`

        self.size += 1; // It doesn't matter how the new node is added, the size always increases

        if self.first_empty == NULL_NODE_IDX {
            // No more empty nodes, so push to the vector
            self.nodes.push(node_data)
        } else {
            // Get the index of the first empty node, and remove it from the head of the empty list
            let empty_idx = self.first_empty;
            let next_empty = match self.get(empty_idx) {
                PathNode::Empty { next } => next,
                _ => panic!("`next_empty` pointed to a non-empty node"),
            };
            self.first_empty = next_empty;
            // Replace the empty node with the new node
            self.nodes[empty_idx] = node_data;
            empty_idx
        }
    }

    /// Run a garbage collection pass to mark any unreachable nodes as 'empty'
    pub(super) fn gc(&mut self, heads: impl IntoIterator<Item = PathId>) {
        // Mark which nodes are reachable, by marking all the non-start nodes as 'empty' then
        // marking any node reachable from `heads` as non-empty.  Start nodes cannot be GCed.
        for node_data in self.nodes.iter_mut().skip(self.num_starts) {
            *node_data |= EMPTY_BIT_MASK;
        }
        for head in heads {
            self.mark_path_as_non_empty(head);
        }
        // Rebuild the empty node list and count the number of non-empty nodes
        self.size = self.nodes.len();
        self.first_empty = NULL_NODE_IDX;
        for (idx, node_data) in self.nodes.iter_mut_enumerated().skip(self.num_starts) {
            let is_empty = *node_data & EMPTY_BIT_MASK != 0;
            if is_empty {
                // Push this node to the `empty` list
                *node_data = EMPTY_BIT_MASK | (self.first_empty.raw() as u64);
                self.first_empty = idx;
                self.size -= 1;
            }
        }
    }

    /// Mark this [`PathId`] and everything it reaches as 'non-empty'
    fn mark_path_as_non_empty(&mut self, head: PathId) {
        // Skip this if we've already marked this node (and its descendants) as reachable
        if self.nodes[head] & EMPTY_BIT_MASK == 0 {
            return;
        }
        // Set this node's 'empty' bit to `0` (it would have been set to `1` at the start of the GC
        // pass)
        self.nodes[head] &= !EMPTY_BIT_MASK;
        match self.get(head) {
            PathNode::Start(_) => {} // We've finished this linked list
            PathNode::Cons { last, succ: _ } => self.mark_path_as_non_empty(last),
            PathNode::Empty { .. } => unreachable!("We just set the 'empty' bit to 0"),
        }
    }

    /// Return the path which finishes at a given [`PathId`].
    pub(super) fn flatten(&self, node_idx: PathId) -> (StartIdx, Vec<SuccIdx>) {
        let mut succs = Vec::new();
        let start_idx = self.flatten_recursive(node_idx, &mut succs);
        (start_idx, succs)
    }

    fn flatten_recursive(&self, node_idx: PathId, succs: &mut Vec<SuccIdx>) -> StartIdx {
        match self.get(node_idx) {
            PathNode::Start(start_idx) => start_idx,
            PathNode::Cons { last, succ } => {
                let start_idx = self.flatten_recursive(last, succs);
                succs.push(succ);
                start_idx
            }
            PathNode::Empty { .. } => panic!("GCed nodes shouldn't be in a path"),
        }
    }

    /// Gets the [`PathNode`] at a given [`PathId`].
    fn get(&self, idx: PathId) -> PathNode {
        let data = self.nodes[idx];
        // Deconstruct the 64-bit value
        let is_empty = (data & EMPTY_BIT_MASK) != 0; // Node is `Empty` iff `EMPTY_BIT` is 1
        let is_start = idx.index() < self.num_starts; // Node is `Start` if it's within `num_starts`
        let low_32_bits = PathId::from_raw(data as u32); // Read bits `0..32`
        let high_31_bits = (data >> 32) as u32 & 0x7FFF_FFFF; // Read bits `32..63`

        // Construct node
        if is_start {
            assert!(!is_empty); // Start nodes shouldn't be GCed
            PathNode::Start(StartIdx::from_raw(high_31_bits))
        } else if is_empty {
            PathNode::Empty { next: low_32_bits }
        } else {
            PathNode::Cons {
                last: low_32_bits,
                succ: SuccIdx::from_raw(high_31_bits),
            }
        }
    }

    #[allow(dead_code)]
    fn debug_nodes(&self) {
        for (idx, data) in self.nodes.iter_enumerated() {
            let empty_bit = data >> 63;
            let high_31_bits = (data >> 32) & 0x7FFF_FFFF;
            let low_32_bits = *data as u32;
            println!(
                "{:>4?}: {} {:031b} {:032b} -> {:?}",
                idx.raw(),
                empty_bit,
                high_31_bits,
                low_32_bits,
                self.get(idx)
            );
        }
    }
}

impl DataSize for Paths {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.nodes.len() * std::mem::size_of::<u64>()
    }
}

#[track_caller]
fn as_high_31_bits(v: u32) -> u64 {
    assert_eq!(
        v & (1 << 31),
        0,
        "High 31-bit values should be smaller than 2^31"
    );
    (v as u64) << 32
}

const EMPTY_BIT_MASK: u64 = 1 << 63; // Bits `63..64`

index_vec::define_index_type! {
    #[derive(DataSize)]
    pub struct PathId = u32;
}
const NULL_NODE_IDX: PathId = PathId::from_raw_unchecked(u32::MAX);
