use std::collections::BinaryHeap;

/// A storage for marshalling nodes
pub trait Frontier<Node>: Default {
    /// Remove the next node from the `Frontier`
    fn pop(&mut self) -> Option<Node>;
    /// Add `node` to the `Frontier`
    fn push(&mut self, node: Node);
    /// Return the number of nodes currently in the `Frontier`
    fn len(&self) -> usize;
    /// Remove nodes from `self` until the length is under `len`.  The nodes removed should have
    /// the longest time to go before being `pop`ped.
    fn truncate(&mut self, len: usize);
}

/// A [`Frontier`] which always pops the largest scoring node, while performing all operations in
/// `O(log n)` time (except `truncate`, which takes `O(n log n)`).
#[derive(Debug, Clone)]
pub struct BestFirst<Node: Ord> {
    heap: BinaryHeap<Node>,
}

impl<Node: Ord> Frontier<Node> for BestFirst<Node> {
    #[inline]
    fn pop(&mut self) -> Option<Node> {
        self.heap.pop()
    }

    #[inline]
    fn push(&mut self, node: Node) {
        self.heap.push(node);
    }

    #[inline]
    fn len(&self) -> usize {
        self.heap.len()
    }

    fn truncate(&mut self, len: usize) {
        let heap = std::mem::replace(&mut self.heap, BinaryHeap::default());
        let mut nodes = heap.into_vec();
        nodes.sort_by(|a, b| b.cmp(a)); // Sort highest score first
        if len < nodes.len() {
            nodes.drain(len..);
        }
        self.heap = BinaryHeap::from(nodes);
    }
}

impl<Node: Ord> Default for BestFirst<Node> {
    fn default() -> Self {
        Self {
            heap: BinaryHeap::new(),
        }
    }
}
