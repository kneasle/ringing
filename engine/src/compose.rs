use std::{cmp::Ordering, collections::HashSet, sync::Arc};

use shortlist::Shortlist;

use crate::{
    fast_row::{CompressedRow, FastRow},
    Engine, SegmentID,
};

/// The mutable data required to generate a composition.  Each worker thread will have their own
/// `EngineTask` (but all share the same [`Engine`]).
#[derive(Debug, Clone)]
pub(crate) struct EngineWorker {
    thread_id: usize,
    /// A `Shortlist` of found compositions
    shortlist: Shortlist<Comp>,
    /// Which of the possible starting points was chosen
    starting_node: usize,
    /// Which nodes have already been rung (to compute falseness)
    nodes: HashSet<CompressedNode>,
    /// Which links where chosen after each node.  These are indices into the `links` field on each
    /// `Segment`.  Therefore, this is cheap to track during the composing loop and reconstruction
    /// a human-friendly representation just requires a traversal of the [`Engine`]'s [`Layout`].
    comp_prefix: Vec<usize>,
}

impl EngineWorker {
    /// Creates a new `EngineWorker`
    pub fn compose(engine: Arc<Engine>, thread_id: usize) {
        let mut worker = EngineWorker {
            thread_id,
            shortlist: Shortlist::new(engine.config.num_comps),
            starting_node: 0,
            nodes: HashSet::new(),
            comp_prefix: Vec::new(),
        };

        assert!(FastRow::are_cpu_features_enabled());

        for (i, start_node) in engine.start_nodes.iter().enumerate() {
            worker.starting_node = i;
            // This unsafety is OK, because we check the CPU flags before starting the composing
            // loop
            unsafe { worker.explore_node(&engine, start_node.clone(), 0, 0.0) };
        }

        let mut discovered_comps = worker.shortlist.into_vec();
        discovered_comps.sort();
        discovered_comps.reverse();
        for c in discovered_comps {
            println!("  {}", c.to_string(&engine));
        }
    }

    /// Assuming that the node has already been checked for falseness, add this node to the
    /// composition and explore its branches.  This function is the core composing loop: ~100% of
    /// Monument's runtime will be spent here.
    ///
    /// # Safety
    ///
    /// This function is only safe if the `ssse3` CPU feature flags are enabled.
    #[target_feature(enable = "ssse3")]
    unsafe fn explore_node(
        &mut self,
        engine: &Engine,
        node: Node,
        length: usize,
        accumulated_music: f32,
    ) {
        let seg_table = engine.get_seg_table(node.seg_id);

        // Reject node if it would make the composition too long
        if length > engine.len_range.end {
            return;
        }

        // Check if the composition comes round
        if engine.len_range.contains(&length) && node.seg_id.v == 0 && node.row == FastRow::rounds()
        {
            self.save_comp(engine, length, accumulated_music);
        }

        // Reject node if it's false against any node in the composition prefix
        for &(fch, false_segment) in &seg_table.false_segments {
            let false_node = Node::new(false_segment, node.row.mul_unchecked(fch));
            if self.nodes.contains(&false_node.compress()) {
                return;
            }
        }

        // Accumulate the music for this node
        let music_after_this_node = accumulated_music + seg_table.music.evaluate(node.row);

        // If we haven't pruned this branch, add the current node to the node table ...
        let compressed_node = node.compress();
        self.nodes.insert(compressed_node);
        // ... then explore all nodes reachable from it ...
        let length_after_this_node = length + seg_table.length;
        for (i, link) in seg_table.links.iter().enumerate() {
            self.comp_prefix.push(i);
            self.explore_node(
                engine,
                Node::new(link.end_segment, node.row.mul_unchecked(link.transposition)),
                length_after_this_node,
                music_after_this_node,
            );
            self.comp_prefix.pop();
        }
        // ... then unload this node from the persistent state
        self.nodes.remove(&compressed_node);
    }

    /// Save the currently loaded composition
    #[cold]
    fn save_comp(&mut self, _engine: &Engine, length: usize, score: f32) {
        let comp = Comp {
            length,
            score,
            starting_node: self.starting_node,
            calls: self.comp_prefix.clone(),
        };

        // println!("FOUND COMP! {}", comp.to_string(engine));

        self.shortlist.push(comp);
    }
}

/// A single node of the composition - this is a [`SectionID`] (usually some part of the plain
/// course), along with a [`Row`] describing the course head.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) struct Node {
    row: FastRow,
    seg_id: SegmentID,
}

impl Node {
    pub fn new(seg_id: SegmentID, row: FastRow) -> Self {
        Node { row, seg_id }
    }

    fn compress(self) -> CompressedNode {
        CompressedNode {
            row: self.row.compress(),
            seg_id: self.seg_id,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct CompressedNode {
    row: CompressedRow,
    seg_id: SegmentID,
}

/// A completed composition
#[derive(Debug, Clone)]
pub struct Comp {
    pub starting_node: usize,
    pub calls: Vec<usize>,
    pub length: usize,
    pub score: f32,
}

impl Comp {
    #[allow(dead_code)]
    fn to_string(&self, engine: &Engine) -> String {
        let mut string = format!("(len: {}, score: {}) ", self.length, self.score);

        let mut current_seg_id = engine.start_nodes[self.starting_node].seg_id;
        for &link_ind in &self.calls {
            let link = &engine.get_seg_table(current_seg_id).links[link_ind];
            string.push_str(&link.display_name);
            current_seg_id = link.end_segment;
        }

        string
    }
}

impl PartialOrd for Comp {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.score.partial_cmp(&other.score)
    }
}

impl Ord for Comp {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl PartialEq for Comp {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score
    }
}

impl Eq for Comp {}
