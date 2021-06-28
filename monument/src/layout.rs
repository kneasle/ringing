use std::{
    fmt::Write,
    fmt::{Debug, Formatter},
    ops::Deref,
};

use bellframe::{Bell, Row, RowBuf};
use itertools::Itertools;

use crate::{graph::NodeId, mask::Mask};

/// A newtyped integer which is used to refer to a specific composition segment
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct SegmentId {
    pub(crate) idx: usize,
}

impl From<usize> for SegmentId {
    #[inline(always)]
    fn from(idx: usize) -> Self {
        SegmentId { idx }
    }
}

impl From<SegmentId> for usize {
    #[inline(always)]
    fn from(seg_id: SegmentId) -> usize {
        seg_id.idx
    }
}

impl Debug for SegmentId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "SegId({})", self.idx)
    }
}

/// A mid-level representation of the course layout of a composition.  In this representation, a
/// layout is a set of [`Segment`]s, which are sequences of [`Row`]s combined with links to the
/// [`Segment`]s which can come after them.  Every useful composition structure (that I know of)
/// can be represented like this, but it is not efficient to use [`Layout`]s directly in the
/// composing loop.  Therefore, [`Engine`] compiles a `Layout` (along with extra info like desired
/// composition length, music requirements, etc.) into a node graph that can be efficiently
/// traversed.
#[derive(Debug, Clone)]
pub struct Layout {
    /// A list of blocks of [`Row`]s, from which the [`Segment`]s are taken (more precisely, each
    /// [`Segment`] corresponds to a subsequence of some block in `blocks`).  In most cases, this
    /// will be the plain course of the given method(s).
    // TODO: This should have type `Vec<proj_core::Block>`, but we first have to relax the
    // restriction that blocks must begin at rounds
    pub blocks: Vec<Vec<RowBuf>>,
    /// The rows contained in `(<rounds>, i)` will be in `segment_rows[i]`.  These are usually
    /// ranges of the plain course of a single method, but could contain the plain courses of
    /// multiple methods (in the case of spliced).  If a segment contains rounds, it will be
    /// assumed that it is a possible starting point for the composition.
    pub segments: Vec<Segment>,
    /// The bells which are fixed within the composition
    pub fixed_bells: Vec<Bell>,
}

impl Layout {
    /// Find which [`Segment`]s can contain rounds, and split these corresponding [`Node`]s into
    /// two new segments - one which starts at rounds (i.e. a 'start' node), and one which finishes
    /// at rounds (i.e. an 'end' node).  These 'end' nodes are allowed to have 0 length.
    pub fn generate_starts_and_end_nodes(&mut self) {
        let mut new_segments = Vec::<(Option<SegmentId>, Segment)>::new();
        for (seg_idx, s) in self.segments.iter().enumerate() {
            'row_loop: for (row_idx, r) in self.segment_rows(SegmentId::from(seg_idx)).enumerate() {
                // Check that the fixed bells are all in their home positions in this row (by
                // testing for misplaced bells and rejecting the row in the case of misplaced
                // bells).
                for fixed_bell in &self.fixed_bells {
                    if r[fixed_bell.index()] != *fixed_bell {
                        continue 'row_loop;
                    }
                }
                // If we've got here then the current row contains all the fixed bells in their
                // home positions, and thus rounds could occur in this location.  Therefore, the
                // course head which would contain rounds in this location is the inverse of that
                // current row.
                let course_head_containing_rounds = r.inv();

                // These look like they're the wrong way round, but it is fine because an 'end'
                // node covers the run-up to rounds whereas a 'start' node starts at rounds and
                // continues with the rest of the block
                let (block_idx, row_range) = s.row_range;
                let (end_seg_range, start_seg_range) =
                    self.split_range(block_idx, row_range, row_idx);

                // Start segment
                new_segments.push((
                    // Start nodes aren't truncating any other segments
                    None,
                    Segment {
                        row_range: (block_idx, start_seg_range),
                        position: Position::Start,
                        // Start nodes can only exist with one course head
                        course_head_masks: vec![Mask::full_row(&course_head_containing_rounds)],
                        // All other values are inherited from the 'parent' node
                        ..s.clone()
                    },
                ));
                // End segment
                new_segments.push((
                    // This end segment will truncate its 'parent' segment - i.e. if the
                    // composition reaches this specific instance of the parent segment then it
                    // must stop at rounds (thus 'truncating' the parent into the end segment)
                    Some(SegmentId::from(seg_idx)),
                    Segment {
                        row_range: (block_idx, end_seg_range),
                        position: Position::End,
                        // End nodes can only exist with one course head
                        course_head_masks: vec![Mask::full_row(&course_head_containing_rounds)],
                        // End nodes can't lead anywhere
                        links: Vec::new(),
                        // End nodes can't be further truncated
                        truncations: Vec::new(),
                        // All other values are inherited from the 'parent' node
                        ..s.clone()
                    },
                ));
            }
        }

        // Check that rounds is actually reachable
        assert!(
            !new_segments.is_empty(),
            "No nodes satisfying the course head masks can contain rounds"
        );

        // Add these new segments to the layout
        for (truncation_parent, new_seg) in new_segments {
            // We're pushing this new segment to `self.segments`, so its index (and therefore ID)
            // will be the length of `self.segments` **before** pushing
            let new_seg_id = SegmentId::from(self.segments.len());
            // Tell the parent to truncate to this node
            if let Some(p) = truncation_parent {
                self.segments[p.idx].truncations.push(new_seg_id);
            }
            // Add the new segment
            self.segments.push(new_seg);
        }
    }

    /// Splits a [`SegmentRange`] into two pieces at a given index (indexed from the start of the
    /// range).
    ///
    /// # Panics
    ///
    /// Panics if the index exceeds the length of the range, or `block_idx` exceeds the length of
    /// `self.blocks`.
    pub fn split_range(
        &self,
        block_idx: usize,
        range: SegmentRange,
        split_idx: usize,
    ) -> (SegmentRange, SegmentRange) {
        assert!(split_idx <= range.length);
        (
            SegmentRange {
                start_idx: range.start_idx,
                length: split_idx,
            },
            SegmentRange {
                start_idx: (range.start_idx + split_idx) % self.blocks[block_idx].len(),
                length: range.length - split_idx,
            },
        )
    }

    /// Gets an [`Iterator`] over the rows in the plain course of the segment at a given index
    pub fn segment_rows(&self, seg_id: SegmentId) -> impl Iterator<Item = &Row> + Clone {
        let (block_idx, range) = self.segments[seg_id.idx].row_range;

        self.blocks[block_idx]
            .iter()
            .cycle()
            .skip(range.start_idx)
            .take(range.length)
            .map(Deref::deref)
    }

    /// Given a [`NodeId`], returns the [`NodeId`] of the truncated version of that node (i.e.
    /// converting a longer node into an end node if it contains rounds).  If the node can't be
    /// truncated, then the original ID is returned.
    pub(crate) fn truncate(&self, node_id: NodeId) -> NodeId {
        let segment = self.get_segment(node_id.seg);
        'truncate_loop: for &truncated_id in &segment.truncations {
            // Check that the node's course head is permitted by the truncated node (for example,
            // if `i` is the ID of the segment containing rounds, then (i, 13245678) will not be
            // able to be truncated because the course head masks will not match).
            for mask in &self.get_segment(truncated_id).course_head_masks {
                if !mask.matches(&node_id.row) {
                    // If any mask fails then this segment can't be truncated, so skip to the next
                    // one
                    continue 'truncate_loop;
                }
            }
            // If all the masks matched the current node, then we truncate the node
            return NodeId::new(truncated_id, node_id.row);
        }
        // If none of the truncations succeeded, then the node is preserved
        node_id
    }

    pub fn get_segment(&self, segment_id: SegmentId) -> &Segment {
        &self.segments[segment_id.idx]
    }

    pub fn debug_string(&self) -> String {
        let mut s = String::new();
        for (block_idx, b) in self.blocks.iter().enumerate() {
            writeln!(s, "===== BLOCK {} =====", block_idx).unwrap();
            let segs_in_block = self
                .segments
                .iter()
                .enumerate()
                .filter(|(_i, s)| s.row_range.0 == block_idx)
                .collect_vec();

            // Run through each row of the block, printing each segment which overlaps with it
            for (row_idx, r) in b.iter().enumerate() {
                write!(s, "{}", r).unwrap();
                for &(seg_idx, seg) in &segs_in_block {
                    let row_range = seg.row_range.1;

                    if (row_idx + b.len() - row_range.start_idx) % b.len() < row_range.length {
                        write!(s, " {:>2}", seg_idx).unwrap();
                    } else {
                        write!(s, "   ").unwrap();
                    }
                }
                write!(s, "\n").unwrap();
            }
        }
        s
    }
}

/// The range of [`Row`]s covered by a [`Segment`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SegmentRange {
    /// The index within the block of the first [`Row`] covered by this [`Segment`]
    pub start_idx: usize,
    /// The number of [`Row`]s in this [`Segment`].  Note that we can't simply use
    /// `std::ops::Range<usize>` here because we would not be able to differentiate between
    /// 0-length segments (e.g. the segment of the plain course generated when a comp comes round
    /// at a call) and segments which cover an entire block.
    pub length: usize,
}

#[derive(Debug, Clone)]
pub struct Segment {
    /// The [`Row`]s contained in this `Segment`.  This is a tuple of (index of block, subrange of
    /// block).  The range is allowed to wrap around the end of the block as many times as desired
    /// (although the resulting `Segment` would be false against itself and therefore unusable).
    pub row_range: (usize, SegmentRange),
    /// The ways that this `Segment` can be lead to other `Segment`s (in possibly different
    /// courses).
    pub links: Vec<SegmentLink>,
    /// List of positions where the fixed [`Bell`]s can be in the course heads for this `Segment`.
    /// These lists are indexed against [`Layout::fixed_bells`] (i.e.
    /// `valid_fixed_bell_patterns[i][j]` would refer to `Layout::fixed_bells[j]`.
    pub course_head_masks: Vec<Mask>,
    /// The name used to refer to this [`Segment`] (useful for differentiating unique methods in
    /// spliced).  In single methods, this is usually just set to the empty string.
    pub name: String,
    /// The [`SegmentId`]s of the segments which this [`Segment`] can get truncated to (these
    /// IDs should refer to 'end' `Segment`s).
    pub truncations: Vec<SegmentId>,
    /// The position of this `Segment` within the composition
    pub position: Position,
}

impl Segment {
    /// If this segment can only exist in one possible course, then by extension it must correspond
    /// to exactly one node in the search graph.  This returns the [`NodeId`] of that node.
    pub(crate) fn as_node_id(&self, seg_id: SegmentId) -> Option<NodeId> {
        if self.course_head_masks.len() != 1 {
            return None;
        }
        let course_head = self.course_head_masks[0].as_row()?;
        Some(NodeId::new(seg_id, course_head))
    }
}

/// The different positions a node can be in the composition
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Position {
    Start,
    End,
    /// The node doesn't contain rounds, so can appear anywhere in the centre of the composition
    Central,
}

/// A structure representing the link between two course segments.  These are usually calls, but
/// can also be plain lead-ends or method splices.
#[derive(Debug, Clone)]
pub struct SegmentLink {
    pub display_name: String,
    pub debug_name: String,
    pub end_segment: SegmentId,
    pub transposition: RowBuf,
}
