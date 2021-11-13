use std::cmp::Ordering;

use bellframe::Row;
use monument_utils::RowCounts;

use crate::{EndIdx, Layout, LinkIdx, NodeId, StandardNodeId};

/// A section of a composition with no internal links, uniquely determined by a [`NodeId`] (which
/// specifies the first row of the [`Node`]).
#[derive(Debug, Clone)]
pub struct NodeRange {
    pub node_id: NodeId,
    pub length: usize,
    pub links: Vec<(LinkIdx, NodeId)>,

    /// The [`String`] which should be printed when this node is expanded.  This is usually a
    /// sequence of method shorthands (e.g. "YYY") when ringing spliced.
    pub label: String,
    pub method_counts: RowCounts,

    /// If this `NodeRange` is a end point, then this is `Some(idx)` where `idx` indexes into
    /// [`Layout::ends`].
    pub end: Option<End>,
}

impl NodeRange {
    /// Returns the [`NodeRange`], starting at a given [`StandardNodeId`].  If this [`NodeRange`] would
    /// never terminate (because no [`Link`]s can be applied to it), then `None` is returned.
    pub fn new(layout: &Layout, id: &NodeId) -> Option<Self> {
        match id {
            NodeId::Standard(std_id) => Self::new_standard(layout, std_id),
            NodeId::ZeroLengthEnd => Some(NodeRange::zero_len_end(layout.num_methods())),
        }
    }

    /// Returns the [`NodeRange`], starting at a given [`StandardNodeId`].  If this [`NodeRange`] would
    /// never terminate (because no [`Link`]s can be applied to it), then `None` is returned.
    fn new_standard(layout: &Layout, id: &StandardNodeId) -> Option<Self> {
        let block = &layout.blocks[id.row_idx.block];
        let block_len = block.len();
        let length_between = |from: usize, to: usize| (to + block_len - from) % block_len;

        // Figure out which links are going to finish this segment
        let mut outgoing_links = Vec::<(LinkIdx, NodeId)>::new();
        let mut shortest_length: Option<usize> = None;

        for (link_idx, link) in layout.links.iter_enumerated() {
            // If this link doesn't come from the correct block, then it can't finish the segment
            if link.from.block != id.row_idx.block {
                continue;
            }
            // If this link's course head mask doesn't match the current course, then it can't be
            // called
            if !link.ch_mask.matches(&id.course_head) {
                continue;
            }

            let length = length_between(id.row_idx.row, link.from.row);
            // Add one to the length, because `link.from` refers to the lead **end** not the lead
            // **head**
            let length = length + 1;

            let cmp_to_shortest_len = match shortest_length {
                Some(best_len) => length.cmp(&best_len),
                // If no links have been found then this is automatically the best
                None => Ordering::Less,
            };

            // If this length is strictly better than the existing ones, then all the accumulated
            // links are no longer the best.  Additionally, this segment can no longer be an end
            // node if this link is taken before the end is reached.
            if cmp_to_shortest_len == Ordering::Less {
                outgoing_links.clear();
                shortest_length = Some(length);
            }
            // If this node is at least as good as the current best, then add this link to the list
            if cmp_to_shortest_len != Ordering::Greater {
                outgoing_links.push((link_idx, layout.id_after(link, &id.course_head)));
            }
        }

        let mut end_idx = None;
        // Determine whether or not this segment can end the composition before any links can be
        // taken
        for (idx, end) in layout.ends.iter_enumerated() {
            // An end could occur if we're in the same course and block as the end point
            if end.course_head.as_row() == id.course_head.as_ref()
                && end.row_idx.block == id.row_idx.block
            {
                let mut len = length_between(id.row_idx.row, end.row_idx.row);

                // Make a special case to disallow 0-length blocks which are both starts and ends.
                // This happens a lot because almost all compositions start and end at the same row
                // (rounds), and therefore the starts and ends will happen at the same locations.
                // Thus, each start node would generate a 0-length segment corresponding to a
                // 0-length composition which immediately comes round.  This is clearly not useful,
                // so instead we require that the whole block is rung before reaching the end.
                if len == 0 && id.is_start {
                    len = block_len;
                }

                // `true` if this end is reached before any of the links out of this segment
                let is_improvement = match shortest_length {
                    // If this node ends at the same location that a link could be taken, then the
                    // links take precedence (hence the strict inequality)
                    Some(l) => len < l,
                    // Any length is an improvement over an infinite length
                    None => true,
                };
                if is_improvement {
                    end_idx = Some(idx);
                    shortest_length = Some(len);
                    outgoing_links.clear(); // Ends take precedence over links
                }
            }
        }

        // If `id.is_start`, then check that the `NodeId` actually corresponds to a start
        if id.is_start {
            let start = layout
                .starts
                .iter_enumerated()
                .find(|(_idx, start)| start.ch_and_row_idx() == id.ch_and_row_idx());
            assert!(
                start.is_some(),
                "`NodeId` sets `is_start` but has no corresponding start"
            );
        }

        // De-duplicate the links (removing pairs of links which link to the same node).  We do
        // already perform some de-duplication when building the Layout, but this de-duplication is
        // also necessary in case we end up with two links that have different course head masks
        // that both match the current course.
        //
        // Two examples where this is useful:
        // 1. If we have `pB` matching `1xxxxx7890` (generated by `xB`) and `pB` matching
        //    `1234567xx0` (generated by potentially calling BFI) then these would not be
        //    de-duplicated earlier but both match the plain course (CH `1234567890`).
        // 2. Handling 0-length end nodes in spliced.  If a splice is made just before rounds, then
        //    we'll end up with a `link` for each method that could be spliced into.  However, this
        //    is equivalent to saying 'splicing to Bristol and instantly coming round' is somehow
        //    different to 'splicing to Yorkshire and instantly coming round'.  By the definition
        //    of `NodeId`, 0-length ends are defined to be equal, so the de-duplication will
        //    consider all of these to be equivalent and keep only one.
        let mut deduped_links = Vec::<(LinkIdx, NodeId)>::with_capacity(outgoing_links.len());
        for (link_idx, resulting_id) in outgoing_links {
            if deduped_links.iter().all(|(_, id)| id != &resulting_id) {
                deduped_links.push((link_idx, resulting_id));
            }
        }

        let shortest_length = shortest_length?;

        // Generate the string for this node from the lead labels annotating the block
        let mut label = String::new();
        for l in block
            .annots()
            .cycle()
            .skip(id.row_idx.row)
            .take(shortest_length)
            .map(Option::as_ref)
            .flatten()
        {
            label.push_str(l);
        }

        // If some way of ending this segment was found (i.e. a Link or an end-point), then build a
        // new Some(NodeRange), otherwise bubble the `None` value
        Some(NodeRange {
            links: deduped_links,
            length: shortest_length,
            method_counts: RowCounts::single_count(
                shortest_length,
                id.row_idx.block.index(),
                layout.num_methods(),
            ),
            label,
            node_id: NodeId::Standard(id.clone()),
            end: end_idx.map(End::Idx), // Zero-length nodes are handled when links are **created**
        })
    }

    pub fn zero_len_end(num_methods: usize) -> Self {
        Self {
            node_id: NodeId::ZeroLengthEnd,
            length: 0,
            method_counts: RowCounts::zero(num_methods),
            links: Vec::new(),
            label: String::new(),
            end: Some(End::ZeroLength),
        }
    }

    /// Returns the [`Row`]s covered by this [`NodeRange`] **of the plain course**
    pub fn untransposed_rows<'l>(
        &self,
        layout: &'l Layout,
    ) -> Box<dyn Iterator<Item = &'l Row> + 'l> {
        match self.node_id.row_idx() {
            Some(row_idx) => Box::new(layout.untransposed_rows(row_idx, self.length)),
            None => Box::new(std::iter::empty()),
        }
    }
}

/// Description of how a composition ends
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum End {
    /// The composition ends immediately after a call or splice
    ZeroLength,
    /// The composition ends at a specific [`StartOrEnd`]
    Idx(EndIdx),
}

impl End {
    pub fn label<'l>(&self, layout: &'l Layout) -> &'l str {
        match self {
            End::ZeroLength => "",
            End::Idx(idx) => &layout.ends[*idx].label,
        }
    }
}
