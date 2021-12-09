use std::{cmp::Ordering, collections::HashMap};

use bellframe::{Row, RowBuf};
use itertools::Itertools;

use crate::{
    layout::{EndIdx, Layout, Link, LinkIdx, NodeId, Rotation, StandardNodeId, StartIdx},
    utils::RowCounts,
};

/// The length of a `Node` **in one part**.  This and [`TotalLength`] allow the compiler to catch
/// people mistaking the different length semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PerPartLength(pub usize);

/// The combined length of a `Node` **in all parts**.  This and [`PerPartLength`] allow the
/// compiler to catch people mistaking the different length semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TotalLength(pub usize);

/// A section of a composition with no internal links, uniquely determined by a [`NodeId`] (which
/// specifies the first row of the [`Node`]).
// TODO: Remove this and make `RangeFactory` take a constructor function instead
#[derive(Debug, Clone)]
pub struct NodeRange {
    pub node_id: NodeId,
    /// The combined length of this node in all parts
    pub per_part_length: PerPartLength,
    /// The combined length of this node in all parts
    pub total_length: TotalLength,

    /// The [`String`] which should be printed when this node is expanded.  This is usually a
    /// sequence of method shorthands (e.g. "YYY") when ringing spliced.
    pub label: String,
    pub method_counts: RowCounts,

    /// How this node ends.  It either ends with a set of successor links, or it's an [`End`] and
    /// has no successors.
    pub range_end: RangeEnd,
}

impl NodeRange {
    pub fn zero_len_end(num_methods: usize) -> Self {
        Self {
            node_id: NodeId::ZeroLengthEnd,
            per_part_length: PerPartLength(0),
            total_length: TotalLength(0),
            method_counts: RowCounts::zero(num_methods),
            label: String::new(),
            range_end: RangeEnd::End(End::ZeroLength),
        }
    }

    /// Returns the [`Row`]s covered by this [`NodeRange`] **of the plain course**
    pub fn untransposed_rows<'l>(
        &self,
        layout: &'l Layout,
    ) -> Box<dyn Iterator<Item = &'l Row> + 'l> {
        match self.node_id {
            NodeId::Standard(StandardNodeId { row_idx, .. }) => {
                Box::new(layout.untransposed_rows(row_idx, self.per_part_length))
            }
            NodeId::ZeroLengthEnd => Box::new(std::iter::empty()),
        }
    }

    pub fn end(&self) -> Option<End> {
        match &self.range_end {
            RangeEnd::End(e) => Some(*e),
            _ => None,
        }
    }

    pub fn links(&self) -> &[(LinkIdx, NodeId, Rotation)] {
        match &self.range_end {
            RangeEnd::End(_) => &[],
            RangeEnd::NotEnd(links) => links,
        }
    }
}

/// The ways that a [`NodeRange`] could end.  Nodes either [`End`] the composition with no
/// successors, or the node leads to a set of successors without ending the composition.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum RangeEnd {
    NotEnd(Vec<(LinkIdx, NodeId, Rotation)>),
    End(End),
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

////////////////////////
// NODE RANGE FACTORY //
////////////////////////

/// A struct to generate [`NodeRange`]s, given a [`Layout`] and a set of part heads.
#[derive(Debug)]
pub struct RangeFactory<'a> {
    layout: &'a Layout,
    part_heads: Vec<RowBuf>,
    /// For every CH found so far, this maps it to an 'equivalent' CH (i.e. a CH that's generated
    /// in a different part) along with the index of the part that would be generated (i.e. the
    /// [`Rotation`]).  For example, if the part head is `13425678` (generating 3 parts:
    /// `12345678`, `13425678`, `14235678`) then we would expect to see something like:
    /// ```text
    /// course_head_equiv = {
    ///     ...
    ///     Row(15623478): (Row(15623478), 0),
    ///     Row(15634278): (Row(15623478), 1),
    ///     Row(15642378): (Row(15623478), 2),
    ///     ...
    /// }
    /// ```
    /// Note how the three CHs all map to the same CH (i.e. they'll be combined into exactly one
    /// node in the graph), but the [`Rotation`]s keep track of information about which part is
    /// being rung.
    ch_equiv_map: HashMap<RowBuf, (RowBuf, Rotation)>,
}

impl<'a> RangeFactory<'a> {
    pub fn new(layout: &'a Layout, part_head: &Row) -> Self {
        let part_heads = part_head.closure_from_rounds();

        // For multi-parts to work, we only allow starts if they have corresponding ends (and vice
        // versa).  TODO

        let mut ch_equiv_map = HashMap::new();
        // In order to make node expansion easier, we make sure that the ends are given rotations
        // of 0.  This way, the [`NodeId`]s of nodes which reach part heads will always be made
        // equivalent to their counterpart which contains rounds (thus obviating the need to check
        // for reaching all part heads).
        for e in &layout.ends {
            equiv_ch(&e.course_head, &mut ch_equiv_map, &part_heads);
        }
        Self {
            layout,
            part_heads,
            ch_equiv_map,
        }
    }

    /// Consumes the factory and returns the sequence of part heads used in this composition.
    pub fn finish(self) -> (HashMap<RowBuf, (RowBuf, Rotation)>, Vec<RowBuf>) {
        (self.ch_equiv_map, self.part_heads)
    }

    pub fn start_ids(&mut self) -> Vec<(NodeId, StartIdx, Rotation)> {
        self.layout
            .starts
            .iter()
            .enumerate()
            .map(|(idx, start)| {
                let source_id = StandardNodeId::new(start.course_head.clone(), start.row_idx, true);
                let (equiv_id, rotation) = self.equiv_id(source_id);
                (NodeId::Standard(equiv_id), StartIdx::new(idx), rotation)
            })
            .collect_vec()
    }

    /// Returns the [`NodeRange`] which starts at a given [`NodeId`].  If this [`NodeRange`] would
    /// never terminate (because no [`Link`]s or ends can be applied to it), then `None` is
    /// returned.
    pub fn gen_range(&mut self, id: &NodeId) -> Option<NodeRange> {
        match id {
            NodeId::Standard(std_id) => self.gen_range_standard(std_id),
            NodeId::ZeroLengthEnd => Some(NodeRange::zero_len_end(self.layout.num_methods())),
        }
    }

    /// Returns the [`NodeRange`], starting at a given [`StandardNodeId`].  If this [`NodeRange`] would
    /// never terminate (because no [`Link`]s can be applied to it), then `None` is returned.
    fn gen_range_standard(&mut self, id: &StandardNodeId) -> Option<NodeRange> {
        // Sanity check that the only IDs to be generated should have rotation 0
        match self.ch_equiv_map.get(id.course_head.as_ref()) {
            Some((_, 0)) => {}
            Some((equiv_ch, rotation)) => panic!(
                "Generating {:?} but it has non-zero rotation ({}) of equiv class {:?}",
                id, rotation, equiv_ch
            ),
            None => panic!(
                "Generating node {:?} without giving it an equivalence class",
                id
            ),
        }
        // Sanity check that if `id.is_start`, then this `NodeId` actually corresponds to a start
        if id.is_start {
            let start = self
                .layout
                .starts
                .iter_enumerated()
                .find(|(_idx, start)| start.ch_and_row_idx() == id.ch_and_row_idx());
            assert!(
                start.is_some(),
                "`NodeId` sets `is_start` but has no corresponding start"
            );
        }

        let block = &self.layout.blocks[id.row_idx.block];
        let block_len = block.len();
        let length_between = |from: usize, to: usize| (to + block_len - from) % block_len;

        // Figure out which links are going to finish this segment
        let mut outgoing_links = Vec::<(LinkIdx, NodeId, Rotation)>::new();
        let mut shortest_length: Option<usize> = None;

        for (link_idx, link) in self.layout.links.iter_enumerated() {
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
                let (id_after, link_rotation) = self.id_after(link, &id.course_head);
                outgoing_links.push((link_idx, id_after, link_rotation));
            }
        }

        let mut end_idx = None;
        // Determine whether or not this segment can end the composition before any links can be
        // taken
        for (idx, end) in self.layout.ends.iter_enumerated() {
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
        let mut deduped_links =
            Vec::<(LinkIdx, NodeId, Rotation)>::with_capacity(outgoing_links.len());
        for (link_idx, resulting_id, rotation) in outgoing_links {
            // Only keep links if there isn't already a link with the same ID & rotation
            if !deduped_links
                .iter()
                .any(|(_, id, rotation2)| id == &resulting_id && rotation == *rotation2)
            {
                deduped_links.push((link_idx, resulting_id, rotation));
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
        let per_part_length = shortest_length;
        let total_length = per_part_length * self.part_heads.len();
        Some(NodeRange {
            per_part_length: PerPartLength(per_part_length),
            total_length: TotalLength(total_length),
            method_counts: RowCounts::single_count(
                total_length,
                id.row_idx.block.index(),
                self.layout.num_methods(),
            ),
            label,
            node_id: NodeId::Standard(id.clone()),
            range_end: end_idx
                .map(End::Idx) // Zero-length nodes are handled when links are **created**
                .map_or_else(|| RangeEnd::NotEnd(deduped_links), RangeEnd::End),
        })
    }

    /// Gets the [`NodeId`] of the node that would appear after this [`Link`] is applied to a given
    /// course.
    fn id_after(&mut self, link: &Link, source_ch: &Row) -> (NodeId, Rotation) {
        assert!(link.ch_mask.matches(source_ch));
        let (equiv_ch, rotation) = {
            let new_ch = source_ch * link.ch_transposition.as_row();
            equiv_ch(&new_ch, &mut self.ch_equiv_map, &self.part_heads)
        };

        let node_id = if self.layout.idx_of_end(&equiv_ch, link.to).is_some() {
            // If this leads directly to an end node, then it creates a 0-length end
            NodeId::ZeroLengthEnd
        } else {
            NodeId::Standard(StandardNodeId {
                course_head: equiv_ch.to_arc(),
                row_idx: link.to,
                is_start: false, // Nodes reached by taking a link can't be start nodes
            })
        };
        (node_id, rotation)
    }

    fn equiv_id(&mut self, source_id: StandardNodeId) -> (StandardNodeId, Rotation) {
        let (equiv_ch, rotation) = equiv_ch(
            &source_id.course_head,
            &mut self.ch_equiv_map,
            &self.part_heads,
        );
        let equiv_id = StandardNodeId::new(equiv_ch, source_id.row_idx, source_id.is_start);
        (equiv_id, rotation)
    }
}

/// Maps a course head to its `(equivalence class, rotation)` pair
fn equiv_ch(
    ch: &Row,
    equiv_map: &mut HashMap<RowBuf, (RowBuf, Rotation)>,
    part_heads: &[RowBuf],
) -> (RowBuf, Rotation) {
    assert!(part_heads[0].is_rounds());
    // If the equivalence class of this node has already been generated, then we simply look up the
    // rotation in the table
    if let Some((equiv_ch, rotation)) = equiv_map.get(ch) {
        return (equiv_ch.to_owned(), *rotation);
    }
    // Otherwise, we have to create a new equivalence class for it.  We also need to add lookup
    // entries for each CH equivalent to this one
    for (rotation, ph) in part_heads.iter().enumerate() {
        let equiv_ch = ph * ch;
        equiv_map.insert(equiv_ch, (ch.to_owned(), rotation as Rotation));
    }
    // If the first part head is rounds, then `ph * ch = ch` when `rotation = 0`.  Therefore, this
    // CH will be given class 0.
    #[allow(clippy::unnecessary_cast)]
    (ch.to_owned(), 0 as Rotation)
}
