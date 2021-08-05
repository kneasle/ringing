use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    ops::Deref,
};

use bellframe::{Bell, Method, Row, RowBuf};

use crate::mask::Mask;

use super::{
    single_method::{CallSpec, SingleMethodError},
    Config,
};

/// A representation of the course layout of a composition, and how Monument understands
/// composition structure.  In this representation, a
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
    /// The [`Link`]s by which segments of composition can be connected.  These are usually calls,
    /// but can also be the _absence_ of a call - note here that Monument will not implicitly add
    /// 'plain' links; they have to be explicitly added (and potentially named).
    ///
    /// Given a starting [`RowIdx`] of a course segment, Monument will extend it until the first
    /// [`Link`] which contains a matching course head [`Mask`].
    pub links: Vec<Link>,
    /// The [`RowIdx`]s and course heads where the composition can be started
    pub starts: Vec<(RowBuf, RowIdx, String)>,
    /// The [`RowIdx`]s and course heads where the composition can be finished.  If the composition
    /// starts and finishes at the same [`Row`], then `starts` and `ends` are likely to be equal
    /// (because every possible starting point is also an end point).  The only exceptions to this
    /// are cases where e.g. snap finishes are allowed but snap starts are not.
    pub ends: Vec<(RowBuf, RowIdx, String)>,
}

impl Layout {
    /// Create a new `Layout` for a single [`Method`].
    pub fn single_method(
        method: &Method,
        calls: &[CallSpec],
        // The course head masks, along with which bell is 'calling bell' during that course.
        // Allowing different calling bells allows us to do things like keep using W,M,H during
        // courses of e.g. `1xxxxx0987`.
        input_course_head_masks: &[(Mask, Bell)],
        config: &Config,
        // Which sub-lead indices are considered valid starting or finishing points for the
        // composition.  If these are `None`, then any location is allowed
        allowed_start_indices: Option<&[usize]>,
        allowed_end_indices: Option<&[usize]>,
    ) -> Result<Self, SingleMethodError> {
        // Delegate to the `single_method` module
        super::single_method::single_method_layout(
            method,
            calls,
            input_course_head_masks,
            config,
            allowed_start_indices,
            allowed_end_indices,
        )
    }

    /// Returns the [`Segment`], starting at a given [`NodeId`].  If this [`Segment`] would never
    /// terminate (because no [`Link`]s can be applied to it), then `None` is returned.
    pub(crate) fn get_segment(&self, id: &NodeId) -> Option<Segment> {
        // println!("\n{:?}", id);

        let block_len = self.blocks[id.row_idx.block].len();
        let length_between = |from: usize, to: usize| (to + block_len - from) % block_len;

        // Figure out which links are going to finish this segment
        let mut outgoing_links = Vec::<(usize, NodeId)>::new();
        let mut shortest_length: Option<usize> = None;

        for (link_idx, link) in self.links.iter().enumerate() {
            // If this link doesn't come from the correct block, then it can't finish the segment
            if link.from.block != id.row_idx.block {
                continue;
            }
            // If this link's course head mask doesn't match the current course, then it can't be
            // called
            if !link.course_head_mask.matches(&id.course_head) {
                continue;
            }

            let length = length_between(id.row_idx.row, link.from.row);
            // Add one to the length, because `link.from` refers to the lead **end** not the lead
            // **head**
            let length = length + 1;

            let cmp_to_shortest_len = match shortest_length {
                Some(best_len) => length.cmp(&best_len),
                // If no lengths have been found, then all lengths are strictly better than no
                // length
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
                outgoing_links.push((link_idx, link.id_after(&id.course_head)));

                /* println!(
                    "{:>3} --[{}]-> {:>3} = {} ({:?})",
                    id.row_idx.row_idx,
                    link.debug_name,
                    link.from.row_idx,
                    length,
                    cmp_to_shortest_len
                ); */
            }
        }

        let mut end_idx = None;
        // Determine whether or not this segment can end the composition before any calls
        for (idx, (end_ch, end_row_idx, _name)) in self.ends.iter().enumerate() {
            if end_ch == &id.course_head && end_row_idx.block == id.row_idx.block {
                let len = length_between(id.row_idx.row, end_row_idx.row);
                // Make a special case to disallow 0-length blocks which are both starts and ends.
                // This happens a lot because almost all compositions start and end at the same row
                // (i.e. rounds), and therefore the starts and ends will happen at the same
                // locations.  Therefore, each start node would generate a 0-length segment,
                // corresponding to a 0-length composition which immediately comes round.  This is
                // clearly not useful, hence the special case.
                if len == 0 && id.is_start {
                    continue;
                }

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
                    outgoing_links.clear();
                }
            }
        }

        // Decide which of `self.starts` this node corresponds to (if it is a start)
        let start_idx = if id.is_start {
            let idx = self
                .starts
                .iter()
                .position(|(course_head, row_idx, _name)| {
                    course_head == &id.course_head && *row_idx == id.row_idx
                });
            // Sanity check that nodes marked `is_start` actually do correspond to a start node
            assert!(
                idx.is_some(),
                "NodeId has `is_start`, but it doesn't come from `Layout::starts`"
            );
            idx
        } else {
            None
        };

        // De-duplicate the links (removing pairs of links which link to the same node).  We do
        // already perform some de-duplication when building the Layout, but this deduplication is
        // also necessary in case we end up with two links that have different course head masks
        // that both match the current course.  For example, if we have `pB` matching `1xxxxx7890`
        // (generated by `xB`) and `pB` matching `1234567xx0` (generated by potentially calling
        // BFI) then these would not be de-duplicated earlier but both match the plain course (CH
        // `1234567890`).
        let mut deduped_links = Vec::<(usize, NodeId)>::with_capacity(outgoing_links.len());
        for (link_idx, resulting_id) in outgoing_links {
            let link = &self.links[link_idx];
            // Only push links if they are different to all nodes pushed so far to `deduped_links`
            if deduped_links
                .iter()
                .all(|(idx2, _id)| !link.eq_without_name_or_ch_mask(&self.links[*idx2]))
            {
                deduped_links.push((link_idx, resulting_id));
            }
        }

        // If some way of ending this segment was found (i.e. a Link or an end-point), then build a
        // new Some(Segment), otherwise bubble the `None` value
        shortest_length.map(|length| Segment {
            links: deduped_links,
            length,
            node_id: id.clone(),
            start_idx,
            end_idx,
        })
    }

    /// Gets the [`RowIdx`] of the last row within a [`RowRange`] (or `None` if that range has size
    /// 0).
    pub(crate) fn last_row_idx(&self, row_range: RowRange) -> Option<RowIdx> {
        if row_range.length == 0 {
            None
        } else {
            let block_len = self.blocks[row_range.start.block].len();
            Some(RowIdx::new(
                row_range.start.block,
                // The subtraction here cannot overflow, because the outer `if` statement
                // guarantees that `row_range.length > 0`
                (row_range.start.row + row_range.length - 1) % block_len,
            ))
        }
    }

    /// Return the [`Row`]s covered by a given range
    pub(crate) fn untransposed_rows(
        &self,
        row_idx: RowIdx,
        length: usize,
    ) -> impl Iterator<Item = &'_ Row> {
        let block = &self.blocks[row_idx.block];
        block
            .iter()
            .cycle()
            .skip(row_idx.row)
            .take(length)
            .map(Deref::deref)
    }
}

/// A link between two segments of a course
#[derive(Debug, Clone)]
pub struct Link {
    /// Which [`Row`] in the [`Layout`] this `Link` starts from.  This is a half-open bound - for
    /// example, if this `Link` represents a call over the lead end then this index refers to the
    /// lead **head**, not the lead **end**.
    pub from: RowIdx,
    /// Which [`Row`] the composition will be at after this `Link` is taken
    pub to: RowIdx,

    /// A [`Mask`] which determines which course heads this `Link` can be applied to
    pub course_head_mask: Mask,
    /// The transposition of the course head taken when this is applied
    pub course_head_transposition: RowBuf,

    /// The name of this `Link`, used in debugging
    pub debug_name: String,
    /// The name of this `Link` used when generating human-friendly call strings
    pub display_name: String,
}

impl Link {
    /// Gets the [`NodeId`] of the node that would appear after this [`Link`] is applied to a given
    /// course.
    fn id_after(&self, course_head: &Row) -> NodeId {
        // Sanity check that this link could actually be applied in this location.
        assert!(self.course_head_mask.matches(course_head));
        NodeId::new(
            course_head * self.course_head_transposition.as_row(),
            self.to,
            // Nodes reached by taking a link can't be start nodes
            false,
        )
    }

    /// Returns `true` if `self` and `other` are equal (but ignoring the name and CH masks)
    pub(crate) fn eq_without_name_or_ch_mask(&self, other: &Self) -> bool {
        self.from == other.from
            && self.to == other.to
            && self.course_head_transposition == other.course_head_transposition
    }
}

/// The unique index of a [`Row`] within a [`Layout`].  This is essentially a `(block_idx,
/// row_idx)` pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RowIdx {
    pub block: usize,
    pub row: usize,
}

impl RowIdx {
    pub fn new(block_idx: usize, row_idx: usize) -> Self {
        Self {
            block: block_idx,
            row: row_idx,
        }
    }
}

/// The unique identifier for a single node (i.e. an instantiated course segment) in the
/// composition.  This node is assumed to end at the closest [`Link`] where the course head matches
/// one of the supplied [course head masks](Link::course_head_masks).
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct NodeId {
    pub course_head: RowBuf,
    pub row_idx: RowIdx,
    /// Start nodes have to be treated separately in the case where the rounds can appear as the
    /// first [`Row`] of a segment.  In this case, the start segment is full-length whereas any
    /// non-start segments become 0-length end segments (because the composition comes round
    /// instantly).
    pub is_start: bool,
}

impl NodeId {
    pub fn new(course_head: RowBuf, row_idx: RowIdx, is_start: bool) -> Self {
        Self {
            course_head,
            row_idx,
            is_start,
        }
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeId({})", self)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{}:{}{}",
            self.course_head,
            self.row_idx.block,
            self.row_idx.row,
            if self.is_start { ",is_start" } else { "" }
        )
    }
}

/// A range of [`Row`]s covered by a [`Segment`]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub(crate) struct RowRange {
    pub(crate) start: RowIdx,
    pub(crate) length: usize,
}

impl RowRange {
    pub(crate) fn new(start: RowIdx, length: usize) -> Self {
        Self { start, length }
    }
}

/// A section of a composition with no internal links, uniquely determined by a [`NodeId`].
#[derive(Debug, Clone)]
pub(crate) struct Segment {
    pub(crate) node_id: NodeId,
    pub(crate) length: usize,
    pub(crate) links: Vec<(usize, NodeId)>,

    pub(crate) start_idx: Option<usize>,
    pub(crate) end_idx: Option<usize>,
}

impl Segment {
    /// Returns the [`Row`]s covered this [`Segment`] **of the plain course**
    pub fn untransposed_rows<'l>(&self, layout: &'l Layout) -> impl Iterator<Item = &'l Row> {
        layout.untransposed_rows(self.node_id.row_idx, self.length)
    }
}
