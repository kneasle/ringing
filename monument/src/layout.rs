use bellframe::RowBuf;

use crate::mask::Mask;

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
    pub starts: Vec<NodeId>,
    /// The [`RowIdx`]s and course heads where the composition can be finished.  If the composition
    /// starts and finishes at the same [`Row`], then `starts` and `ends` are likely to be equal
    /// (because every possible starting point is also an endpoint).  The only exception to this is
    /// cases where e.g. snap finishes are allowed but snap starts are not.
    pub ends: Vec<NodeId>,
}

/// A link between two courses
#[derive(Debug, Clone)]
pub struct Link {
    /// Which [`Row`] in the [`Layout`] this `Link` starts from.  This is a half-open bound - for
    /// example, if this `Link` represents a call over the lead end then this index refers to the
    /// lead **head**, not the lead **end**.
    pub row_idx_from: RowIdx,
    /// Which [`Row`] the composition will be at after this `Link` is taken
    pub row_idx_to: RowIdx,

    /// A [`Mask`] which determines which course heads this `Link` can be applied to
    pub course_head_mask: Mask,
    /// The transposition of the course head taken when this is applied
    pub course_head_transposition: RowBuf,

    /// The name of this `Link`, used in debugging
    pub debug_name: String,
    /// The name of this `Link` used when generating human-friendly call strings
    pub display_name: String,
}

/// The unique index of a [`Row`] within a [`Layout`].  This is essentially a `(block_idx,
/// row_idx)` pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RowIdx {
    pub block_idx: usize,
    pub row_idx: usize,
}

impl RowIdx {
    pub fn new(block_idx: usize, row_idx: usize) -> Self {
        Self { block_idx, row_idx }
    }
}

/// The unique identifier for a single node (i.e. an instantiated course segment) in the
/// composition.  This node is assumed to end at the closest [`Link`] where the course head matches
/// one of the supplied [course head masks](Link::course_head_masks).
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NodeId {
    pub course_head: RowBuf,
    pub row_idx: RowIdx,
}
