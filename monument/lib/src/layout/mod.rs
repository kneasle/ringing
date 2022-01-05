use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    sync::Arc,
};

use bellframe::{AnnotBlock, IncompatibleStages, Mask, Row, RowBuf, Stage};
use node_range::PerPartLength;

pub mod new;
pub mod node_range;

// Imports only used for doc comments
#[allow(unused_imports)]
use crate::graph::{Graph, Node};

/// A somewhat human-friendly representation of the course layout of a composition, meant to be
/// easy to generate.  A `Layout` consists of a set of blocks of rows, which are usually the plain
/// courses of the methods being rung.  Some of these rows can be annotated with a label, which
/// will cause that label to be inserted into the composition string whenever that row is used
/// (useful for adding method labels in spliced).  Branches in the composition (e.g. choices
/// between making/not making calls) are represented as [`Link`]s.
///
/// Every useful composition structure (that I can think of) can be represented like this, but it
/// is not efficient to use `Layout`s directly in the composing loop.  Therefore, Monument
/// compiles a `Layout` (along with extra info like desired composition length, music requirements,
/// etc.) into a [`Graph`] of [`Node`]s.  This graph is then optimised, then usually compiled
/// _again_ into an immutable copy which stores its nodes as a [`Vec`], rather than a
/// [`HashMap`](std::collections::HashMap).
#[derive(Debug, Clone)]
pub struct Layout {
    /// The blocks that make up the composition.  [`Node`]s correspond to ranges of these `blocks`
    /// (pre-)transposed by some course head.
    pub blocks: BlockVec<AnnotBlock<Option<String>>>,
    /// The [`Link`]s by which segments of composition can be connected.  These are usually calls,
    /// but can also be the _absence_ of a call - note here that Monument will not implicitly add
    /// 'plain' links; they have to be explicitly added (and potentially named).
    ///
    /// Given a starting [`RowIdx`] of a course segment, Monument will extend it until the first
    /// [`Link`] which contains a matching course head [`Mask`].
    pub links: LinkVec<Link>,
    /// The [`RowIdx`]s and course heads where the composition can be started
    pub starts: StartVec<StartOrEnd>,
    /// The [`RowIdx`]s and course heads where the composition can be finished.  If the composition
    /// starts and finishes at the same [`Row`], then `starts` and `ends` are likely to be equal
    /// (because every possible starting point is also an end point).  The only exceptions to this
    /// are cases where e.g. snap finishes are allowed but snap starts are not.
    pub ends: EndVec<StartOrEnd>,
    pub stage: Stage,
}

impl Layout {
    pub fn num_methods(&self) -> usize {
        // TODO: Track methods properly
        self.blocks.len()
    }

    pub fn is_spliced(&self) -> bool {
        self.num_methods() > 1
    }

    //////////////////////
    // GRAPH GENERATION //
    //////////////////////

    /// Gets the [`RowIdx`] of the last row within a [`RowRange`] (or `None` if that range has size
    /// 0).
    pub fn last_row_idx(&self, row_range: RowRange) -> Option<RowIdx> {
        (row_range.len.0 > 0).then(|| {
            let block_len = self.blocks[row_range.start.block].len();
            RowIdx::new(
                row_range.start.block,
                // The subtraction here cannot overflow, because this code only executes when
                // `row_range.length > 0`
                (row_range.start.row + row_range.len.0 - 1) % block_len,
            )
        })
    }

    /// Return the [`Row`]s covered by a given range
    pub fn untransposed_rows(
        &self,
        row_idx: RowIdx,
        length: PerPartLength,
    ) -> impl Iterator<Item = &'_ Row> {
        self.blocks[row_idx.block]
            .rows()
            .cycle()
            .skip(row_idx.row)
            .take(length.0)
    }

    /////////////
    // HELPERS //
    /////////////

    /// Returns the [`EndIdx`] of the end at a given position, if it exists.  Used for detecting
    /// 0-length end nodes.
    fn idx_of_end(&self, ch: &Row, row_idx: RowIdx) -> Option<EndIdx> {
        self.ends
            .iter_enumerated()
            .find(|(_idx, end)| end.row_idx == row_idx && &end.course_head == ch)
            .map(|(idx, _end)| idx)
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
    pub ch_mask: Mask,
    /// The transposition of the course head taken when this is applied
    pub ch_transposition: RowBuf,

    /// The name of this `Link`, used in debugging
    pub debug_name: String,
    /// The name of this `Link` used when generating human-friendly call strings
    pub display_name: String,

    /// The score given to each instance of this link
    pub weight: f32,
}

impl Link {
    pub fn is_call(&self) -> bool {
        !self.ch_transposition.is_rounds()
    }

    /// Returns `true` if `self` and `other` are equal (but ignoring the name and CH masks)
    fn eq_without_name_or_ch_mask(&self, other: &Self) -> bool {
        self.from == other.from
            && self.to == other.to
            && self.ch_transposition == other.ch_transposition
    }
}

/// The unique index of a [`Row`] within a [`Layout`].  This is essentially a `(block_idx,
/// row_idx)` pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowIdx {
    pub block: BlockIdx,
    pub row: usize,
}

impl RowIdx {
    pub fn new(block_idx: BlockIdx, row_idx: usize) -> Self {
        Self {
            block: block_idx,
            row: row_idx,
        }
    }
}

/// A point where the composition can start or stop.  This is usually the location of rounds within
/// the composition graph.
#[derive(Debug, Clone)]
pub struct StartOrEnd {
    pub course_head: RowBuf,
    pub row_idx: RowIdx,
    pub label: String,
}

impl StartOrEnd {
    fn ch_and_row_idx(&self) -> (&Row, RowIdx) {
        (&self.course_head, self.row_idx)
    }
}

//////////////////////
// INTERNAL STRUCTS //
//////////////////////

/// The unique identifier for a single node (i.e. an instantiated course segment) in the
/// composition.  This node is assumed to end at the closest [`Link`] where the course head matches
/// one of the supplied [course head masks](Link::ch_mask).
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NodeId {
    /// The ID of any `Node` which comes round instantly.  All such nodes are considered
    /// equivalent, regardless of what method is spliced to.  These are all given the empty string
    /// as a label.
    ZeroLengthEnd,
    Standard(StandardNodeId),
}

impl NodeId {
    pub fn new_standard(course_head: Arc<Row>, row_idx: RowIdx, is_start: bool) -> Self {
        Self::Standard(StandardNodeId {
            course_head,
            row_idx,
            is_start,
        })
    }

    pub fn is_standard(&self) -> bool {
        self.standard().is_some()
    }

    pub fn standard(&self) -> Option<&StandardNodeId> {
        match self {
            NodeId::Standard(s) => Some(s),
            NodeId::ZeroLengthEnd => None,
        }
    }

    pub fn std_id(&self) -> Option<&StandardNodeId> {
        match self {
            Self::ZeroLengthEnd => None,
            Self::Standard(std_id) => Some(std_id),
        }
    }

    pub fn into_std_id(self) -> Option<StandardNodeId> {
        match self {
            Self::ZeroLengthEnd => None,
            Self::Standard(std_id) => Some(std_id),
        }
    }

    pub fn row_idx(&self) -> Option<RowIdx> {
        self.std_id().map(|std_id| std_id.row_idx)
    }

    pub fn course_head_arc(&self) -> Option<&Arc<Row>> {
        self.std_id().map(|std_id| &std_id.course_head)
    }

    pub fn course_head(&self) -> Option<&Row> {
        self.course_head_arc().map(Arc::as_ref)
    }

    pub fn pre_multiply(&self, r: &Row) -> Result<Self, IncompatibleStages> {
        match self {
            Self::ZeroLengthEnd => Ok(Self::ZeroLengthEnd),
            Self::Standard(s) => s.pre_multiply(r).map(Self::Standard),
        }
    }

    pub fn is_start(&self) -> bool {
        self.std_id().map_or(false, |std_id| std_id.is_start)
    }

    pub fn set_start(&mut self, new_start: bool) {
        if let Self::Standard(StandardNodeId { is_start, .. }) = self {
            *is_start = new_start;
        }
    }
}

/// The ID of a node which isn't a 0-length end
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct StandardNodeId {
    pub course_head: Arc<Row>, // `Arc` is used to make cloning cheaper
    pub row_idx: RowIdx,
    // Start nodes have to be treated separately in the case where the rounds can appear as the
    // first [`Row`] of a segment.  In this case, the start segment is full-length whereas any
    // non-start segments become 0-length end segments (because the composition comes round
    // instantly).
    pub is_start: bool,
}

impl StandardNodeId {
    pub fn new(course_head: RowBuf, row_idx: RowIdx, is_start: bool) -> Self {
        Self {
            course_head: course_head.to_arc(),
            row_idx,
            is_start,
        }
    }

    pub fn ch_and_row_idx(&self) -> (&Row, RowIdx) {
        (&self.course_head, self.row_idx)
    }

    pub fn pre_multiply(&self, r: &Row) -> Result<Self, IncompatibleStages> {
        Ok(Self {
            course_head: r.mul_result(&self.course_head)?.to_arc(),
            row_idx: self.row_idx,
            is_start: self.is_start,
        })
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ZeroLengthEnd => write!(f, "NodeId::ZeroLengthEnd"),
            Self::Standard(std_id) => write!(f, "NodeId({})", std_id),
        }
    }
}

impl Debug for StandardNodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Std({})", self)
    }
}

impl Display for StandardNodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{:?}:{}",
            self.course_head, self.row_idx.block, self.row_idx.row,
        )?;
        if self.is_start {
            write!(f, ",is_start")?;
        }
        Ok(())
    }
}

impl From<StandardNodeId> for NodeId {
    fn from(std_id: StandardNodeId) -> NodeId {
        NodeId::Standard(std_id)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RowRange {
    pub start: RowIdx,
    pub len: PerPartLength,
}

impl RowRange {
    pub fn new(start: RowIdx, len: PerPartLength) -> Self {
        Self { start, len }
    }
}

/////////////////
// INDEX TYPES //
/////////////////

index_vec::define_index_type! { pub struct LinkIdx = usize; }
index_vec::define_index_type! { pub struct BlockIdx = usize; }
index_vec::define_index_type! { pub struct StartIdx = usize; }
index_vec::define_index_type! { pub struct EndIdx = usize; }

pub type LinkVec<T> = index_vec::IndexVec<LinkIdx, T>;
pub type BlockVec<T> = index_vec::IndexVec<BlockIdx, T>;
pub type StartVec<T> = index_vec::IndexVec<StartIdx, T>;
pub type EndVec<T> = index_vec::IndexVec<EndIdx, T>;
