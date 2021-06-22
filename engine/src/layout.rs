use bellframe::{Bell, RowBuf};

/// A newtyped integer which is used to refer to a specific composition segment
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct SegmentID {
    pub(crate) v: usize,
}

impl From<usize> for SegmentID {
    #[inline(always)]
    fn from(v: usize) -> Self {
        SegmentID { v }
    }
}

impl From<SegmentID> for usize {
    #[inline(always)]
    fn from(seg_id: SegmentID) -> usize {
        seg_id.v
    }
}

/// A mid-level representation of the course layout of a composition.  In this representation, a
/// layout is a set of [`Segment`]s, which are sequences of [`Row`]s combined with links to the
/// [`Segment`]s which can come after them.  Every useful composition structure can be represented
/// like this, but it is not efficient to use [`Layout`]s directly in the composing loop.
/// Therefore, [`Engine`] compiles this down into a compact representation which can be efficiently
/// queried.
#[derive(Debug, Clone)]
pub struct Layout {
    /// The rows contained in `(<rounds>, i)` will be in `segment_rows[i]`.  These are usually
    /// ranges of the plain course of a single method, but could contain the plain courses of
    /// multiple methods (in the case of spliced).  If a segment contains rounds, it will be
    /// assumed that it is a possible starting point for the composition.
    pub segments: Vec<Segment>,
    /// The bells which must be fixed
    pub fixed_bells: Vec<Bell>,
}

#[derive(Debug, Clone)]
pub struct Segment {
    /// The [`Row`]s contained in this `Segment`
    // TODO: This should probably be `proj_core::Block`, but we first have to relax the restriction
    // that blocks must begin at rounds
    pub rows: Vec<RowBuf>,
    /// The ways that this `Segment` can be lead to other `Segment`s (in possibly different
    /// courses).
    pub links: Vec<SegmentLink<RowBuf>>,
}

/// A structure representing the link between two course segments.  These are usually calls, but
/// can also be plain lead-ends or possibly even method splices.
#[derive(Debug, Clone)]
pub struct SegmentLink<R> {
    pub display_name: String,
    pub debug_name: String,
    pub end_segment: SegmentID,
    pub transposition: R,
}

impl<R> SegmentLink<R> {
    pub fn clone_from<'a, R1>(other: &'a SegmentLink<R1>) -> Self
    where
        R: From<&'a R1>,
    {
        Self {
            display_name: other.display_name.clone(),
            debug_name: other.debug_name.clone(),
            end_segment: other.end_segment,
            transposition: R::from(&other.transposition),
        }
    }
}
