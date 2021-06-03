use std::ops::Range;

use proj_core::{Bell, Method, Row};
use single_method::{single_method_layout, CallSpec, SingleMethodError};

pub mod single_method;

/// A newtyped integer which is used to refer to a specific composition segment
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
struct SegmentID(usize);

/// A static, immutable look-up table, used by IDA* to generate compositions
#[derive(Debug, Clone)]
struct Table {
    len_range: Range<usize>,
}

/// A struct to hold compiled data required for a composition to be generated
#[derive(Debug, Clone)]
pub struct Engine {
    len_range: Range<usize>,
    num_comps: usize,
}

impl Engine {
    pub fn from_layout(len_range: Range<usize>, num_comps: usize, layout: Layout) -> Self {
        Self {
            len_range,
            num_comps,
        }
    }

    pub fn single_method(
        len_range: Range<usize>,
        num_comps: usize,
        method: &Method,
        plain_lead_positions: Option<Vec<String>>,
        calls: &[CallSpec],
        non_fixed_bells: &[Bell],
    ) -> Result<Self, SingleMethodError> {
        let layout = single_method_layout(method, plain_lead_positions, calls, non_fixed_bells);
        Ok(Self::from_layout(len_range, num_comps, layout?))
    }
}

#[derive(Debug, Clone)]
pub struct Layout {
    /// The rows contained in `(<rounds>, i)` will be in `segment_rows[i]`.  These are usually
    /// ranges of the plain course of a single method, but could contain the plain courses of
    /// multiple methods (in the case of spliced).  If a segment contains rounds, it will be
    /// assumed that it is a possible starting point for the composition.
    pub segments: Vec<Segment>,
}

#[derive(Debug, Clone)]
pub struct Segment {
    /// The [`Row`]s contained in this `Segment`
    // TODO: This should probably be `proj_core::Block`, but we first have to relax the restriction
    // that blocks must begin at rounds
    pub rows: Vec<Row>,
    /// The ways that this `Segment` can be lead to other `Segment`s (in possibly different
    /// courses).
    pub links: Vec<SegmentLink>,
}

/// A structure representing the link between two course segments.  These are usually calls, but
/// can also be plain lead-ends or possibly even method splices.
#[derive(Debug, Clone)]
pub struct SegmentLink {
    pub display_name: String,
    pub debug_name: String,
    pub end_segment: usize,
    pub transposition: Row,
}
