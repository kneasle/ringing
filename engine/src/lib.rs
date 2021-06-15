use std::ops::Range;

use bellframe::{Bell, Method, RowBuf};

use segment_table::SegmentTable;
use single_method::{single_method_layout, CallSpec, SingleMethodError};

mod fast_row;
mod segment_table;
pub mod single_method;

/// A newtyped integer which is used to refer to a specific composition segment
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
struct SegmentID(usize);

impl From<usize> for SegmentID {
    #[inline(always)]
    fn from(v: usize) -> Self {
        SegmentID(v)
    }
}

/// The static data required for composition generation.  This data will not be modified and will
/// be shared between threads (once multi-threading is implemented).
#[derive(Debug, Clone)]
pub struct Engine {
    config: Config,
    len_range: Range<usize>,
    segment_tables: Vec<SegmentTable>,
}

impl Engine {
    /// Creates an `Engine` from a course [`Layout`].
    pub fn from_layout(config: Config, len_range: Range<usize>, layout: Layout) -> Self {
        Self {
            len_range,
            config,
            segment_tables: SegmentTable::from_segments(&layout.segments, &layout.fixed_bells),
        }
    }

    /// Creates an `Engine` to generate compositions of a single method.
    pub fn single_method(
        config: Config,
        len_range: Range<usize>,
        method: &Method,
        plain_lead_positions: Option<Vec<String>>,
        calls: &[CallSpec],
        non_fixed_bells: &[Bell],
    ) -> Result<Self, SingleMethodError> {
        let layout = single_method_layout(method, plain_lead_positions, calls, non_fixed_bells);
        Ok(Self::from_layout(config, len_range, layout?))
    }
}

/// General configuration parameters for an [`Engine`].  These do not form parts of the composition
/// specification, instead changing how the [`Engine`] operates.
#[derive(Debug, Clone)]
pub struct Config {
    /// The number of compositions that the [`Engine`] will generate before terminating.  These
    /// `num_comps` compositions are guaranteed to be optimal.
    pub num_comps: usize,
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    pub num_threads: Option<usize>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_comps: 1,
            num_threads: None,
        }
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
    pub links: Vec<SegmentLink>,
}

/// A structure representing the link between two course segments.  These are usually calls, but
/// can also be plain lead-ends or possibly even method splices.
#[derive(Debug, Clone)]
pub struct SegmentLink {
    pub display_name: String,
    pub debug_name: String,
    pub end_segment: usize,
    pub transposition: RowBuf,
}
