use std::ops::Range;

use proj_core::Row;

/// A newtyped integer which is used to refer to a specific composition segment
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
struct Segment(usize);

/// A static, immutable look-up table, used by IDA* to generate compositions
#[derive(Debug, Clone)]
struct Table {
    len_range: Range<usize>,
}

/// A struct to hold compiled data required for a composition to be generated
#[derive(Debug, Clone)]
pub struct Engine {
    table: Table,
}

impl Engine {
    /// Creates a new `Engine` according to a map of course sections
    pub fn new(spec: Spec) -> Self {
        let table = Table {
            len_range: spec.len_range,
        };

        Engine { table }
    }
}

/// A generic specification for a composition that [`Engine`] can generate.  This is set up so that
/// spliced compositions would also work (hence we aren't taking a single method).  This is
/// intended to be only way to created `Engine`s
#[derive(Debug, Clone)]
pub struct Spec {
    /* GENERAL */
    /// What [`Range`] of lengths can the resulting compositions have
    pub len_range: Range<usize>,
    /// Monument will generate this many best compositions
    pub num_comps: usize,

    /* COURSE LAYOUT */
    /// The rows contained in `(<rounds>, i)` will be in `segment_rows[i]`.  These are usually
    /// ranges of the plain course of a single method, but could contain the plain courses of
    /// multiple methods (in the case of spliced).  If a segment contains rounds, it will be
    /// assumed that it is a possible starting point for the composition.
    // TODO: This should probably be `proj_core::Block`, but we first have to relax the restriction
    // that blocks must begin at rounds
    pub segment_rows: Vec<Vec<Row>>,
    /// Calls which connect pairs of segments together (the decision not to make a call is
    /// represented here as a `plain` call)
    pub calls: Vec<CallSpec>,
    /* SCORING (todo) */
}

/// The specification of the effect of a call at a given location in the course.
#[derive(Debug, Clone)]
pub struct CallSpec {
    pub display_name: String,
    pub debug_name: String,
    pub start_segment: usize,
    pub end_segment: usize,
    pub transposition: Row,
}
