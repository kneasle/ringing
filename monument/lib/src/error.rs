//! Error types for the different ways that Monument can fail.

use std::{
    fmt::{Display, Formatter},
    ops::RangeInclusive,
};

use bellframe::{Mask, PlaceNot, RowBuf, Stage};

#[allow(unused_imports)] // Only used for doc comments
use crate::builder::{Call, Method, MusicType, SearchBuilder};
use crate::parameters::OptionalRangeInclusive;

/// Alias for `Result<T, monument::Error>`.
pub type Result<T> = std::result::Result<T, Error>;

/// The different ways that Monument can fail.
#[derive(Debug)]
pub enum Error {
    /* QUERY BUILD ERRORS */
    /// The given `title` couldn't be found in the Central Council library.  Each `suggestions` is
    /// paired with its 'edit distance' (i.e. a metric of how different it is from the requested
    /// `title`)
    MethodNotFound {
        title: String,
        suggestions: Vec<(String, usize)>,
    },
    /// Some method's place notation failed to parse
    MethodPnParse {
        name: String,
        place_notation_string: String,
        error: bellframe::place_not::PnBlockParseError,
    },
    /// Some course mask couldn't be parsed.
    CustomCourseMaskParse {
        method_title: String,
        mask_str: String,
        error: bellframe::mask::ParseError,
    },

    /* QUERY VERIFICATION ERRORS */
    /// Different start/end rows were specified in a multi-part
    DifferentStartEndRowInMultipart,
    /// Some [`Call`] refers to a label that doesn't exist
    UndefinedLabel { call_name: String, label: String },
    /// The [`SearchBuilder`] didn't define any [`Method`]s
    NoMethods,
    /// Two [`Method`]s use the same shorthand
    DuplicateShorthand {
        shorthand: String,
        title1: String,
        title2: String,
    },
    NoCourseHeadInPart {
        mask_in_first_part: Mask,
        part_head: RowBuf,
        mask_in_other_part: Mask,
    },
    /// Some [`Call`] doesn't have enough calling positions to cover the [`Stage`]
    WrongCallingPositionsLength {
        call_name: String,
        calling_position_len: usize,
        stage: Stage,
    },
    /// Two [`Call`]s have the same lead location and name
    DuplicateCall {
        symbol: String,
        label: String,
        pn1: PlaceNot,
        pn2: PlaceNot,
    },

    /* GRAPH BUILD ERRORS */
    /// The given maximum graph size limit was reached
    SizeLimit(usize),
    /// The same chunk of ringing could start at two different strokes, and some
    /// [`MusicType`] relies on the strokes always being the same
    InconsistentStroke,

    /* LENGTH PROVING ERRORS */
    /// The requested length range isn't achievable
    UnachievableLength {
        requested_range: RangeInclusive<usize>,
        next_shorter_len: Option<usize>,
        next_longer_len: Option<usize>,
    },
    /// Some method range isn't achievable
    UnachievableMethodCount {
        method_name: String,
        requested_range: OptionalRangeInclusive,
        next_shorter_len: Option<usize>,
        next_longer_len: Option<usize>,
    },
    /// The total of the minimum method counts is longer than the composition
    TooMuchMethodCount {
        min_total_method_count: usize,
        max_length: usize,
    },
    TooLittleMethodCount {
        max_total_method_count: usize,
        min_length: usize,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            /* QUERY BUILD ERRORS */
            Error::MethodNotFound { title, suggestions } => write!(
                f,
                "No method called {:?} in the Central Council library.  Do you mean {:?}?",
                title, suggestions[0]
            ),
            Error::MethodPnParse {
                name,
                place_notation_string: _,
                error,
            } => write!(f, "Error parsing place notation for {:?}: {}", name, error),
            Error::CustomCourseMaskParse {
                method_title,
                mask_str,
                error,
            } => write!(
                f,
                "Error parsing course mask {} for method {:?}: {}",
                mask_str, method_title, error
            ),

            /* QUERY VERIFICATION ERRORS */
            Error::DifferentStartEndRowInMultipart => {
                write!(f, "Start/end rows must be the same for multipart comps")
            }
            Error::NoMethods => write!(f, "Can't have a composition with no methods"),
            Error::WrongCallingPositionsLength {
                call_name,
                calling_position_len,
                stage,
            } => write!(
                f,
                "Call {:?} only specifies {} calling positions, but the stage has {} bells",
                call_name,
                calling_position_len,
                stage.num_bells()
            ),
            Error::DuplicateShorthand {
                shorthand,
                title1,
                title2,
            } => write!(
                f,
                "Methods {:?} and {:?} share a shorthand ({})",
                title1, title2, shorthand
            ),
            Error::UndefinedLabel { call_name, label } => write!(
                f,
                "Call {:?} refers to a label {:?}, which doesn't exist",
                call_name, label
            ), // TODO: Suggest one that does exist
            Error::NoCourseHeadInPart {
                mask_in_first_part,
                part_head,
                mask_in_other_part,
            } => {
                writeln!(
                    f,
                    "course head `{}` becomes `{}` in the part starting `{}`, which isn't in `course_heads`.",
                    mask_in_first_part, mask_in_other_part, part_head
                )?;
                write!(
                    f,
                    "   help: consider adding `{}` to `course_heads`",
                    mask_in_other_part
                )
            }
            Error::DuplicateCall {
                symbol,
                label,
                pn1,
                pn2,
            } => write!(
                f,
                "Call symbol {:?} (at {:?}) is used for both {} and {}",
                symbol, label, pn1, pn2
            ),

            /* GRAPH BUILD ERRORS */
            Error::SizeLimit(limit) => write!(
                f,
                "Graph size limit of {} chunks reached.  You can set it \
higher with `--graph-size-limit <n>`.",
                limit
            ),
            Error::InconsistentStroke => write!(
                f,
                "The same chunk of ringing can be at multiple strokes, probably \
because you're using a method with odd-length leads"
            ),

            /* LENGTH PROVING ERRORS */
            Error::UnachievableLength {
                requested_range,
                next_shorter_len,
                next_longer_len,
            } => {
                write!(f, "No compositions can fit the required length range (")?;
                write_range(
                    f,
                    "length",
                    Some(*requested_range.start()),
                    Some(*requested_range.end()),
                )?;
                write!(f, ").  ")?;
                // Describe the nearest composition length(s)
                match (next_shorter_len, next_longer_len) {
                    (Some(l1), Some(l2)) => write!(f, "The nearest lengths are {l1} and {l2}."),
                    (Some(l), None) | (None, Some(l)) => write!(f, "The nearest length is {l}."),
                    // TODO: Give this its own error?
                    (None, None) => write!(f, "No compositions are possible."),
                }
            }
            Error::UnachievableMethodCount {
                method_name,
                requested_range,
                next_shorter_len,
                next_longer_len,
            } => {
                assert_ne!((requested_range.min, requested_range.max), (None, None));
                write!(
                    f,
                    "No method counts for {:?} satisfy the requested range (",
                    method_name,
                )?;
                write_range(f, "count", requested_range.min, requested_range.max)?;
                write!(f, ").  ")?;
                // Describe the nearest method counts
                match (next_shorter_len, next_longer_len) {
                    (Some(l1), Some(l2)) => write!(f, "The nearest counts are {l1} and {l2}."),
                    (Some(l), None) | (None, Some(l)) => write!(f, "The nearest count is {l}."),
                    (None, None) => unreachable!(), // Method count of 0 is always possible
                }
            }
            Error::TooMuchMethodCount {
                min_total_method_count,
                max_length,
            } => {
                write!(f, "Too much method counts; the method counts need at least")?;
                write!(
                    f,
                    " {min_total_method_count} rows, but at most {max_length} rows are available."
                )
            }
            Error::TooLittleMethodCount {
                max_total_method_count,
                min_length,
            } => {
                write!(
                    f,
                    "Not enough method counts; the composition needs at least {min_length} rows"
                )?;
                write!(
                    f,
                    " but the methods can make at most {max_total_method_count}."
                )
            }
        }
    }
}

/// Prettily format a (possibly open) inclusive range as an inequality (e.g. `300 <= count <= 500`)
fn write_range<T: Ord + Display>(
    f: &mut impl std::fmt::Write,
    name: &str,
    min: Option<T>,
    max: Option<T>,
) -> std::fmt::Result {
    match (min, max) {
        // Write e.g. `224 <= count <= 224` as `count == 224`
        (Some(min), Some(max)) if min == max => write!(f, "{name} == {min}")?,
        // Otherwise write everything as an inequality
        (min, max) => {
            if let Some(min) = min {
                write!(f, "{min} <= ")?;
            }
            write!(f, "{name}")?;
            if let Some(max) = max {
                write!(f, " <= {max}")?;
            }
        }
    }
    Ok(())
}

impl std::error::Error for Error {}
