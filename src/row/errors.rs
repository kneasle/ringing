use std::{
    error::Error,
    fmt::{Display, Formatter},
};

use crate::{stage::ZeroStageError, Bell, IncompatibleStages, Stage};

/// All the possible ways that a [`Row`] could be invalid.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidRowError {
    /// The [`Row`] would contain no [`Bell`]s
    NoBells,
    /// A [`Bell`] would appear twice in the new [`Row`] (for example in `113456` or `4152357`)
    DuplicateBell(Bell),
    /// A [`Bell`] is not within the range of the [`Stage`] of the new [`Row`] (for example `7` in
    /// `12745` or `5` in `5432`).
    BellOutOfStage(Bell, Stage),
    /// A given Bell would be missing from the [`Row`].  Note that this is only generated if we
    /// already know the [`Stage`] of the new [`Row`], otherwise the other two variants are
    /// sufficient for every case.
    MissingBell(Bell),
}

impl From<ZeroStageError> for InvalidRowError {
    fn from(_: ZeroStageError) -> Self {
        InvalidRowError::NoBells
    }
}

impl Display for InvalidRowError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidRowError::DuplicateBell(bell) => {
                write!(f, "Bell '{}' appears twice.", bell)
            }
            InvalidRowError::BellOutOfStage(bell, stage) => {
                write!(f, "Bell '{}' is not within stage {}", bell, stage)
            }
            InvalidRowError::MissingBell(bell) => {
                write!(f, "Bell '{}' is missing", bell)
            }
            InvalidRowError::NoBells => write!(f, "Row would have no bells"),
        }
    }
}

impl Error for InvalidRowError {}

/// The possible ways that the [`Stage`]s can't match when using `mul_into`
#[derive(Debug, Copy, Clone)]
pub enum MulIntoError {
    RhsStage(IncompatibleStages),
    IntoStage(IncompatibleStages),
}

impl Display for MulIntoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RhsStage(IncompatibleStages {
                lhs_stage,
                rhs_stage,
            }) => write!(f, "Can't multiply stage {} with {}", lhs_stage, rhs_stage),
            Self::IntoStage(IncompatibleStages {
                lhs_stage,
                rhs_stage,
            }) => write!(f, "Can't write stage {} into {}", lhs_stage, rhs_stage),
        }
    }
}

impl Error for MulIntoError {}
