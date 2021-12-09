//! Idiomatic Rust representations of commonly used primitives for Change Ringing compositions.

#![deny(clippy::all)]

mod bell;
pub mod block;
pub mod call;
pub mod mask;
pub mod method;
pub mod method_lib;
pub mod music;
mod parity;
pub mod place_not;
pub mod row;
mod stage;
mod utils;

// Re-export useful data types into the top level of the crate
pub use bell::Bell;
pub use block::{AnnotBlock, AnnotRow, Block};
pub use call::Call;
pub use mask::Mask;
pub use method::Method;
pub use method_lib::MethodLib;
pub use parity::Parity;
pub use place_not::{PlaceNot, PnBlock};
pub use row::{same_stage_vec::SameStageVec, InvalidRowError, Row, RowBuf};
pub use stage::{IncompatibleStages, Stage};
pub use utils::run_len;

#[cfg(feature = "cc_lib_gen")]
pub use method_lib::parse_cc_lib::parse_cc_lib;
