//! Idiomatic Rust representations of commonly used primitives for Change Ringing compositions.

mod bell;
pub mod block;
pub mod call;
pub mod method;
mod method_lib;
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
pub use method::Method;
pub use parity::Parity;
pub use place_not::{PlaceNot, PnBlock};
pub use row::{InvalidRowError, Row, RowBuf};
pub use stage::{IncompatibleStages, Stage};
pub use utils::run_len;

#[cfg(feature = "cc_lib_gen")]
pub use method_lib::parse_cc_lib::parse_cc_lib;
