//! Runner for benchmarks

mod common;

/// Collect and run all the benchmarks
fn main() -> anyhow::Result<()> {
    match common::run(common::RunType::Bench)? {
        common::Outcome::Fail => Err(anyhow::Error::msg("Benchmarks failed")),
        common::Outcome::Pass => Ok(()),
    }
}
