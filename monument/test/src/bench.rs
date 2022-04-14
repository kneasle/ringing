//! Runner for benchmarks

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::Context;

use common::{CaseSource, CaseUse};

mod common;

/// Approximate benchmark times.  We use these whilst determining the benchmark order, to make sure
/// that fast benchmarks get run first.
const APPROX_BENCH_PATH: &str = "test/approx-bench-times.json";
const LAST_BENCH_PATH: &str = "test/.last-bench-times.json";
const PINNED_BENCH_PATH: &str = "test/.pinned-bench-times.json";

/// Collect and run all the benchmarks
fn main() -> anyhow::Result<()> {
    monument_cli::init_logging(log::LevelFilter::Info);

    let cases = sources_to_cases(common::collect_sources(common::RunType::Bench)?)?;

    Ok(())
}

fn sources_to_cases(sources: Vec<(CaseSource, CaseUse)>) -> anyhow::Result<Vec<BenchCase>> {
    let mut approx_bench_times = load_bench_times(APPROX_BENCH_PATH)?;
    let mut last_bench_times = load_bench_times(LAST_BENCH_PATH)?;
    let mut pinned_bench_times = load_bench_times(PINNED_BENCH_PATH)?;

    /*
    let extract_duration = |file: &mut Option<BenchTimesFile>| -> Option<Duration> {
        file.as_mut()?.remove(name).map(Duration::from_secs_f32)
    };
    let benchmarks = BenchmarkDurations {
        approx: extract_duration(&mut approx_bench_times),
        pinned: extract_duration(&mut pinned_bench_times),
        last: extract_duration(&mut last_bench_times),
    };
    */

    todo!()
}

struct BenchCase {
    source: CaseSource,
    ignored: bool,

    /// A rough guide on how long each benchmark will take.  Monument will always run benchmarks by
    /// increasing order of this (and `None < Some(x)` for all `x` so any new cases will be run
    /// first).
    approx: Option<Duration>,
    /// A duration which was explicitly 'pinned' by the user, against which all future benchmark
    /// runs will be tested (until a new result set is pinned).  This is usually used to save the
    /// benchmark times before embarking on optimisation.
    pinned: Option<Duration>,
    /// The last benchmark result
    last: Option<Duration>,
    /// The time from this run
    actual: Option<Duration>,
}

//////////////////////
// BENCH TIME FILES //
//////////////////////

/// The contents of the `{,last-,pinned-}bench-times.json` files.  We use a [`BTreeMap`] so that
/// the test cases are always written in a consistent order (i.e.  alphabetical order by file
/// path), thus making the diffs easier to digest.
///
/// The times are stored in seconds.
type BenchTimesFile = BTreeMap<String, f32>;

fn load_bench_times(path: impl AsRef<Path>) -> anyhow::Result<Option<BenchTimesFile>> {
    let full_path = PathBuf::from(common::PATH_TO_MONUMENT_DIR).join(path);
    let json = match std::fs::read_to_string(&full_path) {
        Ok(json) => json,
        Err(_) => return Ok(None), // It's OK to not find benchmark files
    };
    let results_file: BenchTimesFile = serde_json::from_str(&json)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(Some(results_file))
}
