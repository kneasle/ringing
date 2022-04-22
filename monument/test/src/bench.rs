//! Runner for benchmarks
use std::{
    collections::BTreeMap,
    ops::Deref,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::Context;

use colored::{ColoredString, Colorize};
use common::{CaseSource, CaseUse, PATH_TO_MONUMENT_DIR};
use itertools::Itertools;

mod common;

/// Approximate benchmark times.  We use these whilst determining the benchmark order, to make sure
/// that fast benchmarks get run first.
const APPROX_BENCH_PATH: &str = "test/approx-bench-times.json";
const LAST_BENCH_PATH: &str = "test/.last-bench-times.json";
const PINNED_BENCH_PATH: &str = "test/.pinned-bench-times.json";

/// Collect and run all the benchmarks
fn main() -> anyhow::Result<()> {
    monument_cli::init_logging(log::LevelFilter::Info);

    let mut unrun_cases = sources_to_cases(common::collect_sources(common::RunType::Bench)?)?;
    unrun_cases.sort_by_key(|unrun| unrun.approx_duration);
    let run_cases = unrun_cases.into_iter().map(run_case).collect_vec();
    save_bench_times(
        LAST_BENCH_PATH,
        run_cases
            .iter()
            .map(|run_case| (run_case.source.name(), run_case.actual_duration)),
    )?;

    Ok(())
}

fn sources_to_cases(sources: Vec<(CaseSource, CaseUse)>) -> anyhow::Result<Vec<UnrunBenchCase>> {
    // TODO: Allow benchmarks to be ignored

    let mut approx_bench_times = load_bench_times(APPROX_BENCH_PATH)?;
    let mut last_bench_times = load_bench_times(LAST_BENCH_PATH)?;
    let mut pinned_bench_times = load_bench_times(PINNED_BENCH_PATH)?;

    let mut cases = Vec::new();
    for (source, _case_use) in sources {
        let name = &source.name();

        let extract_duration = |file: &mut Option<BenchTimesFile>| -> Option<Duration> {
            file.as_mut()?.remove(name).map(Duration::from_secs_f32)
        };
        cases.push(UnrunBenchCase {
            source,
            ignored: false,
            approx_duration: extract_duration(&mut approx_bench_times),
            pinned_duration: extract_duration(&mut pinned_bench_times),
            last_duration: extract_duration(&mut last_bench_times),
        });
    }
    Ok(cases)
}

fn run_case(unrun: UnrunBenchCase) -> RunBenchCase {
    println!("\n\n\nBenching {}:", unrun.source.name().white().bold());

    // Run the test and extract the duration
    let config = monument::Config {
        queue_limit: common::QUEUE_LIMIT,
        ..Default::default()
    };
    let query_result = monument_cli::run(
        unrun.source.to_monument_source(),
        None,
        &config,
        monument_cli::CtrlCBehaviour::TerminateProcess,
        false, // Don't print compositions
    );
    let actual_duration = match query_result {
        Ok(Some(result)) => result.duration,
        Ok(None) => unreachable!("Benchmarks don't run with `--no-search`"),
        Err(e) => panic!("Unexpected error:\n{}", e),
    };

    let coloured_time = |prev: Duration| -> ColoredString {
        let (bigger, smaller, is_faster) = if actual_duration > prev {
            (actual_duration, prev, true)
        } else {
            (prev, actual_duration, false)
        };
        let percent_difference = (bigger.as_secs_f32() / smaller.as_secs_f32() - 1.0) * 100.0;
        let percent_string = format!(
            "{:.2}% {}",
            percent_difference,
            if is_faster { "faster" } else { "slower" }
        );

        if percent_difference > 5.0 {
            match is_faster {
                true => percent_string.bright_green().bold(), // Got faster
                false => percent_string.bright_red().bold(),  // Got slower
            }
        } else {
            percent_string.normal()
        }
    };

    // Print the new result to the user
    println!();
    print!("Completed in {:?} ", actual_duration);
    match (unrun.last_duration, unrun.pinned_duration) {
        (None, None) => {}
        (Some(last), None) => println!("({} than last)", coloured_time(last)),
        (Some(last), Some(pinned)) => println!(
            "({} than last, {} than pinned)",
            coloured_time(last),
            coloured_time(pinned)
        ),
        (None, Some(pinned)) => println!("({} than pinned)", coloured_time(pinned)),
    }
    println!();

    RunBenchCase {
        unrun,
        actual_duration,
    }
}

/// A test case which hasn't been run yet
#[derive(Debug)]
struct UnrunBenchCase {
    source: CaseSource,
    ignored: bool,

    /// A rough guide on how long each benchmark will take.  Monument will always run benchmarks by
    /// increasing order of this (and `None < Some(x)` for all `x` so any new cases will be run
    /// first).
    approx_duration: Option<Duration>,
    /// A duration which was explicitly 'pinned' by the user, against which all future benchmark
    /// runs will be tested (until a new result set is pinned).  This is usually used to save the
    /// benchmark times before embarking on optimisation.
    pinned_duration: Option<Duration>,
    /// The last benchmark result
    last_duration: Option<Duration>,
}

/// A test case which has been run
#[derive(Debug)]
struct RunBenchCase {
    unrun: UnrunBenchCase,
    actual_duration: Duration,
}

impl Deref for RunBenchCase {
    type Target = UnrunBenchCase;

    fn deref(&self) -> &UnrunBenchCase {
        &self.unrun
    }
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
    let full_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
    let json = match std::fs::read_to_string(&full_path) {
        Ok(json) => json,
        Err(_) => return Ok(None), // It's OK to not find benchmark files
    };
    let results_file: BenchTimesFile = serde_json::from_str(&json)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(Some(results_file))
}

fn save_bench_times(
    path: &str,
    contents: impl IntoIterator<Item = (String, Duration)>,
) -> anyhow::Result<()> {
    let times: BenchTimesFile = contents
        .into_iter()
        .map(|(name, duration)| (name, duration.as_secs_f32()))
        .collect();
    common::write_json(&times, path)
}
