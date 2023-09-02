mod common;

use std::{collections::BTreeMap, time::Duration};

use colored::{ColoredString, Colorize};

use common::PathFromMonument;
use ringing_utils::PrettyDuration;

const BASELINE_DURATIONS_PATH: &'static str = "test/baseline-benches.toml";
const PINNED_DURATIONS_PATH: &'static str = "test/.pinned-benches.toml";
const LAST_DURATIONS_PATH: &'static str = "test/.last-benches.toml";

fn main() -> anyhow::Result<()> {
    match std::env::args().nth(1).as_deref() {
        Some("pin") => pin(),
        _ => run(),
    }
}

fn run() -> anyhow::Result<()> {
    // Load previous results
    let baseline_durations =
        common::load_results::<'_, Duration>(BASELINE_DURATIONS_PATH, &mut String::new())?;
    let mut pinned_durations =
        common::load_results::<'_, Duration>(PINNED_DURATIONS_PATH, &mut String::new())?;
    let mut last_durations =
        common::load_results::<'_, Duration>(LAST_DURATIONS_PATH, &mut String::new())?;
    // Load benchmarks
    let mut unrun_cases =
        common::load_cases(&["bench/"], "test/ignore.toml", |path, _| CaseData {
            pinned_duration: pinned_durations.remove(path),
            last_duration: last_durations.remove(path),
        })?;
    // Sort results with `new cases < fast cases < slow cases`
    unrun_cases.sort_by_key(|case| baseline_durations.get(&case.path));
    // Run benchmarks
    let mut run_cases = Vec::new();
    for case in unrun_cases {
        run_cases.push(run_bench(case));
        // Save times after every benchmark finishes.  This way, if we ctrl-C the benchmark runner
        // it will still have last/pinned versions for the benches that we did actually run.
        common::write_results(
            &run_cases
                .iter()
                .map(|case| (case.path.clone(), case.duration))
                .collect::<BTreeMap<_, _>>(),
            LAST_DURATIONS_PATH,
        )?;
    }
    Ok(())
}

fn run_bench(unrun_case: common::UnrunTestCase<CaseData>) -> common::RunTestCase<CaseData> {
    println!();
    println!("Running {}", unrun_case.name().white().bold());

    let run_case = unrun_case.run(&["--only-update-line"], /* display_stderr = */ true);

    // Print summary
    println!();
    print!(
        "Completed in {}",
        PrettyDuration(run_case.duration).to_string().white().bold()
    );
    if let Some(pinned) = run_case.pinned_duration {
        print!(
            "{} than pinned",
            relative_time_string(run_case.duration, pinned)
        );
        if run_case.last_duration.is_some() {
            print!("; "); // Delimiter between outputs
        }
    }
    if let Some(last) = run_case.last_duration {
        print!(
            "{} than last",
            relative_time_string(run_case.duration, last)
        );
    }
    println!(")");

    run_case
}

/// Prints the ratio between two [`Duration`]s as a string like `###x faster` or `###x slower`.
fn relative_time_string(current: Duration, other: Duration) -> ColoredString {
    let ratio = current.as_secs_f64() / other.as_secs_f64();
    let (direction, bold_color, ratio) = if ratio < 1.0 {
        ("faster", colored::Color::Green, 1.0 / ratio)
    } else {
        ("slower", colored::Color::Red, ratio)
    };

    let string = format!("{ratio:.2?}x {direction:}");
    if ratio > 1.1 {
        string.color(bold_color).bold()
    } else {
        string.white()
    }
}

#[derive(Debug)]
struct CaseData {
    pinned_duration: Option<Duration>,
    last_duration: Option<Duration>,
}

/// 'pin' the last bench results as a baseline comparison
fn pin() -> anyhow::Result<()> {
    std::fs::copy(
        PathFromMonument::new(LAST_DURATIONS_PATH).relative_to_cargo_toml(),
        PathFromMonument::new(PINNED_DURATIONS_PATH).relative_to_cargo_toml(),
    )?;
    println!("Pinned last benchmarks.");
    Ok(())
}
