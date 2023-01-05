mod common;

use std::time::Duration;

use colored::{ColoredString, Colorize};
use itertools::Itertools;

use common::PathFromMonument;

const PINNED_DURATIONS_PATH: &'static str = "test/.pinned-benches.toml";
const LAST_DURATIONS_PATH: &'static str = "test/.last-benches.toml";

fn main() -> anyhow::Result<()> {
    match std::env::args().nth(1).as_deref() {
        Some("pin") => pin(),
        _ => run(),
    }
}

fn run() -> anyhow::Result<()> {
    // Load benchmarks
    let mut pinned_durations =
        common::load_results::<'_, Duration>(PINNED_DURATIONS_PATH, &mut String::new())?;
    let mut last_durations =
        common::load_results::<'_, Duration>(LAST_DURATIONS_PATH, &mut String::new())?;
    let unrun_cases = common::load_cases(&["bench/"], "test/ignore.toml", |path, _| CaseData {
        pinned_duration: pinned_durations.remove(path),
        last_duration: last_durations.remove(path),
    })?;

    // Run benchmarks
    let run_cases = unrun_cases.into_iter().map(run_test).collect_vec();

    // Save times
    common::write_results(
        &run_cases
            .iter()
            .map(|case| (case.path.clone(), case.duration))
            .collect(),
        LAST_DURATIONS_PATH,
    )
}

fn run_test(unrun_case: common::UnrunTestCase<CaseData>) -> common::RunTestCase<CaseData> {
    println!();
    println!("Running {}", unrun_case.name().white().bold());

    let run_case = unrun_case.run(&["--only-update-line"], /* display_stderr = */ true);

    // Print summary
    println!();
    print!(
        "Completed in {} (",
        format!("{:.2?}s", run_case.duration.as_secs_f64())
            .white()
            .bold()
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
