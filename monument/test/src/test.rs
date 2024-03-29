//! Runner for test cases (not benchmarks)

mod common;

use std::{
    collections::HashSet,
    fmt::{Debug, Write},
    time::{Duration, Instant},
};

use colored::{Color, ColoredString, Colorize};
use common::{PathFromMonument, UnrunTestCase};
use difference::{Changeset, Difference};
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use structopt::StructOpt;

use crate::common::RunTestCase;

const EXPECTED_RESULTS_PATH: &str = "test/results.toml";
const ACTUAL_RESULTS_PATH: &str = "test/.last-results.toml";
const IGNORE_PATH: &str = "test/ignore.toml";
const TEST_DIRS: &[&str] = &[
    "test/cases/", // Test cases which we expect to succeed
    "examples/",   // Examples to show how Monument's TOML format works
];

///////////////////
// MAIN FUNCTION //
///////////////////

/// Collect and run all the test cases, but no benchmarks (which would take too long)
fn main() -> anyhow::Result<()> {
    let args = Args::from_args();
    match args.sub_command {
        Some(SubCommand::Bless { fails, filter }) => {
            // `cargo bless ...`
            let bless_level = if fails {
                BlessLevel::Fails
            } else {
                BlessLevel::OnlyUnspecified
            };
            bless_tests(bless_level, filter.as_deref())
        }
        None => {
            // If no args were given, just run the tests
            match run(args.filter.as_deref())? {
                Outcome::Fail => Err(anyhow::Error::msg("Tests failed")),
                Outcome::Pass => Ok(()),
            }
        }
    }
}

/////////////////
// ARG PARSING //
/////////////////

#[derive(StructOpt)]
struct Args {
    /// Only tests who's paths match this regex will be run
    #[structopt(long, short = "F")]
    filter: Option<String>,
    #[structopt(subcommand)]
    sub_command: Option<SubCommand>,
}

#[derive(StructOpt)]
enum SubCommand {
    Bless {
        #[structopt(long)]
        fails: bool,
        /// Only tests who's paths match this regex will be run
        #[structopt(long, short = "F")]
        filter: Option<String>,
    },
}

///////////////////////////
// TOP-LEVEL RUNNER CODE //
///////////////////////////

/// Run the full test suite.
pub fn run(filter: Option<&str>) -> anyhow::Result<Outcome> {
    monument_cli::init_logging(log::LevelFilter::Warn); // Equivalent to '-q'

    let start = Instant::now();

    // Collect the test cases
    let cases = get_cases(filter)?;
    // Run the tests.
    println!("running {} tests", cases.len());
    let completed_tests: Vec<RunTestCase<CaseData>> = cases.into_par_iter().map(run_test).collect();
    // Collate failures and unspecified tests from all `Suite`s
    report_failures(&completed_tests);
    print_fails_summary(&completed_tests);
    let outcome = print_summary_line(&completed_tests, start.elapsed());
    // Save current results file, used when blessing results
    write_actual_results(completed_tests)?;

    Ok(outcome)
}

fn get_cases(filter: Option<&str>) -> anyhow::Result<Vec<UnrunTestCase<CaseData>>> {
    let mut results = common::load_results(EXPECTED_RESULTS_PATH, &mut String::new())?;

    common::load_cases(
        TEST_DIRS,
        IGNORE_PATH,
        filter,
        |path: &PathFromMonument, is_ignored: bool| {
            let is_example = path.to_string().contains("example");
            CaseData {
                behaviour: if is_ignored {
                    CaseBehaviour::Ignored
                } else if is_example {
                    CaseBehaviour::Example
                } else {
                    CaseBehaviour::Test
                },
                expected_output: results.remove(path),
            }
        },
    )
}

/// Run a test case (skipping ignored cases), determining its [`TestResult`].  Prints a status line
/// once finished.
fn run_test(case: UnrunTestCase<CaseData>) -> RunTestCase<CaseData> {
    let no_search = match case.behaviour {
        CaseBehaviour::Test => false,
        CaseBehaviour::Example => true,
        CaseBehaviour::Ignored => false, // Exact value doesn't matter
    };

    let mut args = vec![
        "-q",                // Info messages might change often
        "--no-comp-numbers", // We only want to test *which* comps are outputted, not their order
    ];
    if no_search {
        args.extend(["-D", "no-search"]);
    }
    let run_case = case.run(&args, /* display_stderr = */ false);

    println!(
        "{} ... {}",
        run_case.name(),
        run_case.colored_outcome_string()
    );
    run_case
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[must_use]
pub enum Outcome {
    /// All tests passed
    Pass,
    /// Some tests/benchmarks failed
    Fail,
}

impl Outcome {
    fn colored_string(self) -> ColoredString {
        match self {
            Self::Pass => ok_string(),
            Self::Fail => fail_string(),
        }
    }
}

fn write_actual_results(cases: Vec<RunTestCase<CaseData>>) -> anyhow::Result<()> {
    let actual_results: common::ResultsFile<String> = cases
        .into_iter()
        .filter_map(|case| case.output.map(|o| (case.base.path, o)))
        .collect();
    common::write_results(&actual_results, ACTUAL_RESULTS_PATH)
}

//////////////////////////
// TOP-LEVEL BLESS CODE //
//////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseBehaviour {
    Test,
    Example,
    Ignored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlessLevel {
    OnlyUnspecified,
    Fails,
}

/// Bless the test results of a given [`BlessLevel`]
pub fn bless_tests(level: BlessLevel, filter: Option<&str>) -> anyhow::Result<()> {
    enum ChangeType {
        New,
        Fail,
        Removed,
    }
    // Load all results files
    let mut expected_results = common::load_results(EXPECTED_RESULTS_PATH, &mut String::new())?;
    let mut actual_results = common::load_results(ACTUAL_RESULTS_PATH, &mut String::new())?;
    // Determine which tests cases we have (any) results for
    let mut test_case_names = HashSet::<PathFromMonument>::new();
    test_case_names.extend(expected_results.keys().cloned());
    test_case_names.extend(actual_results.keys().cloned());

    let filter_regex = common::get_filter_regex(filter)?;
    // Merge the results, using the `BlessLevel` to decide, for each test, which result to keep
    let mut changed_case_names = Vec::<(ChangeType, PathFromMonument)>::new();
    let merged_results = test_case_names
        .into_iter()
        .filter_map(|path: PathFromMonument| {
            let expected_result = expected_results.remove(&path);
            let actual_result = actual_results.remove(&path);
            if !filter_regex.is_match(&path.path) {
                return expected_result.map(|entry| (path, entry));
            }

            let entry = match (expected_result, actual_result) {
                (Some(exp_result), Some(act_result)) => {
                    if level == BlessLevel::Fails && exp_result != act_result {
                        // We're blessing this file iff the results are different
                        changed_case_names.push((ChangeType::Fail, path.clone()));
                        act_result
                    } else {
                        exp_result
                    }
                }
                (None, Some(act_result)) => {
                    // Always bless new tests
                    changed_case_names.push((ChangeType::New, path.clone()));
                    act_result
                }
                (Some(_exp_result), None) => {
                    // Remove results for deleted cases
                    changed_case_names.push((ChangeType::Removed, path.clone()));
                    return None;
                }
                (None, None) => unreachable!(),
            };
            Some((path, entry))
        })
        .collect::<common::ResultsFile<String>>();
    assert!(expected_results.is_empty());
    assert!(actual_results.is_empty());

    if changed_case_names.is_empty() {
        // Special case if there's nothing to bless
        println!("Nothing to bless");
    } else {
        // Print what we've done, and overwrite the results file
        match level {
            BlessLevel::OnlyUnspecified => println!("Blessing only unspecified/new test cases:"),
            BlessLevel::Fails => println!("Blessing all cases, even fails:"),
        }
        for (level, path) in &changed_case_names {
            let name = path.to_string();
            match level {
                ChangeType::New => println!("     (new) {}", unspecified_name_str(&name)),
                ChangeType::Fail => println!("    (fail) {}", fail_name_str(&name)),
                ChangeType::Removed => println!(" (removed) {}", name.yellow().bold()),
            }
        }
        println!("{} test cases blessed", changed_case_names.len());
    }
    // Always write the results, even if there's nothing to bless.  This way, we can handle things
    // like reformatting the JSON files
    common::write_results(&merged_results, EXPECTED_RESULTS_PATH)
}

////////////////////////////
// PRINT ERRORS & SUMMARY //
////////////////////////////

/// Given the completed tests, print reports for the unspecified and failed tests
fn report_failures(run_cases: &[RunTestCase<CaseData>]) {
    // Report all unspecified tests first
    for case in run_cases {
        let path_string = unspecified_name_str(&case.name());
        if case.outcome() == CaseOutcome::Unspecified {
            println!();
            println!("Unspecified results for {path_string}.  This is the output:",);
            println!("{}", case.output.as_ref().unwrap());
        }
    }
    // Report all the failures second
    for case in run_cases {
        let path_string = fail_name_str(&case.name());
        match (&case.expected_output, &case.output) {
            (Some(expected), Some(actual)) if expected != actual => {
                println!();
                if case.panicked {
                    println!("{path_string} panicked with message:");
                } else {
                    println!("{path_string} produced the wrong output:");
                }
                print_diff(expected, actual);
            }
            _ => {} // Everything else isn't a failure
        }
    }
}

/// Print a pretty diff of the outputs
fn print_diff(expected_output: &str, actual_output: &str) {
    // First, diff by lines
    let changeset = Changeset::new(expected_output, actual_output, "\n");
    // Now, print the output, but if it looks like lines have slightly changed highlight only the
    // differences
    let mut diff_iter = changeset.diffs.iter().peekable();
    while let Some(diff) = diff_iter.next() {
        match diff {
            Difference::Same(s) => println!("{s}"),
            // If an addition is popped directly, then it can't be part of a small modification,
            // so just print it normally
            Difference::Add(s) => println!("{}", s.color(Color::BrightGreen)),
            Difference::Rem(rem) => {
                // Small changes to a set of lines will appear as removals followed by almost
                // identical additions.  So, before printing this, peek at the next element and see
                // if it's an addition which can be pretty-printed
                if let Some(Difference::Add(add)) = diff_iter.peek() {
                    let sub_diff = Changeset::new(rem, add, "");
                    let amount_changed = sub_diff.distance as f32 / (rem.len() + add.len()) as f32;
                    if amount_changed < LARGEST_HIGHLIGHTABLE_DIFF {
                        print_fancy_diff(sub_diff);
                        // Skip printing this and the next 'add' because we've printed both of them
                        // already
                        assert!(matches!(diff_iter.next(), Some(Difference::Add(_))));
                        continue; // Don't print the highlighting normally
                    }
                }
                println!("{}", rem.color(Color::BrightRed));
            }
        }
    }
}

fn print_fancy_diff(sub_diff: Changeset) {
    enum DiffType {
        Rem,
        Add,
    }

    // Build up formatted strings for the removal and addition parts
    let mut rem_str = String::new();
    let mut add_str = String::new();
    let mut add_diff = |s: &str, diff_type: DiffType, highlight: bool| {
        let (pushable_str, color) = match diff_type {
            DiffType::Rem => (&mut rem_str, Color::BrightRed),
            DiffType::Add => (&mut add_str, Color::BrightGreen),
        };
        let colored_string = if highlight {
            s.black().on_color(color)
        } else {
            s.color(color)
        };
        write!(pushable_str, "{}", colored_string).unwrap();
    };
    for change in &sub_diff.diffs {
        match change {
            Difference::Same(s) => {
                // Add this as both a rem and an add, both without highlights
                add_diff(s, DiffType::Rem, false);
                add_diff(s, DiffType::Add, false);
            }
            Difference::Add(s) => add_diff(s, DiffType::Add, true),
            Difference::Rem(s) => add_diff(s, DiffType::Rem, true),
        }
    }
    // Print both of these
    println!("{rem_str}");
    println!("{add_str}");
}

const LARGEST_HIGHLIGHTABLE_DIFF: f32 = 0.3;

fn print_fails_summary(completed_tests: &[RunTestCase<CaseData>]) {
    print_summary_section(completed_tests, CaseOutcome::Unspecified, "Unspecified");
    print_summary_section(completed_tests, CaseOutcome::Fail, "Failed");
    print_summary_section(completed_tests, CaseOutcome::Panicked, "Panicked");
}

fn print_summary_section(
    completed_tests: &[RunTestCase<CaseData>],
    outcome: CaseOutcome,
    name: &str,
) {
    let cases = completed_tests
        .iter()
        .filter(|t| t.outcome() == outcome)
        .collect_vec();
    if !cases.is_empty() {
        println!("{name}:");
        for c in cases {
            println!("  {}", c.name().color(outcome.color()).bold());
        }
    }
}

/// Generate and print a summary string for the tests.  Returns the overall outcome of the suite
fn print_summary_line(completed_tests: &[RunTestCase<CaseData>], duration: Duration) -> Outcome {
    // Count the test categories
    let mut num_ok = 0;
    let mut num_ignored = 0;
    let mut num_failures = 0;
    let mut num_unspecified = 0;
    for case in completed_tests {
        match case.outcome() {
            CaseOutcome::Ok | CaseOutcome::Parsed => num_ok += 1,
            CaseOutcome::Ignored => num_ignored += 1,
            CaseOutcome::Unspecified => num_unspecified += 1,
            CaseOutcome::Fail | CaseOutcome::Panicked => num_failures += 1,
        }
    }
    assert_eq!(
        num_ok + num_ignored + num_failures + num_unspecified,
        completed_tests.len()
    ); // All tests should be counted in some way

    let outcome = if num_failures == 0 && num_unspecified == 0 {
        Outcome::Pass
    } else {
        Outcome::Fail
    };
    // Print summary string
    println!();
    println!(
        "test result: {}. {} passed; {} unspecified; {} failed; {} ignored in {:.2?}",
        outcome.colored_string(),
        num_ok,
        num_unspecified,
        num_failures,
        num_ignored,
        duration
    );
    // Print a helpful message about `cargo bless`
    if num_unspecified > 0 {
        println!(
            "{}: run `{}` to add the results for the new tests.",
            "note".white().bold(),
            "cargo bless".bright_white()
        );
    }
    if num_failures > 0 {
        println!(
            "{}: If these failures are correct, then run `{}` to 'bless' your results.",
            "note".white().bold(),
            "cargo bless --fails".bright_white()
        );
    }
    // Return the outcome
    outcome
}

/////////////////////
// TEST CASE TYPES //
/////////////////////

struct CaseData {
    behaviour: CaseBehaviour,
    expected_output: Option<String>,
}

impl RunTestCase<CaseData> {
    fn colored_outcome_string(&self) -> ColoredString {
        self.outcome().colored_string()
    }

    fn outcome(&self) -> CaseOutcome {
        if self.behaviour == CaseBehaviour::Ignored {
            return CaseOutcome::Ignored;
        }
        if self.panicked {
            return CaseOutcome::Panicked;
        }
        let Some(expected_output) = &self.expected_output else {
            return CaseOutcome::Unspecified;
        };
        // Check that outcomes match
        let actual_output = self
            .output
            .as_ref()
            .expect("Non-ignored tests must have output");
        if expected_output == actual_output {
            if self.behaviour == CaseBehaviour::Example {
                CaseOutcome::Parsed
            } else {
                CaseOutcome::Ok
            }
        } else {
            CaseOutcome::Fail
        }
    }
}

/// The outcomes of a test, corresponding to what's printed to the console.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CaseOutcome {
    Ok,
    Parsed,
    Ignored,
    Unspecified,
    Fail,
    Panicked, // Failed by panicking
}

impl CaseOutcome {
    fn string(self) -> &'static str {
        match self {
            Self::Ok => "ok",
            Self::Parsed => "parsed",
            Self::Ignored => "ignored",
            Self::Unspecified => "unspecified",
            Self::Fail => "fail",
            Self::Panicked => "panicked",
        }
    }

    fn colored_string(self) -> ColoredString {
        self.string().color(self.color())
    }

    fn color(self) -> Color {
        match self {
            CaseOutcome::Ok | CaseOutcome::Parsed => OK_COLOR,
            CaseOutcome::Ignored => IGNORED_COLOR,
            CaseOutcome::Unspecified => UNSPECIFIED_COLOR,
            CaseOutcome::Fail | CaseOutcome::Panicked => FAIL_COLOR,
        }
    }
}

///////////
// UTILS //
///////////

fn unspecified_name_str(s: &str) -> ColoredString {
    s.color(UNSPECIFIED_COLOR).bold()
}

fn fail_name_str(s: &str) -> ColoredString {
    s.color(FAIL_COLOR).bold()
}

fn ok_string() -> ColoredString {
    "ok".color(OK_COLOR)
}

fn fail_string() -> ColoredString {
    "fail".color(FAIL_COLOR)
}

const OK_COLOR: Color = Color::Green;
const IGNORED_COLOR: Color = Color::Yellow;
const UNSPECIFIED_COLOR: Color = Color::BrightBlue;
const FAIL_COLOR: Color = Color::BrightRed;
