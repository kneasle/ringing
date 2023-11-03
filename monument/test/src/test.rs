//! Runner for test cases (not benchmarks)

mod common;

use std::{
    collections::HashSet,
    fmt::Debug,
    time::{Duration, Instant},
};

use colored::{Color, ColoredString, Colorize};
use common::{PathFromMonument, UnrunTestCase};
use difference::Changeset;
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
        Some(SubCommand::Bless { fails: fail }) => {
            // `cargo bless ...`
            let bless_level = if fail {
                BlessLevel::Fails
            } else {
                BlessLevel::OnlyUnspecified
            };
            bless_tests(bless_level)
        }
        None => {
            // If no args were given, just run the tests
            match run()? {
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
    #[structopt(subcommand)]
    sub_command: Option<SubCommand>,
}

#[derive(StructOpt)]
enum SubCommand {
    Bless {
        #[structopt(long)]
        fails: bool,
    },
}

///////////////////////////
// TOP-LEVEL RUNNER CODE //
///////////////////////////

/// Run the full test suite.
pub fn run() -> anyhow::Result<Outcome> {
    monument_cli::init_logging(log::LevelFilter::Warn); // Equivalent to '-q'

    let start = Instant::now();

    // Collect the test cases
    let cases = get_cases()?;
    // Run the tests.
    println!("running {} tests", cases.len());
    let completed_tests: Vec<RunTestCase<CaseData>> = cases.into_par_iter().map(run_test).collect();
    // Collate failures and unspecified tests from all `Suite`s
    report_failures(&completed_tests);
    let outcome = print_summary_string(&completed_tests, start.elapsed());
    // Save current results file, used when blessing results
    write_actual_results(completed_tests)?;

    Ok(outcome)
}

fn get_cases() -> anyhow::Result<Vec<UnrunTestCase<CaseData>>> {
    let mut results = common::load_results(EXPECTED_RESULTS_PATH, &mut String::new())?;

    common::load_cases(
        TEST_DIRS,
        IGNORE_PATH,
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
pub fn bless_tests(level: BlessLevel) -> anyhow::Result<()> {
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

    // Merge the results, using the `BlessLevel` to decide, for each test, which result to keep
    let mut changed_case_names = Vec::<(ChangeType, PathFromMonument)>::new();
    let merged_results = test_case_names
        .into_iter()
        .filter_map(|path: PathFromMonument| {
            let entry = match (expected_results.remove(&path), actual_results.remove(&path)) {
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
                ChangeType::New => println!("     (new) {}", unspecified_str(&name)),
                ChangeType::Fail => println!("    (fail) {}", fail_str(&name)),
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
        let path_string = unspecified_str(&case.name());
        if case.outcome() == CaseOutcome::Unspecified {
            println!();
            println!("Unspecified results for {path_string}.  This is the output:",);
            println!("{}", case.output.as_ref().unwrap());
        }
    }
    // Report all the failures second
    for case in run_cases {
        let path_string = fail_str(&case.name());
        match (&case.expected_output, &case.output) {
            (Some(expected), Some(actual)) if expected != actual => {
                println!();
                println!("{} produced the wrong output:", path_string);
                println!("{}", Changeset::new(expected, actual, "\n"));
            }
            _ => {} // Everything else isn't a failure
        }
    }
}

/// Generate and print a summary string for the tests.  Returns the outcome of the test suite
fn print_summary_string(completed_tests: &[RunTestCase<CaseData>], duration: Duration) -> Outcome {
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
            CaseOutcome::Fail => num_failures += 1,
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
        // TODO: Clean this up?
        match self.behaviour {
            CaseBehaviour::Ignored => CaseOutcome::Ignored,
            CaseBehaviour::Example | CaseBehaviour::Test => {
                match (&self.expected_output, &self.output) {
                    (_, None) => unreachable!(),
                    (None, Some(_)) => CaseOutcome::Unspecified,
                    (Some(x), Some(y)) => {
                        if x == y {
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
            }
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
}

impl CaseOutcome {
    fn colored_string(self) -> ColoredString {
        match self {
            Self::Ok => ok_string(),
            Self::Parsed => "parsed".color(Color::Green),
            Self::Ignored => "ignored".color(Color::Yellow),
            Self::Unspecified => "unspecified".color(UNSPECIFIED_COLOR),
            Self::Fail => fail_string(),
        }
    }
}

///////////
// UTILS //
///////////

fn unspecified_str(s: &str) -> ColoredString {
    s.color(UNSPECIFIED_COLOR).bold()
}

fn fail_str(s: &str) -> ColoredString {
    s.color(FAIL_COLOR).bold()
}

fn ok_string() -> ColoredString {
    "ok".color(Color::Green)
}

fn fail_string() -> ColoredString {
    "fail".color(FAIL_COLOR)
}

const FAIL_COLOR: Color = Color::BrightRed;
const UNSPECIFIED_COLOR: Color = Color::BrightBlue;
