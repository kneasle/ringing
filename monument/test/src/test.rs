//! Runner for test cases (not benchmarks)

mod common;

use std::{
    collections::{BTreeMap, HashSet},
    fmt::Debug,
    io::Read,
    path::{Path, PathBuf},
    process::Stdio,
    time::{Duration, Instant},
};

use anyhow::Context;
use colored::{Color, ColoredString, Colorize};
use common::PathFromMonument;
use difference::Changeset;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use regex::Regex;

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
    // Read the args to determine what to do
    let mut args = std::env::args();
    args.next(); // Skip the first arg, which always the path to the test's binary
    if args.next().as_deref() == Some("bless") {
        match args.next().as_deref() {
            // `cargo bless`: bless only unspecified
            None => bless_tests(BlessLevel::OnlyUnspecified),
            // `cargo bless --fails ...`
            Some("--fails") => match args.next() {
                // `cargo bless --fails {something} ...`
                Some(arg) => {
                    println!("Unknown additional arg '{}'", arg);
                    print_bless_usage();
                    Ok(())
                }
                // `cargo bless --fails`: bless everything
                None => bless_tests(BlessLevel::Fails),
            },
            // `cargo bless {something not --fails} ...`
            Some(arg) => {
                println!("Unknown arg '{}'", arg);
                print_bless_usage();
                Ok(())
            }
        }
    } else {
        // If no args were given, just run the tests
        match run()? {
            Outcome::Fail => Err(anyhow::Error::msg("Tests failed")),
            Outcome::Pass => Ok(()),
        }
    }
}

fn print_bless_usage() {
    println!();
    println!("Possible options:");
    println!("    `cargo bless`        : Bless only unspecified/new test cases");
    println!("    `cargo bless --fails`: Bless everything, even failed test cases");
}

///////////////////////////
// TOP-LEVEL RUNNER CODE //
///////////////////////////

/// Run the full test suite.
pub fn run() -> anyhow::Result<Outcome> {
    monument_cli::init_logging(log::LevelFilter::Warn); // Equivalent to '-q'

    let start = Instant::now();

    // Collect the test cases
    let cases = sources_to_cases(common::load_cases(TEST_DIRS, IGNORE_PATH)?)?;
    // Run the tests.
    println!("running {} tests", cases.len());
    let completed_tests: Vec<RunTestCase> = cases.into_par_iter().map(run_test).collect();
    // Collate failures and unspecified tests from all `Suite`s
    report_failures(&completed_tests);
    let outcome = print_summary_string(&completed_tests, start.elapsed());
    // Save current results file, used when blessing results
    write_actual_results(completed_tests)?;

    Ok(outcome)
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
    let mut expected_results = load_results(EXPECTED_RESULTS_PATH)?;
    let mut actual_results = load_results(ACTUAL_RESULTS_PATH)?;
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
        .collect::<ResultsFile>();
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
    write_results(&merged_results, EXPECTED_RESULTS_PATH)
}

////////////////////////
// LOADING TEST CASES //
////////////////////////

/// Convert case paths into [`UnrunTestCase`]s.
fn sources_to_cases(sources: Vec<(PathFromMonument, bool)>) -> anyhow::Result<Vec<UnrunTestCase>> {
    // Combine them with the auxiliary files (results, ignore, timings, etc.) to get the
    // `UnrunTestCase`s
    let mut results = load_results(EXPECTED_RESULTS_PATH)?;

    let mut unrun_test_cases = Vec::new();
    for (path, is_ignored) in sources {
        let is_example = path.to_string().contains("example");
        unrun_test_cases.push(UnrunTestCase {
            expected_output: results.remove(&path),
            state: if is_ignored {
                CaseBehaviour::Ignored
            } else if is_example {
                CaseBehaviour::Example
            } else {
                CaseBehaviour::Test
            },
            path,
        });
    }
    Ok(unrun_test_cases)
}

//////////////////
// RESULT FILES //
//////////////////

/// The contents of the `results.toml` file, found at [`RESULTS_PATH`].  We use a [`BTreeMap`] so
/// that the test cases are always written in a consistent order (i.e. alphabetical order by file
/// path), thus making the diffs easier to digest.
type ResultsFile = BTreeMap<PathFromMonument, String>;

fn load_results(path: impl AsRef<Path>) -> anyhow::Result<ResultsFile> {
    let full_path = PathFromMonument::new(path.as_ref()).relative_to_cargo_toml();
    let toml = std::fs::read_to_string(&full_path)
        .with_context(|| format!("Error loading results file ({:?})", full_path))?;
    let results_file: ResultsFile = toml::from_str(&toml)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(results_file)
}

fn write_actual_results(cases: Vec<RunTestCase>) -> anyhow::Result<()> {
    let actual_results: ResultsFile = cases
        .into_iter()
        .filter_map(|case| case.actual_output.map(|o| (case.base.path, o)))
        .collect();
    write_results(&actual_results, ACTUAL_RESULTS_PATH)
}

fn write_results(results: &ResultsFile, path: &str) -> anyhow::Result<()> {
    let toml = toml::to_string_pretty(results)
        .with_context(|| format!("Error serialising results file {:?}", path))?;

    let path_from_cargo_toml = PathFromMonument::new(path).relative_to_cargo_toml();
    std::fs::write(path_from_cargo_toml, toml.as_bytes())
        .with_context(|| format!("Error writing results to {:?}", path))
}

////////////////////////
// RUNNING TEST CASES //
////////////////////////

/// Run a test case (skipping ignored cases), determining its [`TestResult`].  Prints a status line
/// once finished.
fn run_test(case: UnrunTestCase) -> RunTestCase {
    // Skip any ignored tests
    let no_search = match case.state {
        CaseBehaviour::Test => false,
        CaseBehaviour::Example => true,
        CaseBehaviour::Ignored => {
            return RunTestCase {
                base: case,
                actual_output: None,
            };
        }
    };

    // Determine where the 'monument_cli' executible is.  This is harder than it seems because
    // Cargo allows users (like myself) to override the location of the build directory from the
    // default of `target/`.  The most reliable way to find the executible path is to ask
    // `cargo build` for JSON output of where its executables are placed.  However, I don't want
    // to re-run the compiler during unit tests (`cargo` holds a file lock on the build directory,
    // so multiple instances of `cargo` compiling the same crate can't run in parallel).
    //
    // We have a better trick up our sleeves, though: we are running the tests from an executible
    // **built by cargo**.  Therefore, _the current executible lives in Cargo's build directory_.
    // So, instead of hardcoding an path relative to the user's current directory, we instead
    // hardcode the path relative to the path of the test executible.  This is very portable, but
    // I'm fairly sure that the exact layout of Cargo's build directory is an implementation detail
    // and may change during any release.  But, if this does happen, the only bad effect is that
    // unit tests break, which is annoying but can't affect users.
    let current_exe_path = std::env::args()
        .next()
        .expect("Argv should always include current path");
    let mut monument_cli_path = PathBuf::from(current_exe_path);
    monument_cli_path.pop(); // Pop current exe's filename
    monument_cli_path.pop(); // Pop out of the 'deps/' directory
    monument_cli_path.push("monument_cli");

    // Spawn a command to run Monument, and fetch its stdout output as the test result
    let toml_path = case.path.relative_to_cargo_toml();
    // TODO: Don't hardcode the Monument path
    let cmd = std::process::Command::new(monument_cli_path)
        .arg(&*toml_path.as_os_str().to_string_lossy())
        .args(match no_search {
            false => &[] as &[&str],
            true => &["-D", "no-search"],
        })
        .args(["-q"])
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let mut output = Vec::new();
    cmd.stdout.unwrap().read_to_end(&mut output).unwrap();
    // Strip color codes from the output
    //
    // ANSI colors are 'esc key' (1b in hex) followed by `[` then some codes, finishing
    // with `m`.  We use `.*?` to consume anything, but stopping at the first `m` (`.*?` is
    // the non-greedy version of `.*`)
    let output = String::from_utf8_lossy(&output).into_owned();
    let ansi_escape_regex = Regex::new("\x1b\\[.*?m").unwrap();
    let color_free_output = ansi_escape_regex.replace_all(&output, "").into_owned();

    // Create the `run_case`
    let run_case = RunTestCase {
        base: case,
        actual_output: Some(color_free_output),
    };
    println!(
        "{} ... {}",
        run_case.name(),
        run_case.colored_outcome_string()
    );
    run_case
}

////////////////////////////
// PRINT ERRORS & SUMMARY //
////////////////////////////

/// Given the completed tests, print reports for the unspecified and failed tests
fn report_failures(run_cases: &[RunTestCase]) {
    // Report all unspecified tests first
    for case in run_cases {
        let path_string = unspecified_str(&case.name());
        if case.outcome() == CaseOutcome::Unspecified {
            println!();
            println!("Unspecified results for {path_string}.  This is the output:",);
            println!("{}", case.actual_output.as_ref().unwrap());
        }
    }
    // Report all the failures second
    for case in run_cases {
        let path_string = fail_str(&case.name());
        match (&case.expected_output, &case.actual_output) {
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
fn print_summary_string(completed_tests: &[RunTestCase], duration: Duration) -> Outcome {
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

#[derive(Debug)]
struct UnrunTestCase {
    path: PathFromMonument,
    state: CaseBehaviour,
    expected_output: Option<String>,
}

#[derive(Debug)]
struct RunTestCase {
    base: UnrunTestCase,
    actual_output: Option<String>,
}

impl UnrunTestCase {
    fn name(&self) -> String {
        self.path.to_string()
    }
}

impl RunTestCase {
    fn colored_outcome_string(&self) -> ColoredString {
        self.outcome().colored_string()
    }

    fn outcome(&self) -> CaseOutcome {
        // TODO: Clean this up?
        match self.state {
            CaseBehaviour::Ignored => CaseOutcome::Ignored,
            CaseBehaviour::Example | CaseBehaviour::Test => {
                match (&self.expected_output, &self.actual_output) {
                    (_, None) => unreachable!(),
                    (None, Some(_)) => CaseOutcome::Unspecified,
                    (Some(x), Some(y)) => {
                        if x == y {
                            if self.state == CaseBehaviour::Example {
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

impl std::ops::Deref for RunTestCase {
    type Target = UnrunTestCase;

    fn deref(&self) -> &Self::Target {
        &self.base
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
