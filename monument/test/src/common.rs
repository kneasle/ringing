//! Code that's common between the test and benchmark runners (`test.rs` and `bench.rs`,
//! respectively).

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
    io::Read,
    path::{Path, PathBuf},
    process::Stdio,
    time::{Duration, Instant},
};

use anyhow::Context;
use colored::{Color, ColoredString, Colorize};
use difference::Changeset;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use regex::Regex;
use serde::Deserialize;

// NOTE: All paths are relative to the `monument` directory.  Cargo runs custom test code in the
// same directory as the `Cargo.toml` for that crate (in our case `monument/cli/Cargo.toml`), so
// these will all be prefixed with `PATH_TO_MONUMENT_DIR`.
const IGNORE_PATH: &str = "test/ignore.toml";
const EXPECTED_RESULTS_PATH: &str = "test/results.toml";
const ACTUAL_RESULTS_PATH: &str = "test/.last-results.toml";
const TEST_DIRS: [(&str, CaseState); 2] = [
    ("test/cases/", CaseState::Run), // Test cases which we expect to succeed
    ("examples/", CaseState::Parse), // Examples to show how Monument's TOML format works
];
const PATH_TO_MONUMENT_DIR: &str = "../";

///////////////////////////
// TOP-LEVEL RUNNER CODE //
///////////////////////////

/// Run the full test suite.
pub fn run() -> anyhow::Result<Outcome> {
    monument_cli::init_logging(log::LevelFilter::Warn); // Equivalent to '-q'

    let start = Instant::now();

    // Collect the test cases
    let cases = sources_to_cases(collect_sources()?)?;
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
    let mut test_case_names = HashSet::<PathBuf>::new();
    test_case_names.extend(expected_results.keys().cloned());
    test_case_names.extend(actual_results.keys().cloned());

    // Merge the results, using the `BlessLevel` to decide, for each test, which result to keep
    let mut changed_case_names = Vec::<(ChangeType, PathBuf)>::new();
    let merged_results = test_case_names
        .into_iter()
        .filter_map(|path: PathBuf| {
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
            let name = path_to_string(path);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CaseState {
    Run,
    Parse,
    Ignored,
}

/// Determine where all the tests should come from, and how they should be run
fn collect_sources() -> anyhow::Result<Vec<(PathBuf, CaseState)>> {
    // Collect the individual test sources from every suite
    let mut test_sources: Vec<(PathBuf, CaseState)> = Vec::new();
    for (path, state) in TEST_DIRS {
        // TODO: Make newtypes for the different relative paths?
        let path_relative_to_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);

        // Walk the directory for test cases (i.e. `*.toml` files)
        for entry in walkdir::WalkDir::new(&path_relative_to_cargo_toml) {
            // Get the path of the file, relative to the `monument` directory
            let entry = entry.context("Error reading directory")?;
            let extension = entry.path().extension().and_then(|s| s.to_str());
            let file_path_relative_to_monument_dir =
                entry.path().components().skip(1).collect::<PathBuf>();

            match extension {
                Some("toml") => test_sources.push((file_path_relative_to_monument_dir, state)),
                // Directories should always be ignored
                None => {}
                // Ignore any files that aren't `.toml` or `.md`
                _ => println!("Ignoring {:?}", file_path_relative_to_monument_dir),
            }
        }
    }
    Ok(test_sources)
}

/// Convert ([`PathBuf`], [`SuiteState`]) pairs into [`UnrunTestCase`]s.
fn sources_to_cases(sources: Vec<(PathBuf, CaseState)>) -> anyhow::Result<Vec<UnrunTestCase>> {
    // Combine them with the auxiliary files (results, ignore, timings, etc.) to get the
    // `UnrunTestCase`s
    let mut results = load_results(EXPECTED_RESULTS_PATH)?;
    let mut ignore = load_ignore()?;

    let mut unrun_test_cases = Vec::new();
    for (path, state) in sources {
        // Load the values from the ignore/result files
        let ignore_reason = ignore.remove(&path);
        let expected_output = results.remove(&path);
        // Decide whether to ignore this case
        let state = match ignore_reason {
            Some(IgnoreReason::MusicFile) => {
                assert!(
                    expected_output.is_none(),
                    "Music file {:?} shouldn't have results",
                    path
                );
                continue; // Fully ignore music files
            }
            Some(IgnoreReason::ExplicitIgnore) => CaseState::Ignored,
            None => state,
        };
        // Combine everything into an `UnrunTestCase`
        unrun_test_cases.push(UnrunTestCase {
            path,
            state,
            expected_output,
        });
    }
    Ok(unrun_test_cases)
}

/////////////////////////
// IGNORE/RESULT FILES //
/////////////////////////

/// The contents of the `results.toml` file, found at [`RESULTS_PATH`].  We use a [`BTreeMap`] so
/// that the test cases are always written in a consistent order (i.e. alphabetical order by file
/// path), thus making the diffs easier to digest.
type ResultsFile = BTreeMap<PathBuf, String>;

/// The contents of the `ignore.toml` file, found at [`IGNORE_PATH`].
#[derive(Debug, Deserialize)]
struct IgnoreFile {
    ignore: HashSet<PathBuf>,
    music_files: HashSet<PathBuf>,
}

/// Reasons why a given test file could be ignored
#[derive(Debug, Clone, Copy)]
enum IgnoreReason {
    /// The file was explicitly ignored to suppress a failing test
    ExplicitIgnore,
    /// The file was a music file, and never should have been tested in the first place
    MusicFile,
}

fn load_ignore() -> anyhow::Result<HashMap<PathBuf, IgnoreReason>> {
    // Load the `IgnoreFile`
    let full_ignore_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(IGNORE_PATH);
    let ignore_toml = std::fs::read_to_string(&full_ignore_path)
        .with_context(|| format!("Error loading ignore file ({:?})", full_ignore_path))?;
    let ignore_file: IgnoreFile = toml::from_str(&ignore_toml)
        .with_context(|| format!("Error parsing ignore file ({:?})", full_ignore_path))?;
    // Flatten the `IgnoreFile` into a single `HashMap`
    let mut ignore_map: HashMap<PathBuf, IgnoreReason> = ignore_file
        .ignore
        .into_iter()
        .map(|path| (path, IgnoreReason::ExplicitIgnore))
        .collect();
    for path in ignore_file.music_files {
        if ignore_map.contains_key(&path) {
            return Err(anyhow::Error::msg(format!(
                "{:?} can't be in `ignore` and `music_files`",
                path
            )));
        }
        ignore_map.insert(path, IgnoreReason::MusicFile);
    }
    Ok(ignore_map)
}

fn load_results(path: impl AsRef<Path>) -> anyhow::Result<ResultsFile> {
    let full_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
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

    let path_from_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
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
        CaseState::Run => false,
        CaseState::Parse => true,
        CaseState::Ignored => {
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
    let toml_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(&case.path);
    // TODO: Don't hardcode the Monument path
    let cmd = std::process::Command::new(monument_cli_path)
        .arg(&path_to_string(&toml_path))
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
    path: PathBuf,
    state: CaseState,
    expected_output: Option<String>,
}

#[derive(Debug)]
struct RunTestCase {
    base: UnrunTestCase,
    actual_output: Option<String>,
}

impl UnrunTestCase {
    fn name(&self) -> String {
        path_to_string(&self.path)
    }
}

impl RunTestCase {
    fn colored_outcome_string(&self) -> ColoredString {
        self.outcome().colored_string()
    }

    fn outcome(&self) -> CaseOutcome {
        // TODO: Clean this up?
        match self.state {
            CaseState::Ignored => CaseOutcome::Ignored,
            CaseState::Parse | CaseState::Run => match (&self.expected_output, &self.actual_output)
            {
                (_, None) => unreachable!(),
                (None, Some(_)) => CaseOutcome::Unspecified,
                (Some(x), Some(y)) => {
                    if x == y {
                        if self.state == CaseState::Parse {
                            CaseOutcome::Parsed
                        } else {
                            CaseOutcome::Ok
                        }
                    } else {
                        CaseOutcome::Fail
                    }
                }
            },
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

fn path_to_string(path: &Path) -> String {
    path.as_os_str().to_string_lossy().into_owned()
}

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
