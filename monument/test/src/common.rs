//! Code that's common between the test and benchmark runners (`test.rs` and `bench.rs`,
//! respectively).

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display, Formatter},
    path::{Path, PathBuf},
};

use anyhow::Context;
use colored::{Color, ColoredString, Colorize};
use difference::Changeset;
use itertools::Itertools;
use monument::Config;
use monument_cli::{CtrlCBehaviour, DebugOption};
use ordered_float::OrderedFloat;
use path_slash::PathExt;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize};

// NOTE: All paths are relative to the `monument` directory.  Cargo runs custom test code in the
// same directory as the `Cargo.toml` for that crate (in our case `monument/cli/Cargo.toml`), so
// these will all be prefixed with `PATH_TO_MONUMENT_DIR`.
//
// Also NOTE: We use JSON for the results file, because serialization to JSON always succeeds
// whilst serialization to TOML fails when mixing empty and non-empty lists (which we do all the
// time because some tests produce no results).
const IGNORE_PATH: &str = "test/ignore.toml";
const EXPECTED_RESULTS_PATH: &str = "test/results.json";
const ACTUAL_RESULTS_PATH: &str = "test/.last-results.json";
const TEST_DIRS: [(&str, SuiteUse); 2] = [
    ("test/cases/", SuiteUse::Test), // Test cases which we expect to succeed
    ("examples/", SuiteUse::Example), // Examples to show how Monument's TOML format works
];
const PATH_TO_MONUMENT_DIR: &str = "../";

/// The maximum number of items allowed in the queue at one time
const QUEUE_LIMIT: usize = 10_000_000;

///////////////////////////
// TOP-LEVEL RUNNER CODE //
///////////////////////////

/// Run the full test suite.
pub fn run(run_type: RunType) -> anyhow::Result<Outcome> {
    monument_cli::init_logging(log::LevelFilter::Warn); // Equivalent to '-q'

    // Collect the test cases
    let cases = sources_to_cases(collect_sources(run_type)?)?;
    // Run the tests.  We run _tests_ in parallel, but _benchmarks_ sequentially
    println!("running {} tests", cases.len());
    let run = |c: UnrunTestCase| run_and_print_test(c, run_type);
    let completed_tests: Vec<RunTestCase> = match run_type {
        RunType::Test => cases.into_par_iter().map(run).collect(),
        RunType::Bench => cases.into_iter().map(run).collect(),
    };
    // Collate failures and unspecified tests from all `Suite`s
    report_failures(&completed_tests);
    let outcome = print_summary_string(&completed_tests);
    // Save current results file, used when blessing results
    write_actual_results(completed_tests)?;
    Ok(outcome)
}

/// The different ways that the test suite can be run
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// Each of `cargo test` and `cargo bench` will only use one of these, so each will cause the
// `dead_code` lint
#[allow(dead_code)]
pub enum RunType {
    /// Run using `cargo test`
    Test,
    /// Run using `cargo bench`
    Bench,
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
    let mut test_case_names = HashSet::<String>::new();
    test_case_names.extend(expected_results.keys().map(String::clone));
    test_case_names.extend(actual_results.keys().map(String::clone));

    // Merge the results, using the `BlessLevel` to decide, for each test, which result to keep
    let mut changed_case_names = Vec::<(ChangeType, String)>::new();
    let merged_results = test_case_names
        .into_iter()
        .filter_map(|name: String| {
            let entry = match (expected_results.remove(&name), actual_results.remove(&name)) {
                (Some(exp_result), Some(act_result)) => {
                    if level == BlessLevel::Fails && exp_result != act_result {
                        // We're blessing this file iff the results are different
                        changed_case_names.push((ChangeType::Fail, name.clone()));
                        act_result
                    } else {
                        exp_result
                    }
                }
                (None, Some(act_result)) => {
                    // Always bless new tests
                    changed_case_names.push((ChangeType::New, name.clone()));
                    act_result
                }
                (Some(_exp_result), None) => {
                    // Remove results for deleted cases
                    changed_case_names.push((ChangeType::Removed, name.clone()));
                    return None;
                }
                (None, None) => unreachable!(),
            };
            Some((name, entry))
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
        for (level, name) in &changed_case_names {
            match level {
                ChangeType::New => println!("     (new) {}", unspecified_str(name)),
                ChangeType::Fail => println!("    (fail) {}", fail_str(name)),
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

/// The ways a test suite can be used
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SuiteUse {
    /// Simple and fast cases which are designed to test parts of Monument, found in
    /// `monument/test/cases/`.
    /// These are always run.
    Test,
    /// Slower inputs designed to illustrate parts of Monument, found in `monument/examples`.
    Example,
    /// Slower inputs designed to be test Monument's performance, found in `monument/test/bench`.
    Bench,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SuiteState {
    Run,
    Parse,
    Ignore,
}

/// Determine where all the tests should come from, and how they should be run
fn collect_sources(run_type: RunType) -> anyhow::Result<Vec<(CaseSource, SuiteState)>> {
    // Collect the individual test sources from every suite
    let mut test_sources: Vec<(CaseSource, SuiteState)> = Vec::new();
    for (path, suite_use) in TEST_DIRS {
        // Determine whether tests in this directory should be run
        let state = match (run_type, suite_use) {
            // If `cargo test`, parse everything but only run `test/cases/`
            (RunType::Test, SuiteUse::Test) => SuiteState::Run,
            (RunType::Test, SuiteUse::Bench | SuiteUse::Example) => SuiteState::Parse,
            // If `cargo bench`, only run benchmarks and ignore everything else
            // TODO: Run `examples/` as benchmarks?
            (RunType::Bench, SuiteUse::Bench) => SuiteState::Run,
            (RunType::Bench, SuiteUse::Test | SuiteUse::Example) => SuiteState::Ignore,
        };

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
                Some("toml") => {
                    test_sources.push((
                        CaseSource::TomlFile(file_path_relative_to_monument_dir),
                        state,
                    ));
                }
                Some("md") => {
                    // Parse the file as markdown
                    let markdown = std::fs::read_to_string(entry.path()).with_context(|| {
                        format!(
                            "Error reading test suite file {:?}",
                            &path_relative_to_cargo_toml
                        )
                    })?;
                    for (heading_path, spec) in markdown::extract_cases(&markdown) {
                        test_sources.push((
                            CaseSource::SectionOfFile {
                                file_path: file_path_relative_to_monument_dir.clone(),
                                section_name: heading_path,
                                spec,
                                music_file: None,
                            },
                            state,
                        ));
                    }
                }
                // Ignore any files that aren't `.toml` or `.md`
                _ => print!("Ignoring {:?}", file_path_relative_to_monument_dir),
            }
        }
    }
    Ok(test_sources)
}

/// Convert ([`CaseSource`], [`SuiteState`]) pairs into [`UnrunTestCase`]s.  TODO: Implement this
/// as an iterator chain
fn sources_to_cases(sources: Vec<(CaseSource, SuiteState)>) -> anyhow::Result<Vec<UnrunTestCase>> {
    // Combine them with the auxiliary files (results, ignore, timings, etc.) to get the
    // `UnrunTestCase`s
    let mut results = load_results(EXPECTED_RESULTS_PATH)?;
    let mut ignore = load_ignore()?;

    let mut unrun_test_cases = Vec::new();
    for (source, state) in sources {
        let name = &source.name();
        // Load the values from the ignore/result files
        let ignore_value = ignore.remove(name);
        let results_value = results.remove(name);
        // Combine everything into an `UnrunTestCase`
        let ignored = match ignore_value {
            Some(IgnoreReason::MusicFile) => {
                assert!(
                    results_value.is_none(),
                    "Music file {:?} shouldn't have results",
                    name
                );
                continue; // Fully ignore music files
            }
            Some(IgnoreReason::ExplicitIgnore) => true,
            None => false,
        };
        let expected_results = match results_value {
            // Errors are always emitted, even for 'parsed' tests.  So errors take precedence over
            // everything.
            Some(ResultsFileEntry::Error { error_message }) => ExpectedResult::Error(error_message),
            // If no errors, then 'parsed' takes precedence over everything else
            _ if state == SuiteState::Parse => ExpectedResult::Parsed,
            // If the state is `SuiteState::Run`, then we want the expected comps
            Some(ResultsFileEntry::Comps { comps }) => ExpectedResult::Comps(comps),
            None => ExpectedResult::NoCompsGiven,
        };
        unrun_test_cases.push(UnrunTestCase {
            source,
            ignored,
            expected_results,
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
type ResultsFile = BTreeMap<String, ResultsFileEntry>;

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
enum ResultsFileEntry {
    Error { error_message: String },
    Comps { comps: Vec<Comp> },
}

/// The contents of the `ignore.toml` file, found at [`IGNORE_PATH`].
#[derive(Debug, Deserialize)]
struct IgnoreFile {
    ignore: HashSet<String>,
    music_files: HashSet<String>,
}

/// Reasons why a given test file could be ignored
#[derive(Debug, Clone, Copy)]
enum IgnoreReason {
    /// The file was explicitly ignored to suppress a failing test
    ExplicitIgnore,
    /// The file was a music file, and never should have been tested in the first place
    MusicFile,
}

fn load_ignore() -> anyhow::Result<HashMap<String, IgnoreReason>> {
    // Load the `IgnoreFile`
    let full_ignore_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(IGNORE_PATH);
    let ignore_toml = std::fs::read_to_string(&full_ignore_path)
        .with_context(|| format!("Error loading ignore file ({:?})", full_ignore_path))?;
    let ignore_file: IgnoreFile = toml::from_str(&ignore_toml)
        .with_context(|| format!("Error parsing ignore file ({:?})", full_ignore_path))?;
    // Flatten the `IgnoreFile` into a single `HashMap`
    let mut ignore_map: HashMap<String, IgnoreReason> = ignore_file
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
    let results_file: ResultsFile = serde_json::from_str(&toml)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(results_file)
}

fn write_actual_results(cases: Vec<RunTestCase>) -> anyhow::Result<()> {
    let actual_results: ResultsFile = cases
        .into_iter()
        .filter_map(|case| {
            let name = case.name();
            let entry = match case.actual_result {
                ActualResult::Ignored | ActualResult::Parsed => return None, // If no result, skip
                ActualResult::Error(error_message) => ResultsFileEntry::Error { error_message },
                ActualResult::Comps(comps) => ResultsFileEntry::Comps { comps },
            };
            Some((name, entry))
        })
        .collect();
    write_results(&actual_results, ACTUAL_RESULTS_PATH)
}

fn write_results(results: &ResultsFile, path: &str) -> anyhow::Result<()> {
    let unformatted_json = serde_json::to_string(results)
        .with_context(|| format!("Error serialising results file {:?}", path))?;
    let formatted_json = goldilocks_json_fmt::format_within_width(&unformatted_json, 115)
        .expect("`serde_json` should emit valid JSON");

    let path_from_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
    std::fs::write(path_from_cargo_toml, formatted_json.as_bytes())
        .with_context(|| format!("Error writing results to {:?}", path))
}

////////////////////////
// RUNNING TEST CASES //
////////////////////////

/// Run a specific test case, printing a line when finished.  The type signature is a bit weird but
/// means that this can be used directly when mapping over iterators.
fn run_and_print_test(unrun_case: UnrunTestCase, run_type: RunType) -> RunTestCase {
    let config = Config {
        queue_limit: QUEUE_LIMIT,
        thread_limit: match run_type {
            RunType::Test => Some(1), // For tests, we run cases in parallel - one thread each
            RunType::Bench => None,   // For bench, we run cases sequentially so no thread limit
        },
        ..Config::default()
    };
    // Run the test
    let run_case = run_test(unrun_case, &config);
    // Determine what should be printed after the '...'
    println!(
        "{} ... {}",
        run_case.name(),
        run_case.outcome().colored_string()
    );
    // Return the completed results
    run_case
}

/// Run a test case (skipping ignored cases), determining its [`TestResult`]
fn run_test(case: UnrunTestCase, config: &Config) -> RunTestCase {
    // Skip any ignored tests
    if case.ignored {
        return RunTestCase {
            base: case,
            actual_result: ActualResult::Ignored,
        };
    }

    // Determine the source of this test
    let source = match &case.source {
        CaseSource::TomlFile(path) => {
            monument_cli::Source::Path(PathBuf::from(PATH_TO_MONUMENT_DIR).join(path))
        }
        CaseSource::SectionOfFile {
            spec, music_file, ..
        } => monument_cli::Source::Str {
            spec,
            music_file: music_file.as_deref(),
        },
    };

    // Run Monument
    let no_search = case.expected_results == ExpectedResult::Parsed;
    let monument_result = monument_cli::run(
        source,
        no_search.then(|| DebugOption::StopBeforeSearch),
        config,
        CtrlCBehaviour::TerminateProcess, // Don't bother recovering comps whilst testing
    );
    // Convert Monument's `Result` into a `Results`
    let actual_results = match monument_result {
        // Successful search run
        Ok(Some(result)) => {
            assert!(!no_search);
            // Convert compositions and sort them by score, resolving tiebreaks using the comp string.
            // This guarantees a consistent ordering even if Monument's output order is non-deterministic
            let mut comps = result
                .comps
                .iter()
                .map(|c| Comp::new(c, &result.query))
                .collect_vec();
            comps.sort_by_key(|comp| (comp.avg_score, comp.string.clone()));
            ActualResult::Comps(comps)
        }
        // Successful `no_search` run, since no comp array was generated (the file was just
        // parsed and verified)
        Ok(None) => {
            assert!(no_search);
            ActualResult::Parsed
        }
        // Search failed in some way
        Err(e) => {
            // ANSI colours are 'esc key' (1b in hex) followed by `[` then some codes, finishing
            // with `m`.  We use `.*?` to consume anything, but stopping at the first `m` (`.*?` is
            // the non-greedy version of `.*`)
            let ansi_escape_regex = Regex::new("\x1b\\[.*?m").unwrap();
            let error_string = format!("{:?}", e);
            let color_free_error_string = ansi_escape_regex
                .replace_all(&error_string, "")
                .into_owned();
            ActualResult::Error(color_free_error_string)
        }
    };
    // Add the actual results to get a full search
    RunTestCase {
        base: case,
        actual_result: actual_results,
    }
}

////////////////////////////
// PRINT ERRORS & SUMMARY //
////////////////////////////

/// Given the completed tests, print reports for the unspecified and failed tests
fn report_failures(run_cases: &[RunTestCase]) {
    // Report all unspecified tests first
    for case in run_cases {
        let path_string = unspecified_str(&case.name());
        match case.outcome() {
            CaseOutcome::Unspecified(Ok(comps)) => {
                println!();
                println!(
                    "Unspecified results for {}.  These {} comps were generated:",
                    path_string,
                    comps.len()
                );
                println!("{}", Comp::multiline_string(comps));
            }
            CaseOutcome::Unspecified(Err(err_msg)) => {
                println!();
                println!(
                    "Unspecified results for {}.  This error was generated:",
                    path_string
                );
                println!("{}", err_msg);
            }
            _ => {}
        }
    }
    // Report all the failures second
    for case in run_cases {
        let path_string = fail_str(&case.name());
        match case.outcome() {
            CaseOutcome::ParseError(err_msg) => {
                println!();
                println!("{} returned an unexpected parse error:", path_string);
                println!("{}", err_msg);
            }
            CaseOutcome::Specified { expected, actual } if expected != actual => {
                match (expected, actual) {
                    // If the comps are different, print a fancy diff between the two sets.
                    (Ok(exp_comps), Ok(act_comps)) => {
                        println!();
                        println!("{} produced the wrong comps:", path_string);
                        println!(
                            "{}",
                            Changeset::new(
                                &Comp::multiline_string(exp_comps),
                                &Comp::multiline_string(act_comps),
                                "\n"
                            )
                        );
                    }
                    // If the errors are different, print a fancy diff between the two messages
                    (Err(exp_msg), Err(act_msg)) => {
                        println!();
                        println!("{} produced the wrong error message:", path_string);
                        println!("{}", Changeset::new(exp_msg, act_msg, "\n"));
                    }
                    // If we got an error but expected comps
                    (Ok(exp_comps), Err(act_msg)) => {
                        println!();
                        println!(
                            "{} returned an unexpected error (expected {} comps):",
                            path_string,
                            exp_comps.len()
                        );
                        println!("{}", act_msg);
                    }
                    // If we got comps but expected an error message
                    (Err(exp_msg), Ok(act_comps)) => {
                        println!();
                        println!(
                            "{} returned {} comps, but we expected this error message:",
                            path_string,
                            act_comps.len()
                        );
                        println!("{}", exp_msg);
                    }
                }
            }
            // Everything else isn't a failure
            CaseOutcome::Parsed
            | CaseOutcome::Ignored
            | CaseOutcome::Unspecified(_)
            | CaseOutcome::Specified { .. } => {}
        }
    }
}

/// Generate and print a summary string for the tests.  Returns the outcome of the test suite
fn print_summary_string(completed_tests: &[RunTestCase]) -> Outcome {
    // Count the test categories
    let mut num_ok = 0;
    let mut num_ignored = 0;
    let mut num_failures = 0;
    let mut num_unspecified = 0;
    for case in completed_tests {
        match case.outcome() {
            CaseOutcome::Parsed => num_ok += 1,
            CaseOutcome::Specified { actual, expected } if actual == expected => num_ok += 1,

            CaseOutcome::Ignored => num_ignored += 1,
            CaseOutcome::Unspecified(_) => num_unspecified += 1,

            CaseOutcome::ParseError(_) | CaseOutcome::Specified { .. } => num_failures += 1,
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
        "test result: {}. {} passed; {} unspecified; {} failed; {} ignored",
        outcome.colored_string(),
        num_ok,
        num_unspecified,
        num_failures,
        num_ignored,
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
    source: CaseSource,
    ignored: bool,
    expected_results: ExpectedResult,
    // pinned_duration: Duration,
    // last_duration: Duration,
}

impl UnrunTestCase {
    fn name(&self) -> String {
        self.source.name()
    }
}

#[derive(Debug)]
struct RunTestCase {
    base: UnrunTestCase,
    // duration: Duration,
    actual_result: ActualResult,
}

impl RunTestCase {
    fn outcome(&self) -> CaseOutcome {
        use ActualResult as ActR;
        use ExpectedResult as ExpR;

        match (&self.expected_results, &self.actual_result) {
            (_, ActR::Ignored) => {
                assert!(self.ignored);
                CaseOutcome::Ignored
            }
            (ExpR::NoCompsGiven, ActR::Parsed) => unreachable!(),
            (ExpR::NoCompsGiven, act_r) => CaseOutcome::Unspecified(act_r.comps_or_err()),
            (ExpR::Parsed, ActR::Parsed) => CaseOutcome::Parsed,
            (ExpR::Parsed, ActR::Comps(_)) => unreachable!(),
            (ExpR::Parsed, ActR::Error(e)) => CaseOutcome::ParseError(e),
            (expected, actual) => CaseOutcome::Specified {
                expected: expected.comps_or_err(),
                actual: actual.comps_or_err(),
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

#[derive(Debug)]
enum CaseSource {
    /// This case was loaded from a TOML file.
    ///
    /// NOTE: The path is relative to the `monument/` directory
    TomlFile(PathBuf),
    /// This case was loaded from a section of another file
    SectionOfFile {
        file_path: PathBuf,
        /// A unique name for this test within the file
        section_name: String,
        spec: String,
        music_file: Option<String>,
    },
}

impl CaseSource {
    /// Generate the unique name for this test.  This is the file's path (always delimited with '/'
    /// so the names are consistent across all platforms), optionally extended with a section name
    /// (if the same file contains many test cases).
    fn name(&self) -> String {
        match &self {
            Self::TomlFile(path) => path.to_slash_lossy(),
            Self::SectionOfFile {
                file_path,
                section_name,
                ..
            } => format!("{}: {}", file_path.to_slash_lossy(), section_name),
        }
    }
}

/// The possible results we'd expect from a test case
#[derive(Debug, PartialEq, Eq)]
enum ExpectedResult {
    /// No comps were specified (only makes sense for `expected_results`)
    NoCompsGiven,
    /// A test case that is only parsed, but no search run
    Parsed,
    /// The test ends with an error, which renders to this string
    Error(String),
    /// The test produces compositions
    Comps(Vec<Comp>),
}

/// The possible results from running a test case
#[derive(Debug, PartialEq, Eq)]
enum ActualResult {
    /// The result of a test run that was ignored (only makes sense for `actual_results`)
    Ignored,
    /// A test case that is only parsed, but no search run
    Parsed,
    /// The test ends with an error, which renders to this string
    Error(String),
    /// The test produces compositions
    Comps(Vec<Comp>),
}

impl ExpectedResult {
    fn comps_or_err(&self) -> CompOrErr {
        match self {
            Self::Error(e) => Err(e),
            Self::Comps(comps) => Ok(comps),
            _ => panic!(),
        }
    }
}

impl ActualResult {
    fn comps_or_err(&self) -> CompOrErr {
        match self {
            Self::Error(e) => Err(e),
            Self::Comps(comps) => Ok(comps),
            _ => panic!(),
        }
    }
}

/// The outcomes of a test, corresponding to what's printed to the console.
#[derive(Debug, Clone, Copy)]
enum CaseOutcome<'case> {
    Parsed,
    ParseError(&'case str),
    Ignored,
    Unspecified(CompOrErr<'case>),
    Specified {
        expected: CompOrErr<'case>,
        actual: CompOrErr<'case>,
    },
}

type CompOrErr<'case> = Result<&'case [Comp], &'case str>;

impl CaseOutcome<'_> {
    fn colored_string(self) -> ColoredString {
        match self {
            Self::Parsed => "parsed".color(Color::Green),
            Self::Ignored => "ignored".color(Color::Yellow),
            Self::Unspecified(_) => "unspecified".color(UNSPECIFIED_COLOR),
            Self::Specified { expected, actual } if expected == actual => ok_string(),
            Self::Specified { .. } | Self::ParseError(_) => fail_string(),
        }
    }
}

/// A simplified version of [`monument::Comp`] that can be easily (de)serialised.
#[derive(Debug, Clone, Eq, PartialEq, Deserialize, Serialize)]
struct Comp {
    length: usize,
    string: String,
    #[serde(deserialize_with = "de_avg_score")]
    avg_score: OrderedFloat<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    part_head: Option<String>, // Part head for multi-part compositions
}

impl Comp {
    fn multiline_string(comps: &[Self]) -> String {
        if comps.is_empty() {
            "<no comps>".to_owned()
        } else {
            comps.iter().map(Self::to_string).join("\n")
        }
    }

    /// Round an `f32` to 15 significant binary decimal places.  This is almost always enough to
    /// remove any floating-point rounding errors, but not enough that obviously different scores
    /// become the same.
    fn round_score(score: f32) -> OrderedFloat<f32> {
        // The least significant 4 bits of the mantissa will be set to 0s
        let rounding_factor = f32::powi(2.0, 15);
        let rounded_score = (score * rounding_factor).round() / rounding_factor;
        OrderedFloat(rounded_score)
    }
}

impl Comp {
    fn new(source: &monument::Comp, query: &monument::Query) -> Self {
        Self {
            length: source.length,
            string: source.call_string(query),
            avg_score: Self::round_score(source.avg_score.0),
            // Only store part heads for multi-part strings
            part_head: query
                .is_multipart()
                .then(|| source.part_head(query).to_string()),
        }
    }
}

impl Display for Comp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:>4}/{:>8.5}", self.length, self.avg_score)?;
        if let Some(p) = &self.part_head {
            write!(f, "/{}", p)?;
        }
        write!(f, ": {}", self.string)
    }
}

fn de_avg_score<'de, D: Deserializer<'de>>(de: D) -> Result<OrderedFloat<f32>, D::Error> {
    f32::deserialize(de).map(Comp::round_score)
}

//////////////
// MARKDOWN //
//////////////

/// Utilities for extracting test cases from markdown files
mod markdown {
    use std::ops::Deref;

    use itertools::Itertools;
    use pulldown_cmark::{CodeBlockKind, Event, HeadingLevel, Parser, Tag};

    /// Extract `(case_name, case_toml)` pairs from a markdown string.  Each test comes from a
    /// fenced `toml` code region, and is labelled by which headers it's in (ignoring the `h1`
    /// header, which is assumed to be at the top of the file).
    pub fn extract_cases(markdown: &str) -> Vec<(String, String)> {
        let mut cases = Vec::new();
        // This represents the nested sections in which the parser head is situated.  The
        // `HeadingLevel`s are expected to be strictly increasing.  For example, in the following
        // TOML file:
        //
        // ```toml
        // # Title
        //
        // ## Chapter 1
        //
        // ### Paragraph 1.1
        //
        // ## Chapter 2
        //
        // #### Paragraph 2.1
        //
        // <--- Parser head is here
        //
        // #### Paragraph 2.2
        // ```
        //
        // the header path would be `[(H1, "Title"), (H2, "Chapter 2"), (H4, "Paragraph 2.1")]`
        let mut header_path = Vec::<(HeadingLevel, String)>::new();

        let mut parser = Parser::new(markdown);
        while let Some(event) = parser.next() {
            match &event {
                // Starting a heading of some level.  In this case, read the level (h1, h2, etc.)
                // and the contained text and updating `header_path` appropriately.  Thus, any
                // contained test cases will be named correctly
                Event::Start(tag @ Tag::Heading(new_level, _, _)) => {
                    let title = read_text_contained_in_tag(&mut parser, tag);
                    // End any header sections which are at least as deep as this header.  For
                    // example, in this case:
                    // ```toml
                    // # Title
                    //
                    // ## Chapter 1
                    //
                    // ### Paragraph 1.1
                    //
                    // ## Chapter 2  <-- parsing this tag
                    // ```
                    // we should remove `Chapter 1` and `Paragraph 1.1` (because they have level
                    // >=H2), leaving the path as `Title`.  Once the new header is pushed, the new
                    // path becomes `Title/Chapter 1`.
                    loop {
                        match header_path.last() {
                            None => break, // Removed all headers, new header is always valid
                            Some((level, _title)) if level < new_level => break, // Header is too big
                            Some(_) => {
                                header_path.pop();
                            }
                        }
                    }
                    header_path.push((*new_level, title));
                }

                // Starting a TOML code block.  In this case, we read the contained text and add it
                // as a new test case
                Event::Start(tag @ Tag::CodeBlock(CodeBlockKind::Fenced(lang)))
                    if lang.deref() == "toml" =>
                {
                    let name = header_path
                        .iter()
                        .filter(|(level, _)| *level != HeadingLevel::H1)
                        .map(|(_level, name)| name)
                        .join("::");
                    let toml = read_text_contained_in_tag(&mut parser, tag);
                    cases.push((name, toml));
                }

                // Any other events aren't interesting
                _ => {}
            }
        }
        cases
    }

    /// Read text/code elements from a [`Parser`] until a given [`Tag`] ends.
    ///
    /// # Panics
    ///
    /// Panics if the file ends before the [`Tag`] is closed.
    fn read_text_contained_in_tag(parser: &mut Parser, closing_tag: &Tag) -> String {
        let mut text_in_tag = String::new();
        loop {
            let event = parser.next().expect("Found EOF before closing tag");
            match &event {
                // TODO: Handle a case where multiple instances of the `closing_tag` are nested (in
                // this case we should keep a counter so we can only return on the correct instance
                // of `closing_tag`).  However, the tags we need (headers and code blocks) can't be
                // nested so this is fine.
                Event::End(tag) if tag == closing_tag => return text_in_tag,
                Event::Code(text) | Event::Text(text) => text_in_tag.push_str(text),
                _ => {}
            }
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
