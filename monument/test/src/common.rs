//! Code that's common between the test and benchmark runners (`test.rs` and `bench.rs`,
//! respectively).

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display, Formatter},
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use anyhow::Context;
use colored::{Color, ColoredString, Colorize};
use difference::Changeset;
use itertools::Itertools;
use monument_cli::{DebugOption, Environment};
use ordered_float::OrderedFloat;
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
const TEST_DIRS: [(&str, SuiteState); 2] = [
    ("test/cases/", SuiteState::Run), // Test cases which we expect to succeed
    ("examples/", SuiteState::Parse), // Examples to show how Monument's TOML format works
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
                ChangeType::New => println!("     (new) {:?}", unspecified_str(&name)),
                ChangeType::Fail => println!("    (fail) {:?}", fail_str(&name)),
                ChangeType::Removed => println!(" (removed) {:?}", name.yellow().bold()),
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
enum SuiteState {
    Run,
    Parse,
}

/// Determine where all the tests should come from, and how they should be run
fn collect_sources() -> anyhow::Result<Vec<(PathBuf, SuiteState)>> {
    // Collect the individual test sources from every suite
    let mut test_sources: Vec<(PathBuf, SuiteState)> = Vec::new();
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
fn sources_to_cases(sources: Vec<(PathBuf, SuiteState)>) -> anyhow::Result<Vec<UnrunTestCase>> {
    // Combine them with the auxiliary files (results, ignore, timings, etc.) to get the
    // `UnrunTestCase`s
    let mut results = load_results(EXPECTED_RESULTS_PATH)?;
    let mut ignore = load_ignore()?;

    let mut unrun_test_cases = Vec::new();
    for (path, state) in sources {
        // Load the values from the ignore/result files
        let ignore_value = ignore.remove(&path);
        let results_value = results.remove(&path);
        // Combine everything into an `UnrunTestCase`
        let ignored = match ignore_value {
            Some(IgnoreReason::MusicFile) => {
                assert!(
                    results_value.is_none(),
                    "Music file {:?} shouldn't have results",
                    path
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
            path,
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
type ResultsFile = BTreeMap<PathBuf, ResultsFileEntry>;

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
enum ResultsFileEntry {
    Error { error_message: String },
    Comps { comps: Vec<Comp> },
}

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
    let results_file: ResultsFile = serde_json::from_str(&toml)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(results_file)
}

fn write_actual_results(cases: Vec<RunTestCase>) -> anyhow::Result<()> {
    let actual_results: ResultsFile = cases
        .into_iter()
        .filter_map(|case| {
            let RunTestCase {
                base,
                actual_result,
            } = case;
            let entry = match actual_result {
                ActualResult::Ignored | ActualResult::Parsed => return None, // If no result, skip
                ActualResult::Error(error_message) => ResultsFileEntry::Error { error_message },
                ActualResult::Comps(comps) => ResultsFileEntry::Comps { comps },
            };
            Some((base.path, entry))
        })
        .collect();
    write_results(&actual_results, ACTUAL_RESULTS_PATH)
}

fn write_results(results: &ResultsFile, path: &str) -> anyhow::Result<()> {
    let unformatted_json = serde_json::to_string(results)
        .with_context(|| format!("Error serialising results file {:?}", path))?;
    let formatted_json = goldilocks_json_fmt::format_within_width(&unformatted_json, 120)
        .expect("`serde_json` should emit valid JSON");

    let path_from_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
    std::fs::write(path_from_cargo_toml, formatted_json.as_bytes())
        .with_context(|| format!("Error writing results to {:?}", path))
}

////////////////////////
// RUNNING TEST CASES //
////////////////////////

/// Run a test case (skipping ignored cases), determining its [`TestResult`].  Prints a status line
/// once finished.
fn run_test(case: UnrunTestCase) -> RunTestCase {
    // Skip any ignored tests
    if case.ignored {
        return RunTestCase {
            base: case,
            actual_result: ActualResult::Ignored,
        };
    }

    // Determine the source of this test
    let toml_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(&case.path);

    let no_search = case.expected_results == ExpectedResult::Parsed;

    // Run Monument
    let options = monument_cli::args::Options {
        debug_option: no_search.then(|| DebugOption::StopBeforeSearch),
        ..Default::default()
    };
    let monument_result = monument_cli::run(&toml_path, &options, Environment::TestHarness);
    // Convert Monument's `Result` into a `Results`
    let actual_result = match monument_result {
        // Successful search run
        Ok(Some(result)) => {
            assert!(!no_search);
            // Convert compositions and sort them by score, resolving tiebreaks using the comp string.
            // This guarantees a consistent ordering even if Monument's output order is non-deterministic
            let mut comps = result.comps.iter().map(Comp::new).collect_vec();
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

    let run_case = RunTestCase {
        base: case,
        actual_result,
    };
    println!(
        "{} ... {}",
        run_case.name(),
        run_case.outcome().colored_string()
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
fn print_summary_string(completed_tests: &[RunTestCase], duration: Duration) -> Outcome {
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
    ignored: bool,
    expected_results: ExpectedResult,
    // pinned_duration: Duration,
    // last_duration: Duration,
}

#[derive(Debug)]
struct RunTestCase {
    base: UnrunTestCase,
    // duration: Duration,
    actual_result: ActualResult,
}

impl UnrunTestCase {
    fn name(&self) -> String {
        path_to_string(&self.path)
    }
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
    fn new(source: &monument::Composition) -> Self {
        let is_multipart = !source.part_head().is_rounds();
        Self {
            length: source.length(),
            string: source.call_string(),
            avg_score: Self::round_score(source.average_score()),
            // Only store part heads for multi-part strings
            part_head: is_multipart.then(|| source.part_head().to_string()),
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

///////////
// UTILS //
///////////

fn de_avg_score<'de, D: Deserializer<'de>>(de: D) -> Result<OrderedFloat<f32>, D::Error> {
    f32::deserialize(de).map(Comp::round_score)
}

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
