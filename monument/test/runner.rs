use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
};

use colored::*;
use difference::Changeset;
use itertools::Itertools;
use monument_cli::DebugOption;
use ordered_float::OrderedFloat;
use serde::Deserialize;
use toml::Value;

/// The maximum number of chunks contained in the queue when generating comps
const QUEUE_LIMIT: usize = 10_000_000;

const IGNORE_FILE_NAME: &str = "_ignore.toml";
const RESULTS_FILE_NAME: &str = "_results.toml";

fn main() -> SuiteResult<()> {
    // Read blessedness for CLI args
    let blessedness = match std::env::args()
        .skip(1)
        .next()
        .borrow()
        .as_ref()
        .map(String::as_str)
    {
        None => Blessedness::Unblessed,
        Some("--bless") => Blessedness::JustUnspecified,
        Some("--bless-fails") => Blessedness::Failures,
        Some(x) => panic!(
            "Unknown arg {:?}.  Expected one of `--bless` or `--bless-fails`",
            x
        ),
    };

    monument_cli::init_logging(log::LevelFilter::Warn);

    // This always run in the directory of the `monument_cli` crate, i.e.
    // `$REPO_ROOT/monument/cli/`.  Hence, all paths must be relative to `monument_cli`'s
    // `Cargo.toml` file, so all paths start with `../` to reach the `monument/` directory.  This
    // file would be accessed as `../test/runner.rs`
    let suites = [
        run_tests_in_directory("../test/cases/", false)?,
        run_tests_in_directory("../examples/", true)?,
    ];

    // Collate failures and unspecified tests from all `Suite`s
    let mut unspecified = Vec::<(&Path, &[Comp])>::new();
    let mut failures = Vec::<(&Path, &TestErr)>::new();
    let mut num_ignored = 0;
    for suite in &suites {
        for case in &suite.test_cases {
            match &case.actual_result {
                Ok(TestOk::Ignored) => num_ignored += 1,
                Ok(TestOk::Unspecified(comps)) => unspecified.push((&case.file_path, comps)),
                Err(e) => failures.push((&case.file_path, e)),
                _ => {}
            }
        }
    }

    // Print a summary of the tests
    let total_num_tests = suites
        .iter()
        .map(|suite| suite.test_cases.len())
        .sum::<usize>();
    let num_unspecified = unspecified.len();
    let num_failures = failures.len();
    let num_ok = total_num_tests - num_ignored - num_unspecified - num_failures;
    println!();
    println!(
        "test result: {}. {} passed; {} unspecified; {} failed; {} ignored",
        if num_failures > 0 {
            fail_string()
        } else {
            ok_string()
        },
        num_ok,
        num_unspecified,
        num_failures,
        num_ignored,
    );

    // Report unspecified comps
    for (path, comps) in unspecified {
        let path_string = get_path_string(path).color(Color::BrightBlue).bold();
        println!();
        print!("Unspecified results for {}.  ", path_string);
        if blessedness >= Blessedness::JustUnspecified {
            println!("Writing them to `_results.toml`.")
        } else {
            println!("These were generated:");
            println!("{}", Comp::multiline_string(comps));
        }
    }

    // Report failures
    for (path, error) in failures {
        let path_string = get_path_string(path).color(Color::BrightRed).bold();
        match error {
            TestErr::Error(err) => {
                println!();
                println!("{} returned an unexpected error:", path_string);
                println!("{:#?}", err);
            }
            TestErr::CompMismatch { expected, actual } => {
                // Compute & print a diff between the comps.
                //
                // TODO: Nicer (side-by-side) diffing?
                println!();
                print!("{} produced the wrong comps", path_string);
                if blessedness >= Blessedness::Failures {
                    println!(".  Writing them to `_results.toml`.");
                } else {
                    println!(":");
                    println!(
                        "{}",
                        Changeset::new(
                            &Comp::multiline_string(expected),
                            &Comp::multiline_string(actual),
                            "\n"
                        )
                    );
                }
            }
        }
    }

    // Write back to the 'results' file(s)
    for suite in &suites {
        suite.bless(blessedness);
    }

    // Report to Cargo when tests fail
    if num_failures > 0 || num_unspecified > 0 {
        Err(SuiteError::TestsFailed)
    } else {
        Ok(())
    }
}

/// Given a test directory, run all the test cases contained within it.
fn run_tests_in_directory(
    dir_path: impl AsRef<Path>,
    stop_before_search: bool,
) -> SuiteResult<Suite> {
    let dir_path = dir_path.as_ref();
    // List the test cases
    let (cases, unused_results) = collect_tests(dir_path)?;
    let test_cases = cases
        .into_iter()
        .map(|(file_path, expected_result)| {
            run_and_print_test(file_path, expected_result, stop_before_search)
        })
        .collect_vec();
    Ok(Suite {
        dir_path: dir_path.to_owned(),
        test_cases,
        unused_results,
    })
}

/// Given a directory of `.toml` files (e.g. the `test/` or `examples/` folder), collect each
/// test case and match it up with its expected results.
fn collect_tests(dir_path: &Path) -> SuiteResult<(Vec<(PathBuf, ExpectedResult)>, ResultsFile)> {
    // Load the ignore/results files.  Both refer to file **name**s, without TOML extensions
    let mut buffer = String::new();
    let ignore_stems =
        read_toml_or_default::<IgnoreFile>(dir_path.join(IGNORE_FILE_NAME), &mut buffer)?
            .file_names
            .into_iter()
            .collect::<HashSet<_>>();
    let mut result_map: ResultsFile =
        read_toml_or_default(dir_path.join(RESULTS_FILE_NAME), &mut buffer)?;

    // Load test cases
    let mut test_cases = Vec::<(PathBuf, ExpectedResult)>::new();
    let contents =
        std::fs::read_dir(dir_path).map_err(|e| SuiteError::LoadDir(dir_path.to_owned(), e))?;
    for entry in contents {
        let entry = entry.map_err(|e| SuiteError::LoadDir(dir_path.to_owned(), e))?;
        let file_path = entry.path();

        let is_file = entry
            .file_type()
            .map_err(|e| SuiteError::LoadFile(file_path.clone(), e))?
            .is_file();
        let is_toml = file_path.extension().and_then(OsStr::to_str) == Some("toml");

        if is_file && is_toml {
            let file_stem = file_path.file_stem().and_then(OsStr::to_str);
            if matches!(file_stem, Some("_ignore" | "_results")) {
                continue; // Fully ignore '_ignore.toml' or '_results.toml' files
            }
            let should_ignore = file_stem.map_or(true, |stem| ignore_stems.contains(stem));
            let results_entry = file_stem.and_then(|stem| result_map.remove(stem));
            // Decide on the results against which we should test
            let results = match (should_ignore, results_entry) {
                (true, _) => ExpectedResult::Ignored,
                (false, None) => ExpectedResult::NoneGiven,
                (false, Some(results)) => ExpectedResult::Results(results),
            };
            // Add this test case
            test_cases.push((file_path, results));
        }
    }
    Ok((test_cases, result_map))
}

fn run_and_print_test(
    file_path: PathBuf,
    expected_result: ExpectedResult,
    stop_before_search: bool,
) -> TestCase {
    let debug_print = stop_before_search.then(|| DebugOption::StopBeforeSearch);

    // Run the test
    let actual_result = run_test(&file_path, &expected_result, debug_print);
    // Determine what should be printed after the '...'
    let result_string = match &actual_result {
        Ok(TestOk::Ignored) => "ignored".color(Color::Yellow),
        Ok(TestOk::Parsed) => "parsed".color(Color::Green),
        Ok(TestOk::Success) => ok_string(),
        Ok(TestOk::Unspecified(_)) => "unspecified".color(Color::Blue),
        Err(_) => fail_string(),
    };
    println!("{} ... {}", get_path_string(&file_path), result_string);
    // Return the finished test case
    TestCase {
        expected_result,
        actual_result,
        file_path,
    }
}

/// Run a test case (skipping ignored cases), determining its [`TestResult`]
fn run_test(
    file_path: &Path,
    expected_result: &ExpectedResult,
    debug_print: Option<DebugOption>,
) -> TestResult {
    if expected_result == &ExpectedResult::Ignored {
        return Ok(TestOk::Ignored); // Don't run ignored tests
    }
    // Run Monument and propagate the error if it occurs
    let run_result =
        monument_cli::run(file_path, debug_print, QUEUE_LIMIT).map_err(TestErr::Error)?;
    let result = match run_result {
        None => return Ok(TestOk::Parsed), // Parsing tests don't generate comps to check
        Some(r) => r,
    };
    // Determine the actual comps
    let mut comps = result.comps.iter().map(Comp::from).collect_vec();
    // Sort comps by their score, resolving tiebreaks by sorting by the comp string.  This
    // guarantees a consistent ordering even if Monument's output order is non-deterministic
    comps.sort_by_key(|comp| (comp.avg_score, comp.string.clone()));
    // Get the expected comps, if they exist
    let expected_comps = match &expected_result {
        ExpectedResult::Ignored => unreachable!("Ignored tests shouldn't be run"),
        ExpectedResult::NoneGiven => return Ok(TestOk::Unspecified(comps)),
        ExpectedResult::Results(expected_comps) => expected_comps,
    };
    // If expected comps were specified, check that Monument still gets them right
    if expected_comps == &comps {
        Ok(TestOk::Success) // Full integration test succeeds if the comps match
    } else {
        // If the integration tests don't match, then the case fails
        Err(TestErr::CompMismatch {
            expected: expected_comps.clone(),
            actual: comps,
        })
    }
}

/// Attempt to read a TOML file.  If there's an error _loading_ the file (e.g. because it doesn't
/// exist), return the `Ok(T::default())`.  If there's an error parsing the TOML, return an error
/// containing the problem and the path of the offending TOML file.
fn read_toml_or_default<'de, T: Deserialize<'de> + Default>(
    path: PathBuf,
    buffer: &'de mut String,
) -> SuiteResult<T> {
    match std::fs::read_to_string(&path) {
        Ok(contents) => {
            *buffer = contents;
            toml::from_str(buffer).map_err(|e| SuiteError::ParseToml(path, e))
        }
        // If we couldn't open the file, ignore the error and return a default value
        Err(_) => Ok(T::default()),
    }
}

/// Remove the leading `../` from a path, and convert it to UTF-8
fn get_path_string(path: &Path) -> String {
    path.components()
        .skip(1)
        .collect::<PathBuf>()
        .to_str()
        .unwrap_or("<non-utf-8 path>")
        .to_owned()
}

fn ok_string() -> ColoredString {
    "ok".color(Color::Green)
}

fn fail_string() -> ColoredString {
    "fail".color(Color::BrightRed)
}

///////////////////////
// SUITE-LEVEL TYPES //
///////////////////////

/// Struct to hold the contents of an `_ignore.toml` file
#[derive(Debug, Default, Deserialize)]
struct IgnoreFile {
    #[serde(rename = "ignore")]
    file_names: Vec<String>, // TODO: Make this `Vec<PathBuf>`?
}

/// Type of the contents of an `_ignore.toml` file
type ResultsFile = HashMap<String, Vec<Comp>>;

/// The result generated when a test suite is run
#[derive(Debug)]
struct Suite {
    test_cases: Vec<TestCase>,
    dir_path: PathBuf,
    unused_results: ResultsFile,
}

impl Suite {
    /// 'Bless' the results of this test suite run.  This updates the `_results.toml` file so that
    /// re-running the tests will automatically pass.
    fn bless(&self, blessedness: Blessedness) {
        let results = self.blessed_results(blessedness);
        let results_toml = Self::results_to_toml_value(results).to_string();
        let results_path = self.dir_path.join(RESULTS_FILE_NAME);
        std::fs::write(results_path, &results_toml).unwrap();
    }

    /// Returns a [`HashMap`] containing the tests which should be written back to `_results.toml`
    /// given a level of [`Blessedness`]
    fn blessed_results(&self, blessedness: Blessedness) -> HashMap<&str, &[Comp]> {
        let mut results = HashMap::new();
        // Add unused results
        for (file_name, comps) in &self.unused_results {
            results.insert(file_name.as_str(), comps.as_slice());
        }
        // Add results from test cases which got run
        for case in &self.test_cases {
            // Determine what comps to write back for this test case
            let comps_to_save = match &case.actual_result {
                // Never write ignored/parsing tests to `_results.toml`
                Ok(TestOk::Parsed | TestOk::Ignored) => None,
                // Always write successful tests back to `_results.toml`
                Ok(TestOk::Success) => {
                    let comps = case.expected_result.comps();
                    assert!(
                        comps.is_some(),
                        "`TestOk::Success` should only be generated when expected comps are given."
                    );
                    comps
                }
                // Write unspecified tests back for `--bless` and `--bless-fails`
                Ok(TestOk::Unspecified(actual_comps)) => {
                    (blessedness >= Blessedness::JustUnspecified).then(|| actual_comps.as_slice())
                }
                // Write 'incorrect' tests back only for `--bless-fails`
                Err(TestErr::CompMismatch { actual, expected }) => Some(
                    if blessedness >= Blessedness::Failures {
                        actual
                    } else {
                        expected
                    }
                    .as_slice(),
                ),
                Err(TestErr::Error(_)) => case.expected_result.comps(),
            };
            // Add this case back
            if let Some(comps) = comps_to_save {
                let file_path_str = case
                    .file_path
                    .file_stem()
                    .and_then(OsStr::to_str)
                    .expect("Non UTF-8 file found");
                results.insert(file_path_str, comps);
            }
        }
        results
    }

    fn results_to_toml_value(results: HashMap<&str, &[Comp]>) -> toml::Value {
        Value::Table(
            results
                .into_iter()
                .map(|(file_name, comps)| {
                    let comp_values = comps.iter().map(Comp::ser_to_toml_value).collect_vec();
                    (file_name.to_owned(), Value::Array(comp_values))
                })
                .collect(),
        )
    }
}

#[derive(Debug)]
enum SuiteError {
    /// Error when listing a given directory
    LoadDir(PathBuf, std::io::Error),
    /// Error when reading a test/example file
    LoadFile(PathBuf, std::io::Error),
    /// Error when parsing a test/example file
    ParseToml(PathBuf, toml::de::Error),
    /// There were failed unit tests
    TestsFailed,
}

type SuiteResult<T> = Result<T, SuiteError>;

/// The different levels that a user can 'bless' test results.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Blessedness {
    /// Preserve the original results (no CLI arg, default)
    Unblessed,
    /// Bless any unspecified results, but not failures.  Used to initialise new test cases.  CLI
    /// arg of `--bless`
    JustUnspecified,
    /// Bless the results of any failed tests.  Used e.g. when changing error messages or other
    /// small implementation details.  CLI arg of `--bless-fails`
    Failures,
}

//////////////////////
// CASE-LEVEL TYPES //
//////////////////////

/// A single test case for Monument
#[derive(Debug)]
struct TestCase {
    file_path: PathBuf,
    expected_result: ExpectedResult,
    actual_result: TestResult,
}

#[derive(Debug, Clone, Eq, PartialEq, Deserialize)]
struct Comp {
    length: usize,
    string: String,
    avg_score: OrderedFloat<f32>,
    part_head: Option<String>, // Part head for multi-part compositions
}

impl Comp {
    fn multiline_string(comps: &[Self]) -> String {
        comps.iter().map(Self::to_string).join("\n")
    }

    fn ser_to_toml_value(&self) -> Value {
        let mut comp_map = toml::map::Map::new();
        comp_map.insert("length".to_owned(), Value::Integer(self.length as i64));
        comp_map.insert("string".to_owned(), Value::String(self.string.clone()));
        if let Some(ph) = self.part_head.clone() {
            comp_map.insert("part_head".to_owned(), Value::String(ph));
        }
        comp_map.insert(
            "avg_score".to_owned(),
            Value::Float(self.avg_score.0 as f64),
        );
        Value::Table(comp_map)
    }
}

impl From<&monument::Comp> for Comp {
    fn from(source: &monument::Comp) -> Self {
        Self {
            length: source.length,
            string: source.display_string(),
            avg_score: source.avg_score,
            // Only store part heads for multi-part strings
            part_head: source
                .query
                .is_multipart()
                .then(|| source.part_head().to_string()),
        }
    }
}

impl Display for Comp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:>4}/{:>7.5}", self.length, self.avg_score,)?;
        if let Some(p) = &self.part_head {
            write!(f, "/{}", p)?;
        }
        write!(f, ": {}", self.string)
    }
}

/// The results of a [`TestCase`]
#[derive(Debug, Eq, PartialEq)]
enum ExpectedResult {
    /// The file should not be tested
    Ignored,
    /// The file should be tested, but no results were given
    NoneGiven,
    /// The file should be tested and its compared against this
    Results(Vec<Comp>),
}

impl ExpectedResult {
    fn comps(&self) -> Option<&[Comp]> {
        match self {
            ExpectedResult::Results(comps) => Some(comps),
            ExpectedResult::Ignored | ExpectedResult::NoneGiven => None,
        }
    }
}

/// The ways a test file can run without failing
#[derive(Debug)]
enum TestOk {
    /// The file was ignored
    Ignored,
    /// The file was successfully parsed but not run
    Parsed,
    /// The file ran and the comps matched
    Success,
    /// The file ran but no comps were specified
    Unspecified(Vec<Comp>),
}

/// The ways a test file can fail
#[derive(Debug)]
enum TestErr {
    /// Monument returned an error running the file
    Error(monument_cli::Error),
    /// Monument returned the wrong composition
    CompMismatch {
        expected: Vec<Comp>,
        actual: Vec<Comp>,
    },
}

type TestResult = Result<TestOk, TestErr>;
