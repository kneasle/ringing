//! Code shared between both the test and benchmark runners

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::{Path, PathBuf},
    process::Stdio,
    time::{Duration, Instant},
};

use anyhow::Context;
use path_slash::PathExt;
use regex::Regex;
use serde::{Deserialize, Serialize};

pub fn load_cases<D>(
    dirs: &[&str],
    ignore_path: &str,
    filter: Option<&str>,
    mut data_fn: impl FnMut(&PathFromMonument, bool) -> D,
) -> anyhow::Result<Vec<UnrunTestCase<D>>> {
    let ignore_file = load_ignore(ignore_path)?;
    let filter_regex = get_filter_regex(filter)?;
    // Collect the individual test sources from every suite
    let mut cases: Vec<UnrunTestCase<D>> = Vec::new();
    for path in dirs {
        let path_relative_to_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
        // Walk the directory for test cases (i.e. `*.toml` files)
        for entry in walkdir::WalkDir::new(path_relative_to_cargo_toml) {
            // Get the path of the file, relative to the `monument` directory
            let entry = entry.context("Error reading directory")?;
            let extension = entry.path().extension().and_then(|s| s.to_str());
            let path_from_monument =
                PathFromMonument::new(entry.path().components().skip(1).collect::<PathBuf>());

            match extension {
                Some("toml") => {
                    if !filter_regex.is_match(&entry.path().to_string_lossy()) {
                        continue; // Skip any files which aren't matched by the filter
                    }

                    let is_ignored = match ignore_file.get(&path_from_monument) {
                        Some(IgnoreReason::MusicFile) => continue, // Skip music files completely
                        Some(IgnoreReason::ExplicitIgnore) => true,
                        None => false,
                    };
                    cases.push(UnrunTestCase {
                        data: data_fn(&path_from_monument, is_ignored),
                        ignored: is_ignored,
                        path: path_from_monument,
                    });
                }
                // Directories should always be ignored
                None => {}
                // Ignore any files that aren't `.toml` or `.md`
                _ => println!("Ignoring {:?}", path_from_monument.path),
            }
        }
    }
    Ok(cases)
}

pub fn get_filter_regex(filter: Option<&str>) -> Result<Regex, anyhow::Error> {
    let filter = filter.unwrap_or(".*");
    Regex::new(&format!("^{filter}$")).context("Error parsing filter")
}

#[derive(Debug)]
pub struct UnrunTestCase<D> {
    pub path: PathFromMonument,
    pub ignored: bool,
    pub data: D,
}

#[derive(Debug)]
pub struct RunTestCase<D> {
    pub base: UnrunTestCase<D>,
    pub duration: Duration,
    pub panicked: bool,
    pub output: Option<String>,
}

impl<D> UnrunTestCase<D> {
    pub fn name(&self) -> String {
        self.path.to_string()
    }
}

impl<D> std::ops::Deref for RunTestCase<D> {
    type Target = UnrunTestCase<D>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<D> std::ops::Deref for UnrunTestCase<D> {
    type Target = D;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<D> UnrunTestCase<D> {
    pub fn run(self, args: &[&str], display_stderr: bool) -> RunTestCase<D> {
        if self.ignored {
            return RunTestCase {
                base: self,
                duration: Duration::ZERO,
                panicked: false,
                output: None,
            };
        }

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
        let toml_path = self.path.relative_to_cargo_toml();
        let start = Instant::now();
        let cmd = std::process::Command::new(monument_cli_path)
            .arg(&*toml_path.as_os_str().to_string_lossy())
            .args(args)
            .stdout(Stdio::piped())
            .stderr(if display_stderr {
                Stdio::inherit()
            } else {
                Stdio::piped()
            })
            .spawn()
            .unwrap();
        let output = cmd.wait_with_output().unwrap();
        let duration = start.elapsed();

        let panicked = match output.status.code().unwrap() {
            0 => false,   // Success
            255 => false, // Expected failure (i.e. a Monument error message)
            101 => true,  // Panic
            x => panic!("Unexpected return code: {x}"),
        };
        let output_text = if panicked {
            &output.stderr
        } else {
            &output.stdout
        };
        // Strip color codes from the output
        //
        // ANSI colors are 'esc key' (1b in hex) followed by `[` then some codes, finishing
        // with `m`.  We use `.*?` to consume anything, but stopping at the first `m` (`.*?` is
        // the non-greedy version of `.*`)
        let output = String::from_utf8_lossy(output_text).into_owned();
        let ansi_escape_regex = Regex::new("\x1b\\[.*?m").unwrap();
        let color_free_output = ansi_escape_regex.replace_all(&output, "").into_owned();

        RunTestCase {
            base: self,
            duration,
            panicked,
            output: Some(color_free_output),
        }
    }
}

//////////////////
// IGNORE FILES //
//////////////////

fn load_ignore(path: &str) -> anyhow::Result<HashMap<PathFromMonument, IgnoreReason>> {
    // Load the `IgnoreFile`
    let full_ignore_path = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
    let ignore_toml = std::fs::read_to_string(&full_ignore_path)
        .with_context(|| format!("Error loading ignore file ({:?})", full_ignore_path))?;
    let ignore_file: IgnoreFile = toml::from_str(&ignore_toml)
        .with_context(|| format!("Error parsing ignore file ({:?})", full_ignore_path))?;
    // Flatten the `IgnoreFile` into a single `HashMap`
    let mut ignore_map: HashMap<PathFromMonument, IgnoreReason> = ignore_file
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
    // Wrap the paths in `PathFromMonument`s
    Ok(ignore_map)
}

/// The contents of the `ignore.toml` file, found at [`IGNORE_PATH`].
#[derive(Debug, Deserialize)]
struct IgnoreFile {
    ignore: HashSet<PathFromMonument>,
    music_files: HashSet<PathFromMonument>,
}

/// Reasons why a given test file could be ignored
#[derive(Debug, Clone, Copy)]
enum IgnoreReason {
    /// The file was explicitly ignored to suppress a failing test
    ExplicitIgnore,
    /// The file was a music file, and never should have been tested in the first place
    MusicFile,
}

//////////////////
// RESULT FILES //
//////////////////

/// The contents of a results file.  We use a [`BTreeMap`] so that the test cases are always
/// written in a consistent order (i.e. alphabetical order by file path), thus making the diffs
/// easier to digest.
pub type ResultsFile<T> = BTreeMap<PathFromMonument, T>;

pub fn load_results<'s, T: Deserialize<'s>>(
    path: impl AsRef<Path>,
    toml_buf: &'s mut String,
) -> anyhow::Result<ResultsFile<T>> {
    let full_path = PathFromMonument::new(path).relative_to_cargo_toml();
    if !full_path.exists() {
        return Ok(ResultsFile::new()); // Return empty results if file doesn't exist
    }

    *toml_buf = std::fs::read_to_string(&full_path)
        .with_context(|| format!("Error loading results file ({:?})", full_path))?;
    let results_file: ResultsFile<T> = toml::from_str(toml_buf)
        .with_context(|| format!("Error parsing results file ({:?})", full_path))?;
    Ok(results_file)
}

pub fn write_results<T: Serialize>(results: &ResultsFile<T>, path: &str) -> anyhow::Result<()> {
    let toml = toml::to_string_pretty(results)
        .with_context(|| format!("Error serialising results file {:?}", path))?;

    let path_from_cargo_toml = PathFromMonument::new(path).relative_to_cargo_toml();
    std::fs::write(path_from_cargo_toml, toml.as_bytes())
        .with_context(|| format!("Error writing results to {:?}", path))
}

///////////
// PATHS //
///////////

/// A path relative to Monument's home directory.  The test runner doesn't run from this directory,
/// instead running in the same directory as `monument_cli`'s `Cargo.toml` file.  A
/// `PathFromMonument` `p` can be converted to a standard `PathBuf` with `PathBuf::from(p)`.
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(transparent)]
pub struct PathFromMonument {
    /// String representing the path, always using forward slashes
    pub path: String,
}

const PATH_TO_MONUMENT_DIR: &str = "../";

impl PathFromMonument {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().to_slash_lossy().into_owned(),
        }
    }

    pub fn relative_to_cargo_toml(&self) -> PathBuf {
        let mut path = PathBuf::from(PATH_TO_MONUMENT_DIR);
        path.push(&self.path);
        path
    }
}

impl std::fmt::Display for PathFromMonument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)
    }
}
