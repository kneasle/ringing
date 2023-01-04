//! Code shared between both the test and benchmark runners

use std::{
    collections::{HashMap, HashSet},
    io::Read,
    path::{Path, PathBuf},
    process::Stdio,
    time::{Duration, Instant},
};

use anyhow::Context;
use regex::Regex;
use serde::{Deserialize, Serialize};

pub fn load_cases<D>(
    dirs: &[&str],
    ignore_path: &str,
    mut data_fn: impl FnMut(&PathFromMonument, bool) -> D,
) -> anyhow::Result<Vec<UnrunTestCase<D>>> {
    let ignore_file = load_ignore(ignore_path)?;
    // Collect the individual test sources from every suite
    let mut cases: Vec<UnrunTestCase<D>> = Vec::new();
    for path in dirs {
        let path_relative_to_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);

        // Walk the directory for test cases (i.e. `*.toml` files)
        for entry in walkdir::WalkDir::new(path_relative_to_cargo_toml) {
            // Get the path of the file, relative to the `monument` directory
            let entry = entry.context("Error reading directory")?;
            let extension = entry.path().extension().and_then(|s| s.to_str());
            let path_from_monument = PathFromMonument {
                path: entry.path().components().skip(1).collect::<PathBuf>(),
            };

            match extension {
                Some("toml") => {
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
                Stdio::null()
            })
            .spawn()
            .unwrap();
        let mut output = Vec::new();
        cmd.stdout.unwrap().read_to_end(&mut output).unwrap();
        let duration = start.elapsed();

        // Strip color codes from the output
        //
        // ANSI colors are 'esc key' (1b in hex) followed by `[` then some codes, finishing
        // with `m`.  We use `.*?` to consume anything, but stopping at the first `m` (`.*?` is
        // the non-greedy version of `.*`)
        let output = String::from_utf8_lossy(&output).into_owned();
        let ansi_escape_regex = Regex::new("\x1b\\[.*?m").unwrap();
        let color_free_output = ansi_escape_regex.replace_all(&output, "").into_owned();

        RunTestCase {
            base: self,
            duration,
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
    // Wrap the paths in `PathFromMonument`s
    Ok(ignore_map
        .into_iter()
        .map(|(path, ign)| (PathFromMonument { path }, ign))
        .collect())
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

///////////
// PATHS //
///////////

/// A path relative to Monument's home directory.  The test runner doesn't run from this directory,
/// instead running in the same directory as `monument_cli`'s `Cargo.toml` file.  A
/// `PathFromMonument` `p` can be converted to a standard `PathBuf` with `PathBuf::from(p)`.
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(transparent)]
pub struct PathFromMonument {
    path: PathBuf,
}

const PATH_TO_MONUMENT_DIR: &str = "../";

impl PathFromMonument {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().to_owned(),
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
        write!(f, "{}", self.path.as_os_str().to_string_lossy())
    }
}
