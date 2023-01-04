//! Code shared between both the test and benchmark runners

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use anyhow::Context;
use serde::{Deserialize, Serialize};

pub fn load_cases(
    dirs: &[&str],
    ignore_path: impl AsRef<Path>,
) -> anyhow::Result<Vec<(PathFromMonument, bool)>> {
    let ignore_file = load_ignore(ignore_path.as_ref())?;
    // Collect the individual test sources from every suite
    let mut test_sources: Vec<(PathFromMonument, bool)> = Vec::new();
    for path in dirs {
        let path_relative_to_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);

        // Walk the directory for test cases (i.e. `*.toml` files)
        for entry in walkdir::WalkDir::new(&path_relative_to_cargo_toml) {
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
                    test_sources.push((path_from_monument, is_ignored));
                }
                // Directories should always be ignored
                None => {}
                // Ignore any files that aren't `.toml` or `.md`
                _ => println!("Ignoring {:?}", path_from_monument.path),
            }
        }
    }
    Ok(test_sources)
}

fn load_ignore(path: &Path) -> anyhow::Result<HashMap<PathFromMonument, IgnoreReason>> {
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
