//! Code that's common between the test and benchmark runners (`test.rs` and `bench.rs`,
//! respectively).

use std::path::PathBuf;

use anyhow::Context;
use colored::Colorize;
use path_slash::PathExt;

/// The path from `monument_cli`'s `Cargo.toml` file to the `monument/` directory.
///
/// Custom test/bench harnesses are always given a `$PWD` of the directory containing that
/// library's `Cargo.toml` directory, but the `monument/` directory is a much more convenient root.
/// Therefore, we add this as a prefix to every path before loading files.
pub const PATH_TO_MONUMENT_DIR: &str = "../";

const SUITE_SOURCES: [(&str, SuiteStorage, SuiteType); 4] = [
    // Test cases which we expect to succeed
    ("test/cases/", SuiteStorage::Dir, SuiteType::Tests),
    // Tests which are expected to have errors, to check the error messages
    (
        "test/error-messages.md",
        SuiteStorage::DedicatedFile,
        SuiteType::Tests,
    ),
    // Examples to show how Monument's TOML format works
    ("examples/", SuiteStorage::Dir, SuiteType::Examples),
    // Long, real-world, test cases used to test Monument's performance
    ("test/bench/", SuiteStorage::Dir, SuiteType::Benches),
];

/// The maximum number of items allowed in the queue at one time
pub const QUEUE_LIMIT: usize = 10_000_000;

/// Determine where all the tests should come from, and how they should be run
pub fn collect_sources(run_type: RunType) -> anyhow::Result<Vec<(CaseSource, CaseUse)>> {
    // Collect the individual test sources from every suite
    let mut test_sources = Vec::new();
    for (path, storage, suite_type) in SUITE_SOURCES {
        // Determine whether tests in this directory should be run
        let case_use = match (run_type, suite_type) {
            // If `cargo test`, parse everything but only run `test/cases/`
            (RunType::Test, SuiteType::Tests) => CaseUse::Run,
            (RunType::Test, SuiteType::Benches) => CaseUse::Parse,
            (RunType::Test, SuiteType::Examples) => CaseUse::Parse,
            // If `cargo bench`, only run benchmarks and ignore everything else
            (RunType::Bench, SuiteType::Benches) => CaseUse::Run,
            (RunType::Bench, SuiteType::Tests | SuiteType::Examples) => {
                println!(
                    "{} {} (not benchmark suite)",
                    "ignoring".color(colors::IGNORED),
                    path
                );
                continue;
            }
        };

        // TODO: Make newtypes for the different relative paths?
        let path_relative_to_cargo_toml = PathBuf::from(PATH_TO_MONUMENT_DIR).join(path);
        match storage {
            SuiteStorage::Dir => {
                // Walk the directory for test cases (i.e. `*.toml` files)
                for entry in walkdir::WalkDir::new(&path_relative_to_cargo_toml) {
                    // Get the path of the file, relative to the `monument` directory
                    let entry = entry.context("Error reading directory")?;
                    if entry.path().extension().and_then(|s| s.to_str()) != Some("toml") {
                        continue; // Skip anything that isn't a TOML file
                    }
                    let file_path_relative_to_monument_dir =
                        entry.path().components().skip(1).collect::<PathBuf>();
                    test_sources.push((
                        CaseSource::TomlFile(file_path_relative_to_monument_dir),
                        case_use,
                    ));
                }
            }
            SuiteStorage::DedicatedFile => {
                // Parse the file as markdown
                let markdown =
                    std::fs::read_to_string(&path_relative_to_cargo_toml).with_context(|| {
                        format!(
                            "Error reading test suite file {:?}",
                            &path_relative_to_cargo_toml
                        )
                    })?;
                let file_path_relative_to_monument_dir = path_relative_to_cargo_toml
                    .components()
                    .skip(1)
                    .collect::<PathBuf>();
                for (heading_path, spec) in markdown::extract_cases(&markdown) {
                    test_sources.push((
                        CaseSource::SectionOfFile {
                            file_path: file_path_relative_to_monument_dir.clone(),
                            section_name: heading_path,
                            spec,
                            music_file: None,
                        },
                        case_use,
                    ));
                }
            }
        }
    }
    Ok(test_sources)
}

#[derive(Debug)]
pub enum CaseSource {
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
    pub fn name(&self) -> String {
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

#[derive(Debug, Clone, Copy)]
// `common.rs` is only imported by one of `test.rs` or `bench.rs` at a time, so Rust's otherwise
// excellent dead code detection will flag this as a false positive
#[allow(dead_code)]
pub enum RunType {
    Test,
    Bench,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseUse {
    /// Run everything, including the test
    Run,
    /// Run all of the test, except the search itself
    Parse,
}

/// The different ways a test suite can be stored
#[derive(Debug, Clone, Copy)]
enum SuiteStorage {
    /// The suite is a directory of single files, each of which is a test case
    Dir,
    /// The suite is a single markdown file containing a tree of headers, containing TOML code
    /// blocks for the spec files
    DedicatedFile,
    // TODO: Load examples in the guide
}

#[derive(Debug, Clone, Copy)]
enum SuiteType {
    Tests,
    Benches,
    Examples,
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

////////////
// COLORS //
////////////

pub mod colors {
    use colored::{Color, ColoredString, Colorize};

    pub fn unspecified_str(s: &str) -> ColoredString {
        s.color(UNSPECIFIED).bold()
    }

    pub fn fail_str(s: &str) -> ColoredString {
        s.color(FAIL).bold()
    }

    pub const OK: Color = Color::Green;
    pub const FAIL: Color = Color::BrightRed;
    pub const UNSPECIFIED: Color = Color::BrightBlue;
    pub const IGNORED: Color = Color::Yellow;
}
