//! Use the `examples/` folder as test cases/benchmarks

use std::{
    collections::HashMap,
    fmt::Write,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use colored::Colorize;
use glob::glob;
use itertools::Itertools;
use monument::Config;
use monument_cli::{
    spec::AbstractSpec,
    test_data::{CompResult, TestData},
};

pub fn main(max_runtime: Duration, is_bench: bool) -> std::io::Result<()> {
    let mut repo_root_path = std::env::current_dir()?;
    // This always runs in the `<project root>/cli` directory not the git repo
    // root so we pop the `/cli` dir off the end of the path
    repo_root_path.pop();
    let examples_dir = repo_root_path.join("examples/");

    // Read all the `.toml` files from `test-cases/`, and parse them into `Spec`s
    let mut test_file_glob = examples_dir
        .as_os_str()
        .to_owned()
        .into_string()
        .expect("Failed to convert OS string");
    test_file_glob.push_str("/**/*.toml");
    let mut test_cases = glob(&test_file_glob)
        .expect("Failed to parse glob")
        .filter_map(|file_path| TestCase::from_path(file_path.unwrap()))
        .collect_vec();

    // Sort the test cases by their expected runtime (i.e. run the faster tests first)
    test_cases.sort_by(|a, b| {
        let runtime_a = a.test_data.runtime_estimate;
        let runtime_b = b.test_data.runtime_estimate;
        runtime_a.partial_cmp(&runtime_b).unwrap()
    });

    // Repeatedly remove long tests until the runtime drops below the required amount.  This is a
    // really dumb quadratic time algorithm (which could easily be done in linear time) but I don't
    // care - this is **really** unlikely to ever be a bottleneck
    let max_runtime_secs = max_runtime.as_secs_f32();
    while test_cases
        .iter()
        .map(|c| c.test_data.runtime_estimate)
        .sum::<f32>()
        > max_runtime_secs
    {
        test_cases.pop();
    }

    // Load previous & pinned benchmark results
    let prev_bench_file_path = repo_root_path.join(".benches_prev.json");
    let prev_benches = load_saved_benches(&prev_bench_file_path).unwrap_or_else(HashMap::new);
    let pinned_benches = load_saved_benches(&repo_root_path.join(".benches_pinned.json"));

    // The bench results of this run will be accumulated here
    let mut bench_results: HashMap<String, f64> = prev_benches
        .iter()
        .map(|(k, v)| (k.to_owned(), v.as_secs_f64()))
        .collect();

    // Run the test cases
    let config = Config::default();
    for t in test_cases {
        // Get previous benchmark results
        let relative_path = pathdiff::diff_paths(&t.file_path, &repo_root_path).unwrap();
        let relative_path_str = relative_path.as_os_str().to_owned().into_string().unwrap();

        let prev_time = prev_benches.get(&relative_path_str).copied();
        let pinned_time = pinned_benches
            .as_ref()
            .and_then(|map| map.get(&relative_path_str).copied());

        // Run the test case
        let time = run_test_case(
            &relative_path,
            t,
            config.clone(),
            prev_time,
            pinned_time,
            is_bench,
        );
        bench_results.insert(relative_path_str, time.as_secs_f64());

        // Save the bench results to disk as the previous results for the next benchmark test.  We
        // do this after every test case, so that if the user terminates a benchmark run early,
        // those results will be preserved
        if is_bench {
            std::fs::write(
                &prev_bench_file_path,
                serde_json::to_string(&bench_results).unwrap(),
            )
            .unwrap();
        }
    }

    Ok(())
}

fn run_test_case(
    relative_path: &Path,
    test_case: TestCase,
    config: Config,
    prev_time: Option<Duration>,
    pinned_time: Option<Duration>,
    is_bench: bool,
) -> Duration {
    println!(
        "\nTesting {}",
        format!("{:?}", relative_path).white().bold()
    );

    // Run the test
    let arc_config = Arc::new(config);
    let spec = test_case.spec.to_spec(&arc_config).unwrap();
    let results = monument::compose(&spec, &arc_config);
    let mut comps = results
        .comps
        .iter()
        .map(|c| CompResult::from_comp(c, spec.layout()))
        .collect_vec();

    // Print the results
    let summary = summary_message(
        comps.len(),
        results.time_taken,
        prev_time,
        pinned_time,
        is_bench,
    );
    match test_case.test_data.results {
        // If there weren't any then we assume that this test case hasn't been made yet, so we
        // print out the results in such a way that they can be copy/pasted into the TOML file
        None => {
            println!("{} ({})", "No results".bright_cyan().bold(), summary);
            print_toml_string(comps);
        }

        // If the test file specifies some compositions, check that our results were compatible
        Some(expected_comps_float) => {
            let mut expected_comps = expected_comps_float
                .into_iter()
                .map(CompResult::from)
                .collect_vec();

            match are_results_compatible(&mut expected_comps, &mut comps) {
                // If the results are OK, say so and carry on
                Ok(()) => println!("{} ({})", "Results OK".bright_green().bold(), summary),
                // If the results are invalid, then print a longer explanation
                Err(error_messages) => {
                    println!("{} ({})", "FAILED TEST!".bright_red().bold(), summary);
                    println!("");
                    for m in error_messages {
                        println!("ERROR: {}", m);
                    }
                    println!("");
                    println!("These comps were generated:");
                    for c in comps {
                        println!(
                            "(len: {}, rank score: {}) {}",
                            c.length, c.ranking_score, c.call_string
                        );
                    }
                }
            }
        }
    }

    results.time_taken
}

/// Load the saved benchmark stats from `<repo_root>/.benches`.  Each item in the tuple is a
/// mapping from file paths (relative to the repo root) to the time taken in seconds.
fn load_saved_benches(path: &Path) -> Option<HashMap<String, Duration>> {
    let json = std::fs::read_to_string(path).ok()?;
    let float_map: HashMap<String, f64> = serde_json::from_str(&json).unwrap();
    Some(
        float_map
            .into_iter()
            .map(|(k, v)| (k, Duration::from_secs_f64(v)))
            .collect(),
    )
}

/// Check the compatibility of two sets of compositions.  This is essentially element-wise
/// equality, but taking into account that the order that comps are generated is non-deterministic.
fn are_results_compatible(
    expected_comps: &mut [CompResult],
    comps: &mut [CompResult],
) -> Result<(), Vec<String>> {
    let mut error_messages: Vec<String> = Vec::new();

    // Check that both comp lists are the same length
    if expected_comps.len() != comps.len() {
        error_messages.push(format!(
            "Expected {} comps, found {}",
            expected_comps.len(),
            comps.len()
        ));
    }

    // Make sure that the composition lists are sorted (which is a bit painful because floating
    // point numbers are pesky).  That way they can easily be split into worst-comps and
    // non-worst-comps sections
    expected_comps.sort_by_key(|c| c.ranking_score);
    comps.sort_by_key(|c| c.ranking_score);

    // Check that there are the right number of worst comps (these exact comps can be different if
    // the search space is searched in a different order)
    let worst_comp_score = expected_comps.first().unwrap().ranking_score;
    let num_worst_comps = comps
        .iter()
        .filter(|c| c.ranking_score == worst_comp_score)
        .count();
    let num_exp_worst_comps = expected_comps
        .iter()
        .filter(|c| c.ranking_score == worst_comp_score)
        .count();
    if num_exp_worst_comps != num_worst_comps {
        error_messages.push(format!(
            "Expected {} (worst) comps with score {}, got {}",
            num_exp_worst_comps, worst_comp_score, num_worst_comps
        ));
    }

    // Check that the set of non-worst compositions is (set) equal to what we expect.  By the
    // pigeon hole principle, it suffices to check that one of them is contained in the other
    // (we've already checked that the lengths are equal).
    for exp_comp in expected_comps
        .iter()
        .filter(|c| c.ranking_score != worst_comp_score)
    {
        if comps.iter().find(|c| *c == exp_comp).is_none() {
            error_messages.push(format!(
                "Didn't find comp '{}' (len: {}, rank score: {})",
                exp_comp.call_string, exp_comp.length, exp_comp.ranking_score
            ));
        }
    }

    // Print error messages, or an OK message
    if error_messages.is_empty() {
        Ok(())
    } else {
        Err(error_messages)
    }
}

fn summary_message(
    num_comps: usize,
    time_taken: Duration,
    prev_time: Option<Duration>,
    pinned_time: Option<Duration>,
    is_bench: bool,
) -> String {
    /// Format a scaling factor with fancy colouring
    fn color_scaling(factor: f64) -> String {
        let s = format!("{:.2}x", factor);
        if factor < 0.9 {
            // >10% slower is considered a slowdown, and should be red
            s.bright_red().bold().to_string()
        } else if factor > 1.1 {
            // >10% faster is considered a speedup, and should be green
            s.bright_green().bold().to_string()
        } else {
            // Otherwise, the time has not changed noticeably and so we don't colour the string
            s
        }
    }

    let mut s = format!("{} comps in ", num_comps);

    // Print the timing string, making it bold if we're benchmarking
    let time_string = format!("{:.2?}", time_taken);
    if is_bench {
        s.push_str(&time_string.white().bold().to_string());
    } else {
        s.push_str(&time_string);
    }

    // Print timing comparison (but only in benchmark mode)
    if is_bench {
        if let Some(prev) = prev_time {
            write!(
                s,
                ": {} prev",
                color_scaling(prev.as_secs_f64() / time_taken.as_secs_f64())
            )
            .unwrap();
        }
        if let Some(pinned) = pinned_time {
            // Change the delimiter depending on whether or not there was a previous time
            s.push_str(match prev_time {
                Some(_) => ", ",
                None => ": ",
            });
            write!(
                s,
                "{} pinned",
                color_scaling(pinned.as_secs_f64() / time_taken.as_secs_f64())
            )
            .unwrap();
        }
    }

    s
}

/// Print a test string that can be copied back into the TOML file to make the test pass
fn print_toml_string(comps: Vec<CompResult>) {
    println!(
        "No expected results found.  It looks like you want to copy this into the `.toml` file:"
    );
    println!("```toml");
    println!("results = [");
    for c in comps {
        println!(
            "    {{ length = {}, score = {}, call_string = {:?}, ranking_score = {} }},",
            c.length,
            c.score.to_radix(),
            c.call_string,
            c.ranking_score.to_radix()
        );
    }
    println!("]");
    println!("```");
}

/// The data required to run a test case
#[derive(Debug, Clone)]
struct TestCase {
    file_path: PathBuf,
    spec: AbstractSpec,
    test_data: TestData,
}

impl TestCase {
    fn from_path(file_path: PathBuf) -> Option<Self> {
        let mut spec = match AbstractSpec::read_from_file(&file_path) {
            Ok(f) => f,
            Err(e) => {
                println!(
                    "Error parsing {}: {}",
                    file_path
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .white()
                        .bold(),
                    e
                );
                return None;
            }
        };

        // Extract the test data, replacing it with `None`
        let test_data = std::mem::replace(&mut spec.test_data, None)?;

        Some(Self {
            spec,
            test_data,
            file_path,
        })
    }
}
