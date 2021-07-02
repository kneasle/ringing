use std::path::PathBuf;

use glob::glob;
use itertools::Itertools;
use monument::Config;
use monument_cli::{
    spec::AbstractSpec,
    test_data::{CompResult, TestData},
};

fn main() -> std::io::Result<()> {
    let mut testing_dir = std::env::current_dir()?;
    // For some reason, this always runs in the `<project root>/cli` directory not the git repo
    // root so we pop the `/cli` dir off the end of the path
    testing_dir.pop();
    testing_dir.push("test-cases/");

    let mut test_file_glob = testing_dir
        .as_os_str()
        .to_owned()
        .into_string()
        .expect("Failed to convert OS string");
    test_file_glob.push_str("/**/*.toml");

    // Read all the `.toml` files from `test-cases/`, and parse them into `Spec`s
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

    // Run the test cases
    for t in test_cases {
        run_test_case(t, Config::default());
    }

    Ok(())
}

fn run_test_case(test_case: TestCase, config: Config) {
    println!("\n\nTesting {:?}", test_case.file_path);

    let spec = test_case.spec.to_spec(&config).unwrap();
    let mut comps = monument::compose(&spec, &config)
        .into_iter()
        .map(|c| CompResult::from_comp(c, &spec))
        .collect_vec();

    match test_case.test_data.results {
        // If the test file specifies some compositions, check that our results were compatible
        Some(expected_comps_float) => {
            let mut expected_comps = expected_comps_float
                .into_iter()
                .map(CompResult::from)
                .collect_vec();
            are_results_compatible(&mut expected_comps, &mut comps)
        }
        // If there weren't any then we assume that this test case hasn't been made yet, so we
        // print out the results in such a way that they can be copy/pasted into the TOML file
        None => print_test_string(comps),
    }
}

/// Check the compatibility of two sets of compositions.  This is essentially element-wise
/// equality, but taking into account that the order that comps are generated is non-deterministic.
fn are_results_compatible(expected_comps: &mut [CompResult], comps: &mut [CompResult]) {
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
        println!("Results OK ({} comps)", comps.len());
    } else {
        println!("");
        println!("FAILED TEST!");
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

/// Print a test string that can be copied back into the TOML file to make the test pass
fn print_test_string(comps: Vec<CompResult>) {
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
                println!("Parsing error: {}", e);
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
