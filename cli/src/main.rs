use std::sync::Arc;

use log::LevelFilter;
use monument::Config;
use monument_cli::{args::CliArgs, spec::AbstractSpec};
use number_prefix::NumberPrefix;
use structopt::StructOpt;

/// Entry point for the CLI command `monument`
fn main() {
    // Parse the CLI args
    let args = CliArgs::from_args();
    let config = Arc::new(Config::from(&args));

    // Parse the TOML file into an 'abstract' specification
    let mut abstr_spec = AbstractSpec::read_from_file(&args.input_file).unwrap();
    // Remove the test data to stop it clogging up the terminal
    abstr_spec.test_data = None;
    if config.log_level >= LevelFilter::Debug {
        println!("{:#?}", abstr_spec);
    }

    // Convert the abstract specification into a concrete specification accepted by Monument's
    // engine
    let comp_spec = abstr_spec.to_spec(&config).unwrap();

    // Use this concrete specification to generate compositions
    let results = monument::compose(&comp_spec, &config);

    // Print the results
    let nodes_per_sec = results.stats.nodes_considered as f64 / results.time_taken.as_secs_f64();
    let comps_per_sec = results.stats.comps_found as f64 / results.time_taken.as_secs_f64();
    println!(
        "Completed in {:.2?} ({} nodes/s, {} comps/s)",
        results.time_taken,
        fmt_with_prefix(nodes_per_sec),
        fmt_with_prefix(comps_per_sec)
    );
    dbg!(&results.stats);
    for c in results.comps {
        println!("{}", c.to_string(comp_spec.layout()));
    }
}

/// Format a big number with a suffix
fn fmt_with_prefix(num: f64) -> String {
    match NumberPrefix::decimal(num) {
        NumberPrefix::Standalone(n) => format!("{:.3}", n),
        NumberPrefix::Prefixed(prefix, n) => format!("{:.1}{}", n, prefix),
    }
}
