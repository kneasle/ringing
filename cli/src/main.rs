use std::{fmt::Write, sync::Arc, time::Duration};

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
        "Completed in {} ({} nodes/s, {} comps/s)",
        fmt_duration(results.time_taken),
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

/// Format a [`Duration`] in a human-friendly way
fn fmt_duration(d: Duration) -> String {
    let total_secs = d.as_secs_f64();
    if total_secs < 60.0 {
        // If the Duration is shorter than a minute, then using duration's 'debug' output to 2
        // decimal places does a pretty nice job
        format!("{:.2?}", d)
    } else {
        // If the Duration is longer than a minute, we split it into days, hours, mins and (some
        // decimal number of) seconds like `#d #h #m #.#s`.  Again, the seconds goes to 2 decimal
        // places

        // Split out the (integer) number of minutes
        let total_mins = (total_secs / 60.0).floor() as usize;
        let num_secs = total_secs - total_mins as f64 * 60.0;
        // Split out the (integer) number of hours
        let total_hours = total_mins / 60;
        let num_mins = total_mins % 60;

        // Generate the format string
        let mut fmt_string = String::new();
        if total_hours > 0 {
            write!(fmt_string, "{}h ", total_hours).unwrap();
        }
        if total_mins > 0 {
            write!(fmt_string, "{}m ", num_mins).unwrap();
        }
        write!(fmt_string, "{:.2}s", num_secs).unwrap();
        fmt_string
    }
}
