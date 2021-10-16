use args::CliArgs;
use log::LevelFilter;
use spec::Spec;
use structopt::StructOpt;

mod args;
mod spec;
mod test_data;

fn main() {
    // Parse CLI args
    let args = CliArgs::from_args();
    let log_level = args.log_level();

    // Generate & debug print the TOML file specifying the search
    let mut spec = Spec::read_from_file(&args.input_file).unwrap();
    // Remove data for the testing harness; it's not useful for the CLI version and just clogs up
    // any debugging.
    spec.test_data = None;
    if log_level >= LevelFilter::Debug {
        println!("{:#?}", spec);
    }

    // Convert the `Spec` into a `Graph` and other data required for running a search
    let data = spec.lower().unwrap();
    let graph = data.unoptimised_graph();

    dbg!(graph.node_map().len());
}
