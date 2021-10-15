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
    // Remove the test data to stop it clogging up the terminal
    spec.test_data = None;
    if log_level >= LevelFilter::Debug {
        println!("{:#?}", spec);
    }

    // Convert the `Spec` into a `Graph` and other data required for running a search
    let (layout, music_types, len_range) = spec.lower().unwrap();

    dbg!(layout);
}
