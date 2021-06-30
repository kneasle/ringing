use std::path::PathBuf;

use monument::Config;
use monument_cli::spec::Spec;
use structopt::StructOpt;

/// Entry point for the CLI command `monument`
fn main() {
    // Parse the CLI args
    let args = CliArgs::from_args();

    let spec_toml = std::fs::read_to_string(&args.input_file).unwrap();
    let spec: Spec = toml::from_str(&spec_toml).unwrap();
    println!("{:#?}", spec);

    let engine = spec.create_engine(Config::from(args)).unwrap();

    engine.compose();
}

/// A struct storing the CLI args taken by Monument.  `StructOpt` will generate the argument
/// parsing/help code for us.
#[derive(Debug, Clone, StructOpt)]
#[structopt(name = "Monument", about = "A music-oriented composing engine")]
struct CliArgs {
    /// The name of the specification file for Monument (`*.toml`)
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,

    /// The maximum number of threads that Monument will use
    #[structopt(short = "T", long)]
    num_threads: Option<usize>,
}

impl From<CliArgs> for Config {
    fn from(args: CliArgs) -> Config {
        Config {
            num_threads: args.num_threads,
            sort_successor_links: true,
            normalise_music: true,
        }
    }
}
