use std::path::PathBuf;

use monument::Config;
use structopt::StructOpt;

/// A struct storing the CLI args taken by Monument.  `StructOpt` will generate the argument
/// parsing/help code for us.
#[derive(Debug, Clone, StructOpt)]
#[structopt(name = "Monument", about = "A music-oriented composing engine")]
pub struct CliArgs {
    /// The name of the specification file for Monument (`*.toml`)
    #[structopt(parse(from_os_str))]
    pub input_file: PathBuf,

    /// The maximum number of threads that Monument will use
    #[structopt(short = "T", long)]
    pub num_threads: Option<usize>,
}

impl From<&CliArgs> for Config {
    fn from(args: &CliArgs) -> Config {
        Config {
            num_threads: args.num_threads,
            ..Config::default()
        }
    }
}
