use std::path::PathBuf;

use log::LevelFilter;
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

    /// Makes Monument print more output (`-vv` will produce all output).
    #[structopt(short, long = "verbose", parse(from_occurrences))]
    pub verbosity: usize,
    /// Makes Monument print less output (`-qq` will only produce errors).
    #[structopt(short, long = "quiet", parse(from_occurrences))]
    pub quietness: usize,
}

impl From<&CliArgs> for Config {
    fn from(args: &CliArgs) -> Config {
        // Parse the `-q`/`-v` args into verbosity
        let log_level = match args.verbosity as isize - args.quietness as isize {
            -2 => LevelFilter::Error, // -qq
            -1 => LevelFilter::Warn,  // -q
            0 => LevelFilter::Info,   // <no args>
            1 => LevelFilter::Debug,  // -v
            2 => LevelFilter::Trace,  // -vv
            x => {
                if x < 0 {
                    LevelFilter::Off // -qqq (or more `q`s)
                } else {
                    LevelFilter::Trace // -vvv (or more `v`s)
                }
            }
        };

        Config {
            num_threads: args.num_threads,
            log_level,
            ..Config::default()
        }
    }
}
