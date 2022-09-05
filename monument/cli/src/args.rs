use std::path::PathBuf;

use log::LevelFilter;
use structopt::StructOpt;

use crate::DebugOption;

/// A struct storing the CLI args taken by Monument.  `StructOpt` will generate the argument
/// parsing/help code for us.
#[derive(Debug, Clone, StructOpt)]
#[structopt(name = "Monument", about = "Fast and flexible composing engine")]
pub struct CliArgs {
    /// The name of the specification file for Monument (`*.toml`)
    #[structopt(parse(from_os_str))]
    pub input_file: PathBuf,

    #[structopt(flatten)]
    pub config: Options,

    /// Makes Monument print more output (`-vv` will produce all output).
    #[structopt(short, long = "verbose", parse(from_occurrences))]
    pub verbosity: usize,
    /// Makes Monument print less output (`-qq` will only produce errors).
    #[structopt(short, long = "quiet", parse(from_occurrences))]
    pub quietness: usize,
}

/// Parameters passed directly into `monument_cli::run`, used to generated the [`monument::Config`]
/// for the search.
#[derive(Default, Debug, Clone, StructOpt)]
pub struct Options {
    /// The maximum number of threads that Monument will use.
    // TODO: Uncomment this once multi-threading is possible
    // #[structopt(short = "T", long)]
    pub num_threads: Option<usize>,
    /// The maximum number of chunks in the chunk graph.  Exceeding this during generation will
    /// cause an error.  Defaults to 100K.
    #[structopt(long)]
    pub graph_size_limit: Option<usize>,
    /// The maximum number of prefixes that Monument will store at once.  Defaults to 10 million.
    #[structopt(short = "Q", long)]
    pub queue_limit: Option<usize>,

    /// Debug options.  `spec`, `query`, `layout` and `graph` print the corresponding data
    /// structures.  `no-search` will run as normal but stop just before starting the full search.
    #[structopt(short = "D", long)]
    pub debug_option: Option<DebugOption>,
}

impl CliArgs {
    /// Parse the `-q`/`-v` args into the [`LevelFilter`] to give to the `log` library
    pub fn log_level(&self) -> LevelFilter {
        match self.verbosity as isize - self.quietness as isize {
            x if x < -2 => LevelFilter::Off, // -qqq (or more `q`s)
            -2 => LevelFilter::Error,        // -qq
            -1 => LevelFilter::Warn,         // -q
            0 => LevelFilter::Info,          // <none of -q or -v>
            1 => LevelFilter::Debug,         // -v
            2 => LevelFilter::Trace,         // -vv
            _ => LevelFilter::Trace,         // -vvv (or more `v`s)
        }
    }
}
