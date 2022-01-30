#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

use std::path::PathBuf;

use log::LevelFilter;
use monument_cli::DebugOption;
use structopt::StructOpt;

/// Max number of comp prefixes stored in the queues of all threads
const DEFAULT_QUEUE_LIMIT: usize = 10_000_000;

fn main() {
    let args = CliArgs::from_args();
    monument_cli::init_logging(args.log_level());
    let maybe_result = monument_cli::run(
        &args.input_file,
        args.debug,
        args.queue_limit.unwrap_or(DEFAULT_QUEUE_LIMIT),
    )
    .unwrap();
    if let Some(result) = maybe_result {
        result.print();
    }
}

/// A struct storing the CLI args taken by Monument.  `StructOpt` will generate the argument
/// parsing/help code for us.
#[derive(Debug, Clone, StructOpt)]
#[structopt(name = "Monument", about = "Fast and flexible composing engine")]
pub struct CliArgs {
    /// The name of the specification file for Monument (`*.toml`)
    #[structopt(parse(from_os_str))]
    pub input_file: PathBuf,

    /// The maximum number of threads that Monument will use.
    // TODO: Uncomment this once multi-threading is possible
    // #[structopt(short = "T", long)]
    pub num_threads: Option<usize>,
    /// The maximum number of prefixes that Monument will store at once.  Defaults to 10 million.
    #[structopt(short = "Q", long)]
    pub queue_limit: Option<usize>,

    /// Makes Monument print more output (`-vv` will produce all output).
    #[structopt(short, long = "verbose", parse(from_occurrences))]
    pub verbosity: usize,
    /// Makes Monument print less output (`-qq` will only produce errors).
    #[structopt(short, long = "quiet", parse(from_occurrences))]
    pub quietness: usize,

    /// Debug options.  `spec`, `query`, `layout` and `graph` print the corresponding data
    /// structures.  `no-search` will run as normal but stop just before starting the full search.
    #[structopt(short = "D", long)]
    pub debug: Option<DebugOption>,
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
