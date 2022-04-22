#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

use std::path::PathBuf;

use colored::Colorize;
use log::LevelFilter;
use monument::Config;
use monument_cli::{CtrlCBehaviour, DebugOption};
use structopt::StructOpt;

fn main() {
    let args = CliArgs::from_args();
    monument_cli::init_logging(args.log_level());
    let result = monument_cli::run(
        monument_cli::Source::Path(args.input_file.clone()),
        args.debug_option,
        &args.config(),
        CtrlCBehaviour::RecoverComps,
    );
    match result {
        Ok(Some(query_result)) => query_result.print(),
        Ok(None) => assert!(args.debug_option.is_some()),
        Err(e) => {
            // In the case of an error, print the error message nicely then terminate the program
            // with code -1 without causing a panic message.
            println!("{} {:?}", "Error:".bright_red().bold(), e);
            drop(args);
            std::process::exit(-1);
        }
    }
}

/// A struct storing the CLI args taken by Monument.  `StructOpt` will generate the argument
/// parsing/help code for us.
#[derive(Debug, Clone, StructOpt)]
#[structopt(name = "Monument", about = "Fast and flexible composing engine")]
struct CliArgs {
    /// The name of the specification file for Monument (`*.toml`)
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,

    /// The maximum number of threads that Monument will use.
    // TODO: Uncomment this once multi-threading is possible
    // #[structopt(short = "T", long)]
    num_threads: Option<usize>,
    /// The maximum number of chunks in the chunk graph.  Exceeding this during generation will
    /// cause an error.  Defaults to 100K.
    #[structopt(long)]
    graph_size_limit: Option<usize>,
    /// The maximum number of prefixes that Monument will store at once.  Defaults to 10 million.
    #[structopt(short = "Q", long)]
    queue_limit: Option<usize>,

    /// Makes Monument print more output (`-vv` will produce all output).
    #[structopt(short, long = "verbose", parse(from_occurrences))]
    verbosity: usize,
    /// Makes Monument print less output (`-qq` will only produce errors).
    #[structopt(short, long = "quiet", parse(from_occurrences))]
    quietness: usize,

    /// Debug options.  `spec`, `query`, `layout` and `graph` print the corresponding data
    /// structures.  `no-search` will run as normal but stop just before starting the full search.
    #[structopt(short = "D", long)]
    debug_option: Option<DebugOption>,
}

impl CliArgs {
    /// Parse the `-q`/`-v` args into the [`LevelFilter`] to give to the `log` library
    fn log_level(&self) -> LevelFilter {
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

    fn config(&self) -> Config {
        let mut config = Config {
            num_threads: self.num_threads,
            // Don't `drop` any of the search data structures, since Monument will exit shortly
            // after the search terminates.  With the `Arc`-based data structures, this is
            // seriously beneficial - it shaves many seconds off Monument's total running time.
            mem_forget_search_data: true,
            ..Config::default()
        };
        if let Some(q) = self.queue_limit {
            config.queue_limit = q;
        }
        if let Some(g) = self.graph_size_limit {
            config.graph_size_limit = g;
        }
        config
    }
}
