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
    /// The maximum number of bytes of heap memory that Monument's search routine can allocate.
    /// Defaults to 90% of what's available.
    #[structopt(short = "M", long, parse(try_from_str = parse_big_int))]
    pub mem_limit: Option<usize>,

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

/// Parse a big integer like '100' or '140M'
fn parse_big_int(s: &str) -> anyhow::Result<usize> {
    let (last_char_idx, last_char) = s.char_indices().last().unwrap();
    let mut number_string = &s[..last_char_idx];
    let mut multiplier = 1usize;
    match last_char {
        'k' | 'K' => multiplier = 1_000,
        'm' | 'M' => multiplier = 1_000_000,
        'g' | 'G' => multiplier = 1_000_000_000,
        't' | 'T' => multiplier = 1_000_000_000_000,
        '0'..='9' => number_string = s, // Part of the number
        _ => {
            return Err(anyhow::Error::msg(
                "Expected number with a multiplier from [KMGT]",
            ));
        }
    }
    Ok(number_string.parse::<usize>()? * multiplier)
}
