use std::{path::PathBuf, str::FromStr};

use log::LogLevelFilter as LevelFilter;
use monument::DebugOutput;
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
    /// The maximum number of threads that Monument will use
    #[structopt(short = "Q", long)]
    pub queue_limit: Option<usize>,

    /// Makes Monument print more output (`-vv` will produce all output).
    #[structopt(short, long = "verbose", parse(from_occurrences))]
    pub verbosity: usize,
    /// Makes Monument print less output (`-qq` will only produce errors).
    #[structopt(short, long = "quiet", parse(from_occurrences))]
    pub quietness: usize,

    /// Debug print an internal data structure and terminate.  Options are `spec`, `query`,
    /// `layout` and `graph`.
    #[structopt(short = "D", long)]
    pub debug_print: Option<DebugPrint>,
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

/// What item should be debug printed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugPrint {
    Spec,
    Query,
    Layout,
    Graph,
    /// Stop just before the search starts, to let the user see what's been printed out without
    /// scrolling
    Search,
}

impl FromStr for DebugPrint {
    type Err = String;

    fn from_str(v: &str) -> Result<Self, String> {
        Ok(match v.to_lowercase().as_str() {
            "spec" => Self::Spec,
            "query" => Self::Query,
            "layout" => Self::Layout,
            "graph" => Self::Graph,
            "search" => Self::Search,
            _ => {
                return Err(format!(
                    "Unknown value {:?}. Expected `spec`, `query`, `layout`, `graph` or `search`.",
                    v
                ))
            }
        })
    }
}

impl From<DebugPrint> for Option<DebugOutput> {
    fn from(dbg_print: DebugPrint) -> Option<DebugOutput> {
        Some(match dbg_print {
            DebugPrint::Graph => DebugOutput::Graph,
            DebugPrint::Search => DebugOutput::StopBeforeSearch,
            _ => return None,
        })
    }
}
