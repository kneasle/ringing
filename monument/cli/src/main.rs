use std::{
    num::ParseIntError,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use args::CliArgs;
use bellframe::{
    place_not::{self, PnBlockParseError},
    InvalidRowError,
};
use log::log;
use monument::Config;
use spec::Spec;
use structopt::StructOpt;

use crate::args::DebugPrint;

mod args;
mod spec;

/// Max number of comp prefixes stored in the queues of all threads
const DEFAULT_QUEUE_LIMIT: usize = 10_000_000;

fn main() {
    // Parse CLI args
    let args = CliArgs::from_args();

    // Initialise logging
    pretty_logger::init(
        pretty_logger::Destination::Stderr,
        args.log_level(),
        pretty_logger::Theme::default(),
    )
    .unwrap();

    // Run Monument
    run(
        &args.input_file,
        args.debug_print,
        args.queue_limit.unwrap_or(DEFAULT_QUEUE_LIMIT),
    )
    .unwrap();
}

/// The possible ways that a run of Monument could fail
#[derive(Debug)]
pub enum Error {
    NoMethods,
    CcLibNotFound,
    PartHeadParse(InvalidRowError),
    SpecFile(PathBuf, spec::TomlReadError),
    MusicFile(PathBuf, spec::TomlReadError),
    MethodNotFound { suggestions: Vec<String> },
    CallPnParse(String, place_not::ParseError),
    MethodPnParse(PnBlockParseError),
    LeadLocationIndex(String, ParseIntError),
    LayoutGen(monument::layout::new::Error),
}

fn run(
    input_file: &Path,
    debug_print: Option<DebugPrint>,
    queue_limit: usize,
) -> Result<(), Error> {
    let start_time = Instant::now();

    /// If the user specifies a [`DebugPrint`] flag with e.g. `-d layout`, then debug print the
    /// corresponding value and exit.
    macro_rules! debug_print {
        ($variant: ident, $val: expr) => {
            if debug_print == Some(DebugPrint::$variant) {
                dbg!($val);
                return Ok(());
            }
        };
    }

    // Generate & debug print the TOML file specifying the search
    let spec =
        Spec::read_from_file(input_file).map_err(|e| Error::SpecFile(input_file.to_owned(), e))?;
    debug_print!(Spec, spec);

    // Convert the `Spec` into a `Layout` and other data required for running a search
    log::info!("Generating `Layout`");
    let query = Arc::new(spec.lower(input_file)?);
    debug_print!(Query, query);
    debug_print!(Layout, &query.layout);

    // Generate config
    let mut config = Config {
        queue_limit,
        num_threads: Some(1),
        ..Config::default()
    };

    // Run query and handle its debug output
    let query_result =
        monument::run_query(query.clone(), &mut config, debug_print.and_then(Into::into));
    match query_result {
        Ok(comps) => {
            println!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
            for c in comps {
                c.long_string(&query.layout);
            }

            println!("Search completed in {:?}", Instant::now() - start_time);
        }
        Err(Some(graph)) => {
            dbg!(graph);
        }
        Err(None) => {}
    };

    Ok(())
}
