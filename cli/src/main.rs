use std::{
    num::ParseIntError,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    time::Instant,
};

use args::CliArgs;
use bellframe::{
    place_not::{self, PnBlockParseError},
    InvalidRowError,
};
use itertools::Itertools;
use log::log;
use monument::{
    graph::optimise::passes,
    layout::Layout,
    search::{frontier::BestFirst, search, Comp},
};
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
    let data = spec.lower(input_file)?;
    debug_print!(Data, data);
    debug_print!(Layout, data.layout);

    // Compile this `Layout` to an unoptimised `Graph`
    log::info!("Building `Graph`");
    let graph = data.unoptimised_graph();
    debug_print!(Graph, graph);
    // Split the graph into multiple graphs, each with exactly one start node.  Optimising these
    // independently and then searching in parallel is almost always better because the
    // optimisation passes have more concrete information about each graph.
    // let mut graphs = vec![graph];
    let mut graphs = graph.split_by_start_node();

    // Optimise the graphs
    log::info!("Optimising `Graph`s");
    let mut passes = passes::default();
    for g in &mut graphs {
        g.optimise(&mut passes, &data);
        log::debug!(
            "{} nodes, {} starts, {} ends",
            g.node_map().len(),
            g.start_nodes().len(),
            g.end_nodes().len()
        );
    }

    // Run graph search on each graph in parallel
    if debug_print == Some(DebugPrint::Search) {
        return Ok(());
    }
    log::info!("Starting tree search");
    let comps = Arc::from(Mutex::new(Vec::<Comp>::new()));
    let data = Arc::new(data);
    let num_threads = graphs.len();
    let handles = graphs
        .into_iter()
        .map(|graph| {
            let data = data.clone();
            let comps = comps.clone();
            std::thread::spawn(move || {
                let on_find_comp = |c: Comp| {
                    print_comp(&c, &data.layout);
                    comps.lock().unwrap().push(c);
                };
                search::<BestFirst<_>, _>(&graph, &data, queue_limit / num_threads, on_find_comp);
            })
        })
        .collect_vec();
    // Wait for the worker threads to finish
    for h in handles {
        h.join().unwrap();
    }

    // Display all the comps in sorted order
    println!("\n\n\n\nSEARCH COMPLETE!\n\n\n");
    let mut comps = comps.lock().unwrap().clone();
    comps.sort_by_key(|comp| comp.avg_score);
    for c in comps {
        print_comp(&c, &data.layout);
    }

    println!("Search completed in {:?}", Instant::now() - start_time);

    Ok(())
}

fn print_comp(c: &Comp, layout: &Layout) {
    println!(
        "len: {}, ms: {:>3?}, score: {:>6.2}, avg: {:.6}, rot: {}, str: {}",
        c.length,
        c.method_counts.counts(),
        c.score,
        c.avg_score,
        c.rotation,
        c.display_string(layout)
    );
}
