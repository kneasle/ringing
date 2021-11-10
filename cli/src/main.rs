use std::sync::{Arc, Mutex};

use args::CliArgs;
use itertools::Itertools;
use log::log;
use monument_graph::{layout::Layout, optimise::passes};
use monument_search::{frontier::BestFirst, search, Comp};
use spec::Spec;
use structopt::StructOpt;

mod args;
mod spec;

/// Max number of comp prefixes stored in the queues of all threads
const QUEUE_LIMIT: usize = 10_000_000;

fn main() {
    // Parse CLI args
    let args = CliArgs::from_args();
    let log_level = args.log_level();

    pretty_logger::init(
        pretty_logger::Destination::Stderr,
        log_level,
        pretty_logger::Theme::default(),
    )
    .unwrap();

    // Generate & debug print the TOML file specifying the search
    let spec = Spec::read_from_file(&args.input_file).unwrap();
    log::trace!("spec = {:#?}", spec);

    // Convert the `Spec` into a `Graph` and other data required for running a search
    log::info!("Generating `Layout`");
    let data = spec.lower().unwrap();
    let graph = data.unoptimised_graph().to_multipart(&data).unwrap();
    // Split the graph into multiple graphs, each with exactly one start node.  Optimising these
    // independently and then searching in parallel is almost always better because the
    // optimisation passes have more concrete information about each graph.
    let mut graphs = graph.split_by_start_node();

    log::info!("Optimising `Graph`s");
    // Optimise the graphs
    let mut passes = passes::default();
    for g in &mut graphs {
        g.optimise(&mut passes, &data);
    }

    log::info!("Starting tree search");
    // Run graph search on each graph in parallel
    let comps = Arc::from(Mutex::new(Vec::<Comp>::new()));
    let data = Arc::new(data);
    let num_threads = graphs.len();
    let queue_limit = QUEUE_LIMIT / num_threads;
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
                search::<BestFirst<_>, _>(&graph, &data, queue_limit, on_find_comp);
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
}

fn print_comp(c: &Comp, layout: &Layout) {
    println!(
        "len: {}, ms: {:?}, score: {:>6.2}, avg: {:.6}, rot: {}, str: {}",
        c.length,
        c.method_counts.counts(),
        c.score,
        c.avg_score,
        c.rotation,
        c.display_string(layout)
    );
}
