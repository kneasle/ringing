use std::sync::{Arc, Mutex};

use args::CliArgs;
use itertools::Itertools;
use log::LevelFilter;
use monument_graph::{layout::Layout, optimise::passes};
use monument_search::{frontier::BestFirst, search, Comp};
use spec::Spec;
use structopt::StructOpt;

mod args;
mod spec;
mod test_data;

fn main() {
    // Parse CLI args
    let args = CliArgs::from_args();
    let log_level = args.log_level();

    // Generate & debug print the TOML file specifying the search
    let mut spec = Spec::read_from_file(&args.input_file).unwrap();
    // Remove data for the testing harness; it's not useful for the CLI version and just clogs up
    // any debugging.
    spec.test_data = None;
    if log_level >= LevelFilter::Debug {
        println!("{:#?}", spec);
    }

    // Convert the `Spec` into a `Graph` and other data required for running a search
    let data = spec.lower().unwrap();
    let graph = data.unoptimised_graph();
    // Split the graph into multiple graphs, each with exactly one start node.  Optimising these
    // independently and then searching in parallel is almost always better because the
    // optimisation passes have more concrete information about each graph.
    let mut graphs = graph.split_by_start_node();

    // Optimise the graphs
    let mut passes = passes::default();
    for g in &mut graphs {
        g.optimise(&mut passes, &data);
    }

    // Run graph search on each graph in parallel
    let comps = Arc::from(Mutex::new(Vec::<Comp>::new()));
    let data = Arc::new(data);
    let handles = graphs
        .into_iter()
        .map(|graph| {
            let data = data.clone();
            let comps = comps.clone();
            std::thread::spawn(move || {
                search::<BestFirst<_>, _>(&graph, &data, |c| {
                    print_comp(&c, &data.layout);
                    comps.lock().unwrap().push(c);
                });
            })
        })
        .collect_vec();
    // Wait for the worker threads to finish
    for h in handles {
        h.join().unwrap();
    }

    // TODO: Display all the comps in sorted order
}

fn print_comp(c: &Comp, layout: &Layout) {
    println!(
        "len: {}, ms: {:?}, score: {:>6.2}, avg: {:.6}, str: {}",
        c.length,
        c.method_counts.counts(),
        c.score,
        c.avg_score,
        c.display_string(layout)
    );
}
