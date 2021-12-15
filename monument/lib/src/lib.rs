#![deny(clippy::all)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod graph;
pub mod layout;
pub mod music;
mod search;
mod utils;

pub use utils::OptRange;

use itertools::Itertools;
use layout::{node_range::End, Layout, LinkIdx, StartIdx};
use music::Score;
use utils::{Rotation, RowCounts};

use std::{
    ops::Range,
    sync::{Arc, Mutex},
};

use bellframe::RowBuf;
use graph::{optimise::Pass, Graph};
use log::log;

/// Information provided to Monument which specifies what compositions are generated.
///
/// Compare this to [`Config`], which determines _how_ those compositions are generated (and
/// therefore determines how quickly the results are generated).
#[derive(Debug, Clone)]
pub struct Query {
    pub layout: layout::Layout,
    pub part_head: RowBuf,
    pub len_range: Range<usize>,
    pub num_comps: usize,

    pub method_count_range: Range<usize>,
    pub music_types: Vec<music::MusicType>,
    pub max_duffer_rows: Option<usize>,
}

/// Configuration parameters for Monument which **don't** change which compositions are emitted.
pub struct Config {
    /// Number of threads used to generate compositions.  If `None`, this uses the number of
    /// **physical** CPU cores (i.e. ignoring hyper-threading).
    pub num_threads: Option<usize>,
    pub queue_limit: usize,
    pub optimisation_passes: Vec<Pass>,
    pub split_by_start_node: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            num_threads: None,
            queue_limit: 10_000_000,
            optimisation_passes: graph::optimise::passes::default(),
            split_by_start_node: false,
        }
    }
}

/// A `Comp`osition generated by Monument.
#[derive(Debug, Clone)]
pub struct Comp {
    pub start_idx: StartIdx,
    pub start_node_label: String,
    pub links: Vec<(LinkIdx, String)>,
    pub end: End,

    pub rotation: Rotation,
    pub length: usize,
    pub method_counts: RowCounts,
    pub score: Score,
    /// Average [`Score`] generated by each row in the composition.   This is used to rank
    /// compositions to prevent the search algorithm being dominated by long compositions.
    pub avg_score: Score,
}

impl Comp {
    pub fn display_string(&self, layout: &Layout) -> String {
        let mut s = String::new();
        // Start
        s.push_str(&layout.starts[self.start_idx].label);
        // Nodes & links
        s.push_str(&self.start_node_label);
        for (link_idx, link_label) in &self.links {
            s.push_str(&layout.links[*link_idx].display_name);
            s.push_str(link_label);
        }
        // End
        s.push_str(self.end.label(layout));
        s
    }

    pub fn long_string(&self, layout: &Layout) -> String {
        format!(
            "len: {}, ms: {:>3?}, score: {:>6.2}, avg: {:.6}, rot: {}, str: {}",
            self.length,
            self.method_counts.counts(),
            self.score,
            self.avg_score,
            self.rotation,
            self.display_string(layout)
        )
    }
}

////////////
// SEARCH //
////////////

/// Run a query
pub fn run_query(
    query_arc: Arc<Query>,
    config: &mut Config,
    debug_output: Option<DebugOutput>,
) -> Result<Vec<Comp>, Option<Graph>> {
    log::info!("Building `Graph`");
    let graph = query_arc.unoptimised_graph();
    if debug_output == Some(DebugOutput::Graph) {
        return Err(Some(graph)); // Return the graph if the caller wants to inspect it
    }

    log::debug!("Optimising graph(s)");
    let mut graphs = if config.split_by_start_node {
        graph.split_by_start_node()
    } else {
        vec![graph]
    };
    for g in &mut graphs {
        g.optimise(&mut config.optimisation_passes, &query_arc);
        log::info!(
            "Optimised graph has {} nodes, {} starts, {} ends",
            g.node_map().len(),
            g.start_nodes().len(),
            g.end_nodes().len()
        );
    }

    if debug_output == Some(DebugOutput::StopBeforeSearch) {
        return Err(None); // Stop early if the caller requested that
    }

    log::info!("Starting tree search");
    let comps_arc = Arc::from(Mutex::new(Vec::<Comp>::new()));
    let num_threads = graphs.len(); // config.num_threads.unwrap_or_else(num_cpus::get_physical);
    let queue_limit = config.queue_limit;

    let handles = graphs
        .into_iter()
        .map(|graph| {
            let query = query_arc.clone();
            let comps = comps_arc.clone();
            std::thread::spawn(move || {
                let on_find_comp = |c: Comp| {
                    log::info!("{}", c.long_string(&query.layout));
                    comps.lock().unwrap().push(c);
                };
                search::search(&graph, &query, queue_limit / num_threads, on_find_comp);
            })
        })
        .collect_vec();
    // Wait for the worker threads to finish
    for h in handles {
        h.join().unwrap();
    }

    // Return all the comps in ascending order of goodness
    let mut comps = comps_arc.lock().unwrap().clone();
    comps.sort_by_key(|comp| comp.avg_score);
    Ok(comps)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugOutput {
    /// Return the unoptimised [`Graph`]
    Graph,
    /// Stop just before the search starts, to let the user see what's been printed out without
    /// scrolling
    StopBeforeSearch,
}

impl Query {
    fn unoptimised_graph(&self) -> Graph {
        graph::Graph::from_layout(
            &self.layout,
            &self.music_types,
            // `- 1` makes sure that the length limit is an **inclusive** bound
            self.len_range.end - 1,
            &self.part_head,
        )
    }
}
