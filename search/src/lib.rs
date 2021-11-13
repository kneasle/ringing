use std::{cmp::Ordering, fmt::Debug, rc::Rc};

use bit_vec::BitVec;
use frontier::Frontier;
use log::log;
use monument_graph::{self as m_gr, music::Score, Data, Rotation};
use monument_layout::{node_range::End, Layout, LinkIdx, StartIdx};
use monument_utils::{coprime_bitmap, RowCounts};

pub mod frontier;
mod graph;

pub use graph::Graph;
use graph::NodeIdx;

/// Searches a [`Graph`](m_gr::Graph) for composition, according to some extra [`Data`].  For each
/// composition `c` found, `on_comp(c)` will be called.
pub fn search<Ftr: Frontier<CompPrefix> + Debug, CompFn: FnMut(Comp)>(
    graph: &m_gr::Graph,
    data: &Data,
    queue_limit: usize,
    mut comp_fn: CompFn,
) {
    let num_parts = graph.num_parts() as Rotation;
    let rotation_bitmap = coprime_bitmap(num_parts);
    // Lower the hash-based graph into a graph that's immutable but faster to traverse
    let graph = crate::graph::Graph::new(graph, data);

    // Initialise the frontier to just the start nodes
    let mut frontier = Ftr::default();
    for (node_idx, start_idx) in graph.starts.iter() {
        let node = &graph.nodes[*node_idx];
        frontier.push(CompPrefix::new(
            CompPath::Start(*start_idx),
            *node_idx,
            node.falseness.clone(),
            0, // Start out with no rotation (i.e. we start at the 0th part head, which is rounds)
            node.score,
            node.length,
            node.method_counts.clone(),
        ));
    }

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).
    let mut iter_count = 0;
    let mut num_comps = 0;
    while let Some(prefix) = frontier.pop() {
        let CompPrefix {
            path,
            node_idx,
            unreachable_nodes,
            rotation,

            score,
            length,
            method_counts,
            avg_score,
        } = prefix;
        let node = &graph.nodes[node_idx];

        // Check if the comp has come round
        if let Some(end) = node.end {
            // TODO: Check that the rotation is valid
            if data.len_range.contains(&length)
                && method_counts.is_feasible(0, data.method_count_range.clone())
                && rotation_bitmap & (1 << rotation) != 0
            {
                let (start_idx, start_node_label, links) = path.flatten(&graph, data);
                let comp = Comp {
                    start_idx,
                    start_node_label,
                    links,
                    end,

                    rotation,
                    length,
                    method_counts,
                    score,
                    avg_score,
                };
                comp_fn(comp);
                num_comps += 1;

                if num_comps == data.num_comps {
                    break; // Stop the search once we've got enough comps
                }
            }
            continue; // Don't expand comps after they've come round
        }

        // Expand this node
        let path = Rc::new(path);
        for link in &node.succs {
            let next_idx = link.next_node;
            let succ_node = &graph.nodes[next_idx];

            let rotation = (rotation + link.rot) % num_parts;
            let length = length + succ_node.length;
            let score = score + succ_node.score + link.score;
            let method_counts = &method_counts + &succ_node.method_counts;

            if length + succ_node.dist_to_rounds >= data.len_range.end {
                continue; // Node would make comp too long
            }
            if unreachable_nodes.get(next_idx.index()).unwrap() {
                continue; // Node is unreachable (i.e. false against something already in the comp)
            }
            if !method_counts
                .is_feasible(data.len_range.end - length, data.method_count_range.clone())
            {
                continue; // Can't recover the method balance before running out of rows
            }

            // Compute which nodes are unreachable after this node has been added
            let mut new_unreachable_nodes = unreachable_nodes.clone();
            new_unreachable_nodes.or(&succ_node.falseness);

            frontier.push(CompPrefix::new(
                CompPath::Cons(path.clone(), link.source_idx, next_idx),
                next_idx,
                new_unreachable_nodes,
                rotation,
                score,
                length,
                method_counts,
            ));
        }

        // If the queue gets too long, then halve its size
        if frontier.len() >= queue_limit {
            log::debug!("Truncating queue");
            frontier.truncate(queue_limit / 2);
        }

        // Print stats every so often
        iter_count += 1;
        if iter_count % 1_000_000 == 0 {
            let mut total_len = 0;
            let mut max_len = 0;
            frontier.iter().for_each(|n| {
                total_len += n.length;
                max_len = max_len.max(n.length);
            });

            log::info!(
                "{} iters, {} items in queue, avg/max len {:.0}/{}",
                iter_count,
                frontier.len(),
                total_len as f32 / frontier.len() as f32,
                max_len
            );
        }
    }
}

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
}

/////////////////
// COMP PREFIX //
/////////////////

/// The prefix of a composition.  These are ordered by score.
#[derive(Debug, Clone)]
pub struct CompPrefix {
    /// The path traced to this node
    path: CompPath,

    node_idx: NodeIdx,
    unreachable_nodes: BitVec,

    /// The number of part heads through which we have rotated.  This is kept in the range
    /// `0..graph.num_parts`
    rotation: Rotation,

    /// Score refers to the **end** of the current node
    score: Score,
    /// Length refers to the **end** of the current node
    length: usize,
    /// Method counts refers to the **end** of the current node
    method_counts: RowCounts,

    /// Score per row in the composition
    avg_score: Score,
}

impl CompPrefix {
    fn new(
        path: CompPath,
        node_idx: NodeIdx,
        unreachable_nodes: BitVec,
        rotation: Rotation,
        score: Score,
        length: usize,
        method_counts: RowCounts,
    ) -> Self {
        Self {
            path,
            node_idx,
            unreachable_nodes,
            rotation,
            score,
            length,
            method_counts,
            avg_score: score / length as f32,
        }
    }
}

impl PartialEq for CompPrefix {
    fn eq(&self, other: &Self) -> bool {
        self.avg_score == other.avg_score
    }
}

impl Eq for CompPrefix {}

impl PartialOrd for CompPrefix {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CompPrefix {
    fn cmp(&self, other: &Self) -> Ordering {
        self.avg_score.cmp(&other.avg_score)
    }
}

/// A route through the composition graph, stored as a reverse linked list.  This allows for
/// multiple compositions with the same prefix to share the data for that prefix.
#[derive(Debug, Clone)]
enum CompPath {
    /// The start of a composition, along with the index within `Graph::start_nodes` of this
    /// specific start
    Start(StartIdx),
    /// The composition follows the sequence in the [`Rc`], followed by taking the `n`th successor
    /// to that node.
    Cons(Rc<Self>, LinkIdx, NodeIdx),
}

impl CompPath {
    fn flatten(&self, graph: &Graph, data: &Data) -> (StartIdx, String, Vec<(LinkIdx, String)>) {
        let mut links = Vec::new();
        let (start_idx, start_node_label) = self.flatten_recursive(graph, data, &mut links);
        (start_idx, start_node_label, links)
    }

    // Recursively flatten `self`, returning the start idx
    fn flatten_recursive(
        &self,
        graph: &Graph,
        data: &Data,
        out: &mut Vec<(LinkIdx, String)>,
    ) -> (StartIdx, String) {
        match self {
            &Self::Start(start_idx) => {
                let (start_node_idx, _) = graph
                    .starts
                    .iter()
                    .find(|(_, start_idx_2)| start_idx == *start_idx_2)
                    .unwrap();
                let label = graph.node_label(*start_node_idx);
                (start_idx, label)
            }
            Self::Cons(lhs, link, node_idx) => {
                let start = lhs.flatten_recursive(graph, data, out);
                out.push((*link, graph.node_label(*node_idx)));
                start
            }
        }
    }
}
