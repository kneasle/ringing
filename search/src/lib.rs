use std::{cmp::Ordering, fmt::Debug, rc::Rc};

use bit_vec::BitVec;
use frontier::Frontier;
use monument_graph::{self as m_gr, layout::LinkIdx, music::Score, Data};

pub mod frontier;
mod graph;

use graph::{Graph, NodeIdx};

pub fn search<Ftr: Frontier<CompPrefix> + Debug>(graph: &m_gr::Graph, data: &Data) {
    // Lower the graph into a graph that's immutable but faster to traverse
    let lowered_graph = Graph::from(graph);
    search_lowered::<Ftr>(&lowered_graph, data)
}

fn search_lowered<Ftr: Frontier<CompPrefix> + Debug>(graph: &Graph, data: &Data) {
    // Initialise the frontier to just the start nodes
    let mut frontier = Ftr::default();
    for (i, (node_idx, _label)) in graph.starts.iter().enumerate() {
        let node = &graph.nodes[*node_idx];
        frontier.push(CompPrefix::new(
            CompPath::Start(i),
            *node_idx,
            node.falseness.clone(),
            node.score,
            node.length,
        ));
    }

    let mut comps_found = 0usize;

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).
    while let Some(prefix) = frontier.pop() {
        let CompPrefix {
            path,
            node_idx,
            unreachable_nodes,

            score,
            length,
            avg_score,
        } = prefix;
        let node = &graph.nodes[node_idx];

        if let Some(end_label) = &node.end_label {
            // Found a comp
            if data.len_range.contains(&length) {
                println!(
                    "q: {:>8}, len: {}, score: {:>6.2}, avg: {}, str: {}",
                    frontier.len(),
                    length,
                    score,
                    avg_score,
                    path.comp_string(graph, data, end_label)
                );
                comps_found += 1;
            }
            if comps_found == data.num_comps {
                return;
            }
            continue; // Don't expand comps that come round
        }

        // Expand this node
        let path = Rc::new(path);
        for &(link_idx, succ_idx) in &node.succs {
            let succ_node = &graph.nodes[succ_idx];

            let length = length + succ_node.length;
            let score = score + succ_node.score;

            if length >= data.len_range.end {
                continue; // Node would make comp too long
            }

            if unreachable_nodes.get(succ_idx.index()).unwrap() {
                continue; // Node is unreachable (i.e. false)
            }

            // Compute which nodes are unreachable after this node has been added
            let mut new_unreachable_nodes = unreachable_nodes.clone();
            new_unreachable_nodes.or(&succ_node.falseness);

            frontier.push(CompPrefix::new(
                CompPath::Cons(path.clone(), link_idx),
                succ_idx,
                new_unreachable_nodes,
                score,
                length,
            ));
        }
    }
}

/// The prefix of a composition.  These are ordered by score.
#[derive(Debug, Clone)]
pub struct CompPrefix {
    /// The path traced to this node
    path: CompPath,

    node_idx: NodeIdx,
    unreachable_nodes: BitVec,

    /// Score refers to the **end** of the current node
    score: Score,
    /// Length refers to the **end** of the current node
    length: usize,

    /// Score per row in the composition
    avg_score: Score,
}

impl CompPrefix {
    pub fn new(
        path: CompPath,
        node_idx: NodeIdx,
        unreachable_nodes: BitVec,
        score: Score,
        length: usize,
    ) -> Self {
        Self {
            path,
            node_idx,
            unreachable_nodes,
            score,
            length,
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
pub enum CompPath {
    /// The start of a composition, along with the index within `Graph::start_nodes` of this
    /// specific start
    Start(usize),
    /// The composition follows the sequence in the [`Rc`], followed by taking the `n`th successor
    /// to that node.
    Cons(Rc<Self>, LinkIdx),
}

impl CompPath {
    fn comp_string(&self, graph: &Graph, data: &Data, end_label: &str) -> String {
        let mut s = String::new();
        self.fmt_recursive(graph, data, &mut s);
        s.push_str(end_label);
        s
    }

    fn fmt_recursive(&self, graph: &Graph, data: &Data, s: &mut String) {
        match self {
            Self::Start(idx) => {
                let (_node_idx, label) = &graph.starts[*idx];
                s.push_str(label);
            }
            Self::Cons(lhs, link_idx) => {
                lhs.fmt_recursive(graph, data, s);
                s.push_str(&data.layout.links[*link_idx].display_name);
            }
        }
    }
}
