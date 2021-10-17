use std::{cmp::Ordering, fmt::Debug, rc::Rc};

use frontier::Frontier;
use monument_graph::{music::Score, Data, Graph, Node};

pub mod frontier;

pub fn search<'graph, Ftr: Frontier<CompPrefix<'graph>> + Debug>(
    graph: &'graph Graph,
    data: &Data,
) {
    // Initialise the frontier to just the start nodes
    let mut frontier = Ftr::default();
    for (start_idx, start_id) in graph.start_nodes().iter().enumerate() {
        if let Some(node) = graph.get_node(start_id) {
            frontier.push(CompPrefix::new(
                CompPath::Start(start_idx),
                node.score(),
                node.length(),
                node,
            ));
        }
    }

    let mut comps_found = 0usize;

    // Repeatedly choose the best prefix and expand it (i.e. add each way of extending it to the
    // frontier).
    while let Some(prefix) = frontier.pop() {
        let CompPrefix {
            path,
            score,
            length,
            node,
            avg_score: _,
        } = prefix;

        if node.is_end() {
            // Found a comp
            println!("COMP of len {}", length);
            if data.len_range.contains(&length) {
                println!("{}: {}", frontier.len(), path.comp_string(graph, data));
                comps_found += 1;
            }
            if comps_found == data.num_comps {
                return;
            }
            continue; // Don't expand comps that come round
        }

        // Expand this node
        let path = Rc::new(path);
        for (i, (_link_idx, succ_id)) in node.successors().iter().enumerate() {
            if let Some(succ_node) = graph.get_node(succ_id) {
                let length = length + succ_node.length();
                let score = score + succ_node.score();

                if length >= data.len_range.end {
                    continue; // Node would make comp too long
                }

                // TODO: Check for falseness

                frontier.push(CompPrefix::new(
                    CompPath::Cons(path.clone(), i),
                    score,
                    length,
                    succ_node,
                ));
            }
        }
    }
}

/// The prefix of a composition.  These are ordered by score.
#[derive(Debug, Clone)]
pub struct CompPrefix<'graph> {
    /// The path traced to this node
    path: CompPath,

    /// Score refers to the **end** of the current node
    score: Score,
    /// Length refers to the **end** of the current node
    length: usize,

    /// Score per row in the composition
    avg_score: Score,

    /// The current node
    node: &'graph Node,
    // TODO: Falseness storage
}

impl<'graph> CompPrefix<'graph> {
    pub fn new(path: CompPath, score: Score, length: usize, node: &'graph Node) -> Self {
        Self {
            path,
            score,
            length,
            avg_score: score / length as f32,
            node,
        }
    }
}

impl PartialEq for CompPrefix<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.avg_score == other.avg_score
    }
}

impl Eq for CompPrefix<'_> {}

impl PartialOrd for CompPrefix<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CompPrefix<'_> {
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
    Cons(Rc<Self>, usize),
}

impl CompPath {
    fn comp_string(&self, graph: &Graph, data: &Data) -> String {
        let mut s = String::new();
        self.fmt_recursive(graph, data, &mut s);
        s
    }

    fn fmt_recursive<'graph>(
        &self,
        graph: &'graph Graph,
        data: &Data,
        s: &mut String,
    ) -> &'graph Node {
        match self {
            Self::Start(idx) => {
                let (label, start_node) = graph.get_start(*idx).unwrap();
                s.push_str(label);
                start_node
            }
            Self::Cons(lhs, succ_idx) => {
                let lhs_node = lhs.fmt_recursive(graph, data, s);
                let (link_idx, succ_id) = &lhs_node.successors()[*succ_idx];
                s.push_str(&data.layout.links[*link_idx].debug_name);
                graph.get_node(succ_id).unwrap()
            }
        }
    }
}
