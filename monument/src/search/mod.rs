use std::{cmp::Ordering, collections::BinaryHeap, fmt::Debug, rc::Rc};

use bit_vec::BitVec;
use log::log;

use crate::{
    layout::{LinkIdx, Rotation, StartIdx},
    music::Score,
    utils::{coprime_bitmap, RowCounts},
    Comp, Query,
};

mod graph;

pub use graph::Graph;
use graph::NodeIdx;

/// Searches a [`Graph`](m_gr::Graph) for composition, according to some extra [`Data`].  For each
/// composition `c` found, `on_comp(c)` will be called.
pub(crate) fn search<CompFn: FnMut(Comp)>(
    graph: &crate::graph::Graph,
    query: &Query,
    queue_limit: usize,
    mut comp_fn: CompFn,
) {
    let len_range = (query.len_range.start as u32)..(query.len_range.end as u32);
    let num_parts = graph.num_parts() as Rotation;
    let rotation_bitmap = coprime_bitmap(num_parts);
    // Lower the hash-based graph into a graph that's immutable but faster to traverse
    let graph = self::graph::Graph::new(graph, query);

    // Initialise the frontier to just the start nodes
    let mut frontier = BinaryHeap::new();
    for (node_idx, start_idx, rotation) in graph.starts.iter() {
        let node = &graph.nodes[*node_idx];
        frontier.push(CompPrefix::new(
            CompPath::Start(*start_idx),
            *node_idx,
            node.falseness.clone(),
            *rotation,
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
            inner,
            avg_score,
            length,
        } = prefix;
        let PrefixInner {
            path,
            node_idx,
            unreachable_nodes,
            rotation,

            score,
            method_counts,
        } = *inner;
        let node = &graph.nodes[node_idx];

        // Check if the comp has come round
        if let Some(end) = node.end {
            if len_range.contains(&length)
                && method_counts.is_feasible(0, query.method_count_range.clone())
                && rotation_bitmap & (1 << rotation) != 0
            {
                let (start_idx, start_node_label, links) = path.flatten(&graph, query);
                let comp = Comp {
                    start_idx,
                    start_node_label,
                    links,
                    end,

                    rotation,
                    length: length as usize,
                    method_counts,
                    score,
                    avg_score,
                };
                comp_fn(comp);
                num_comps += 1;

                if num_comps == query.num_comps {
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

            if length + succ_node.dist_to_rounds >= len_range.end {
                continue; // Node would make comp too long
            }
            if unreachable_nodes.get(next_idx.index()).unwrap() {
                continue; // Node is false against something already in the comp
            }
            if !method_counts.is_feasible(
                (len_range.end - length) as usize,
                query.method_count_range.clone(),
            ) {
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
            truncate_heap(&mut frontier, queue_limit / 2);
        }

        // Print stats every so often
        iter_count += 1;
        if iter_count % 1_000_000 == 0 {
            let mut total_len = 0;
            let mut max_len = 0;
            frontier.iter().for_each(|n| {
                total_len += n.length as usize;
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

fn truncate_heap<T: Ord>(heap_ref: &mut BinaryHeap<T>, len: usize) {
    let heap = std::mem::take(heap_ref);
    let mut nodes = heap.into_vec();
    nodes.sort_by(|a, b| b.cmp(a)); // Sort highest score first
    if len < nodes.len() {
        nodes.drain(len..);
    }
    *heap_ref = BinaryHeap::from(nodes);
}

///////////////////
// COMP PREFIXES //
///////////////////

#[derive(Debug, Clone)]
struct CompPrefix {
    /// Data for this prefix which isn't accessed as much as `avg_score` or `length`.  We store it
    /// in a [`Box`] because the frontier spends a lot of time swapping elements, and copying a
    /// 128-bit struct is much much faster than copying an inlined [`PrefixInner`].  `avg_score`
    /// and `length` are accessed so often that they are left unboxed.
    inner: Box<PrefixInner>,
    /// Score per row in the composition
    avg_score: Score,
    /// Length refers to the **end** of the current node.  We use `u32` because [`Score`] is also
    /// 32 bits long, making `CompPrefix` pack into 128 bits
    length: u32,
}

/// The prefix of a composition.  These are ordered by score.
#[derive(Debug, Clone)]
struct PrefixInner {
    /// The path traced to this node
    path: CompPath,

    node_idx: NodeIdx,
    unreachable_nodes: BitVec,

    /// The number of part heads through which we have rotated.  This is kept in the range
    /// `0..graph.num_parts`
    rotation: Rotation,

    /// Score refers to the **end** of the current node
    score: Score,
    /// Method counts refers to the **end** of the current node
    method_counts: RowCounts,
}

impl CompPrefix {
    fn new(
        path: CompPath,
        node_idx: NodeIdx,
        unreachable_nodes: BitVec,
        rotation: Rotation,
        score: Score,
        length: u32,
        method_counts: RowCounts,
    ) -> Self {
        Self {
            inner: Box::new(PrefixInner {
                path,
                node_idx,
                unreachable_nodes,
                rotation,
                score,
                method_counts,
            }),
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
enum CompPath {
    /// The start of a composition, along with the index within `Graph::start_nodes` of this
    /// specific start
    Start(StartIdx),
    /// The composition follows the sequence in the [`Rc`], followed by taking the `n`th successor
    /// to that node.
    Cons(Rc<Self>, LinkIdx, NodeIdx),
}

impl CompPath {
    fn flatten(&self, graph: &Graph, query: &Query) -> (StartIdx, String, Vec<(LinkIdx, String)>) {
        let mut links = Vec::new();
        let (start_idx, start_node_label) = self.flatten_recursive(graph, query, &mut links);
        (start_idx, start_node_label, links)
    }

    /// Recursively flatten `self`, returning the start idx
    fn flatten_recursive(
        &self,
        graph: &Graph,
        query: &Query,
        out: &mut Vec<(LinkIdx, String)>,
    ) -> (StartIdx, String) {
        match self {
            Self::Start(start_idx) => {
                let (start_node_idx, _, _) = graph
                    .starts
                    .iter()
                    .find(|(_, start_idx_2, _)| start_idx == start_idx_2)
                    .unwrap();
                let label = graph.node_label(*start_node_idx);
                (*start_idx, label)
            }
            Self::Cons(lhs, link, node_idx) => {
                let start = lhs.flatten_recursive(graph, query, out);
                out.push((*link, graph.node_label(*node_idx)));
                start
            }
        }
    }
}
