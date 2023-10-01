use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap, HashMap, HashSet},
    ops::RangeInclusive,
    time::Instant,
};

use bellframe::Mask;
use itertools::Itertools;

use crate::{
    graph::{ChunkId, Graph, LinkSide, RowIdx},
    parameters::{MethodIdx, MethodVec, OptionalRangeInclusive},
    query::Query,
    utils::lengths::TotalLength,
};

const METHOD_COUNT_RELAX_FACTOR: f32 = 0.1;

#[derive(Debug)]
pub(crate) struct RefinedRanges {
    pub length: RangeInclusive<TotalLength>,
    pub method_counts: MethodVec<RangeInclusive<TotalLength>>,
}

/// Attempt to prove which composition lengths and method counts are possible.  This result can
/// then be used to either refine the bounds provided by the user (e.g. a peal of Royal can often
/// only be exactly 5040 changes) or generate an error explaining why the query is impossible.
pub(crate) fn prove_lengths(graph: &Graph, query: &Query) -> crate::Result<RefinedRanges> {
    log::debug!("Proving lengths");

    /* TOTAL LENGTH */

    // Work out which lengths are possible
    let possible_lengths = possible_lengths(graph, query);
    // Refine the length bound to what's actually possible, or error if no lengths fall into the
    // requested bound
    let refined_len_range = match matching_lengths(&possible_lengths, &query.length) {
        LengthMatches {
            range: Some(range), ..
        } => range,
        // If at least one of min/max length isn't defined, then the range can't be matched (since
        // such a matching length would become a min/max value).
        LengthMatches {
            next_smaller,
            range: None,
            next_larger,
        } => {
            return Err(crate::Error::UnachievableLength {
                requested_range: query.length.clone(),
                next_shorter_len: next_smaller.map(TotalLength::as_usize),
                next_longer_len: next_larger.map(TotalLength::as_usize),
            });
        }
    };

    log::debug!(
        "  Total length bounded to {}..={}",
        refined_len_range.start(),
        refined_len_range.end(),
    );

    /* METHOD LENGTHS */

    let start = Instant::now();
    // Determine which method lengths are actually possible
    let possible_lengths_by_method = query
        .methods
        .iter_enumerated()
        .map(|(idx, method)| possible_method_counts(idx, method, graph, query))
        .collect::<MethodVec<_>>();
    // Compute min/max preferred/explicit bounds for every method
    let method_bounds_min = method_bounds(query, &refined_len_range, Bound::Min);
    let method_bounds_max = method_bounds(query, &refined_len_range, Bound::Max);
    // Combine all the information to compute the true method bounds
    let mut refined_method_counts = MethodVec::new();
    for (method_idx, possible_lengths) in possible_lengths_by_method.into_iter_enumerated() {
        let min_bound = method_bounds_min[method_idx];
        let max_bound = method_bounds_max[method_idx];
        let method = &query.methods[method_idx];
        let refined_counts = refine_method_counts(min_bound, max_bound, &possible_lengths, method)?;
        refined_method_counts.push(refined_counts);
    }
    log::debug!("  Method count ranges computed in {:.2?}", start.elapsed());

    // Print method counts to the user
    if query.is_spliced() {
        print_method_counts(&refined_method_counts, query);
    }

    // Check for clearly impossible method bounds
    check_final_bounds(&refined_method_counts, &refined_len_range)?;

    Ok(RefinedRanges {
        length: refined_len_range,
        method_counts: refined_method_counts,
    })
}

/////////////////////////
// REFINE TOTAL LENGTH //
/////////////////////////

/// Compute an ascending [`Vec`] of every possible [`TotalLength`] of the composition, up to *and
/// including* the first length after the highest range.
fn possible_lengths(graph: &Graph, query: &Query) -> Vec<TotalLength> {
    // To compute a list of possible lengths, we run a Dijkstra's-style algorithm where we keep a
    // frontier of `(distance, chunk)` where each entry means that `chunk` can be reached at a
    // given `distance`.  When `(distance, <end>)` is reached, it means that it is theoretically
    // possible for a composition to have a length of `distance`.  Thus, we keep expanding until
    // the smallest `distance` in the frontier is longer than the specified maximum limit.
    //
    // However, the loop in this algorithm runs once for every possible length of *every possible
    // chunk* (multiplied by `O(links from each chunk)`).  This mostly runs in under 100ms, but
    // occasionally it can be *extremely slow*, particularly in cases involving spliced with many
    // different lead lengths (because the differing lead lengths mean that every chunk can be
    // reached at lots of different lengths).  To speed things up, we first simplify the graph
    // using `compute_simplified_graph` (see its doc comment for exactly how this simplified graph
    // is generated).

    let start = Instant::now();
    let simple_graph = compute_simplified_graph(query, graph);
    log::debug!("  Simplified graph generated in {:.2?}", start.elapsed());

    /* Run Dijkstra's across the graph, while only computing lengths. */

    let start = Instant::now();

    // Initialise the frontier with the graph's start chunks, each with a distance of 0
    let mut frontier = simple_graph
        .starts
        .iter()
        .map(|start| Reverse((TotalLength::ZERO, LinkSide::Chunk(start))))
        .collect::<BinaryHeap<_>>();

    let mut total_lengths = Vec::<TotalLength>::new();
    let mut last_item = None;
    while let Some(Reverse(item @ (length, next_link_side))) = frontier.pop() {
        // Don't expand the same `(length, chunk)` pair twice.  Note how *both* `length` and
        // `ChunkId` are used for ordering, which guarantees that identical `(length, chunk)` pairs
        // will always be removed from the frontier sequentially.  Once the first of the run of
        // identical `(length, chunk)` pairs has been popped, no more can be added because adding
        // new chunks *must* make the `length` longer (0-length chunks aren't allowed).
        if std::mem::replace(&mut last_item, Some(item)) == Some(item) {
            continue;
        }

        // Follow the new link
        match next_link_side {
            LinkSide::StartOrEnd => {
                // lengths are always added in increasing order, so if `length` already appears in
                // `total_lengths` it will be the `last()` element.
                if total_lengths.last() != Some(&length) {
                    total_lengths.push(length);
                }
                // If this is the first end-length to be too long, then there's no point
                // continuing the search (which otherwise would never finish)
                if length > query.max_length() {
                    break;
                }
            }
            LinkSide::Chunk(next_chunk) => {
                if let Some(succs) = simple_graph.successors.get(next_chunk) {
                    for (chunk_length, succ) in succs {
                        frontier.push(Reverse((length + *chunk_length, succ.as_ref())));
                    }
                }
            }
        }
    }

    log::debug!("  Lengths computed in {:.2?}", start.elapsed());

    total_lengths
}

struct SimpleGraph {
    starts: HashSet<SimpleChunk>,
    successors: HashMap<SimpleChunk, HashSet<(TotalLength, LinkSide<SimpleChunk>)>>,
}

type SimpleChunk = (RowIdx, Mask);

/// Compute a simplified version of the composition [`Graph`], which replaces chunks with
/// `(row_idx, mask)` pairs and convert the chunks into graph edges annotated by their lengths.
/// So, for example, tenors-together Yorkshire Major would produce a graph similar to:
///
/// ```text
///                           (start)
///                              │
///   ╭──────────>────────────╮  │
///   │                       V  V
///   │                   ┍━━━━━━━━━━┑
///   │                   │ 1xxxxx78 |
///   │                   ┕━━━━━━━━━━┙
///   │                       │  │
///   │                ╭──────╯  │ 2 leads
///   │                │         V
///   │                │  ┍━━━━━━━━━━┑
///   │                │  │ 18x7xxxx |
///   ^                │  ┕━━━━━━━━━━┙
///   │                V         │
///   │                │         │ 2 leads
///   │                │         V
///   │                │  ┍━━━━━━━━━━┑
///   │        2 leads │  │ 1xxxx8x7 |
///   │     (a Before) │  ┕━━━━━━━━━━┙
///   │                │      │  ╰────────>─────╮
///   │                │      │                 │
///   │                │      │ 1 lead          │
///   │                V      V                 │
///   │                │  ┍━━━━━━━━━━┑          │
///   │ 2 leads        │  │ 1xxx7x8x |          │
///   │                │  ┕━━━━━━━━━━┙          │ 2 *ROWS*
///   │                │         │              │
///   ^                ╰──────╮  │ 2 leads      V
///   │                       V  V              │
///   │                   ┍━━━━━━━━━━┑          │
///   │                   │ 178xxxxx |          │
///   │                   ┕━━━━━━━━━━┙          │
///   │                       │  │   2 leads    │
///   ╰──────────<────────────╯  ╰────────────╮ │
///                                           V V
///                                          (end)
/// ```
///
/// Now we can run the same algorithm, but over a substantially smaller graph (5 'chunks' vs 600
/// in the original graph).  For the cyclic mixed-length spliced (that caused the massive
/// performance cliff), this results in exactly one 'chunk' per method (for `1xxxxxxx`).  Even
/// if it weren't cyclic, there'd be only 7 'chunks' per method (one for each position of the
/// tenor).  Otherwise, we'd have 720 or 5040 chunks to process.
fn compute_simplified_graph(query: &Query, graph: &Graph) -> SimpleGraph {
    let allowed_lead_masks: MethodVec<_> = query
        .methods
        .iter()
        .map(|m| m.allowed_lead_masks(&query.parameters))
        .collect();
    let get_simple_chunks = |chunk_id: &ChunkId| -> Vec<SimpleChunk> {
        let mut simple_chunks = Vec::new();
        for mask in &allowed_lead_masks[chunk_id.row_idx.method] {
            if mask.matches(&chunk_id.lead_head) {
                simple_chunks.push((chunk_id.row_idx, mask.clone()));
            }
        }
        simple_chunks
    };

    // Compute the starts
    let mut starts = HashSet::<SimpleChunk>::new();
    for (_start_link, start_chunk_id) in &graph.starts {
        starts.extend(get_simple_chunks(start_chunk_id));
    }

    // Compute the simplified graph
    let mut successors =
        HashMap::<SimpleChunk, HashSet<(TotalLength, LinkSide<SimpleChunk>)>>::new();
    for (id, chunk) in &graph.chunks {
        for simple_chunk in get_simple_chunks(id) {
            let successors = successors.entry(simple_chunk).or_default();

            // Add the lengths corresponding to this chunk's successors
            for (_id, succ_link) in chunk.succ_links(graph) {
                match &succ_link.to {
                    LinkSide::StartOrEnd => {
                        successors.insert((chunk.total_length, LinkSide::StartOrEnd));
                    }
                    LinkSide::Chunk(succ_id) => {
                        for succ_simple_chunk in get_simple_chunks(succ_id) {
                            successors
                                .insert((chunk.total_length, LinkSide::Chunk(succ_simple_chunk)));
                        }
                    }
                }
            }
        }
    }

    SimpleGraph { starts, successors }
}

///////////////////
// METHOD COUNTS //
///////////////////

/// Returns an ascending [`Vec`] of the method counts are *actually* possible for a given method.
/// Technically this returns a superset of those lengths, but nearly all the time this will be
/// perfectly accurate.
fn possible_method_counts(
    method_idx: MethodIdx,
    method: &crate::parameters::Method,
    graph: &Graph,
    query: &Query,
) -> Vec<TotalLength> {
    // In order to compute possible count ranges efficiently, we approximate the graph into the
    // following shape:
    //
    //                         ┍━━━━━━━━━━━━━┑
    //                         │  start row  |
    //                         ┕━━━━━━━━━━━━━┙
    //                            │  │││
    //                            │  │││ (start_counts)
    //                     ╭──────╯  │││
    //                     │         │││  ╭──────╮
    //                     │         │││  │ ╭──╮ │
    //                     │         VVV  V V  │ │
    //                     │   ┍━━━━━━━━━━━━━┑ │ │
    //  (start_end_counts) │   │  all chunks | | │ (internal_counts)
    //                     │   ┕━━━━━━━━━━━━━┙ │ |
    //                     │         │││  │ ╰──╯ │
    //                     │         │││  ╰──────╯
    //                     ╰──────╮  │││
    //                            │  │││ (end_counts)
    //                            V  VVV
    //                         ┍━━━━━━━━━━━━━┑
    //                         │   end row   |
    //                         ┕━━━━━━━━━━━━━┙
    //
    // I.e. a composition must include:
    //  - exactly one count from `start_counts`
    //  - any number of counts (including zero) from `interior_counts`
    //  - exactly one counts from `end_counts`
    // or
    //  - exactly one count from `start_end_counts`

    log::trace!("Computing method counts for {}:", method.shorthand());

    // Split the chunk counts into start/internal/end
    let mut start_counts = HashSet::new();
    let mut end_counts = HashSet::new();
    let mut interior_counts = HashSet::new();
    let mut start_end_counts = HashSet::new(); // Counts of chunks which are both starts & ends
    for (id, chunk) in &graph.chunks {
        let only_starts = chunk.pred_links(graph).all(|(_id, link)| link.is_start());
        let any_starts = chunk.pred_links(graph).any(|(_id, link)| link.is_start());
        let only_ends = chunk.succ_links(graph).all(|(_id, link)| link.is_end());
        let any_ends = chunk.succ_links(graph).any(|(_id, link)| link.is_end());

        // If any chunk of any method is otherwise internal but has *some* ends, then we can exit
        // the graph without ringing any start/ends
        if !only_ends && any_ends {
            end_counts.insert(TotalLength::ZERO);
        }

        if id.row_idx.method != method_idx {
            // If we can start/end the composition with a different method, then it's possible to
            // enter/exit the internal chunks of this method *without* using any start/ends
            if any_starts {
                start_counts.insert(TotalLength::ZERO);
            }
            if any_ends {
                end_counts.insert(TotalLength::ZERO);
            }
            continue; // Skip any chunks which aren't for this method
        }

        // Classify this chunk and add this chunk's count to the corresponding set
        let count_group = match (only_starts, only_ends) {
            (true, true) => &mut start_end_counts,
            (true, false) => &mut start_counts,
            (false, true) => &mut end_counts,
            (false, false) => &mut interior_counts,
        };
        count_group.insert(chunk.total_length);
    }

    log::trace!("  Start       counts: {:?}", start_counts);
    log::trace!("  Interior    counts: {:?}", interior_counts);
    log::trace!("  End         counts: {:?}", end_counts);
    log::trace!("  Start & end counts: {:?}", start_end_counts);

    // TODO: Compute required methods and use that in the proof

    /* Traverse the graph to compute the overall counts */

    let mut counts = Vec::new();

    // The counts are every possible sum of:
    // - exactly one count from `start_counts`
    // - any number of counts from `interior_counts`
    // - exactly one count from `end_counts`
    let mut frontier = start_counts
        .iter()
        .cartesian_product(&end_counts)
        .map(|(s, e)| Reverse(*s + *e))
        .collect::<BinaryHeap<_>>();
    while let Some(Reverse(count)) = frontier.pop() {
        if counts.last() == Some(&count) {
            continue; // Don't bother adding to a length we've seen before
        }
        counts.push(count);
        if count > query.max_length() {
            break; // Stop searching as soon as we find one length that's above our limit
        }
        // Add every possible interior length to this
        for l in &interior_counts {
            frontier.push(Reverse(count + *l));
        }
    }

    // Add any `start_end_counts` as full counts, making sure to keep `counts` sorted
    for c in start_end_counts {
        let idx = counts.binary_search(&c).unwrap_or_else(|idx| idx);
        counts.insert(idx, c);
    }

    // If the method never appears in the composition, then it can only take a length of 0
    if counts.is_empty() {
        counts.push(TotalLength::ZERO);
    }

    log::trace!("  Final counts: {:?}", counts);

    counts
}

fn method_bounds(
    query: &Query,
    total_len_range: &RangeInclusive<TotalLength>,
    bound: Bound,
) -> MethodVec<(BoundType, TotalLength)> {
    let get_bound = |range: OptionalRangeInclusive| -> Option<usize> {
        match bound {
            Bound::Min => range.min,
            Bound::Max => range.max,
        }
    };
    let total_len_bound = match bound {
        Bound::Min => total_len_range.start().as_usize() as f32,
        Bound::Max => total_len_range.end().as_usize() as f32,
    };

    let total_method_weight = query
        .methods
        .iter()
        .filter(|m| get_bound(m.count_range).is_none())
        .map(|m| (m.lead_len() as f32).sqrt())
        .sum::<f32>();
    let method_bounds = query
        .methods
        .iter()
        .map(|m| match get_bound(m.count_range) {
            Some(count) => (BoundType::Explicit, TotalLength::new(count)),
            // For methods which don't have a set range, distribute the rows weighted by the
            // square root of each method's lead length
            None => {
                let proportion_of_unbound_methods =
                    (m.lead_len() as f32).sqrt() / total_method_weight;
                let preferred_bound = total_len_bound * proportion_of_unbound_methods;
                // Round the bound 'outwards' to the nearest integer.  Rounding 'outwards' always
                // makes the usable range larger, and means that (with adding/subtracting a small
                // value to deal with rounding errors) the min/max bounds can't cross over.
                let f = 1.0 + METHOD_COUNT_RELAX_FACTOR;
                let rounded_bound = match bound {
                    Bound::Min => (preferred_bound / f - 1e-3).floor() as usize,
                    Bound::Max => (preferred_bound * f + 1e-3).ceil() as usize,
                };
                (BoundType::Preferred, TotalLength::new(rounded_bound))
            }
        })
        .collect::<MethodVec<_>>();
    method_bounds
}

/// Given computed min/max bounds and a list of possible lengths, determine the best range.  If no
/// such range is possible, raise a [`crate::Error`].
fn refine_method_counts(
    (min_type, mut min_len): (BoundType, TotalLength),
    (max_type, mut max_len): (BoundType, TotalLength),
    possible_lengths: &[TotalLength],
    method: &crate::parameters::Method,
) -> crate::Result<RangeInclusive<TotalLength>> {
    use BoundType::{Explicit as Expl, Preferred as Pref};

    log::trace!("Refining method counts for {}", method.shorthand());
    log::trace!(
        "  initial bounds: {}{} ..= {}{}",
        min_len,
        if min_type == Expl { " (explicit)" } else { "" },
        max_len,
        if max_type == Expl { " (explicit)" } else { "" },
    );

    // Handle reversed bounds
    if min_len > max_len {
        match (min_type, max_type) {
            // If one bound is explicit, then the explicit bound takes priority
            (Expl, Pref) => max_len = min_len,
            (Pref, Expl) => min_len = max_len,
            // If they're both explicit, it's the user's fault
            (Expl, Expl) => {
                unreachable!("Don't make the min range bigger than max range, you silly billy")
            }
            // Otherwise, the computed bounds would have to have been the wrong way round,
            // which shouldn't be possible (because they were computed from a maximum bounds, which
            // must be ordered) and rounded away from each other.
            (Pref, Pref) => unreachable!(),
        }
    }

    // Refine the ranges
    let matching_lengths = matching_lengths(possible_lengths, &(min_len..=max_len));
    log::trace!(
        "  Matching lengths: {} < {}{} <= {} <= {}{} < {}",
        match matching_lengths.next_smaller {
            Some(len) => format!("[.., {}]", len),
            None => "[]".to_owned(),
        },
        min_len,
        if min_type == Expl { "(e)" } else { "" },
        match &matching_lengths.range {
            Some(range) => format!("[ {}..={} ]", range.start(), range.end()),
            None => "[]".to_owned(),
        },
        max_len,
        if max_type == Expl { "(e)" } else { "" },
        match matching_lengths.next_larger {
            Some(len) => format!("[{}, ..]", len),
            None => "[]".to_owned(),
        }
    );
    let refined_range = match matching_lengths {
        // If some lengths within this range are possible, then take them as our range
        LengthMatches {
            range: Some(range), ..
        } => range,
        // If no lengths are possible, then attempt to extend any 'preferred' bounds
        LengthMatches {
            next_smaller,
            range: None,
            next_larger,
        } => {
            // (Try to) extend the bounds in both directions
            let refined_min = match min_type {
                Expl => None,         // If the bound is explicit, we can't extend it
                Pref => next_smaller, // Otherwise, extend to next value (if it exists)
            };
            let refined_max = match max_type {
                Expl => None,        // If the bound is explicit, we can't extend it
                Pref => next_larger, // Otherwise, extend to next value (if it exists)
            };
            // Resolve any impossible bounds
            match (refined_min, refined_max) {
                // Expand in both directions if possible
                (Some(min), Some(max)) => min..=max,
                // If only one direction is possible, then require exactly that length
                (Some(len), None) | (None, Some(len)) => len..=len,
                // If _no_ directions are possible then this must have been the user requesting
                // an invalid range (because the length of `0` should always be possible, so
                // `next_smaller` should never be `None`).
                (None, None) => {
                    assert_ne!((min_type, max_type), (Pref, Pref));
                    return Err(crate::Error::UnachievableMethodCount {
                        method_name: method.title(),
                        requested_range: method.count_range,
                        next_shorter_len: next_smaller.map(TotalLength::as_usize),
                        next_longer_len: next_larger.map(TotalLength::as_usize),
                    });
                }
            }
        }
    };

    Ok(refined_range)
}

/// Print the refined method counts to the user in a pleasant and easily-digestible way
fn print_method_counts(
    refined_method_counts: &MethodVec<RangeInclusive<TotalLength>>,
    query: &Query,
) {
    let mut methods_by_count_ranges = BTreeMap::<(TotalLength, TotalLength), Vec<MethodIdx>>::new();
    for (idx, range) in refined_method_counts.iter_enumerated() {
        methods_by_count_ranges
            .entry((*range.start(), *range.end()))
            .or_default()
            .push(idx);
    }
    for ((min, max), methods) in methods_by_count_ranges {
        let row_string = if min == max {
            format!("exactly {min}")
        } else {
            format!("{min} to {max}")
        };
        let methods_string = if methods.len() == query.methods.len() {
            "all methods".to_owned()
        } else {
            let shorthand = |idx: &MethodIdx| -> String { query.methods[*idx].shorthand() };

            match methods.as_slice() {
                [] => unreachable!(),
                [idx] => shorthand(idx).to_owned(),
                // Write 'idxs[0], idxs[1], ... idxs[n], idx2 and idx3'
                [idxs @ .., idx2, idx3] => {
                    let mut s = String::new();
                    for idx in idxs {
                        s.push_str(&shorthand(idx));
                        s.push_str(", ");
                    }
                    s.push_str(&shorthand(idx2));
                    s.push_str(" and ");
                    s.push_str(&shorthand(idx3));
                    s
                }
            }
        };

        log::info!("Requiring {row_string} rows of {methods_string}");
    }
}

enum Bound {
    Min,
    Max,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundType {
    /// This bound was specified by the user and must be adhered to
    Explicit,
    /// This bound has been computed and can be adjusted
    Preferred,
}

///////////////////////////
// FINAL BOUNDS CHECKING //
///////////////////////////

/// Given the method count and length ranges, check that the total method count and the length
/// range overlap (e.g. this will error if we're composing a peal but the total method count is
/// 4000 rows).
fn check_final_bounds(
    method_counts: &MethodVec<RangeInclusive<TotalLength>>,
    length_range: &RangeInclusive<TotalLength>,
) -> crate::Result<()> {
    let min_total_method_count = method_counts
        .iter()
        .map(|range| *range.start())
        .sum::<TotalLength>();
    let max_total_method_count = method_counts
        .iter()
        .map(|range| *range.end())
        .sum::<TotalLength>();
    let min_length = *length_range.start();
    let max_length = *length_range.end();

    // Check for clearly unachievable bounds
    if max_total_method_count < min_length {
        // Even if all the method bounds are maxed out, we still can't reach the minimum range
        return Err(crate::Error::TooLittleMethodCount {
            max_total_method_count: max_total_method_count.as_usize(),
            min_length: min_length.as_usize(),
        });
    }
    if min_total_method_count > max_length {
        // Even if the maximum length is rung, we don't have enough rows to satisfy the method
        // counts
        return Err(crate::Error::TooMuchMethodCount {
            min_total_method_count: min_total_method_count.as_usize(),
            max_length: max_length.as_usize(),
        });
    }

    Ok(())
}

/////////////////////
// LENGTH MATCHING //
/////////////////////

#[derive(Debug)]
struct LengthMatches {
    next_smaller: Option<TotalLength>,
    range: Option<RangeInclusive<TotalLength>>,
    next_larger: Option<TotalLength>,
}

/// Refine the length range based on what lengths are actually possible, returning an
/// [`Error`](crate::Error) if no lengths are possible.  For example, a peal of T/D Major will have
/// its length refined from `5000..=5200` to `5024..=5186`.
fn matching_lengths(lengths: &[TotalLength], range: &RangeInclusive<TotalLength>) -> LengthMatches {
    let mut next_smaller = None;
    let mut min = None;
    let mut max = None;
    let mut next_larger = None;
    for &l in lengths {
        if l < *range.start() {
            // Length is too short
            next_smaller = Some(l);
        } else if l <= *range.end() {
            // Length is within range
            max = Some(l);
            if min.is_none() {
                min = Some(l);
            }
        } else {
            // Length is too long
            if next_larger.is_none() {
                next_larger = Some(l);
            }
        }
    }

    LengthMatches {
        next_smaller,
        range: match (min, max) {
            (Some(min), Some(max)) => Some(min..=max),
            (None, None) => None,
            _ => unreachable!("max/min values must be found together"),
        },
        next_larger,
    }
}
