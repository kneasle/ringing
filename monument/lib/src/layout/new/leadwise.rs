use std::collections::HashMap;

use bellframe::{Mask, Row, RowBuf};
use index_vec::IndexVec;

use super::{Boundary, Result};
use crate::{
    layout::{BlockIdx, BlockVec, Layout, Link, LinkVec, MethodBlock, RowIdx, StartOrEnd},
    CallVec,
};

/// Prefix inserted at the front of every leadwise composition to allow it to be parsed as such
const LEADWISE_PREFIX: &str = "#";

/// Creates a `Layout` where every course is exactly one lead long.
pub fn leadwise(methods: &[super::Method], calls: &CallVec<super::Call>) -> Result<Layout> {
    super::utils::check_duplicate_shorthands(methods)?;

    let stage = methods
        .iter()
        .map(|m| m.stage())
        .max()
        .expect("Can't compute stage of 0 methods");
    let method_blocks = methods
        .iter()
        .map(|m| MethodBlock {
            block: m
                .first_lead()
                .clone_map_annots_with_index(|i, _| (i == 0).then(|| m.shorthand.clone())),
            count_range: m.count_range.clone(),
        })
        .collect::<BlockVec<_>>();

    let fixed_bells = super::utils::fixed_bells(methods, calls, stage);
    let lead_head_mask = Mask::fix_bells(stage, fixed_bells);

    Ok(Layout {
        starts: start_or_ends(&lead_head_mask, methods, Boundary::Start),
        ends: start_or_ends(&lead_head_mask, methods, Boundary::End),
        links: links(methods, calls, &lead_head_mask),
        method_blocks,
        stage,
        leadwise: true,
    })
}

fn start_or_ends<I: index_vec::Idx>(
    lead_head_mask: &Mask,
    methods: &[super::Method],
    boundary: Boundary,
) -> IndexVec<I, StartOrEnd> {
    let mut locs = IndexVec::new();
    for (meth_idx, method) in methods.iter().enumerate() {
        // Closure to construct a `StartOrEnd` at a given row
        let new_start_or_end = |(row_idx, row): (usize, &Row)| {
            assert!(row_idx < method.lead_len());
            let mut label = String::new();
            if boundary.is_start() {
                label.push_str(LEADWISE_PREFIX); // Start all leadwise comps with a `#`
            }
            if row_idx != 0 {
                label.push_str(boundary.snap_label());
            }
            StartOrEnd {
                course_head: !row,
                row_idx: RowIdx::new(BlockIdx::new(meth_idx), row_idx),
                label,
            }
        };

        match method.allowed_indices(boundary) {
            // If the user has specified required indices then we add exactly them, panicking
            // if they are longer than the lead length.
            Some(idxs) => locs.extend(
                idxs.iter()
                    .map(|&idx| new_start_or_end((idx, &method.first_lead().row_vec()[idx]))),
            ),
            // If no indices are specified, then we allow any index which satisfies the
            // lead_head_mask (i.e. any lead index where the fixed bells are at their home
            // positions).
            None => locs.extend(
                method
                    .first_lead()
                    .rows()
                    .enumerate()
                    .filter(|(_, r)| lead_head_mask.matches(r))
                    .map(new_start_or_end),
            ),
        }
    }
    locs
}

fn links(
    methods: &[super::Method],
    calls: &CallVec<super::Call>,
    lead_head_mask: &Mask,
) -> LinkVec<Link> {
    let mut call_starts: Vec<HashMap<&str, Vec<CallStart>>> = Vec::new();
    // Maps each lead label to where calls of that label can **end**
    let mut call_ends: HashMap<&str, Vec<CallEnd>> = HashMap::new();
    for (block_idx, m) in methods.iter().enumerate() {
        let lead = m.first_lead();
        let mut call_starts_for_this_method = HashMap::new();
        for (row_idx_after, annot_row_after) in lead.annot_rows().enumerate() {
            if let Some(label) = annot_row_after.annot() {
                let row_idx_before = (row_idx_after + lead.len() - 1) % lead.len();
                let row_before = lead.get_row(row_idx_before).unwrap();
                let row_after_plain = lead.get_row(row_idx_before + 1).unwrap();

                call_starts_for_this_method
                    .entry(label.as_str())
                    .or_insert_with(Vec::new)
                    .push(CallStart {
                        row_idx: RowIdx::new(block_idx.into(), row_idx_before),
                        row_before: row_before.to_owned(),
                        row_after_plain: row_after_plain.to_owned(),
                    });
                call_ends
                    .entry(label)
                    .or_insert_with(Vec::new)
                    .push(CallEnd {
                        row_idx: RowIdx::new(block_idx.into(), row_idx_after),
                        inv_row: !annot_row_after.row(),
                    });
            }
        }
        call_starts.push(call_starts_for_this_method);
    }

    // Place calls between every `call_start` and every `call_end` of that lead label
    let mut links = Vec::new();
    for (method_idx, _method) in methods.iter().enumerate() {
        for (call_idx, call) in calls.iter_enumerated() {
            let label = call.lead_location.as_str();
            let starts = &call_starts[method_idx][label];
            let ends = &call_ends[label];

            for start in starts {
                let mut row_after_call = start.row_before.clone();
                call.place_not.permute(&mut row_after_call).unwrap();
                for end in ends {
                    // Call
                    links.push(Link {
                        from: start.row_idx,
                        to: end.row_idx,

                        ch_mask: lead_head_mask.clone(),
                        ch_transposition: &row_after_call * &end.inv_row,

                        call_idx: Some(call_idx),
                        calling_position: String::new(),
                    });
                    // Plain
                    links.push(plain_link(
                        start.row_idx,
                        end.row_idx,
                        lead_head_mask,
                        &start.row_after_plain * &end.inv_row,
                    ));
                }
            }
        }
    }

    // Always add plain links at the end of every lead.  In nearly all cases, these will already
    // exist and be deduplicated, but if there are no calls at the end of each lead (e.g. in
    // link-cyclic or Stedman) then these will have to be generated separately.
    for (method_idx_from, method_from) in methods.iter().enumerate() {
        for (method_idx_to, _) in methods.iter().enumerate() {
            links.push(plain_link(
                RowIdx {
                    block: method_idx_from.into(),
                    // - 1 to refer to the lead **end** not the lead **head**
                    row: method_from.lead_len() - 1,
                },
                RowIdx {
                    block: method_idx_to.into(),
                    row: 0,
                },
                lead_head_mask,
                method_from.lead_head().to_owned(),
            ));
        }
    }

    // Deduplicate links and return
    super::utils::dedup_links(&mut links);
    links.into()
}

fn plain_link(from: RowIdx, to: RowIdx, ch_mask: &Mask, ch_transposition: RowBuf) -> Link {
    Link {
        from,
        to,

        ch_mask: ch_mask.clone(),
        ch_transposition,

        call_idx: None,
        calling_position: String::new(),
    }
}

/// A position at which a call could start
#[derive(Debug, Clone, Eq, PartialEq)]
struct CallStart {
    row_idx: RowIdx,
    row_before: RowBuf,
    row_after_plain: RowBuf,
}

/// A position at which a call could end
#[derive(Debug, Clone, Eq, PartialEq)]
struct CallEnd {
    row_idx: RowIdx,
    inv_row: RowBuf,
}
