use std::{
    collections::{HashMap, HashSet},
    ops::Mul,
};

use bellframe::{Mask, Row, RowBuf, Stage};
use index_vec::IndexVec;

use super::{utils::CourseHeadMask, Boundary, Error, Result, SpliceStyle};
use crate::layout::{BlockIdx, BlockVec, Layout, Link, RowIdx, StartOrEnd};

/// Generate a [`Layout`] such that chunks will be labelled by their course-head, rather than by
/// their lead-head.
pub fn coursewise(mut methods: Vec<super::Method>, splice_style: SpliceStyle) -> Result<Layout> {
    // Cache data about each method, and compute the overall stage of the comp
    super::utils::check_duplicate_shorthands(&methods)?;
    let is_spliced = methods.len() > 1;
    let stage = methods
        .iter()
        .map(|m| m.stage())
        .max()
        .ok_or(Error::NoMethods)?;
    assert!(methods.iter().all(|m| m.stage() == stage)); // TODO: Implement mixed stage splicing

    // Pre-process CH masks
    super::utils::preprocess_ch_masks(&mut methods, stage)?;
    for m in &methods {
        super::utils::check_for_ambiguous_courses(
            &m.ch_masks,
            &m.lead_head().closure_from_rounds(),
        )?;
    }

    // Generate links
    let links = generate_links(&methods, stage, is_spliced, splice_style)?.into();

    Ok(Layout {
        links,
        starts: rounds_locations(&methods, stage, Boundary::Start),
        ends: rounds_locations(&methods, stage, Boundary::End),
        // Create a block for each method
        method_blocks: methods
            .into_iter()
            .map(|m| m.course_method_block(is_spliced))
            .collect::<BlockVec<_>>(),
        stage,
    })
}

/////////////////////
// LINK GENERATION //
/////////////////////

/// Generates the set of [`Link`]s which are valid according to the `course_head_masks`
fn generate_links(
    method_datas: &[super::Method],
    stage: Stage,
    is_spliced: bool,
    mut splice_style: SpliceStyle,
) -> Result<Vec<Link>> {
    // If there's only one method, then plain leads are only required where there could be calls.
    // `SpliceStyle::Calls` would be equivalent.
    if method_datas.len() == 1 {
        splice_style = SpliceStyle::CallLocations;
    }

    // This maps lead locations to a list of row indices which occur just before this label.  These
    // correspond to lead **ends**.  For example, Yorkshire Surprise Major with only lead end
    // calls generates `{"LE": [223, 31, 63, 95, 127, 159, 191]}` (all from block #0).
    let call_starts_by_label = call_starts_by_label(method_datas);
    let call_ends = call_ends(method_datas);

    // Generate all possible links (including extra plain leads/splices) and then remove then if
    // necessary (either because they're a duplicate or because they violate the splicing style)
    let mut links = generate_all_links(
        method_datas,
        &call_ends,
        &call_starts_by_label,
        stage,
        is_spliced,
        splice_style,
    )?;
    filter_plain_links(&mut links, splice_style);
    super::utils::dedup_links(&mut links);
    Ok(links)
}

/// For each lead label, list the positions in the course where calls at that label could start
/// (Monument assumes that all courses always replace one piece of [`PlaceNot`]ation).
fn call_starts_by_label(method_datas: &[super::Method]) -> HashMap<&str, Vec<RowIdx>> {
    let mut label_indices = HashMap::<&str, Vec<RowIdx>>::new();

    // For every method (and the block it will generate) ...
    for (idx, d) in method_datas.iter().enumerate() {
        let block_idx = BlockIdx::new(idx); // Each method will correspond to one block

        // ... for every labelled row ...
        let course_len = d.plain_course.len();
        for (row_idx, annot) in d.plain_course.annots().enumerate() {
            if let Some(label) = &annot.label {
                // ... mark that a similarly-labelled call could be called just before this label
                let call_start_idx = (row_idx + course_len - 1) % course_len;

                label_indices
                    .entry(label)
                    .or_insert_with(Vec::new)
                    // Calls start at the row **before** the lead label.  We do `+ course_len` to
                    // avoid underflow if `row_idx == 0`.
                    .push(RowIdx::new(block_idx, call_start_idx));
            }
        }
    }

    label_indices
}

/// A single location where a call could **end**.  Note that this doesn't include information about
/// _which_ calls lead here, just that they _could_.
#[derive(Debug, Clone)]
struct CallEnd {
    /// For a call to be valid, it must produce a row which matches this [`Mask`].
    row_mask: Mask,
    /// The [`Method`] to which this would switch
    method_idx: usize,
    ch_mask_idx: usize,
    row_idx: usize,
}

fn call_ends(method_datas: &[super::Method]) -> Vec<CallEnd> {
    let mut call_ends = Vec::new();

    // For every method ...
    for (method_idx, d) in method_datas.iter().enumerate() {
        // ... for every labelled row in the plain course ...
        for (row_idx, annot_row) in d.plain_course.annot_rows().enumerate() {
            if annot_row.annot().label.is_some() {
                let row_after_call = annot_row.row();
                // ... for every course head mask ...
                for (ch_mask_idx, ch_mask) in d.ch_masks.iter().enumerate() {
                    // ... add a `CallEnd` corresponding to placing a call at this location to
                    // **enter** this course
                    call_ends.push(CallEnd {
                        row_mask: ch_mask.mask().mul(row_after_call),
                        method_idx,
                        ch_mask_idx,
                        row_idx,
                    });
                }
            }
        }
    }

    // Sanity check that any compatible call-end masks also agree on `row_idx`.  This should be
    // guaranteed by the CHs being unambiguous, but checking is quick enough to be worth it.
    for call_end1 in &call_ends {
        for call_end2 in &call_ends {
            if call_end1.method_idx == call_end2.method_idx
                && call_end1.row_mask.is_compatible_with(&call_end2.row_mask)
            {
                assert_eq!(call_end1.row_idx, call_end2.row_idx);
            }
        }
    }

    call_ends
}

/// Static data used when generating links.
#[derive(Debug)]
struct LinkGenData<'a> {
    method_datas: &'a [super::Method],
    call_starts_by_label: &'a HashMap<&'a str, Vec<RowIdx>>,
    call_ends: &'a [CallEnd],
    splice_style: SpliceStyle,
    is_spliced: bool,
    stage: Stage,
}

/// Generate all links allowed within the course head constraints, potentially with duplicates.
/// This will produce every possible splice, and it's up to [`filter_plain_links`] to unwanted
/// splices.
fn generate_all_links(
    method_datas: &[super::Method],
    call_ends: &[CallEnd],
    call_starts_by_label: &HashMap<&str, Vec<RowIdx>>,
    stage: Stage,
    is_spliced: bool,
    splice_style: SpliceStyle,
) -> Result<Vec<Link>> {
    let mut links = Vec::<Link>::new();
    let link_gen_data = LinkGenData {
        method_datas,
        call_starts_by_label,
        call_ends,
        splice_style,
        is_spliced,
        stage,
    };

    /*
    The general approach here is to attempt to place calls in every position, compute which row
    they would lead to, the try to find a `CallEnd` which corresponds to that row.  If such a
    `CallEnd` exists, then this call is valid and should be included in the chunk graph.  We also
    try to place plain leads at every location, and likewise check for any valid `CallEnd`s.
    */

    // For every method and CH mask ...
    for (method_idx, method_data) in method_datas.iter().enumerate() {
        for from_ch_mask in &method_data.ch_masks {
            generate_call_links(
                method_idx,
                method_data,
                from_ch_mask,
                &link_gen_data,
                &mut links,
            )?;
        }
    }
    if splice_style == SpliceStyle::LeadLabels {
        generate_plain_links(method_datas, &link_gen_data, &mut links);
    }

    Ok(links)
}

/// For a given [`Method`] and [`CourseHeadMask`], generate all calls leading **out** of this call,
/// along with any corresponding plain leads (if `splice_style` isn't [`SpliceStyle::LeadLabels`],
/// in which case plain splices are bulk-added separately).
fn generate_call_links(
    method_idx: usize,
    m: &super::Method,
    from_ch_mask: &CourseHeadMask,
    link_gen_data: &LinkGenData,
    links: &mut Vec<Link>,
) -> Result<()> {
    // Closure used to format a call string
    let fmt_call = |symbol: &str, calling_pos: &str| -> String {
        if link_gen_data.is_spliced {
            // If we're ringing spliced, then put calls in `[]`s to differentiate
            // them from method names
            format!("[{}{}]", symbol, calling_pos)
        } else {
            format!("{}{}", symbol, calling_pos)
        }
    };

    // ... test every call in every valid position in the course ...
    for call in &m.calls {
        let lead_label = call.lead_location.as_str();
        let call_starts = link_gen_data
            .call_starts_by_label
            .get(lead_label)
            .ok_or_else(|| Error::UndefinedLeadLocation {
                call_name: call.debug_symbol.to_owned(),
                label: lead_label.to_owned(),
            })?;
        for &from_idx in call_starts {
            if from_idx.block.index() != method_idx {
                continue; // Skip any call starts which aren't from this method
            }

            /* ADD CALL LINKS */

            // Link corresponding to placing this call
            let row_before_call = m.plain_course.get_row(from_idx.row).unwrap();
            let row_after_call = call.place_not.permute_new(row_before_call).unwrap();
            // Get the mask required by the row **after** this call.  This link can be
            // generated only if a `CallEnd` is compatible with this mask.
            let mask_after_call = from_ch_mask.mask().mul(&row_after_call);

            // Get the debug/display names for any link in this position
            let tenor_place = mask_after_call
                .place_of(from_ch_mask.calling_bell())
                .expect("Course head mask doesn't fix the calling bell");
            let calling_position = call.calling_positions.get(tenor_place).ok_or_else(|| {
                Error::CallingPositionsTooShort {
                    call_name: call.debug_symbol.to_owned(),
                    calling_position_len: call.calling_positions.len(),
                    stage: link_gen_data.stage,
                }
            })?;

            // Add links for this call, splicing to any available method
            let is_call_possible = add_links_for_call_position(
                from_idx,
                from_ch_mask.mask(),
                &row_after_call,
                &fmt_call(&call.debug_symbol, calling_position),
                &fmt_call(&call.display_symbol, calling_position),
                call.weight,
                None, // Calls are always allowed to change method
                link_gen_data,
                links,
            );

            /* ADD CORRESPONDING PLAIN LINKS */

            // Plain links should be added whenever there's a call, or every lead for
            // `SpliceStyle::LeadLabels`
            if link_gen_data.splice_style != SpliceStyle::LeadLabels && is_call_possible {
                let idx_after_plain = (from_idx.row + 1) % m.plain_course.len();
                let row_after_plain = m.plain_course.get_row(idx_after_plain).unwrap();
                add_links_for_call_position(
                    from_idx,
                    from_ch_mask.mask(),
                    row_after_plain,
                    &fmt_call("p", calling_position),
                    "",  // Don't display plain leads in output
                    0.0, // Plain leads have no weight
                    // If we're only splicing on calls, then don't add plain links that
                    // change method
                    (link_gen_data.splice_style == SpliceStyle::Calls).then(|| method_idx),
                    link_gen_data,
                    links,
                );
            }
        }
    }
    Ok(())
}

/// Generate plain links at every possible lead location (i.e. what's expected for
/// [`SpliceStyle::LeadLabels`]).
fn generate_plain_links(
    methods: &[super::Method],
    link_gen_data: &LinkGenData,
    links: &mut Vec<Link>,
) {
    for starts in link_gen_data.call_starts_by_label.values() {
        for &from_idx in starts {
            let method_data = &methods[from_idx.block.index()];
            let row_after_plain = method_data.plain_course.get_row(from_idx.row + 1).unwrap();
            for ch_mask_from in &method_data.ch_masks {
                add_links_for_call_position(
                    from_idx,
                    ch_mask_from.mask(),
                    row_after_plain,
                    if link_gen_data.is_spliced { "[p]" } else { "p" },
                    "",   // Don't display plain leads in output
                    0.0,  // Plain leads have no weight
                    None, // Splices to any methods are allowed
                    link_gen_data,
                    links,
                );
            }
        }
    }
}

/// For a given call in a given position, create links which come out of this into any possible
/// method.  Returns `true` if calls were added.
#[allow(clippy::too_many_arguments)]
fn add_links_for_call_position(
    from_idx: RowIdx,
    from_ch_mask: &Mask,
    row_after_call: &Row,

    debug_name: &str,
    display_name: &str,
    weight: f32,
    // `Some(i)` means that only links to method `i` is allowed, otherwise all links are allowed
    required_method_idx: Option<usize>,

    // Same across all calls:
    data: &LinkGenData,
    links: &mut Vec<Link>,
) -> bool {
    let mask_after_call = from_ch_mask.mul(row_after_call);
    let mut was_call_added = false;

    // Find any `CallEnd`s which this call could lead to
    for call_end in data.call_ends {
        if !call_end.row_mask.is_compatible_with(&mask_after_call) {
            continue;
        }

        if let Some(i) = required_method_idx {
            if i != call_end.method_idx {
                continue;
            }
        }

        let method_to = &data.method_datas[call_end.method_idx];
        let block_to = BlockIdx::new(call_end.method_idx);

        // If the mask generated by this call is compatible with some call end,
        // then the course we're going into satisfies some course head mask and
        // this call should be included in the resulting Layout.

        // Compute the course head transposition generated by this call
        let ch_transposition = Row::solve_xa_equals_b(
            method_to.plain_course.get_row(call_end.row_idx).unwrap(),
            row_after_call,
        )
        .unwrap();

        let ch_mask_of_new_course = method_to.ch_masks[call_end.ch_mask_idx].mask();
        // The course head mask for which courses can appear **before** this
        // call.  This mask may be stricter than `ch_mask` if the `call_end`'s
        // CH mask specifies more bells than `ch_mask`.  For example, sV going
        // into `1xxx7856` will have `source_mask = 1x6x5x78` rather than
        // `1xxxxx78`.
        let source_ch_mask = from_ch_mask
            .combine(&ch_mask_of_new_course.mul(&ch_transposition.inv()))
            .unwrap();

        // The `Link` referring to the call happening at this lead
        links.push(Link {
            from: from_idx,
            to: RowIdx::new(block_to, call_end.row_idx),
            ch_mask: source_ch_mask.clone(),
            ch_transposition,
            debug_name: debug_name.to_owned(),
            display_name: display_name.to_owned(),
            weight,
        });
        was_call_added = true;
    }
    was_call_added
}

/// Remove any plain links which violate the given [`SpliceStyle`].  If there's only one method,
/// then [`SpliceStyle::CallLocations`] has the desired effect.
fn filter_plain_links(links: &mut Vec<Link>, splice_style: SpliceStyle) {
    let call_starts = links
        .iter()
        .map(|link| (link.ch_mask.clone(), link.from))
        .collect::<HashSet<_>>();

    links.retain(|link| {
        if link.is_call() {
            return true; // Always keep the links corresponding to calls
        }
        if splice_style == SpliceStyle::LeadLabels {
            return true; // If we can splice at every label, then keep all plain links
        }

        let is_call_location = call_starts.contains(&(link.ch_mask.clone(), link.from));

        // If the link isn't a splice and we're only splicing at call locations, then keep the link
        // iff it has a corresponding call
        if link.from.block == link.to.block {
            return is_call_location;
        }

        // At this point, we know:
        // - this is a splice with no call
        // - `splice_style` is not `SpliceStyle::LeadLabels`
        // Therefore, we keep a link iff it's a call location and we're using
        // `SpliceStyle::CallLocations`
        splice_style == SpliceStyle::CallLocations && is_call_location
    });
}

/////////////////
// STARTS/ENDS //
/////////////////

fn rounds_locations<I: index_vec::Idx>(
    methods: &[super::Method],
    stage: Stage,
    boundary: Boundary,
) -> IndexVec<I, StartOrEnd> {
    let rounds = RowBuf::rounds(stage);

    let mut positions = IndexVec::new();
    for (method_idx, method) in methods.iter().enumerate() {
        for ch_mask in &method.ch_masks {
            for (row_idx, annot_row) in method.plain_course.annot_rows().enumerate() {
                let transposed_mask = ch_mask.mask().mul(annot_row.row());
                // If rounds satisfies `transposed_mask`, then this location can contain rounds
                if transposed_mask.matches(&rounds) {
                    // Decide whether this is snap start/finish
                    let sub_lead_index = annot_row.annot().sub_lead_idx;
                    let is_snap = sub_lead_index != 0;
                    let course_head_containing_rounds = annot_row.row().inv();

                    if method
                        .allowed_indices(boundary)
                        .map_or(true, |idxs| idxs.contains(&sub_lead_index))
                    {
                        positions.push(StartOrEnd {
                            course_head: course_head_containing_rounds.clone(),
                            row_idx: RowIdx::new(BlockIdx::new(method_idx), row_idx),
                            label: (if is_snap { boundary.snap_label() } else { "" }).to_owned(),
                        });
                    }
                }
            }
        }
    }

    positions
}
