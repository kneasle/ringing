use std::{
    collections::{HashMap, HashSet},
    ops::Mul,
};

use bellframe::{method::RowAnnot, AnnotBlock, Bell, Mask, Method, Row, RowBuf, Stage};
use itertools::Itertools;

use super::{BlockIdx, BlockVec, CourseHeadMask, Layout, Link, RowIdx, StartOrEnd};

/// Helper function to generate a [`Layout`] from human-friendly inputs (i.e. what [`Method`]s,
/// [`Call`](super::Call)s and course heads to use).
pub(super) fn from_methods(
    methods: &[(Method, String)],
    calls: &[super::Call],
    splice_style: SpliceStyle,
    // The course head masks, along with which bell is 'calling bell' during that course.
    // Allowing different calling bells allows us to do things like keep using W,M,H during
    // courses of e.g. `1xxxxx0987`.
    ch_masks: Vec<(Mask, Bell)>,
    // Which sub-lead indices are considered valid starting or finishing points for the
    // composition.  If these are `None`, then any location is allowed
    allowed_start_indices: Option<&[usize]>,
    allowed_end_indices: Option<&[usize]>,
    plain_lead_weight: f32,
) -> Result<Layout> {
    // Cache data about each method, and compute the overall stage of the comp
    let (mut method_datas, stage) = gen_method_data(methods, calls, ch_masks)?;
    let is_spliced = method_datas.len() > 1;

    // Pre-process & check CH masks:
    for d in &mut method_datas {
        // Remove redundant CH masks, and check that no courses can have two different calling bells.
        d.ch_masks = dedup_ch_masks(&d.ch_masks)?;
        // Check that two CH masks don't label the same course at two different leads.
        check_for_ambiguous_courses(&d.ch_masks, &d.lead_heads)?;
    }

    // Generate links
    let links = generate_links(
        &method_datas,
        plain_lead_weight,
        stage,
        is_spliced,
        splice_style,
    )?
    .into();

    Ok(Layout {
        links,
        starts: rounds_locations(&method_datas, stage, allowed_start_indices, "<").into(),
        ends: rounds_locations(&method_datas, stage, allowed_end_indices, ">").into(),
        // Create a block for each method
        blocks: method_datas
            .into_iter()
            .map(|d| d.into_block(is_spliced))
            .collect::<BlockVec<_>>(),
        stage,
    })
}

/// The different styles of spliced that can be generated
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label
    LeadLabels,
    /// Splice only happen whenever a call _could_ have happened
    CallLocations,
    /// Splices only happen when calls are actually made
    Calls,
}

/// The ways that [`Layout::single_method`] can fail
#[derive(Debug, Clone)]
pub enum Error {
    NoMethods,
    UndefinedLeadLocation(String),
    CallingPositionsTooShort {
        call_name: String,
        calling_position_len: usize,
        stage: Stage,
    },
    /// Some courses match two different [`CourseHeadMask`]s with **different** calling bells.
    ConflictingCallingBell(CourseHeadMask, CourseHeadMask),
    AmbiguousCourseHeadPosition {
        /// The first possible course head for the ambiguous course
        mask1: Mask,
        /// The course head mask given by the user which `mask1` satisfies
        input_mask1: Mask,
        /// The second possible course head for the ambiguous course
        mask2: Mask,
        /// The course head mask given by the user which `mask1` satisfies
        input_mask2: Mask,
    },
}

pub type Result<T> = std::result::Result<T, Error>;

///////////////////////////////
// GENERATION OF METHOD DATA //
///////////////////////////////

/// Generate a [`MethodData`] struct for each source [`Method`].  This also adds fixed bells (e.g.
/// the treble) to the course heads.
fn gen_method_data<'a>(
    methods: &'a [(Method, String)],
    calls: &'a [super::Call],
    ch_masks: Vec<(Mask, Bell)>,
) -> Result<(Vec<MethodData<'a>>, Stage)> {
    let stage = methods
        .iter()
        .map(|(method, _shorthand)| method.stage())
        .max()
        .ok_or(Error::NoMethods)?;
    assert!(methods.iter().all(|(m, _)| m.stage() == stage)); // TODO: Implement mixed stage splicing

    let calls_per_method = methods
        .iter()
        .map(|_| calls.iter().collect_vec()) // TODO: Allow calls to be limited to some methods
        .collect_vec();

    // Add fixed bells (e.g. the treble) to the CH masks.  Skipping this would preserve the
    // correctness of the graph but makes the falseness detection consume a completely unnecessary
    // amount of time and memory.
    //
    // TODO: If we're mixing methods with different fixed bells (e.g. Stedman and treble-oriented
    // methods), should this be done per-method - perhaps optionally?)
    let ch_masks = add_fixed_bells(ch_masks, methods, &calls_per_method, stage);
    // Convert the (Mask, Bell) pairs into (possibly many) `CourseHeadMask`s
    let ch_masks = ch_masks
        .into_iter()
        .flat_map(|(mask, bell)| CourseHeadMask::new(mask, bell))
        .collect_vec();

    let method_datas = methods
        .iter()
        .map(|(m, shorthand)| MethodData::new(m, shorthand.to_owned(), calls, &ch_masks))
        .collect_vec();
    Ok((method_datas, stage))
}

/// Detect fixed bells (a place bell which is preserved by all methods' calls and plain leads) and
/// fix it in all the course heads.  In most cases, this will add the treble as a fixed bell
/// (allowing the falseness detection to use it to reduce the size of the falseness table).
fn add_fixed_bells(
    ch_masks: Vec<(Mask, Bell)>,
    methods: &[(Method, String)],
    calls_per_method: &[Vec<&super::Call>],
    stage: Stage,
) -> Vec<(Mask, Bell)> {
    let fixed_bells = fixed_bells(methods, calls_per_method, stage);

    let mut fixed_ch_masks = Vec::with_capacity(ch_masks.len());
    'mask_loop: for (mut mask, calling_bell) in ch_masks {
        // Attempt to add the fixed bells to this mask
        for &b in &fixed_bells {
            if let Err(()) = mask.fix(b) {
                // If a bell is known to be fixed in its home position but a mask requires it to be
                // outside of its home position, then that mask will never be satisfied and can be
                // removed.  For example, this would remove `x1xxx...` as a course head in Surprise
                // with standard calls because we know that the treble cannot leave 1st place.
                continue 'mask_loop;
            }
        }
        // If all fixed bells could be set, then keep this mask
        fixed_ch_masks.push((mask, calling_bell))
    }
    fixed_ch_masks
}

/// Returns the place bells which are always preserved by plain leads and all calls (e.g. hunt
/// bells in non-variable-hunt compositions).
fn fixed_bells(
    methods: &[(Method, String)],
    calls_per_method: &[Vec<&super::Call>],
    stage: Stage,
) -> Vec<Bell> {
    // Start off with all bells fixed
    let mut all_fixed_bells = stage.bells().collect_vec();
    for ((method, _shorthand), calls) in methods.iter().zip_eq(calls_per_method) {
        // Start the set with the bells which are fixed by the plain lead of every method
        let mut fixed_bells: HashSet<Bell> = method.lead_head().fixed_bells().collect();
        for call in calls {
            // For each call, remove the bells which aren't fixed by that call (e.g. the 2 in
            // Grandsire is unaffected by a plain lead, but affected by calls)
            filter_bells_fixed_by_call(method, call, &mut fixed_bells);
        }
        // Intersect the fixed bells of this method with the full list
        all_fixed_bells.retain(|b| fixed_bells.contains(b));
    }
    all_fixed_bells
}

// For every position that this call could be placed, remove any bells which **aren't** preserved
// by placing the call at this location.
fn filter_bells_fixed_by_call(method: &Method, call: &super::Call, set: &mut HashSet<Bell>) {
    // Note that all calls are required to only substitute one piece of place notation.
    for sub_lead_idx_after_call in method.label_indices(&call.lead_location) {
        let idx_before_call = (sub_lead_idx_after_call + method.lead_len() - 1) % method.lead_len();
        let idx_after_call = idx_before_call + 1; // in range `1..=method.lead_len()`

        // The row before a call in this location in the _first lead_
        let row_before_call = method.first_lead().get_row(idx_before_call).unwrap();
        // The row after a plain call in this location in the _first lead_
        let row_after_no_call = method.first_lead().get_row(idx_after_call).unwrap();
        // The row after a call in this location in the _first lead_
        let mut row_after_call = row_before_call.to_owned();
        call.place_not.permute(&mut row_after_call).unwrap();

        // A bell is _affected_ by the call iff it's in a different place in `row_after_call` than
        // `row_after_no_call`.  These should be removed from the set, because they are no longer
        // fixed.
        for (bell_after_no_call, bell_after_call) in
            row_after_no_call.bell_iter().zip(&row_after_call)
        {
            if bell_after_call != bell_after_no_call {
                set.remove(&bell_after_call);
            }
        }
    }
}

/////////////////////////////////
// COURSE HEAD MASK PROCESSING //
/////////////////////////////////

/// Remove course head masks which are an exact subset of another mask.  E.g. if `1xxx5678` and
/// `1xxxxx78` are both defined, then `1xxx5678` can be removed (assuming they both specify the
/// same calling bell).  It is an error if two masks are compatible but specify different calling
/// bells.
fn dedup_ch_masks(course_head_masks: &[CourseHeadMask]) -> Result<Vec<CourseHeadMask>> {
    let mut deduped_ch_masks = Vec::with_capacity(course_head_masks.len());
    'outer: for (i, ch_mask) in course_head_masks.iter().enumerate() {
        // Look for another CH mask who's matched CHs are a superset of `mask`
        for (other_i, other_ch_mask) in course_head_masks.iter().enumerate() {
            if i == other_i {
                continue; // Don't compare masks against themselves
            }
            if ch_mask.mask.is_subset_of(&other_ch_mask.mask)
                && ch_mask.calling_bell == other_ch_mask.calling_bell
            {
                continue 'outer; // Skip this `ch_mask` if it is implied by another mask
            }
        }
        deduped_ch_masks.push(ch_mask.clone());
    }
    Ok(deduped_ch_masks)
}

/// Generate an error if any course could be given two course heads at **different** lead
/// locations.
///
/// For example, in a method with Plain Bob lead heads, `1xxxxx78` and `1x56x8x7` would create an
/// ambiguity because some courses could either be labelled `15xxx678` or `1x56x8x7`.
fn check_for_ambiguous_courses(
    course_head_masks: &[CourseHeadMask],
    lead_heads: &[RowBuf],
) -> Result<()> {
    for (i, ch_mask) in course_head_masks.iter().enumerate() {
        let mask = ch_mask.mask();
        for lead_head in lead_heads.iter() {
            if lead_head.is_rounds() {
                continue; // It's OK for two CH masks to label the same lead as a course head
            }

            // ... compute mask for this lead head at this location ...
            let transposed_mask = mask.mul(lead_head);
            // ... and check that it isn't compatible with any of the course head mask we've
            // seen so far.  We only need to check each pair of CH masks once (since ambiguity
            // is symmetric).
            for other_ch_mask in &course_head_masks[..=i] {
                let other_mask = other_ch_mask.mask();
                // If the lead-end is compatible with a course head mask, then derive which
                // leads are ambiguous, and return an error
                if let Some(course_head_2) = transposed_mask.combine(other_ch_mask.mask()) {
                    // Generate the second course head by transposing `mask2` at this lead
                    // back to the course head satisfying `mask`
                    let course_head_1 = other_mask.mul(&lead_head.inv()).combine(mask).unwrap();
                    return Err(Error::AmbiguousCourseHeadPosition {
                        mask1: course_head_1,
                        input_mask1: mask.clone(),
                        mask2: course_head_2,
                        input_mask2: other_mask.clone(),
                    });
                }
            }
        }
    }
    Ok(())
}

/////////////////////
// LINK GENERATION //
/////////////////////

/// Generates the set of [`Link`]s which are valid according to the `course_head_masks`
fn generate_links(
    method_datas: &[MethodData],
    plain_lead_weight: f32,
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
        plain_lead_weight,
        stage,
        is_spliced,
        splice_style,
    )?;
    filter_plain_links(&mut links, splice_style);
    dedup_links(&mut links);
    Ok(links)
}

/// For each lead label, list the positions in the course where calls at that label could start
/// (Monument assumes that all courses always replace one piece of [`PlaceNot`]ation).
fn call_starts_by_label<'m>(method_datas: &[MethodData<'m>]) -> HashMap<&'m str, Vec<RowIdx>> {
    let mut label_indices = HashMap::<&'m str, Vec<RowIdx>>::new();

    // For every method (and the block it will generate) ...
    for (idx, d) in method_datas.iter().enumerate() {
        let block_idx = BlockIdx::new(idx); // Each method will correspond to one block

        // ... for every labelled row ...
        let course_len = d.plain_course.len();
        for (row_idx, annot) in d.plain_course.annots().enumerate() {
            if let Some(label) = annot.label() {
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
struct CallEnd<'meth> {
    lead_location: &'meth str,
    /// For a call to be valid, it must produce a row which matches this [`Mask`].
    row_mask: Mask,
    /// The [`Method`] to which this would switch
    method_idx: usize,
    ch_mask_idx: usize,
    row_idx: usize,
}

fn call_ends<'meth>(method_datas: &[MethodData<'meth>]) -> Vec<CallEnd<'meth>> {
    let mut call_ends = Vec::new();

    // For every method ...
    for (method_idx, d) in method_datas.iter().enumerate() {
        // ... for every labelled row in the plain course ...
        for (row_idx, annot_row) in d.plain_course.annot_rows().enumerate() {
            if let Some(label) = annot_row.annot().label() {
                let row_after_call = annot_row.row();
                // ... for every course head mask ...
                for (ch_mask_idx, ch_mask) in d.ch_masks.iter().enumerate() {
                    // ... add a `CallEnd` corresponding to placing a call at this location to
                    // **enter** this course
                    call_ends.push(CallEnd {
                        lead_location: label,
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

#[derive(Debug)]
struct LinkGenData<'a> {
    method_datas: &'a [MethodData<'a>],
    call_ends: &'a [CallEnd<'a>],
    stage: Stage,
}

/// Generate all links allowed within the course head constraints, potentially with duplicates.
/// This will produce every possible splice, and it's up to [`filter_plain_links`] to unwanted
/// splices.
fn generate_all_links(
    method_datas: &[MethodData],
    call_ends: &[CallEnd],
    call_starts_by_label: &HashMap<&str, Vec<RowIdx>>,
    plain_lead_weight: f32,
    stage: Stage,
    is_spliced: bool,
    splice_style: SpliceStyle,
) -> Result<Vec<Link>> {
    let mut links = Vec::<Link>::new();
    let link_gen_data = LinkGenData {
        method_datas,
        call_ends,
        stage,
    };

    /*
    The general approach here is to attempt to place calls in every position, compute which row
    they would lead to, the try to find a `CallEnd` which corresponds to that row.  If such a
    `CallEnd` exists, then this call is valid and should be included in the node graph.  We also
    try to place plain leads at every location, and likewise check for any valid `CallEnd`s.
    */

    // For every method and CH mask ...
    for (method_idx, d) in method_datas.iter().enumerate() {
        for from_ch_mask in &d.ch_masks {
            // ... test every call in every valid position in the course ...
            for call in &d.calls {
                let lead_label = call.lead_location.as_str();
                let call_starts = call_starts_by_label
                    .get(lead_label)
                    .ok_or_else(|| Error::UndefinedLeadLocation(lead_label.to_owned()))?;
                for &from_idx in call_starts {
                    if from_idx.block.index() != method_idx {
                        continue; // Skip any call starts which aren't from this method
                    }

                    /* ADD CALL LINKS */

                    // Link corresponding to placing this call
                    let row_before_call = d.plain_course.get_row(from_idx.row).unwrap();
                    let row_after_call = call.place_not.permute_new(row_before_call).unwrap();
                    // Get the mask required by the row **after** this call.  This link can be
                    // generated only if a `CallEnd` is compatible with this mask.
                    let mask_after_call = from_ch_mask.mask.mul(&row_after_call);

                    // Get the debug/display names for any link in this position
                    let tenor_place = mask_after_call
                        .place_of(from_ch_mask.calling_bell())
                        .expect("Course head mask doesn't fix the calling bell");
                    let calling_position =
                        call.calling_positions.get(tenor_place).ok_or_else(|| {
                            Error::CallingPositionsTooShort {
                                call_name: call.debug_symbol.to_owned(),
                                calling_position_len: call.calling_positions.len(),
                                stage,
                            }
                        })?;

                    // Closure used to format a call string
                    let fmt_call = |symbol: &str, calling_pos: &str| -> String {
                        if is_spliced {
                            // If we're ringing spliced, then put calls in `[]`s to differentiate
                            // them from method names
                            format!("[{}{}]", symbol, calling_pos)
                        } else {
                            format!("{}{}", symbol, calling_pos)
                        }
                    };

                    // Add links for this call, splicing to any available method
                    add_links_for_call(
                        from_idx,
                        &from_ch_mask.mask,
                        &row_after_call,
                        &fmt_call(&call.debug_symbol, calling_position),
                        &fmt_call(&call.display_symbol, calling_position),
                        call.weight,
                        None, // Calls are always allowed to change method
                        &link_gen_data,
                        &mut links,
                    );

                    // Add corresponding plain links, according to the splicing style
                    if splice_style != SpliceStyle::LeadLabels {
                        let idx_after_plain = (from_idx.row + 1) % d.plain_course.len();
                        let row_after_plain = d.plain_course.get_row(idx_after_plain).unwrap();
                        add_links_for_call(
                            from_idx,
                            &from_ch_mask.mask,
                            row_after_plain,
                            &fmt_call("p", calling_position),
                            "", // Don't display plain leads in output
                            plain_lead_weight,
                            // Splicing on plain leads at call locations is allowed for
                            // `CallLocations`, but not for just `Calls`
                            (splice_style == SpliceStyle::CallLocations).then(|| method_idx),
                            &link_gen_data,
                            &mut links,
                        );
                    }
                }
            }
        }
    }

    /* Generate plain links for method splices */

    // If splices are possible every lead, then the composition can branch at every lead.
    // Therefore, we need a plain link at every lead to every possible method (including the one
    // being spliced from).
    if splice_style == SpliceStyle::LeadLabels {
        for &from_idx in call_starts_by_label.values().flatten() {
            let d = &method_datas[from_idx.block.index()];
            let row_after_plain = d
                .plain_course
                .get_row((from_idx.row + 1) % d.plain_course.len())
                .unwrap();

            // Note that we need to create specific plain links per mask in case different methods
            // have different CH mask requirements
            for from_ch_mask in &d.ch_masks {
                add_links_for_call(
                    from_idx,
                    &from_ch_mask.mask,
                    row_after_plain,
                    if is_spliced { "[p]" } else { "p" }, // Debug with no call position
                    "",                                   // Don't display plain leads
                    plain_lead_weight,
                    None, // We want to allow splices at every lead
                    &link_gen_data,
                    &mut links,
                );
            }
        }
    }

    Ok(links)
}

/// For a given call in a given position, create links which come out of this into any possible
/// method
#[allow(clippy::too_many_arguments)]
fn add_links_for_call(
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
) {
    let mask_after_call = from_ch_mask.mul(row_after_call);

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
    }
}

/// Remove any [`Link`]s which are equal to another [`Link`] (ignoring names).
///
/// This is required because [`generate_all_links`] creates a large number of identical plain call
/// links if there are multiple calls at the same position (which there almost always are).
///
/// This doesn't always actually lead to the generation of duplicate compositions (because there
/// could be two identical calls with different but compatible course head masks), so the graph
/// generation code has to perform de-duplication regardless.  However, de-duplication makes the
/// code both more performant and, more importantly, makes the resulting [`Layout`]s easier to
/// debug.
fn dedup_links(links: &mut Vec<Link>) {
    // The indices of links which are special cases of some other link (or are identical to other
    // links)
    let mut redundant_link_idxs = Vec::<usize>::new();
    for (i, link) in links.iter().enumerate() {
        for (i2, link2) in links.iter().enumerate() {
            // Links are always compatible with themselves, and there's no point 'de-duplicating' a
            // link because it's redundant against itself
            if i == i2 {
                continue;
            }

            // This is 'true' if `link` and `link2` are equal apart from their course head masks.
            // We don't check the names, since it's possible that the same link is given two
            // different names (e.g. near near would generate `pI` and `pT` which should be
            // considered identical).  If the links do have different names, then one of them is
            // picked arbitrarily.
            let are_links_otherwise_equal = link.eq_without_name_or_ch_mask(link2);

            if are_links_otherwise_equal {
                if link.ch_mask == link2.ch_mask {
                    // If the links are identical, then we remove the one with the least index.
                    // This way, exactly one link from a group of identical links (the one with the
                    // highest index) will survive
                    if i < i2 {
                        redundant_link_idxs.push(i);
                    }
                } else if link.ch_mask.is_subset_of(&link2.ch_mask) {
                    // If `link2`'s CH mask is more general than `link`'s, and `link` and `link2`
                    // are otherwise equal, then `link` is a special case of `link2` and is
                    // therefore redundant
                    redundant_link_idxs.push(i);
                }
            }
        }
    }

    // Now actually remove the unnecessary links, making sure to iterate backwards so that the
    // indices keep pointing to the right elements
    redundant_link_idxs.sort_unstable();
    redundant_link_idxs.dedup();
    for idx in redundant_link_idxs.into_iter().rev() {
        links.remove(idx);
    }
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

fn rounds_locations(
    method_datas: &[MethodData],
    stage: Stage,
    allowed_sub_lead_indices: Option<&[usize]>,
    snap_label: &str,
) -> Vec<StartOrEnd> {
    let rounds = RowBuf::rounds(stage);

    let mut positions = Vec::new();
    for (method_idx, d) in method_datas.iter().enumerate() {
        for ch_mask in &d.ch_masks {
            for (row_idx, annot_row) in d.plain_course.annot_rows().enumerate() {
                let transposed_mask = ch_mask.mask().mul(annot_row.row());
                // If rounds satisfies `transposed_mask`, then this location can contain rounds
                if transposed_mask.matches(&rounds) {
                    // Decide whether this is snap start/finish
                    let sub_lead_index = annot_row.annot().sub_lead_idx();
                    let is_snap = sub_lead_index != 0;
                    let course_head_containing_rounds = annot_row.row().inv();

                    if allowed_sub_lead_indices.map_or(true, |idxs| idxs.contains(&sub_lead_index))
                    {
                        positions.push(StartOrEnd {
                            course_head: course_head_containing_rounds.clone(),
                            row_idx: RowIdx::new(BlockIdx::new(method_idx), row_idx),
                            label: (if is_snap { snap_label } else { "" }).to_owned(),
                        });
                    }
                }
            }
        }
    }

    positions
}

/// Cached data about each [`Method`] in the resulting [`Layout`]
#[derive(Debug, Clone)]
struct MethodData<'a> {
    /// The [`Method`] which this data is about
    method: &'a Method,
    /// The string used to represent one lead of this method
    shorthand: String,
    /// The calls which can be applied to this [`Method`].
    calls: Vec<&'a super::Call>,
    /// The [`CourseHeadMask`]s which can be applied to this method
    ch_masks: Vec<CourseHeadMask>,

    /// The plain course of this [`Method`], with sub-lead indices and labels
    plain_course: AnnotBlock<RowAnnot<'a>>,
    /// The lead heads of the plain course of this [`Method`], in order from rounds
    lead_heads: Vec<RowBuf>,
}

impl<'a> MethodData<'a> {
    fn new(
        method: &'a Method,
        shorthand: String,
        calls: &'a [super::Call],
        ch_masks: &[CourseHeadMask],
    ) -> Self {
        Self {
            method,
            shorthand,
            calls: calls.iter().collect_vec(), // TODO: Allow calls to be assigned to specific methods
            ch_masks: ch_masks.to_owned(), // TODO: Allow CH masks to be assigned to specific methods

            plain_course: method.plain_course(),
            lead_heads: method.lead_head().closure_from_rounds(),
        }
    }

    fn into_block(self, is_spliced: bool) -> AnnotBlock<Option<String>> {
        let shorthand = &self.shorthand;
        self.plain_course.map_annots(|annot| {
            (annot.sub_lead_idx() == 0 && is_spliced).then(|| shorthand.clone())
        })
    }
}
