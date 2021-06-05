use std::{collections::HashMap, iter::repeat_with};

use bellframe::{method::LABEL_LEAD_END, AnnotRow, Bell, Method, PlaceNot, Row, Stage};
use itertools::Itertools;

use super::{Layout, Segment, SegmentLink};

/// Generates a [`Layout`] for a single [`Method`]
pub fn single_method_layout(
    method: &Method,
    plain_lead_positions: Option<Vec<String>>,
    calls: &[CallSpec],
    non_fixed_bells: &[Bell],
) -> Result<Layout, SingleMethodError> {
    /* Computing the full layout from method/calls is a pipeline of several several stages:
     * 1. Generate `HashMap`s of the rows where calls can be applied (along with the indices of the
     *    fixed bells at that location).  These values could be derived on-the-fly by running
     *    linear search over the rows in `plain_course`, but these maps make the code clearer and
     *    faster.
     * 2. Using these maps, consider each call and try to place it in every location.  Now,
     *    generate the row after the call and if the fixed bell pattern of this row appears
     *    somewhere in the plain course then the fixed bells are unaffected and the call is
     *    allowed.  In this case, compute the course head transposition and calling position of
     *    this call and store it in a list of `CallJump`s.
     * 3. Use these call jumps (and the plain lead versions) to split the course up into a set of
     *    (not necessarily mutually exclusive) fragments which have to be rung consecutively.  At
     *    this point, we also add plain leads as links between segments (the DFS engine can't tell
     *    different link types apart).
     * 4. Convert the `CallJump`s and row ranges into the light-weight Layout struct. */

    /* COMMONLY USED VALUES */
    // We can safely unwrap here because `method.stage()` can't be zero
    let fixed_bells = (0..method.stage().as_usize())
        .map(Bell::from_index)
        .filter(|b| !non_fixed_bells.contains(b))
        .collect_vec();
    let tenor = Bell::tenor(method.stage()).unwrap();
    let plain_course = method.plain_course();
    let course_len = plain_course.len();
    let plain_lead_positions =
        plain_lead_positions.unwrap_or_else(|| default_plain_lead_positions(method.stage()));

    /* 1. Generate maps for call locations */

    // This maps lead locations to a list of row indices which occur just before this label.  These
    // correspond to lead **ends**.
    let mut call_starts: HashMap<&str, Vec<usize>> = HashMap::new();
    // This maps `(lead_location, fixed_bell_indices)` to the index of the row within the course
    // where that pattern occurs.  These correspond to lead **heads**.
    let mut call_ends: HashMap<(&str, Vec<usize>), usize> = HashMap::new();
    for (i, annot_row) in plain_course.annot_rows()[..course_len].iter().enumerate() {
        if let Some(lead_location) = annot_row.annot().1 {
            // Generate fixed bell positions
            let fixed_bell_indices = fixed_bells
                .iter()
                .map(|b| annot_row.row().place_of(*b).unwrap())
                .collect_vec();
            let key = (lead_location, fixed_bell_indices);
            // We shouldn't have the same fixed bell pattern in multiple places in the course
            assert!(call_ends.get(&key).is_none());
            // Add this location to the maps
            call_ends.insert(key, i);
            call_starts
                .entry(lead_location)
                .or_insert_with(Vec::new)
                // We add `plain_course` before subtracting one so that we don't cause an underflow
                // when i is 0
                .push((i + course_len - 1) % course_len);
        }
    }

    /* 2. Generate a map of which rows are connected by calls */

    #[derive(Debug, Clone)]
    struct CallJump<'a> {
        from_index: usize,
        to_index: usize,
        new_course_head: Row,
        call: &'a CallSpec,
        calling_position: &'a str,
    }

    let mut call_jumps = Vec::<CallJump>::new();
    for (i, call) in calls.iter().enumerate() {
        let lead_loc = call.lead_location.as_str();

        // Test this call at every correctly named location in the course
        for &from_index in call_starts
            .get(lead_loc)
            .ok_or_else(|| SingleMethodError::UndefinedLeadLocation(lead_loc.to_owned()))?
        {
            // Determine what the row after this call would be.
            //
            // This unsafety is OK because all the rows & pns are parsed within this function,
            // which is provided a single Stage
            let row_after_call = unsafe {
                call.place_not
                    .permute_new_unchecked(plain_course.get_row(from_index).unwrap())
            };
            // Check that the row after this call has the fixed bells in a pattern which appears in
            // the plain course (and therefore the call won't affect the fixed bells).  If the
            // fixed bells _are_ unaffected, then this call location is allowed.
            if let Some(&to_index) =
                call_ends.get(&(lead_loc, get_bell_inds(&fixed_bells, &row_after_call)))
            {
                // Find the calling position of this call, returning an error if the call doesn't
                // have enough calling positions
                let tenor_place = row_after_call.place_of(tenor).unwrap();
                let call_pos = call.calling_positions.get(tenor_place).ok_or_else(|| {
                    SingleMethodError::CallingPositionsTooShort {
                        call_name: call.debug_symbol.to_owned(),
                        call_index: i,
                        calling_position_len: call.calling_positions.len(),
                        stage: method.stage(),
                    }
                })?;
                // Calculate the course head of the course which is reached by applying this call
                // during the plain course.
                //
                // This unsafety is OK because all the rows & pns are parsed within this
                // function, which is provided a single Stage
                let new_course_head = unsafe {
                    plain_course
                        .get_row(to_index)
                        .unwrap()
                        .tranposition_to_unchecked(&row_after_call)
                };
                // Group all this data into a `CallJump`
                call_jumps.push(CallJump {
                    from_index,
                    to_index,
                    new_course_head,
                    call,
                    calling_position: &call_pos,
                });
            }
        }
    }

    /* 3. Use this call mapping to split every course into a set of (not necessarily mutually
     * exclusive) ranges */

    // We have to start a range after every call, and end a range just before that call
    let mut range_starts = call_jumps
        .iter()
        .map(|call_jump| call_jump.to_index)
        .collect_vec();
    let mut range_ends = call_jumps
        .iter()
        .map(|call_jump| call_jump.from_index)
        .collect_vec();
    // If a call is omitted, then we can perform an un-jump from one lead end to the
    // consecutive lead head.  These have to be added manually if we have calls which affect
    // but don't permute fix bells (e.g. 4ths place calls in n-ths place methods).
    range_starts.extend(range_ends.iter().map(|&x| (x + 1) % course_len));

    // Sort and de-duplicate the starts and ends (later algorithms rely on binary searching
    // these arrays)
    range_starts.sort_unstable();
    range_starts.dedup();
    range_ends.sort_unstable();
    range_ends.dedup();

    // Calculate the ranges.  Each of `range_starts` corresponds to a unique range, which ends
    // at the first range_end which is encountered (wrapping round the end of the course if
    // needed).  **NOTE:** this also increments all end-points, so that the ranges are
    // half-open (which is the semantics of `..`)
    let segment_ranges = range_starts
        .iter()
        .map(|&start| {
            let range_end_ind = range_ends.binary_search(&start).unwrap_err();
            let end = *range_ends.get(range_end_ind).unwrap_or(&range_ends[0]);
            // `end` is an inclusive bound, but `..` is half-open.  Therefore, we add one here
            // to correct for this
            start..end + 1
        })
        .collect_vec();

    /* 4. Convert the `CallJump`s and ranges into a Layout, and return. */

    // Conversion table from range starts to their index within `ranges`
    let segment_index_by_start = segment_ranges
        .iter()
        .enumerate()
        .map(|(i, r)| (r.start, i))
        .collect::<HashMap<_, _>>();

    let segments = segment_ranges
        .iter()
        .map(|range| {
            /* CLONE THE ROWS THAT ARE CONTAINED IN THIS RANGE */
            let mut rows = Vec::new();

            if range.end > range.start {
                // If the range doesn't wrap over the course end, then just we can just slice the
                // plain course
                rows.extend(
                    plain_course.annot_rows()[range.clone()]
                        .iter()
                        .map(AnnotRow::row)
                        .cloned(),
                );
            };

            /* GENERATE LINKS */

            // For fragments that end at the course head, `range.end` will equal `course_len`
            // but the modulo maps that back to 0 so it can be connected to the first segment
            let next_segment_start_index = range.end % course_len;

            /* Calculate which calling position this plain lead should be named. */
            let plain_lead_tenor_place = plain_course
                .get_row((range.end + course_len - 1) % course_len)
                .unwrap()
                .place_of(tenor)
                .unwrap();
            let plain_calling_pos = plain_lead_positions
                .get(plain_lead_tenor_place)
                .expect("Plain calling position string is shorter than stage");
            /* Linking this segment with a plain lead */
            let plain_lead_link = SegmentLink {
                // Plain leads are elided when displaying
                display_name: String::new(),
                debug_name: format!("p{}", plain_calling_pos),
                end_segment: segment_index_by_start[&next_segment_start_index],
                transposition: Row::rounds(method.stage()),
            };

            /* Linking this segment with a call */
            let mut links = vec![plain_lead_link];
            links.extend(
                call_jumps
                    .iter()
                    // Remove any calls that don't appear at the end of this range.  The `- 1`
                    // corrects for the fact that call starts refer to lead ends, whereas
                    // ranges are half open and therefore their end index refers to the next lead
                    // **head**.  We need `+ course_len` here to prevent the subtraction from
                    // causing an underflow if `range.end == 0`.
                    .filter(|call_jump| {
                        call_jump.from_index == (range.end + course_len - 1) % course_len
                    })
                    .map(|call_jump| {
                        let call_pos = call_jump.calling_position;
                        SegmentLink {
                            display_name: format!("{}{}", call_jump.call.display_symbol, call_pos),
                            debug_name: format!("{}{}", call_jump.call.debug_symbol, call_pos),
                            end_segment: segment_index_by_start[&call_jump.to_index],
                            transposition: call_jump.new_course_head.clone(),
                        }
                    }),
            );

            /* COMBINE ROWS AND LINKS INTO A `Segment` */

            Segment { rows, links }
        })
        .collect_vec();

    Ok(Layout { segments })
}

#[derive(Debug, Clone)]
pub enum SingleMethodError {
    UndefinedLeadLocation(String),
    CallingPositionsTooShort {
        call_name: String,
        call_index: usize,
        calling_position_len: usize,
        stage: Stage,
    },
}

/// The specification for a call that can be used in a composition
#[derive(Debug, Clone)]
pub struct CallSpec {
    display_symbol: String,
    debug_symbol: String,
    lead_location: String,
    place_not: PlaceNot,
    calling_positions: Vec<String>,
}

impl CallSpec {
    pub fn new(
        display_symbol: String,
        debug_symbol: String,
        lead_location: String,
        place_not: PlaceNot,
        calling_positions: Option<Vec<String>>,
    ) -> Self {
        Self {
            display_symbol,
            debug_symbol,
            lead_location,
            calling_positions: calling_positions
                .unwrap_or_else(|| default_calling_positions(&place_not)),
            place_not,
        }
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_bob(place_not: PlaceNot) -> Self {
        Self::new(
            String::new(),
            "-".to_owned(),
            LABEL_LEAD_END.to_owned(),
            place_not,
            None,
        )
    }

    /// Create a bob which replaces the lead end with a given [`PlaceNot`]
    pub fn lead_end_single(place_not: PlaceNot) -> Self {
        Self::new(
            "s".to_owned(),
            "s".to_owned(),
            LABEL_LEAD_END.to_owned(),
            place_not,
            None,
        )
    }
}

fn default_plain_lead_positions(stage: Stage) -> Vec<String> {
    let named_positions = "LBIFXVES?N";

    // Generate calling positions that aren't M, W or H
    let mut positions =
        // Start off with the single-char position names
        named_positions
        .chars()
        .map(|c| c.to_string())
        // Extending forever with numbers
        .chain(repeat_with(|| "?".to_owned()))
        // But we consume one value per place in the Stage
        .take(stage.as_usize())
        .collect_vec();

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a place indexed from the end of the stage (so 0 is the highest place)
    macro_rules! replace_mwh {
        ($ind: expr, $new_val: expr) => {
            if let Some(place) = stage.as_usize().checked_sub(1 + $ind) {
                if let Some(v) = positions.get_mut(place) {
                    v.clear();
                    v.push($new_val);
                }
            }
        };
    }

    // Middles are only defined on >= 7 bells
    if stage >= Stage::TRIPLES {
        replace_mwh!(if stage.is_even() { 3 } else { 2 }, 'M');
    }
    // Wrongs are only defined on >= 6 bells
    if stage >= Stage::MINOR {
        replace_mwh!(if stage.is_even() { 0 } else { 1 }, 'W');
    }
    // Homes are defined on >= 5 bells
    if stage >= Stage::DOUBLES {
        replace_mwh!(if stage.is_even() { 1 } else { 0 }, 'H');
    }

    positions
}

fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
    let named_positions = "LIBFVXSEN";

    // Generate calling positions that aren't M, W or H
    let mut positions =
        // Start off with the single-char position names
        named_positions
        .chars()
        .map(|c| c.to_string())
        // Extending forever with numbers
        .chain((named_positions.len()..).map(|i| (i + 1).to_string()))
        // But we consume one value per place in the Stage
        .take(place_not.stage().as_usize())
        .collect_vec();

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a given (0-indexed) place
    macro_rules! replace_pos {
        ($ind: expr, $new_val: expr) => {
            if let Some(v) = positions.get_mut($ind) {
                v.clear();
                v.push($new_val);
            }
        };
    }

    // Edge case: if 2nds are made in `place_not`, then I/B are replaced with B/T.  Note that
    // places are 0-indexed
    if place_not.contains(1) {
        replace_pos!(1, 'B');
        replace_pos!(2, 'T');
    }

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a place indexed from the end of the stage (so 0 is the highest place)
    macro_rules! replace_mwh {
        ($ind: expr, $new_val: expr) => {
            if let Some(place) = place_not.stage().as_usize().checked_sub(1 + $ind) {
                if place >= 4 {
                    if let Some(v) = positions.get_mut(place) {
                        v.clear();
                        v.push($new_val);
                    }
                }
            }
        };
    }

    // Add MWH (M and W are swapped round for odd stages)
    if place_not.stage().is_even() {
        replace_mwh!(2, 'M');
        replace_mwh!(1, 'W');
        replace_mwh!(0, 'H');
    } else {
        replace_mwh!(2, 'W');
        replace_mwh!(1, 'M');
        replace_mwh!(0, 'H');
    }

    positions
}

/// Returns the indices of a set of [`Bells`] within a given [`Row`]
fn get_bell_inds(bells: &[Bell], r: &Row) -> Vec<usize> {
    bells.iter().map(|b| r.place_of(*b).unwrap()).collect_vec()
}

#[cfg(test)]
mod tests {
    use bellframe::{PlaceNot, Stage};
    use itertools::Itertools;

    fn char_vec(string: &str) -> Vec<String> {
        string.chars().map(|c| c.to_string()).collect_vec()
    }

    #[test]
    fn default_plain_lead_positions() {
        #[rustfmt::skip]
        let cases = &[
            (Stage::DOUBLES, char_vec("LBIFH")),
            (Stage::MINOR, char_vec("LBIFHW")),
            (Stage::TRIPLES, char_vec("LBIFMWH")),
            (Stage::MAJOR, char_vec("LBIFMVHW")),
            (Stage::CATERS, char_vec("LBIFXVMWH")),
            (Stage::ROYAL, char_vec("LBIFXVMSHW")),
            (Stage::CINQUES, char_vec("LBIFXVESMWH")),
            (Stage::MAXIMUS, char_vec("LBIFXVESMNHW")),
        ];

        for (stage, exp_positions) in cases {
            let positions = super::default_plain_lead_positions(*stage);
            assert_eq!(positions, *exp_positions);
        }
    }

    #[test]
    fn default_calling_positions() {
        #[rustfmt::skip]
        let cases = &[
            ("145", Stage::DOUBLES, char_vec("LIBFH")),
            ("125", Stage::DOUBLES, char_vec("LBTFH")),
            ("1", Stage::DOUBLES, char_vec("LIBFH")),

            ("14", Stage::MINOR, char_vec("LIBFWH")),
            ("1234", Stage::MINOR, char_vec("LBTFWH")),
            ("1456", Stage::MINOR, char_vec("LIBFWH")),

            ("147", Stage::TRIPLES, char_vec("LIBFWMH")),
            ("12347", Stage::TRIPLES, char_vec("LBTFWMH")),

            ("14", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1234", Stage::MAJOR, char_vec("LBTFVMWH")),
            ("16", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1678", Stage::MAJOR, char_vec("LIBFVMWH")),
            ("1256", Stage::MAJOR, char_vec("LBTFVMWH")),
            ("123456", Stage::MAJOR, char_vec("LBTFVMWH")),

            ("14", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("16", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("18", Stage::ROYAL, char_vec("LIBFVXSMWH")),
            ("1890", Stage::ROYAL, char_vec("LIBFVXSMWH")),

            ("14", Stage::MAXIMUS, char_vec("LIBFVXSENMWH")),
            ("1234", Stage::MAXIMUS, char_vec("LBTFVXSENMWH")),
        ];

        for (pn_str, stage, exp_positions) in cases {
            let positions =
                super::default_calling_positions(&PlaceNot::parse(pn_str, *stage).unwrap());
            assert_eq!(positions, *exp_positions);
        }
    }
}
