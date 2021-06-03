use std::collections::HashMap;

use itertools::Itertools;
use proj_core::{method::LABEL_LEAD_END, Bell, Method, PlaceNot, Row, Stage};

use super::Layout;

pub fn single_method_layout(
    method: &Method,
    calls: &[CallSpec],
    non_fixed_bells: &[Bell],
) -> Result<Layout, SingleMethodError> {
    /* COMMONLY USED VALUES */
    // We can safely unwrap here because `method.stage()` can't be zero
    let fixed_bells = (0..method.stage().as_usize())
        .map(Bell::from_index)
        .filter(|b| !non_fixed_bells.contains(b))
        .collect_vec();
    let tenor = Bell::tenor(method.stage()).unwrap();
    let plain_course = method.plain_course();
    let course_len = plain_course.len();

    /* Generate maps for call locations */

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

    /* Generate a map of which rows are connected by calls */

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

    dbg!(call_jumps);

    unimplemented!();
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
    use itertools::Itertools;
    use proj_core::{PlaceNot, Stage};

    fn char_vec(string: &str) -> Vec<String> {
        string.chars().map(|c| c.to_string()).collect_vec()
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
