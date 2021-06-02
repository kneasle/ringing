use itertools::Itertools;
use proj_core::{method::LABEL_LEAD_END, Method, PlaceNot};

use super::Layout;

pub fn single_method_layout(method: Method, calls: Vec<CallSpec>) -> Layout {
    dbg!(calls);

    panic!();
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
