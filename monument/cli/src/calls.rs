use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use itertools::Itertools;
use monument::query::{Call, CallVec};
use serde::Deserialize;

const DEFAULT_BOB_WEIGHT: f32 = -1.8;
const DEFAULT_SINGLE_WEIGHT: f32 = -2.3;
const DEFAULT_MISC_CALL_WEIGHT: f32 = -3.0;

/// The values of the `base_calls` attribute
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BaseCalls {
    None,
    Near,
    Far,
}

impl BaseCalls {
    pub fn create_calls(
        self,
        stage: Stage,
        bobs_only: bool,
        singles_only: bool,
        bob_weight: Option<f32>,
        single_weight: Option<f32>,
    ) -> anyhow::Result<CallVec<Call>> {
        assert!(stage >= Stage::MINIMUS); // Calls aren't defined on < 4 bells

        let (mut bob, mut single) = match self {
            BaseCalls::None => return Ok(CallVec::new()),
            BaseCalls::Near => (
                lead_end_bob(PlaceNot::parse("14", stage).unwrap()),
                lead_end_single(PlaceNot::parse("1234", stage).unwrap()),
            ),
            BaseCalls::Far => {
                let n = stage.num_bells_u8();
                (
                    lead_end_bob(PlaceNot::from_slice(&mut [0, n - 3], stage).unwrap()),
                    lead_end_single(
                        PlaceNot::from_slice(&mut [0, n - 3, n - 2, n - 1], stage).unwrap(),
                    ),
                )
            }
        };

        /// Any value of `{bob,single}_weight` smaller than this will trigger a warning to set
        /// `{single,bob}s_only = true`.
        const BIG_NEGATIVE_WEIGHT: f32 = -100.0;

        if let Some(w) = bob_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a singles only composition; consider using `singles_only = true` explicitly.");
            }
            bob.weight = w;
        }
        if let Some(w) = single_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a bobs only composition; consider using `bobs_only = true` explicitly.");
            }
            single.weight = w;
        }

        let calls = match (bobs_only, singles_only) {
            (false, false) => vec![bob, single],
            (true, false) => vec![bob],
            (false, true) => vec![single],
            (true, true) => {
                return Err(anyhow::Error::msg(
                    "Composition can't be both `bobs_only` and `singles_only`",
                ))
            }
        };
        Ok(CallVec::from(calls))
    }
}

impl Default for BaseCalls {
    fn default() -> Self {
        Self::Near
    }
}

/// The specification of a single call type used in a composition.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SpecificCall {
    place_notation: String,
    symbol: String,
    debug_symbol: Option<String>,
    #[serde(default = "lead_end")]
    label: CallLabel,
    /// Deprecated alias for `label`
    lead_location: Option<CallLabel>,
    calling_positions: Option<CallingPositions>,
    #[serde(default = "default_misc_call_score")]
    weight: f32,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum CallLabel {
    /// Call goes from/to the same label
    Same(String),
    /// Call goes from/to different labels (e.g. for cases like Leary's 23, which use 6ths place
    /// calls in 8ths place methods)
    Different { from: String, to: String },
}

/// The different ways the user can specify a set of calling positions
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
enum CallingPositions {
    /// The calling positions should be the `char`s in the given string
    Str(String),
    /// Each calling position is explicitly listed
    List(Vec<String>),
}

impl SpecificCall {
    pub(crate) fn create_call(&self, stage: Stage) -> anyhow::Result<Call> {
        let place_not = PlaceNot::parse(&self.place_notation, stage).map_err(|e| {
            anyhow::Error::msg(format!(
                "Can't parse place notation {:?} for call {:?}: {}",
                self.place_notation,
                self.debug_symbol.as_ref().unwrap_or(&self.symbol),
                e
            ))
        })?;
        let calling_positions = match &self.calling_positions {
            None => default_calling_positions(&place_not),
            Some(CallingPositions::Str(s)) => s.chars().map(|c| c.to_string()).collect_vec(),
            Some(CallingPositions::List(positions)) => positions.clone(),
        };
        if self.lead_location.is_some() {
            return Err(anyhow::Error::msg(
                "`calls.lead_location` has been renamed to `label`",
            ));
        }
        let (label_from, label_to) = match &self.label {
            CallLabel::Same(loc) => (loc.clone(), loc.clone()),
            CallLabel::Different { from, to } => (from.clone(), to.clone()),
        };
        Ok(Call {
            display_symbol: self.symbol.clone(),
            debug_symbol: self.debug_symbol.as_ref().unwrap_or(&self.symbol).clone(),
            calling_positions,
            label_from,
            label_to,
            place_not,
            weight: self.weight,
        })
    }
}

/// Check for which assign the same `symbol` at the same `label`.
pub fn check_for_duplicate_call_names<'calls>(
    call_specs: &'calls CallVec<Call>,
    symbol_name: &str,
    get_symbol: impl Fn(&'calls Call) -> &'calls str,
) -> anyhow::Result<()> {
    let sorted_calls = call_specs
        .iter()
        .map(|call| (get_symbol(call), &call.label_from, &call.place_not))
        .sorted_by_key(|&(sym, lead_loc, _pn)| (sym, lead_loc));
    for ((sym1, label1, pn1), (sym2, label2, pn2)) in sorted_calls.tuple_windows() {
        if sym1 == sym2 && label1 == label2 {
            return Err(anyhow::Error::msg(format!(
                "Call {} symbol {:?} (at {:?}) is used for both {} and {}",
                symbol_name, sym1, label1, pn1, pn2
            )));
        }
    }
    Ok(())
}

/// Create a bob which replaces the lead end with a given [`PlaceNot`]
fn lead_end_bob(place_not: PlaceNot) -> Call {
    Call {
        display_symbol: String::new(),
        debug_symbol: "-".to_owned(),
        calling_positions: default_calling_positions(&place_not),
        label_from: LABEL_LEAD_END.to_owned(),
        label_to: LABEL_LEAD_END.to_owned(),
        place_not,
        weight: DEFAULT_BOB_WEIGHT,
    }
}

/// Create a bob which replaces the lead end with a given [`PlaceNot`]
fn lead_end_single(place_not: PlaceNot) -> Call {
    Call {
        display_symbol: "s".to_owned(),
        debug_symbol: "s".to_owned(),
        calling_positions: default_calling_positions(&place_not),
        label_from: LABEL_LEAD_END.to_owned(),
        label_to: LABEL_LEAD_END.to_owned(),
        place_not,
        weight: DEFAULT_SINGLE_WEIGHT,
    }
}

#[allow(clippy::branches_sharing_code)]
fn default_calling_positions(place_not: &PlaceNot) -> Vec<String> {
    let named_positions = "LIBFVXSEN"; // TODO: Does anyone know any more than this?

    // Generate calling positions that aren't M, W or H
    let mut positions =
        // Start off with the single-char position names
        named_positions
        .chars()
        .map(|c| c.to_string())
        // Extending forever with numbers (extended with `ths` to avoid collisions with positional
        // calling positions)
        .chain((named_positions.len()..).map(|i| format!("{}ths", i + 1)))
        // But we consume one value per place in the Stage
        .take(place_not.stage().num_bells())
        .collect_vec();

    /// A cheeky macro which generates the code to perform an in-place replacement of a calling
    /// position at a given (0-indexed) place
    macro_rules! replace_pos {
        ($idx: expr, $new_val: expr) => {
            if let Some(v) = positions.get_mut($idx) {
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
            if let Some(place) = place_not.stage().num_bells().checked_sub(1 + $ind) {
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

fn lead_end() -> CallLabel {
    CallLabel::Same(LABEL_LEAD_END.to_owned())
}

fn default_misc_call_score() -> f32 {
    DEFAULT_MISC_CALL_WEIGHT
}

#[cfg(test)]
mod tests {
    use bellframe::{PlaceNot, Stage};
    use itertools::Itertools;

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
