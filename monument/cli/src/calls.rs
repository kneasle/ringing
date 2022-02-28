use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use itertools::Itertools;
use monument::layout::new::{default_calling_positions, Call};
use serde::{de, Deserialize, Deserializer};

/// The values of the `base_calls` attribute
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BaseCalls {
    None,
    Near,
    Far,
}

impl BaseCalls {
    pub fn to_call_specs(
        self,
        stage: Stage,
        bobs_only: bool,
        singles_only: bool,
        bob_weight: Option<f32>,
        single_weight: Option<f32>,
    ) -> anyhow::Result<Vec<Call>> {
        // Panic if the comp has less than 4 bells.  I don't expect anyone to use Monument to
        // generate comps on fewer than 8 bells, but we should still check because the alternative
        // is UB
        assert!(stage >= Stage::MINIMUS);

        let (mut bob, mut single) = match self {
            BaseCalls::None => return Ok(vec![]),
            BaseCalls::Near => (
                Call::lead_end_bob(PlaceNot::parse("14", stage).unwrap()),
                Call::lead_end_single(PlaceNot::parse("1234", stage).unwrap()),
            ),
            BaseCalls::Far => {
                let n = stage.num_bells_u8();
                (
                    // The unsafety here is OK, because the slice is always sorted (unless stage <
                    // MINIMUS, in which case the assert trips)
                    Call::lead_end_bob(unsafe {
                        PlaceNot::from_sorted_slice(&[0, n - 3], stage).unwrap()
                    }),
                    // The unsafety here is OK, because the slice is always sorted (unless stage <
                    // MINIMUS, in which case the assert trips)
                    Call::lead_end_single(unsafe {
                        PlaceNot::from_sorted_slice(&[0, n - 3, n - 2, n - 1], stage).unwrap()
                    }),
                )
            }
        };

        if let Some(w) = bob_weight {
            bob.weight = w;
        }
        if let Some(w) = single_weight {
            single.weight = w;
        }

        Ok(match (bobs_only, singles_only) {
            (false, false) => vec![bob, single],
            (true, false) => vec![bob],
            (false, true) => vec![single],
            (true, true) => {
                return Err(anyhow::Error::msg(
                    "Can't be both `bobs_only` and `singles_only`",
                ))
            }
        })
    }
}

impl Default for BaseCalls {
    fn default() -> Self {
        Self::Near
    }
}

impl<'de> Deserialize<'de> for BaseCalls {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let lower_str = s.to_lowercase();
        Ok(match lower_str.as_str() {
            "none" => BaseCalls::None,
            "near" => BaseCalls::Near,
            "far" => BaseCalls::Far,
            _ => return Err(de::Error::custom(format!("unknown call type '{}'", s))),
        })
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
    lead_location: LeadLocation,
    calling_positions: Option<CallingPositions>,
    #[serde(default = "default_call_score")]
    weight: f32,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum LeadLocation {
    /// from/to are the same location
    Same(String),
    /// from/to are different (e.g. for cases like Leary's 23, which use 6ths place calls in 8ths
    /// place methods)
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
    pub(crate) fn to_call_spec(&self, stage: Stage) -> anyhow::Result<Call> {
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
        let (lead_location_from, lead_location_to) = match &self.lead_location {
            LeadLocation::Same(loc) => (loc.clone(), loc.clone()),
            LeadLocation::Different { from, to } => (from.clone(), to.clone()),
        };
        Ok(Call {
            display_symbol: self.symbol.clone(),
            debug_symbol: self.debug_symbol.as_ref().unwrap_or(&self.symbol).clone(),
            calling_positions,
            lead_location_from,
            lead_location_to,
            place_not,
            weight: self.weight,
        })
    }
}

fn lead_end() -> LeadLocation {
    LeadLocation::Same(LABEL_LEAD_END.to_owned())
}

fn default_call_score() -> f32 {
    -3.0
}
