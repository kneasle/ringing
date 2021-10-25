use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use monument_graph::layout::Call;
use serde::{de, Deserialize, Deserializer};

use super::Error;

/// The values of the `base_calls` attribute
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BaseCalls {
    Near,
    Far,
}

impl BaseCalls {
    pub(crate) fn to_call_specs(self, stage: Stage) -> Vec<Call> {
        let num_bells = stage.num_bells();
        // Panic if the comp has less than 4 bells.  I don't expect anyone to use Monument to
        // generate comps on fewer than 8 bells, but we should still check because the alternative
        // is UB
        assert!(num_bells >= 4);

        match self {
            BaseCalls::Near => vec![
                Call::lead_end_bob(PlaceNot::parse("14", stage).unwrap()),
                Call::lead_end_single(PlaceNot::parse("1234", stage).unwrap()),
            ],
            BaseCalls::Far => {
                vec![
                    // The unsafety here is OK, because the slice is always sorted (unless stage <
                    // MINIMUS, in which case the assert trips)
                    Call::lead_end_bob(unsafe {
                        PlaceNot::from_sorted_slice(&[0, num_bells - 3], stage).unwrap()
                    }),
                    // The unsafety here is OK, because the slice is always sorted (unless stage <
                    // MINIMUS, in which case the assert trips)
                    Call::lead_end_single(unsafe {
                        PlaceNot::from_sorted_slice(
                            &[0, num_bells - 3, num_bells - 2, num_bells - 1],
                            stage,
                        )
                        .unwrap()
                    }),
                ]
            }
        }
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
    lead_location: String,
    calling_positions: Option<Vec<String>>,
    #[serde(default = "default_call_score")]
    weight: f32,
}

impl SpecificCall {
    fn to_call_spec(&self, stage: Stage) -> Result<Call, Error> {
        Ok(Call::new(
            self.symbol.clone(),
            self.debug_symbol.as_ref().unwrap_or(&self.symbol).clone(),
            self.calling_positions.clone(),
            self.lead_location.clone(),
            PlaceNot::parse(&self.place_notation, stage)
                .map_err(|e| Error::CallPnParse(self.place_notation.clone(), e))?,
            self.weight,
        ))
    }
}

pub fn gen_calls<'s>(
    stage: Stage,
    base_calls: Option<&'s BaseCalls>,
    calls: &'s [SpecificCall],
) -> Result<Vec<Call>, Error> {
    // Check if the user hasn't specified any calls
    if base_calls.is_none() && calls.is_empty() {
        return Err(Error::NoCalls);
    }

    // Expand base calls into `Call`s
    let mut call_specs = base_calls.map_or_else(Vec::new, |bc| bc.to_call_specs(stage));
    for specific_call in calls {
        call_specs.push(specific_call.to_call_spec(stage)?);
    }

    Ok(call_specs)
}

#[inline(always)]
fn lead_end() -> String {
    LABEL_LEAD_END.to_owned()
}

fn default_call_score() -> f32 {
    -0.3
}
