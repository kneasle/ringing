use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use monument::layout::new::Call;
use serde::{de, Deserialize, Deserializer};

use super::Error;

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
        bob_weight: Option<f32>,
        single_weight: Option<f32>,
    ) -> Vec<Call> {
        // Panic if the comp has less than 4 bells.  I don't expect anyone to use Monument to
        // generate comps on fewer than 8 bells, but we should still check because the alternative
        // is UB
        assert!(stage >= Stage::MINIMUS);

        let (mut bob, mut single) = match self {
            BaseCalls::None => return vec![],
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
            bob.set_weight(w);
        }
        if let Some(w) = single_weight {
            single.set_weight(w);
        }

        vec![bob, single]
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
    lead_location: String,
    calling_positions: Option<Vec<String>>,
    #[serde(default = "default_call_score")]
    weight: f32,
}

impl SpecificCall {
    pub(crate) fn to_call_spec(&self, stage: Stage) -> Result<Call, Error> {
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

#[inline(always)]
fn lead_end() -> String {
    LABEL_LEAD_END.to_owned()
}

fn default_call_score() -> f32 {
    -3.0
}
