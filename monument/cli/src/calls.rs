use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use itertools::Itertools;
use monument::parameters::{
    default_calling_positions, BaseCallType, CallId, DEFAULT_MISC_CALL_WEIGHT,
};
use serde::Deserialize;

/// The values of the `base_calls` attribute
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BaseCalls {
    None,
    Near,
    Far,
}

impl BaseCalls {
    pub fn as_monument_type(self) -> Option<BaseCallType> {
        match self {
            Self::Near => Some(BaseCallType::Near),
            Self::Far => Some(BaseCallType::Far),
            Self::None => None,
        }
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
pub struct CustomCall {
    place_notation: String,
    #[serde(default = "default_misc_call_score")]
    weight: f32,
    symbol: char,
    debug_symbol: Option<String>, // Deprecated in v0.13.0
    calling_positions: Option<String>,
    #[serde(default = "lead_end")]
    label: CallLabel,
    /// Deprecated alias for `label`
    lead_location: Option<CallLabel>,
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

impl CustomCall {
    pub(super) fn as_monument_call(
        &self,
        id: CallId,
        stage: Stage,
    ) -> anyhow::Result<monument::parameters::Call> {
        let place_notation = PlaceNot::parse(&self.place_notation, stage).map_err(|e| {
            anyhow::Error::msg(format!(
                "Can't parse place notation {:?} for call {:?}: {}",
                self.place_notation, &self.symbol, e
            ))
        })?;
        if self.lead_location.is_some() {
            return Err(anyhow::Error::msg(
                "`calls.lead_location` has been renamed to `label`",
            ));
        }
        if self.debug_symbol.is_some() {
            return Err(anyhow::Error::msg(
                "`debug_symbol` is now calculated automatically.  Use `symbol = \"-\" for bobs.`",
            ));
        }
        let (label_from, label_to) = match self.label.clone() {
            CallLabel::Same(loc) => (loc.clone(), loc),
            CallLabel::Different { from, to } => (from, to),
        };
        let calling_positions = match &self.calling_positions {
            Some(c) => c.chars().collect_vec(),
            None => default_calling_positions(&place_notation),
        };

        Ok(monument::parameters::Call {
            id,
            symbol: self.symbol,
            calling_positions,
            label_from,
            label_to,
            place_notation,
            weight: self.weight,
        })
    }
}

fn lead_end() -> CallLabel {
    CallLabel::Same(LABEL_LEAD_END.to_owned())
}

fn default_misc_call_score() -> f32 {
    DEFAULT_MISC_CALL_WEIGHT
}
