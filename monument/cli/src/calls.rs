use bellframe::{method::LABEL_LEAD_END, PlaceNot, Stage};
use itertools::Itertools;
use monument::builder::{BaseCallType, CallBuilder, DEFAULT_MISC_CALL_WEIGHT};
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

impl CustomCall {
    pub(crate) fn into_call_builder(self, stage: Stage) -> anyhow::Result<CallBuilder> {
        let place_notation = PlaceNot::parse(&self.place_notation, stage).map_err(|e| {
            anyhow::Error::msg(format!(
                "Can't parse place notation {:?} for call {:?}: {}",
                self.place_notation,
                self.debug_symbol.as_ref().unwrap_or(&self.symbol),
                e
            ))
        })?;
        if self.lead_location.is_some() {
            return Err(anyhow::Error::msg(
                "`calls.lead_location` has been renamed to `label`",
            ));
        }
        let (label_from, label_to) = match self.label {
            CallLabel::Same(loc) => (loc.clone(), loc),
            CallLabel::Different { from, to } => (from, to),
        };

        let builder = CallBuilder::new(self.symbol, place_notation)
            .maybe_debug_symbol(self.debug_symbol)
            .maybe_calling_positions(
                self.calling_positions
                    .as_ref()
                    .map(CallingPositions::as_vec),
            )
            .label_from_to(label_from, label_to)
            .weight(self.weight);
        Ok(builder)
    }
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

impl CallingPositions {
    /// Returns the same [`CallingPositions`] as `self`, but always expressed as a [`Vec`] of one
    /// [`String`] per place.
    fn as_vec(&self) -> Vec<String> {
        match self {
            CallingPositions::Str(s) => s.chars().map(|c| c.to_string()).collect_vec(),
            CallingPositions::List(positions) => positions.clone(),
        }
    }
}

fn lead_end() -> CallLabel {
    CallLabel::Same(LABEL_LEAD_END.to_owned())
}

fn default_misc_call_score() -> f32 {
    DEFAULT_MISC_CALL_WEIGHT
}
