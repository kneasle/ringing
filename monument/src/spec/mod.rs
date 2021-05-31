use std::collections::HashMap;

use hmap::hmap;
use proj_core::{method::LABEL_LEAD_END, Bell, Stage};
use serde_derive::Deserialize;

use self::{
    calls::{BaseCalls, Calls},
    length::Length,
};

mod calls;
mod length;

/// The specification for a set of compositions which Monument should find.  The [`Spec`] type is
/// parsed directly from the `TOML`, and can be thought of as an AST representation of the TOML
/// file.  This requires additional processing and verification before it can be fed into the IDA*
/// engine.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Spec {
    /// Which bells are allowed to be affected by calls
    non_fixed_bells: Option<Vec<Bell>>,
    /// The range of lengths of composition which are allowed
    length: Length,
    /// Monument won't stop until it generates the `num_comps` best compositions
    num_comps: usize,
    /// The call type specified by the `base_calls` argument
    base_calls: Option<BaseCalls>,

    /// The [`Method`] who's compositions we are after
    method: MethodSpec,
    /// Which calls to use in the compositions
    calls: Calls,
    /// Which music to use
    music: Vec<MusicSpec>,
}

/// The contents of the `[method]` header in the input TOML file
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MethodSpec {
    #[serde(default)]
    name: String,
    place_notation: String,
    stage: Stage,
    /// The inputs to this map are numerical strings representing sub-lead indices, and the outputs
    /// are the lead location names
    #[serde(default = "default_lead_locations")]
    lead_locations: HashMap<String, String>,
}

/* Music */

/// The specification for one type of music
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum MusicSpec {
    Runs {
        #[serde(rename = "run_length")]
        length: usize,
        weight: f32,
    },
    Pattern {
        pattern: String,
        weight: f32,
    },
}

/* Deserialization helpers */

/// By default, add a lead location "LE" at the lead end (i.e. when the place notation repeats).
#[inline]
fn default_lead_locations() -> HashMap<String, String> {
    hmap! { "0".to_owned() => LABEL_LEAD_END.to_owned() }
}
