use std::{collections::HashMap, num::ParseIntError};

use engine::Engine;
use hmap::hmap;
use proj_core::{
    method::LABEL_LEAD_END,
    place_not::{self, PnBlockParseError},
    Bell, Method, Stage,
};
use serde_derive::Deserialize;

use self::{
    calls::{BaseCalls, SpecificCall},
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
    #[serde(default)]
    calls: Vec<SpecificCall>,
    /// Which music to use
    music: Vec<MusicSpec>,
}

impl Spec {
    pub fn create_engine(&self) -> Result<Engine, SpecConvertError> {
        let method = self.method.gen_method()?;
        let calls = calls::gen_calls(self.method.stage, self.base_calls.as_ref(), &self.calls)?;

        Ok(Engine::single_method(
            self.length.range.clone(),
            self.num_comps,
            method,
            calls,
        ))
    }
}

/// The possible ways that a [`Spec`] -> [`Engine`] conversion can fail
#[derive(Debug, Clone)]
pub enum SpecConvertError<'s> {
    NoCalls,
    CallPnParse(&'s str, place_not::ParseError),
    MethodPnParse(PnBlockParseError),
    LeadLocationIndex(&'s str, ParseIntError),
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

impl MethodSpec {
    fn gen_method(&self) -> Result<Method, SpecConvertError<'_>> {
        let mut m =
            Method::from_place_not_string(self.name.clone(), self.stage, &self.place_notation)
                .map_err(SpecConvertError::MethodPnParse)?;
        for (index_str, name) in &self.lead_locations {
            let index = index_str
                .parse::<isize>()
                .map_err(|e| SpecConvertError::LeadLocationIndex(&index_str, e))?;
            let lead_len = m.lead_len() as isize;
            let wrapped_index = (index % lead_len) + lead_len % lead_len;
            // This cast is OK because we used % twice to guarantee a positive index
            m.set_label(wrapped_index as usize, Some(name.clone()));
        }
        Ok(m)
    }
}

/* Music */

/// The specification for one type of music
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum MusicSpec {
    Runs {
        #[serde(rename = "run_length")]
        length: usize,
        #[serde(default = "get_one")]
        weight: f32,
    },
    Pattern {
        pattern: String,
        #[serde(default = "get_one")]
        weight: f32,
    },
    Patterns {
        patterns: Vec<String>,
        #[serde(default = "get_one")]
        weight: f32,
    },
}

#[inline(always)]
fn get_one() -> f32 {
    1.0
}

/* Deserialization helpers */

/// By default, add a lead location "LE" at the lead end (i.e. when the place notation repeats).
#[inline]
fn default_lead_locations() -> HashMap<String, String> {
    hmap! { "0".to_owned() => LABEL_LEAD_END.to_owned() }
}
