use std::{collections::HashMap, num::ParseIntError, path::Path, sync::Arc};

use bellframe::{
    method::LABEL_LEAD_END,
    music::Regex,
    place_not::{self, PnBlockParseError},
    Bell, Method, MethodLib, Stage,
};
use hmap::hmap;
use itertools::Itertools;
use monument::{
    mask::Mask,
    spec::{single_method::SingleMethodError, Config, Layout, Spec},
    MusicType,
};
use serde_derive::Deserialize;

use crate::test_data::TestData;

use self::{
    calls::{BaseCalls, SpecificCall},
    length::Length,
};

mod calls;
mod length;

/// The specification for a set of compositions which Monument should search.  The [`Spec`] type is
/// parsed directly from the `TOML`, and can be thought of as an AST representation of the TOML
/// file.  This requires additional processing and verification before it can be converted into an
/// [`Engine`].
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AbstractSpec {
    /// The range of lengths of composition which are allowed
    length: Length,
    /// Monument won't stop until it generates the `num_comps` best compositions
    #[serde(default = "get_30")]
    num_comps: usize,

    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed
    start_indices: Option<Vec<usize>>,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed
    end_indices: Option<Vec<usize>>,

    /// Which calls should be used by default
    base_calls: Option<BaseCalls>,
    /// Should Monument normalise music scores by length when generating comps.
    #[serde(default = "get_true")]
    normalise_music: bool,

    /// If set, allows arbitrary splitting of the tenors (warning: this blows up the search size on
    /// large stages)
    #[serde(default)]
    split_tenors: bool,
    /// Which course heads masks are allowed (overrides `split_tenors`)
    course_heads: Option<Vec<String>>,

    /// The [`Method`] who's compositions we are after
    method: MethodSpec,
    /// Which calls to use in the compositions
    #[serde(default)]
    calls: Vec<SpecificCall>,
    /// Which music to use
    music: Vec<MusicSpec>,

    /// Data for the testing/benchmark harness.  It is public so that it can be accessed by the
    /// testing harness
    pub test_data: Option<TestData>,
}

impl AbstractSpec {
    /// Read a `Spec` from a TOML file
    pub fn read_from_file(path: &Path) -> Result<Self, toml::de::Error> {
        let spec_toml = std::fs::read_to_string(&path).unwrap();
        toml::from_str(&spec_toml)
    }

    /// Convert this abstract specification into a concrete [`Spec`] that uniquely defines a set of
    /// compositions.
    pub fn to_spec(&self, config: &Config) -> Result<Arc<Spec>, SpecConvertError> {
        // Parse and verify into its fully specified form
        let method = self.method.gen_method()?;
        let stage = method.stage();

        // This unwrap will only trigger if the method has `Stage` of zero, which I don't think is
        // possible.  Even if it is possible, there aren't any possible compositions on 0 bells so
        // I think a panic is justified.
        let tenor = Bell::tenor(stage).unwrap();

        let calls = calls::gen_calls(stage, self.base_calls.as_ref(), &self.calls)?;
        let music_types = self
            .music
            .iter()
            .map(|b| b.to_music_type(method.stage()))
            .collect_vec();
        let course_head_masks = match (self.course_heads.as_ref(), self.split_tenors) {
            // If masks are specified, parse all the course head mask strings into `Mask`s,
            // defaulting to the tenor as calling bell
            (Some(ch_mask_strings), _) => ch_mask_strings
                .iter()
                .map(|s| (Mask::parse(s), tenor))
                .collect_vec(),
            // If no masks were given but `split_tenors` was `true`, then only fix tenor and treble
            (None, true) => vec![(
                Mask::fix_bells(method.stage(), vec![Bell::TREBLE, tenor]),
                tenor,
            )],
            // Default to tenors together, with the tenor as 'calling bell'
            (None, false) => vec![(gen_tenors_together_mask(stage), tenor)],
        };

        // Generate a `Layout` from the data about the method and calls
        let layout = Layout::single_method(
            &method,
            &calls,
            &course_head_masks,
            config,
            self.start_indices.as_deref(),
            self.end_indices.as_deref(),
        )
        .map_err(SpecConvertError::LayoutGenError)?;

        Ok(Spec::new(
            layout,
            self.length.range.clone(),
            self.num_comps,
            music_types,
            // Always normalise music
            self.normalise_music,
            config,
        ))
    }
}

/// Generate the course head mask representing the tenors together.  This corresponds to
/// `1xxxxx7890ET...`, truncated to the correct stage.
fn gen_tenors_together_mask(stage: Stage) -> Mask {
    // By default, fix the treble and >=7.  Also, make sure to always fix the tenor even on Minor
    // or lower.  Note that we're numbering the bells where the treble is `0`
    let mut fixed_bells = vec![Bell::TREBLE];
    fixed_bells.extend((6..stage.as_usize()).map(Bell::from_index));
    Mask::fix_bells(stage, fixed_bells)
}

/// The possible ways that a [`Spec`] -> [`Engine`] conversion can fail
#[derive(Debug, Clone)]
pub enum SpecConvertError<'s> {
    NoCalls,
    CcLibNotFound,
    MethodNotFound { suggestions: Vec<String> },
    CallPnParse(&'s str, place_not::ParseError),
    MethodPnParse(PnBlockParseError),
    LeadLocationIndex(&'s str, ParseIntError),
    LayoutGenError(SingleMethodError),
}

/// The contents of the `[method]` header in the input TOML file
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum MethodSpec {
    Lib {
        title: String,
        /// The inputs to this map are numerical strings representing sub-lead indices, and the
        /// outputs are the lead location names
        #[serde(default = "default_lead_locations")]
        lead_locations: HashMap<String, String>,
    },
    Custom {
        #[serde(default)]
        name: String,
        place_notation: String,
        stage: Stage,
        /// The inputs to this map are numerical strings representing sub-lead indices, and the
        /// outputs are the lead location names
        #[serde(default = "default_lead_locations")]
        lead_locations: HashMap<String, String>,
    },
}

impl MethodSpec {
    fn gen_method(&self) -> Result<Method, SpecConvertError<'_>> {
        let mut m = self.get_method_without_lead_locations()?;

        for (index_str, name) in self.get_lead_locations() {
            let index = index_str
                .parse::<isize>()
                .map_err(|e| SpecConvertError::LeadLocationIndex(&index_str, e))?;
            let lead_len = m.lead_len() as isize;
            let wrapped_index = ((index % lead_len) + lead_len) % lead_len;
            // This cast is OK because we used % twice to guarantee a positive index
            m.set_label(wrapped_index as usize, Some(name.clone()));
        }
        Ok(m)
    }

    fn get_lead_locations(&self) -> &HashMap<String, String> {
        match self {
            MethodSpec::Lib { lead_locations, .. } => lead_locations,
            MethodSpec::Custom { lead_locations, .. } => lead_locations,
        }
    }

    /// Attempt to generate a new [`Method`] with no lead location annotations
    fn get_method_without_lead_locations(&self) -> Result<Method, SpecConvertError<'_>> {
        match self {
            MethodSpec::Lib { title, .. } => {
                let lib = MethodLib::cc_lib().ok_or(SpecConvertError::CcLibNotFound)?;
                lib.get_by_title_with_suggestions(title, 5)
                    .unwrap_parse_err()
                    .map_err(|suggestions| {
                        // Only suggest the suggestions which (perhaps jointly) have the best edit
                        // distance
                        let first_suggestion_edit_dist = suggestions[0].1;
                        let best_suggestions = suggestions
                            .into_iter()
                            .take_while(|&(_s, i)| i == first_suggestion_edit_dist)
                            .map(|(s, _i)| s.to_owned())
                            .collect_vec();
                        SpecConvertError::MethodNotFound {
                            suggestions: best_suggestions,
                        }
                    })
            }
            MethodSpec::Custom {
                name,
                stage,
                place_notation,
                ..
            } => Method::from_place_not_string(name.clone(), *stage, place_notation)
                .map_err(SpecConvertError::MethodPnParse),
        }
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
    RunsList {
        #[serde(rename = "run_lengths")]
        lengths: Vec<usize>,
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

impl MusicSpec {
    /// Gets the weight given to one instance of this `MusicSpec`
    pub fn weight(&self) -> f32 {
        match self {
            Self::Runs { weight, .. } => *weight,
            Self::RunsList { weight, .. } => *weight,
            Self::Pattern { weight, .. } => *weight,
            Self::Patterns { weight, .. } => *weight,
        }
    }

    /// Gets the [`Regex`]es which match this `MusicSpec`
    pub fn regexes(&self, stage: Stage) -> Vec<Regex> {
        match self {
            Self::Runs { length, .. } => Regex::runs_front_or_back(stage, *length),
            Self::RunsList { lengths, .. } => lengths
                .iter()
                .flat_map(|length| Regex::runs_front_or_back(stage, *length))
                .collect_vec(),
            Self::Pattern { pattern, .. } => vec![Regex::parse(&pattern)],
            Self::Patterns { patterns, .. } => {
                patterns.iter().map(|s| Regex::parse(&s)).collect_vec()
            }
        }
    }

    /// Generates a [`MusicType`] representing `self`.
    pub fn to_music_type(&self, stage: Stage) -> MusicType {
        MusicType::new(self.regexes(stage), self.weight())
    }
}

/* Deserialization helpers */

fn get_one() -> f32 {
    1.0
}

fn get_30() -> usize {
    30
}

fn get_true() -> bool {
    true
}

/// By default, add a lead location "LE" on the 0th row (i.e. when the place notation repeats).
#[inline]
fn default_lead_locations() -> HashMap<String, String> {
    hmap! { "0".to_owned() => LABEL_LEAD_END.to_owned() }
}
