use std::{collections::HashMap, num::ParseIntError, path::Path};

use bellframe::{
    method::LABEL_LEAD_END,
    method_lib::QueryError,
    music::Regex,
    place_not::{self, PnBlockParseError},
    Bell, Mask, Method, MethodLib, Stage,
};
use itertools::Itertools;
use monument_graph::{
    layout::{from_methods, Layout, SpliceStyle},
    music::MusicType,
    Data,
};
use serde::Deserialize;

use crate::test_data::TestData;

use self::{
    calls::{BaseCalls, SpecificCall},
    length::Length,
};

mod calls;

/// The specification for a set of compositions which Monument should search.  The [`Spec`] type is
/// parsed directly from the `TOML`, and can be thought of as an AST representation of the TOML
/// file.  Like ASTs, this specifies a superset of valid programs - so building a composition
/// search can also fail (as can lowering an AST).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Spec {
    /// The range of lengths of composition which are allowed
    length: Length,
    /// Monument won't stop until it generates the `num_comps` best compositions
    #[serde(default = "get_30")]
    num_comps: usize,

    #[serde(default)]
    snap_start: bool,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed
    #[serde(default = "default_start_indices")]
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
    method: Option<MethodSpec>,
    #[serde(default)]
    /// A list of [`Method`] who are being spliced together
    methods: Vec<MethodSpec>,

    /// Which calls to use in the compositions
    #[serde(default)]
    calls: Vec<SpecificCall>,
    /// Which music to use
    music: Vec<MusicSpec>,

    /// Data for the testing/benchmark harness.  It is public so that it can be accessed by the
    /// testing harness.
    ///
    /// TODO: Move testing data to its own files, and figure out how to test non-optimal engines
    pub test_data: Option<TestData>,
}

impl Spec {
    /// Read a `Spec` from a TOML file
    pub fn read_from_file(path: &Path) -> Result<Self, toml::de::Error> {
        let spec_toml = std::fs::read_to_string(&path).unwrap();
        toml::from_str(&spec_toml)
    }

    /// 'Lower' this specification into the information required to build a composition.
    pub fn lower(&self) -> Result<Data, Error> {
        // Generate methods
        let mut methods: Vec<(Method, String)> = self
            .methods
            .iter()
            .map(MethodSpec::gen_method)
            .collect::<Result<_, _>>()?;
        if let Some(m) = &self.method {
            methods.push(m.gen_method()?);
        }

        // Compute useful values
        let stage = methods
            .iter()
            .map(|(m, _)| m.stage())
            .max()
            .ok_or(Error::NoMethods)?;
        let tenor = Bell::tenor(stage);

        let music_types = self
            .music
            .iter()
            .map(|b| b.to_music_type(stage))
            .collect_vec();
        let course_head_masks = match (self.course_heads.as_ref(), self.split_tenors) {
            // If masks are specified, parse all the course head mask strings into `Mask`s,
            // defaulting to the tenor as calling bell
            (Some(ch_mask_strings), _) => ch_mask_strings
                .iter()
                .map(|s| (Mask::parse(s), tenor))
                .collect_vec(),
            // If no masks were given but `split_tenors` was `true`, then only fix tenor and treble
            (None, true) => vec![(Mask::fix_bells(stage, vec![Bell::TREBLE, tenor]), tenor)],
            // Default to tenors together, with the tenor as 'calling bell'
            (None, false) => vec![(gen_tenors_together_mask(stage), tenor)],
        };
        let calls = calls::gen_calls(stage, self.base_calls.as_ref(), &self.calls)?;

        // Generate a `Layout` from the data about the method and calls
        let layout = Layout::from_methods(
            &methods,
            &calls,
            SpliceStyle::LeadLabels, // TODO: Make this configurable
            course_head_masks,
            if self.snap_start {
                None
            } else {
                self.start_indices.as_deref()
            },
            self.end_indices.as_deref(),
        )
        .map_err(Error::LayoutGen)?;

        // Build this layout into a `Graph`
        Ok(Data {
            layout,
            music_types,
            len_range: self.length.range.clone(),
            num_comps: self.num_comps,

            queue_limit: 10_000_000,
        })
    }
}

/// Generate the course head mask representing the tenors together.  This corresponds to
/// `1xxxxx7890ET...`, truncated to the correct stage.
fn gen_tenors_together_mask(stage: Stage) -> Mask {
    // By default, fix the treble and >=7.  Also, make sure to always fix the tenor even on Minor
    // or lower.  Note that we're numbering the bells where the treble is `0`
    let mut fixed_bells = vec![Bell::TREBLE];
    fixed_bells.extend((6..stage.num_bells()).map(Bell::from_index));
    Mask::fix_bells(stage, fixed_bells)
}

/// The possible ways that a [`Spec`] -> [`Engine`] conversion can fail
#[derive(Debug, Clone)]
pub enum Error {
    NoCalls,
    NoMethods,
    CcLibNotFound,
    MethodNotFound { suggestions: Vec<String> },
    CallPnParse(String, place_not::ParseError),
    MethodPnParse(PnBlockParseError),
    LeadLocationIndex(String, ParseIntError),
    LayoutGen(from_methods::Error),
}

///////////////
// METHOD(S) //
///////////////

/// The contents of the `[method]` header in the input TOML file
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum MethodSpec {
    JustTitle(String),
    Lib {
        title: String,
        /// The inputs to this map are numerical strings representing sub-lead indices, and the
        /// outputs are the lead location names
        #[serde(default = "default_lead_labels")]
        lead_locations: HashMap<String, String>,
        shorthand: Option<String>,
    },
    Custom {
        #[serde(default)]
        name: String,
        place_notation: String,
        stage: Stage,
        shorthand: Option<String>,
        /// The inputs to this map are numerical strings representing sub-lead indices, and the
        /// outputs are the lead location names
        #[serde(default = "default_lead_labels")]
        lead_locations: HashMap<String, String>,
    },
}

impl MethodSpec {
    fn gen_method(&self) -> Result<(Method, String), Error> {
        let mut m = self.get_method_without_lead_locations()?;

        for (index_str, name) in self.get_lead_locations() {
            let index = index_str
                .parse::<isize>()
                .map_err(|e| Error::LeadLocationIndex(index_str, e))?;
            let lead_len = m.lead_len() as isize;
            let wrapped_index = ((index % lead_len) + lead_len) % lead_len;
            // This cast is OK because we used % twice to guarantee a positive index
            m.set_label(wrapped_index as usize, Some(name.clone()));
        }

        // Use the custom shorthand, or the first letter of the method
        let shorthand = self
            .shorthand()
            .unwrap_or_else(|| m.name().chars().next().unwrap().to_string());

        Ok((m, shorthand))
    }

    fn shorthand(&self) -> Option<String> {
        match self {
            Self::JustTitle(_) => None,
            Self::Lib { shorthand, .. } => shorthand.clone(),
            Self::Custom { shorthand, .. } => shorthand.clone(),
        }
    }

    fn get_lead_locations(&self) -> HashMap<String, String> {
        match self {
            MethodSpec::JustTitle(_) => default_lead_labels(),
            MethodSpec::Lib { lead_locations, .. } => lead_locations.clone(),
            MethodSpec::Custom { lead_locations, .. } => lead_locations.clone(),
        }
    }

    /// Attempt to generate a new [`Method`] with no lead location annotations
    fn get_method_without_lead_locations(&self) -> Result<Method, Error> {
        match self {
            MethodSpec::Lib { title, .. } | MethodSpec::JustTitle(title) => {
                let lib = MethodLib::cc_lib().ok_or(Error::CcLibNotFound)?;
                lib.get_by_title_with_suggestions(title, 5)
                    .map_err(|err| match err {
                        QueryError::PnParseErr { .. } => {
                            panic!("Method lib contains invalid place notation")
                        }
                        QueryError::NotFound(suggestions) => {
                            // Only suggest the suggestions which (perhaps jointly) have the best edit
                            // distance
                            let first_suggestion_edit_dist = suggestions[0].1;
                            let best_suggestions = suggestions
                                .into_iter()
                                .take_while(|&(_s, i)| i == first_suggestion_edit_dist)
                                .map(|(s, _i)| s.to_owned())
                                .collect_vec();
                            Error::MethodNotFound {
                                suggestions: best_suggestions,
                            }
                        }
                    })
            }
            MethodSpec::Custom {
                name,
                stage,
                place_notation,
                ..
            } => Method::from_place_not_string(name.clone(), *stage, place_notation)
                .map_err(Error::MethodPnParse),
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
    /// Generates a [`MusicType`] representing `self`.
    pub fn to_music_type(&self, stage: Stage) -> MusicType {
        MusicType::new(self.regexes(stage), self.weight())
    }

    /// Gets the weight given to one instance of this `MusicSpec`
    fn weight(&self) -> f32 {
        match self {
            Self::Runs { weight, .. } => *weight,
            Self::RunsList { weight, .. } => *weight,
            Self::Pattern { weight, .. } => *weight,
            Self::Patterns { weight, .. } => *weight,
        }
    }

    /// Gets the [`Regex`]es which match this `MusicSpec`
    fn regexes(&self, stage: Stage) -> Vec<Regex> {
        match self {
            Self::Runs { length, .. } => Regex::runs_front_or_back(stage, *length),
            Self::RunsList { lengths, .. } => lengths
                .iter()
                .flat_map(|length| Regex::runs_front_or_back(stage, *length))
                .collect_vec(),
            Self::Pattern { pattern, .. } => vec![Regex::parse(pattern)],
            Self::Patterns { patterns, .. } => {
                patterns.iter().map(|s| Regex::parse(s)).collect_vec()
            }
        }
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
fn default_lead_labels() -> HashMap<String, String> {
    let mut labels = HashMap::new();
    labels.insert("0".to_owned(), LABEL_LEAD_END.to_owned());
    labels
}

fn default_start_indices() -> Option<Vec<usize>> {
    Some(vec![0]) // Just normal starts
}

////////////
// LENGTH //
////////////

mod length {
    use std::{
        fmt,
        ops::{Range, RangeInclusive},
    };

    use serde::{
        de::{Error, MapAccess, Visitor},
        Deserialize, Deserializer,
    };

    /* Constants for commonly used length ranges */

    pub const PRACTICE: RangeInclusive<usize> = 0..=300;
    pub const QP: RangeInclusive<usize> = 1250..=1350;
    pub const HALF_PEAL: RangeInclusive<usize> = 2500..=2600;
    pub const PEAL: RangeInclusive<usize> = 5000..=5200;

    /// A new-typed range with human-friendly deserialisation
    #[derive(Debug, Clone)]
    #[repr(transparent)]
    pub(super) struct Length {
        pub(super) range: Range<usize>,
    }

    // Convert Length to a ranges
    impl From<Length> for Range<usize> {
        #[inline(always)]
        fn from(l: Length) -> Range<usize> {
            l.range
        }
    }

    impl From<usize> for Length {
        #[inline(always)]
        fn from(v: usize) -> Length {
            Length { range: v..v + 1 }
        }
    }

    impl From<Range<usize>> for Length {
        #[inline(always)]
        fn from(r: Range<usize>) -> Length {
            Length { range: r }
        }
    }

    impl From<RangeInclusive<usize>> for Length {
        #[inline(always)]
        fn from(r: RangeInclusive<usize>) -> Length {
            Length {
                range: *r.start()..r.end() + 1,
            }
        }
    }

    struct LengthVisitor;

    impl<'de> Visitor<'de> for LengthVisitor {
        type Value = Length;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a positive integer or a name like 'peal' or a 'min/max' range")
        }

        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: Error,
        {
            de_signed_num(v)
        }

        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(Length::from(v as usize))
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            #[derive(Deserialize)]
            #[serde(field_identifier, rename_all = "lowercase", deny_unknown_fields)]
            enum Fields {
                Min,
                Max,
            }

            // If we are deserialising a map, we expect the form `{ min: _, max: _ }`
            let mut min: Option<usize> = None;
            let mut max: Option<usize> = None;
            // I don't care about allocating strings here - we'll only serialise one length per run
            while let Some(key) = map.next_key()? {
                match key {
                    Fields::Min => {
                        if min.is_some() {
                            return Err(Error::duplicate_field("min"));
                        }
                        min = Some(map.next_value()?);
                    }
                    Fields::Max => {
                        if max.is_some() {
                            return Err(Error::duplicate_field("max"));
                        }
                        max = Some(map.next_value()?);
                    }
                }
            }
            // Open question: should min default to 0?
            let min = min.ok_or_else(|| Error::missing_field("min"))?;
            let max = max.ok_or_else(|| Error::missing_field("max"))?;
            Ok(Length::from(min..=max))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            let lower_str = v.to_lowercase();
            Ok(Length::from(match lower_str.as_str() {
                "practice" => PRACTICE,
                "qp" => QP,
                "quarter peal" => QP,
                "peal" => PEAL,
                "half peal" => HALF_PEAL,
                _ => return Err(E::custom(format!("unknown length name '{}'", v))),
            }))
        }
    }

    /// Helper function to generate an error message on negative lengths
    #[inline(always)]
    fn de_signed_num<E: Error>(v: i64) -> Result<Length, E> {
        if v >= 0 {
            Ok(Length::from(v as usize))
        } else {
            Err(E::custom(format!("negative length: {}", v)))
        }
    }

    impl<'de> Deserialize<'de> for Length {
        fn deserialize<D>(deserializer: D) -> Result<Length, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(LengthVisitor)
        }
    }
}
