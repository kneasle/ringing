use std::{
    collections::HashMap,
    ops::Range,
    path::{Path, PathBuf},
};

use bellframe::{
    method::LABEL_LEAD_END, method_lib::QueryError, music::Regex, Bell, Mask, Method, MethodLib,
    Row, RowBuf, Stage, Stroke,
};
use itertools::Itertools;
use log::log;
use monument::{
    layout::{
        new::{Call, CourseHeadMaskPreset, SpliceStyle},
        Layout,
    },
    music::{MusicType, StrokeSet},
    OptRange, Query,
};
use serde::Deserialize;

use crate::{
    calls::{BaseCalls, SpecificCall},
    Error,
};

use self::length::Length;

const METHOD_BALANCE_ALLOWANCE: f32 = 0.1; // By how much the method balance is allowed to vary

/// The specification for a set of compositions which Monument should search.  The [`Spec`] type is
/// parsed directly from the `TOML`, and can be thought of as an AST representation of the TOML
/// file.  Like ASTs, this specifies a superset of valid programs - so building a composition
/// search can also fail (as can lowering an AST).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Spec {
    /* GENERAL */
    /// The range of lengths of composition which are allowed
    length: Length,
    /// Monument won't stop until it generates the `num_comps` best compositions
    #[serde(default = "get_30")]
    num_comps: usize,
    /// If `true`, generate compositions lead-wise, rather than course-wise.  This is useful for
    /// cases like cyclic comps where no course heads are preserved across parts.
    ///
    /// If left unset, Monument will determine the best value - i.e. leadwise iff the part head
    /// affects the tenor
    leadwise: Option<bool>,

    /* METHODS */
    /// The [`Method`] who's compositions we are after
    method: Option<MethodSpec>,
    #[serde(default)]
    /// A list of [`Method`] who are being spliced together
    methods: Vec<MethodSpec>,
    /// At which locations method splices are allowed
    #[serde(default)]
    splice_style: SpliceStyle,
    /// Bounds on how many rows of each method is allowed
    #[serde(default)]
    method_count: OptRange,

    /* CALLS */
    /// Which calls should be used by default
    #[serde(default)] // Default to near calls
    base_calls: BaseCalls,
    /// Which calls to use in the compositions
    #[serde(default)]
    calls: Vec<SpecificCall>,
    /// The weight given to each bob from `base_calls`
    bob_weight: Option<f32>,
    /// The weight given to each single from `base_calls`
    single_weight: Option<f32>,

    /* MUSIC */
    /// Path to a file containing a music definition, relative to **this** TOML file
    music_file: Option<PathBuf>,
    /// Specification of which classes of music Monument should consider
    #[serde(default)]
    music: Vec<MusicSpec>,
    /// The [`Stroke`] of the first row of the composition
    #[serde(default = "backstroke")]
    start_stroke: Stroke,
    /// The value for `non_duffer` given to music types when none is explicitly given
    #[serde(default = "get_true")]
    default_non_duffer: bool,
    /// The most consecutive rows of duffer nodes.
    max_duffer_rows: Option<usize>,

    /* COURSES */
    /// A [`Row`] which generates the part heads of this composition
    part_head: Option<String>,
    /// If set, allows arbitrary splitting of the tenors (warning: this blows up the search size on
    /// large stages)
    #[serde(default)]
    split_tenors: bool,
    /// Which course heads masks are allowed (overrides `split_tenors`)
    course_heads: Option<Vec<String>>,
    /// Weights applied to given CH patterns
    #[serde(default)]
    ch_weights: Vec<ChWeightPattern>,

    /* STARTS/ENDS */
    /// Set to `true` to allow comps to not start at the lead head.
    #[serde(default)]
    snap_start: bool,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed
    #[serde(default = "default_start_indices")]
    start_indices: Option<Vec<usize>>,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed
    end_indices: Option<Vec<usize>>,
}

impl Spec {
    /// Read a `Spec` from a TOML file
    pub fn read_from_file(path: &Path) -> Result<Self, TomlReadError> {
        let mut toml_buf = String::new();
        read_toml(path, &mut toml_buf)
    }

    /// 'Lower' this `Spec`ification into a [`Query`]
    pub fn lower(&self, toml_path: &Path) -> Result<Query, Error> {
        // Generate methods
        let mut methods: Vec<(Method, String)> = self
            .methods
            .iter()
            .map(MethodSpec::gen_method)
            .collect::<Result<_, _>>()?;
        if let Some(m) = &self.method {
            methods.push(m.gen_method()?);
        }

        let stage = methods
            .iter()
            .map(|(m, _)| m.stage())
            .max()
            .ok_or(Error::NoMethods)?;

        let part_head = match &self.part_head {
            Some(ph) => RowBuf::parse_with_stage(ph, stage).map_err(Error::PartHeadParse)?,
            None => RowBuf::rounds(stage),
        };

        let (start_indices, end_indices) = self.start_end_indices(&part_head);

        let ch_mask_preset = self.ch_mask_preset(stage)?;
        let calls = self.calls(stage)?;

        let layout = Layout::new(
            &methods,
            &calls,
            self.splice_style,
            ch_mask_preset,
            &part_head,
            self.leadwise,
            start_indices.as_deref(),
            end_indices.as_deref(),
        )
        .map_err(Error::LayoutGen)?;

        let music_types = self.music_types(toml_path, stage)?;
        if music_types.is_empty() {
            // TODO: Add default music
            log::warn!("No music patterns specified.  Are you sure you don't care about music?");
        }

        let method_count_range =
            method_count_range(methods.len(), &self.length.range, self.method_count);
        if methods.len() > 1 {
            log::info!("Method count range: {:?}", method_count_range);
        }
        // Build this layout into a `Graph`
        Ok(Query {
            layout,
            part_head,
            len_range: self.length.range.clone(),
            num_comps: self.num_comps,

            method_count_range,

            music_types,
            start_stroke: self.start_stroke,
            max_duffer_rows: self.max_duffer_rows,
            ch_weights: self.ch_weights(stage, &methods)?,
        })
    }

    fn music_types(&self, toml_path: &Path, stage: Stage) -> Result<Vec<MusicType>, Error> {
        let mut music_types = self
            .music
            .iter()
            .flat_map(|spec| spec.to_music_types(stage, self.default_non_duffer))
            .collect_vec();
        if let Some(relative_music_path) = &self.music_file {
            // Compute the path of the music TOML file
            let mut music_path = toml_path
                .parent()
                .expect("files should always have a parent")
                .to_owned();
            music_path.push(relative_music_path);
            // Parse the TOML
            let mut toml_buf = String::new();
            let music_file: MusicFile = read_toml(&music_path, &mut toml_buf)
                .map_err(|toml_err| Error::MusicFile(music_path.to_owned(), toml_err))?;
            // Add the music types
            music_types.extend(
                music_file
                    .music
                    .iter()
                    .flat_map(|spec| spec.to_music_types(stage, self.default_non_duffer)),
            );
        }
        Ok(music_types)
    }

    fn calls(&self, stage: Stage) -> Result<Vec<Call>, Error> {
        let mut call_specs =
            self.base_calls
                .to_call_specs(stage, self.bob_weight, self.single_weight);
        for specific_call in &self.calls {
            call_specs.push(specific_call.to_call_spec(stage)?);
        }
        Ok(call_specs)
    }

    fn ch_mask_preset(&self, stage: Stage) -> Result<CourseHeadMaskPreset, Error> {
        let tenor = Bell::tenor(stage);
        Ok(if let Some(ch_mask_strings) = &self.course_heads {
            // If masks are specified, parse all the course head mask strings into `Mask`s,
            // defaulting to the tenor as calling bell
            CourseHeadMaskPreset::Custom(
                ch_mask_strings
                    .iter()
                    .map(|s| match Mask::parse_with_stage(s, stage) {
                        Ok(mask) => Ok((mask, tenor)),
                        Err(e) => Err(Error::ChMaskParse(s.to_owned(), e)),
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )
        } else if self.split_tenors {
            CourseHeadMaskPreset::SplitTenors
        } else {
            CourseHeadMaskPreset::TenorsTogether // Default to tenors together
        })
    }

    fn start_end_indices(&self, part_head: &Row) -> (Option<Vec<usize>>, Option<Vec<usize>>) {
        let mut start_indices = if self.snap_start {
            None
        } else {
            self.start_indices.clone()
        };
        let mut end_indices = self.end_indices.clone();

        // If this is a multi-part, then we require that `start_indices` and `end_indices` must be
        // the same.
        //
        // TODO: This is not quite correct.  More specifically, we need to create separate graphs
        // for each of the possible start/end indices.  This is because most of Monument can't tell
        // the difference between multi- and single-part comps, and therefore assumes that any pair
        // of starts/finishes are allowed.  In a multi-part this is not true, because we really
        // want the part-head to randomly cause a lead to re-start.  For the time being, we just
        // panic if we'd generate a multi-part graph with more than one start index.
        if !part_head.is_rounds() {
            let idx_union = match (start_indices, end_indices) {
                (Some(starts), Some(ends)) => {
                    let union_vec = starts
                        .iter()
                        .copied()
                        .filter(|i| ends.contains(i))
                        .collect_vec();
                    Some(union_vec)
                }
                (Some(idxs), None) | (None, Some(idxs)) => Some(idxs),
                (None, None) => None,
            };
            // Check that we only have one start/end index.  If we don't, Monument will try to
            // mix-and-match, usually causing things like reaching part heads at the snap
            assert!(idx_union.as_ref().map_or(false, |idxs| idxs.len() == 1));
            // Set both starts and ends to the same value
            start_indices = idx_union.clone();
            end_indices = idx_union;
        }

        (start_indices, end_indices)
    }

    fn ch_weights(
        &self,
        stage: Stage,
        methods: &[(Method, String)],
    ) -> Result<Vec<(Mask, f32)>, Error> {
        // Get the set of lead heads, so we can expand each CH lead mask to every matching CH mask
        // (i.e. compute each lead head within that course).  For example, CH lead mask `*34` would
        // expand into:
        // `[xxxxxx34, xxxxx4x3, xxx4x3xx, x4x3xxxx, x34xxxxx, xx3x4xxx, xxxx3x4x, xxxxx3x4]`
        let lead_heads = methods
            .iter()
            .flat_map(|(m, _)| m.lead_head().closure())
            .unique()
            .collect_vec();

        let mut weights = Vec::new();
        for pattern in &self.ch_weights {
            // Extract a (slice of patterns, weight)
            use ChWeightPattern::*;
            let (ch_masks, weight) = match pattern {
                Pattern { pattern, weight } => (std::slice::from_ref(pattern), weight),
                Patterns { patterns, weight } => (patterns.as_slice(), weight),
            };
            for mask_str in ch_masks {
                let mask = Mask::parse_with_stage(mask_str, stage)
                    .map_err(|e| Error::ChPatternParse(mask_str.to_owned(), e))?;
                // Add a weight pattern for every CH which contains a lead which matches `mask`
                for lh in &lead_heads {
                    weights.push((&mask * lh, *weight));
                }
            }
        }
        Ok(weights)
    }
}

/// Determine a suitable default range in which method counts must lie, thus enforcing decent
/// method balance.
fn method_count_range(
    num_methods: usize,
    len_range: &Range<usize>,
    user_range: OptRange,
) -> Range<usize> {
    let min_f32 = len_range.start as f32 / num_methods as f32 * (1.0 - METHOD_BALANCE_ALLOWANCE);
    let max_f32 = len_range.end as f32 / num_methods as f32 * (1.0 + METHOD_BALANCE_ALLOWANCE);
    let min = user_range.min.unwrap_or(min_f32.floor() as usize);
    let max = user_range.max.unwrap_or(max_f32.ceil() as usize);
    min..max + 1 // + 1 because the `method_count` is an **inclusive** range
}

/// Error generated when a user tries to read a TOML file from the FS
#[derive(Debug)]
pub enum TomlReadError {
    Io(std::io::Error),
    Parse(toml::de::Error),
}

pub fn read_toml<'de, T: Deserialize<'de>>(
    path: &Path,
    buf: &'de mut String,
) -> Result<T, TomlReadError> {
    *buf = std::fs::read_to_string(&path).map_err(TomlReadError::Io)?;
    toml::from_str(buf).map_err(TomlReadError::Parse)
}

///////////////
// METHOD(S) //
///////////////

#[derive(Debug, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum ChWeightPattern {
    Pattern { pattern: String, weight: f32 },
    Patterns { patterns: Vec<String>, weight: f32 },
}

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

        // Use the custom shorthand, or the first letter of the method's name, or "?" if the method
        // has no name
        let shorthand = self.shorthand().unwrap_or_else(|| {
            m.title()
                .chars()
                .next()
                .map_or_else(|| "?".to_owned(), |c| c.to_string())
        });

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

///////////
// MUSIC //
///////////

/// The specification for a music file
#[derive(Debug, Clone, Deserialize)]
pub struct MusicFile {
    music: Vec<MusicSpec>,
}

/// The specification for one type of music
// TODO: Find a way to get good deserialization without duplicating fields in every enum variant.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum MusicSpec {
    RunLength {
        #[serde(rename = "run_length")]
        length: usize,
        #[serde(default)]
        internal: bool,
        #[serde(flatten)]
        common: MusicCommon,
    },
    RunLengths {
        #[serde(rename = "run_lengths")]
        lengths: Vec<usize>,
        #[serde(default)]
        internal: bool,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Pattern {
        pattern: String,
        /// For each pattern, which music counts are allowed
        #[serde(default)]
        count_each: OptRange,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Patterns {
        patterns: Vec<String>,
        /// For each pattern, which music counts are allowed
        #[serde(default)]
        count_each: OptRange,
        #[serde(flatten)]
        common: MusicCommon,
    },
}

/// Values common to all enum variants of [`MusicSpec`]
#[derive(Debug, Clone, Deserialize)]
pub struct MusicCommon {
    #[serde(default = "get_one")]
    weight: f32,
    /// Possibly unbounded range of counts which are allowed in this music type
    #[serde(rename = "count", default)]
    count_range: OptRange,
    /// If `true`, then any nodes containing this music will be marked as 'non-duffer'
    non_duffer: Option<bool>,
    /// Which strokes this music can apply to
    #[serde(rename = "stroke", default)]
    strokes: StrokeSet,
}

impl MusicSpec {
    /// Generates a [`MusicType`] representing `self`.
    pub fn to_music_types(&self, stage: Stage, default_non_duffer: bool) -> Vec<MusicType> {
        enum LoweredType<'_self> {
            /// Equivalent to [`Self::Runs`] or [`Self::RunsList`]
            Runs(&'_self [usize], &'_self bool),
            /// Equivalent to [`Self::Pattern`] or [`Self::Pattern`]
            Patterns(&'_self [String], &'_self OptRange),
        }

        use std::slice::from_ref;

        // Extract the information from `self` into a normalised form
        let (lowered_type, common) = match self {
            Self::RunLength {
                length,
                internal,
                common,
            } => (LoweredType::Runs(from_ref(length), internal), common),
            Self::RunLengths {
                lengths,
                internal,
                common,
            } => (LoweredType::Runs(lengths, internal), common),
            Self::Pattern {
                pattern,
                count_each,
                common,
            } => (LoweredType::Patterns(from_ref(pattern), count_each), common),
            Self::Patterns {
                patterns,
                count_each,
                common,
            } => (LoweredType::Patterns(patterns, count_each), common),
        };
        let non_duffer = common.non_duffer.unwrap_or(default_non_duffer);

        match lowered_type {
            LoweredType::Runs(lengths, internal) => {
                let regexes = lengths
                    .iter()
                    .flat_map(|length| Regex::runs(stage, *length, *internal))
                    .collect_vec();
                // Runs can't take the `count_each` parameter, so can all be grouped into one
                // `MusicType`
                vec![MusicType::new(
                    regexes,
                    common.weight,
                    common.count_range,
                    non_duffer,
                    common.strokes,
                )]
            }
            LoweredType::Patterns(patterns, count_each) => {
                let regexes = patterns.iter().map(|s| Regex::parse(s));
                if count_each.is_set() {
                    if common.count_range.is_set() {
                        todo!("Mixing `count` and `count_each` isn't implemented yet!");
                    }
                    // If just `count_each` is set, we generate a separate `MusicType` for each
                    // pattern.  Each `MusicType` will contain exactly one `regex` corresponding to
                    // that pattern.
                    regexes
                        .map(|regex| {
                            MusicType::new(
                                vec![regex],
                                common.weight,
                                *count_each,
                                non_duffer,
                                common.strokes,
                            )
                        })
                        .collect_vec()
                } else {
                    // If `count_each` isn't set, we group all the patterns into one `MusicType` and
                    // apply `count` to all the regexes
                    vec![MusicType::new(
                        regexes.collect_vec(),
                        common.weight,
                        common.count_range,
                        non_duffer,
                        common.strokes,
                    )]
                }
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

fn backstroke() -> Stroke {
    Stroke::Back
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
            formatter.write_str(
                r#"a positive integer, a 'min/max' range, "practice", "qp", "half peal" or "peal""#,
            )
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
