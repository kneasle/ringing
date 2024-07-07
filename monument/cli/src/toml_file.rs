use std::{
    collections::HashMap,
    fmt::Write,
    path::{Path, PathBuf},
};

use anyhow::anyhow;
use bellframe::{
    method::LABEL_LEAD_END, method_lib::SearchError, place_not::PnBlockParseError, Bell, Mask,
    MethodLib, RowBuf, Stage, Stroke,
};
use colored::Colorize;
use index_vec::index_vec;
use itertools::Itertools;
use monument::{
    parameters::{
        BaseCallType, CallDisplayStyle, CallId, CallVec, MethodId, MethodVec, MusicType,
        MusicTypeVec, OptionalRangeInclusive, Parameters, DEFAULT_BOB_WEIGHT,
        DEFAULT_SINGLE_WEIGHT,
    },
    utils::IdGenerator,
    Config, PartHeadGroup,
};
use serde::Deserialize;

use crate::{
    calls::{BaseCalls, CustomCall},
    music::{BaseMusic, TomlMusic},
    utils::OptRangeInclusive,
};

use self::length::Length;

#[allow(unused_imports)] // Only used for doc comments
use bellframe::Row;

/// The specification for a set of compositions which Monument should search.  The [`TomlFile`] type is
/// parsed directly from the `TOML`, and can be thought of as an AST representation of the TOML
/// file.  Like ASTs, this specifies a superset of valid programs - so building a composition
/// search can also fail (as can lowering an AST).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TomlFile {
    /* GENERAL */
    /// The range of lengths of composition which are allowed
    length: Length,
    /// Monument won't stop until it generates the `num_comps` best compositions
    #[serde(default = "default_num_comps")]
    num_comps: usize,
    /// Set to `false` to allow Monument to ignore falseness and generate false compositions.
    /// Compositions still won't have internal rounds.
    #[serde(default = "crate::utils::get_true")]
    require_truth: bool,
    /// Allow Monument to ignore falseness and generate false compositions.  Compositions still
    /// won't have internal rounds.
    allow_false: Option<bool>, // Anti-alias of `require_truth`, deprecated in v0.13.0
    /// A [`Row`] which generates the part heads of this composition
    #[serde(default)]
    part_head: String,

    /* CONFIG OPTIONS */
    /// If set, overrides `--graph-size-limit` CLI argument
    graph_size_limit: Option<usize>,

    /* METHODS */
    /// The method who's compositions we are after
    method: Option<TomlMethod>,
    #[serde(default)]
    /// A list of methods to be spliced together
    methods: Vec<TomlMethod>,
    /// At which locations method splices are allowed
    #[serde(default)]
    splice_style: SpliceStyle,
    /// Score which is applied for every change of method.  Defaults to `0.0`
    #[serde(default)]
    splice_weight: f32,
    /// Bounds on how many rows of each method is allowed
    #[serde(default)]
    method_count: OptRangeInclusive,
    /// Set to `true` to allow comps to not start at the lead head.
    #[serde(default)]
    snap_start: bool,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed.  All indices are taken modulo each method's lead length (so
    /// 2, -30 and 34 are all equivalent for Treble Dodging Major).
    start_indices: Option<Vec<isize>>,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed.  All indices are taken modulo each method's lead length (so
    /// 2, -30 and 34 are all equivalent for Treble Dodging Major).
    end_indices: Option<Vec<isize>>,
    /// Score applied when a composition is 'all the work' - i.e. every (working) bell rings every
    /// row of every (working) place bell of every method.  This is smoothly interpolated; a 'half
    /// the work' composition would be given a score of `atw_weight / 2`.
    ///
    /// Setting this to any value will display atw values.  If you just want to display atw, set
    /// `atw_weight = 0.0`.
    atw_weight: Option<f32>,
    /// If set to `require_atw = true`, Monument will only generate `atw` compositions (filtering)
    /// all others.
    #[serde(default)]
    require_atw: bool,

    /* CALLS */
    /// Which calls should be used by default
    #[serde(default)] // Default to near calls
    base_calls: BaseCalls,
    /// If `true`, then singles are excluded from `base_calls`.  This is mutually exclusive with
    /// `singles_only`
    #[serde(default)]
    base_bobs_only: bool,
    /// If `true`, then bobs are excluded from `base_calls`.  This is mutually exclusive with
    /// `bobs_only`.
    #[serde(default)]
    base_singles_only: bool,
    /// The weight given to each bob from `base_calls`
    base_bob_weight: Option<f32>,
    /// The weight given to each single from `base_calls`
    base_single_weight: Option<f32>,
    /// Which calls to use in the compositions
    #[serde(default)]
    calls: Vec<CustomCall>,
    /// Sets the bell who's position will be used to determine calling positions.  Defaults to the
    /// heaviest bell
    calling_bell: Option<u8>,

    /* MUSIC */
    /// Adds preset music patterns to the scoring.  If you truly want no music (e.g. to search for
    /// handbell-friendly comps), set `base_music = "none"`.
    #[serde(default)]
    base_music: BaseMusic,
    /// Path to a file containing a music definition, relative to **this** TOML file
    music_file: Option<PathBuf>,
    /// Specification of which classes of music Monument should consider
    #[serde(default)]
    music: Vec<TomlMusic>,
    /// The [`Stroke`] of the first row of the composition
    #[serde(default = "crate::utils::handstroke")]
    start_stroke: Stroke,

    /* COURSES */
    /// If set, allows arbitrary splitting of the tenors (warning: this blows up the search size on
    /// large stages)
    #[serde(default)]
    split_tenors: bool,
    /// Which course heads masks are allowed (overrides `split_tenors`)
    courses: Option<Vec<String>>,
    course_heads: Option<Vec<String>>, // Alias of `courses`, deprecated in v0.13.0
    /// Score applied to every row with a given CH patterns
    #[serde(default)]
    course_weights: Vec<CourseWeightPattern>,
    ch_weights: Option<Vec<CourseWeightPattern>>, // Alias of `course_weights`, deprecated in v0.13.0
    /// Weight given to every row in a course, for every handbell pair that's coursing
    #[serde(default)]
    handbell_coursing_weight: f32,
    /// If set, Monument will only output compositions which have this call sequence.  For example,
    /// "HHsWsHsW" will output only compositions with the classic 1282 Yorkshire/Cambridge Royal
    /// calling.
    calling: Option<String>,
    /// If a `calling` is given, setting this to `true` will allow Monument to skip round
    /// blocks in the input calling (for example, if this is set to `true` then a calling of
    /// "WWWHHH" would generate "", "WWW", "HHH" and "WWWHHH").
    #[serde(default)]
    omit_round_blocks: bool,
    /// The [`Row`] which starts the composition.  When computing falseness and music, this **is**
    /// considered included in the composition.
    #[serde(default)] // The default/empty string parses to rounds on any stage
    start_row: String,
    /// The [`Row`] which ends the composition.  When computing falseness and music, this is
    /// **not** considered part of the composition; the composition stops just before this is
    /// reached.
    #[serde(default)] // The default/empty string parses to rounds on any stage
    end_row: String,
}

impl TomlFile {
    /// Load and parse a `TomlFile` structure from a TOML file
    pub fn new(toml_path: &Path) -> anyhow::Result<Self> {
        let toml_buf = crate::utils::read_file_to_string(toml_path)?;
        crate::utils::parse_toml(&toml_buf)
    }

    /// Build a set of [`Parameters`] from this `TomlFile`
    pub fn to_params(&self, toml_path: &Path) -> anyhow::Result<Parameters> {
        log::debug!("Generating params");

        // Error on deprecated paramaters
        if self.allow_false.is_some() {
            anyhow::bail!("`allow_false` has been replaced with `require_truth`");
        }
        if self.course_heads.is_some() {
            anyhow::bail!("`course_heads` has been renamed to `courses`");
        }

        let cc_lib =
            bellframe::MethodLib::cc_lib().expect("Couldn't load Central Council method library");
        // Build the methods first so that we can compute the overall `Stage` *before* parsing
        // everything else.
        let all_methods = self.methods.iter().chain(self.method.as_ref());
        let mut parsed_methods = Vec::new();
        for m in all_methods {
            parsed_methods.push((m.as_bellframe_method(&cc_lib)?, m.common()));
        }
        // Compute the stage so we can use it to help with parsing the rest of the file
        let stage = parsed_methods
            .iter()
            .map(|(m, _)| m.stage())
            .max()
            .ok_or_else(|| {
                anyhow!("No methods specified.  Try something like `method = \"Bristol Surprise Major\"`.")
            })?;

        let part_head = parse_row("part head", &self.part_head, stage)?;

        let calling_bell = match self.calling_bell {
            Some(v) => Bell::from_number(v).ok_or_else(|| {
                anyhow::Error::msg("Invalid calling bell: bell number 0 doesn't exist.")
            })?,
            None => stage.tenor(),
        };
        // TODO: Make this configurable
        // TODO: Move this into `lib/`
        let call_display_style = if part_head.is_fixed(calling_bell) {
            CallDisplayStyle::CallingPositions
        } else {
            CallDisplayStyle::Positional
        };

        let params = monument::parameters::Parameters {
            length: self.length.as_total_length_range(),
            stage,
            num_comps: self.num_comps,
            require_truth: self.require_truth,

            methods: self.build_methods(parsed_methods, &part_head, stage)?,
            splice_style: self.splice_style.into(),
            splice_weight: self.splice_weight,
            atw_weight: self.atw_weight,
            require_atw: self.require_atw,

            calls: self.calls(stage)?,
            call_display_style,
            calling_bell,

            part_head_group: PartHeadGroup::new(&part_head),
            start_row: parse_row("start row", &self.start_row, stage)?,
            end_row: parse_row("end row", &self.end_row, stage)?,
            course_weights: self.course_weights(stage)?,
            calling: self.calling.clone(),
            omit_round_blocks: self.omit_round_blocks,

            music_types: self.music(toml_path, stage)?,
            start_stroke: self.start_stroke,
        };
        Ok(params)
    }

    pub fn should_print_atw(&self) -> bool {
        self.atw_weight.is_some() && !self.require_atw
    }

    pub fn config(&self, opts: &crate::args::Options, leak_search_memory: bool) -> Config {
        let mut config = Config {
            thread_limit: opts.num_threads,
            mem_limit: opts.mem_limit,
            leak_search_memory,
            ..Default::default()
        };
        if let Some(limit) = opts.graph_size_limit.or(self.graph_size_limit) {
            config.graph_size_limit = limit;
        }
        config
    }

    /// Also check that `bobs_only` and `singles_only` aren't set at the same time.
    fn calls(&self, stage: Stage) -> anyhow::Result<CallVec<monument::parameters::Call>> {
        let mut call_id_generator = IdGenerator::<CallId>::starting_at_zero();
        // Convert base calls
        let mut calls = self.base_calls(&mut call_id_generator, stage)?;
        // Convert custom calls
        for custom_call in &self.calls {
            calls.push(custom_call.as_monument_call(call_id_generator.next(), stage)?);
        }
        Ok(calls)
    }

    fn base_calls(
        &self,
        id_gen: &mut IdGenerator<CallId>,
        stage: Stage,
    ) -> anyhow::Result<CallVec<monument::parameters::Call>> {
        let base_call_type = match self.base_calls {
            BaseCalls::Near => BaseCallType::Near,
            BaseCalls::Far => BaseCallType::Far,
            BaseCalls::None => return Ok(index_vec![]), // No base calls to generate
        };

        // Suggest `{bobs,singles}_only` if the user gives calls an extreme negative weight
        const BIG_NEGATIVE_WEIGHT: f32 = -100.0;
        if let Some(w) = self.base_bob_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a singles only composition; consider using `singles_only = true` explicitly.");
            }
        }
        if let Some(w) = self.base_single_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a bobs only composition; consider using `bobs_only = true` explicitly.");
            }
        }
        // Check that `{bobs,singles}_only` aren't set at the same time
        if self.base_bobs_only && self.base_singles_only {
            return Err(anyhow::Error::msg(
                "Composition can't be both `bobs_only` and `singles_only`",
            ));
        }

        Ok(monument::parameters::base_calls(
            id_gen,
            base_call_type,
            (!self.base_singles_only).then_some(self.base_bob_weight.unwrap_or(DEFAULT_BOB_WEIGHT)),
            (!self.base_bobs_only)
                .then_some(self.base_single_weight.unwrap_or(DEFAULT_SINGLE_WEIGHT)),
            stage,
        ))
    }

    fn music(&self, toml_path: &Path, stage: Stage) -> anyhow::Result<MusicTypeVec<MusicType>> {
        // Load TOML for the music file
        let music_file_str = match &self.music_file {
            Some(relative_music_path) => {
                let mut music_path = toml_path
                    .parent()
                    .expect("files should always have a parent")
                    .to_owned();
                music_path.push(relative_music_path);
                Some(crate::utils::read_file_to_string(&music_path)?)
            }
            None => None,
        };
        // Generate `MusicDisplay`s necessary to display all the `MusicTypes` we've generated
        crate::music::generate_music(
            &self.music,
            self.base_music,
            music_file_str.as_deref(),
            stage,
        )
    }

    fn course_weights(&self, stage: Stage) -> anyhow::Result<Vec<(Mask, f32)>> {
        let mut course_weights = self.parse_ch_weights(stage)?;

        // Handbell coursing weight
        if self.handbell_coursing_weight != 0.0 {
            // For every handbell pair ...
            for right_bell in stage.bells().step_by(2) {
                let left_bell = right_bell + 1;
                // ... add patterns for `*<left><right>` and `*<right><left>`
                for (b1, b2) in [(left_bell, right_bell), (right_bell, left_bell)] {
                    let mask_string = format!("*{b1}{b2}");
                    let mask = Mask::parse_with_stage(&mask_string, stage).unwrap();
                    course_weights.push((mask, self.handbell_coursing_weight));
                }
            }
        }
        Ok(course_weights)
    }

    fn parse_ch_weights(&self, stage: Stage) -> anyhow::Result<Vec<(Mask, f32)>> {
        let mut weights = Vec::new();
        if self.ch_weights.is_some() {
            anyhow::bail!("`ch_weights` has been renamed to `course_weights`");
        }
        // Add explicit patterns from the `ch_weights` parameter
        for pattern in &self.course_weights {
            // Extract a (slice of patterns, weight)
            use CourseWeightPattern::*;
            let (ch_masks, weight) = match pattern {
                Pattern { pattern, weight } => (std::slice::from_ref(pattern), weight),
                Patterns { patterns, weight } => (patterns.as_slice(), weight),
            };
            // Add the patterns
            for mask_str in ch_masks {
                weights.push((parse_mask("course head weight", mask_str, stage)?, *weight));
            }
        }
        Ok(weights)
    }

    fn build_methods(
        &self,
        parsed_methods: Vec<(bellframe::Method, MethodCommon)>,
        part_head: &Row,
        stage: Stage,
    ) -> anyhow::Result<MethodVec<monument::parameters::Method>> {
        // Warn when using plain bob calls in Stedman or Grandsire
        for (method, _) in &parsed_methods {
            if self.base_calls != BaseCalls::None {
                match method.name.as_str() {
                    // TODO: More precisely, we should check for Grandsire-like methods
                    "Grandsire" | "Stedman" => log::warn!(
                        "It looks like you're using Plain Bob calls in {}.  Try `base_calls = \"none\"`?",
                        method.name
                    ),
                    _ => {}
                }
            }
        }

        /* DEFAULT VALUES */

        let default_method_count = OptionalRangeInclusive::from(self.method_count);
        let default_start_indices = match &self.start_indices {
            Some(indices) => indices.clone(),
            // TODO: Compute actual snaps for multi-treble-dodge methods
            None if self.snap_start => vec![2],
            None => vec![0],
        };
        let default_allowed_courses = match &self.courses {
            // If the user specifies some courses, use them
            Some(ch_strings) => parse_masks("course mask", ch_strings, stage)?,
            // If the user specifies no courses but sets `split_tenors` then allow every course
            None if self.split_tenors => vec![Mask::any(stage)],
            // If no courses are set, fix any bell >=7 which aren't affected by the part head.
            // Usually this will be either all (e.g. 1-part or a part head of `1342` or `124365`)
            // or all (e.g. cyclic), but any other combinations are possible.  E.g. a composition
            // of Maximus with part head of `1765432` will still preserve 8 through 12.
            None => {
                let tenors_unaffected_by_part_head =
                    stage.bells().skip(6).filter(|&b| part_head.is_fixed(b));
                vec![Mask::with_fixed_bells(
                    stage,
                    tenors_unaffected_by_part_head,
                )]
            }
        };

        /* BUILD METHODS */

        let id_gen = IdGenerator::<MethodId>::starting_at_zero();
        let mut methods = MethodVec::new();
        // TODO: Add dummy unused method, to make sure that Monument handles them correctly
        for (mut method, common) in parsed_methods {
            let lead_len_isize = method.lead_len() as isize;
            let wrap_idxs = |idxs: Vec<isize>| -> Vec<usize> {
                let mut wrapped_idxs = Vec::new();
                for idx in idxs {
                    let wrapped_idx = ((idx % lead_len_isize) + lead_len_isize) % lead_len_isize;
                    wrapped_idxs.push(wrapped_idx as usize);
                }
                wrapped_idxs
            };

            // Error for using deprecated terms
            if common.lead_locations.is_some() {
                anyhow::bail!("`methods.lead_locations` has been renamed to `labels`");
            }
            if common.course_heads.is_some() {
                anyhow::bail!("`methods.course_heads` has been renamed to `courses`");
            }
            // Add lead labels
            let labels = common.labels.unwrap_or_else(|| {
                hmap::hmap! {
                    LABEL_LEAD_END.to_owned() => LeadLabels::JustOne(0)
                }
            });
            for (label, indices) in labels {
                for idx in wrap_idxs(indices.into_indices()) {
                    method.add_label(idx, label.clone());
                }
            }
            // Build method
            let allowed_courses = match common.courses {
                Some(ch_strings) => parse_masks("course mask", &ch_strings, stage)?,
                None => default_allowed_courses.clone(),
            };
            let start_indices = common
                .start_indices
                .unwrap_or_else(|| default_start_indices.clone());
            let end_indices = common
                .end_indices
                .unwrap_or_else(|| match &self.end_indices {
                    Some(idxs) => idxs.clone(),
                    None => (0..method.lead_len() as isize).collect_vec(),
                });
            methods.push(monument::parameters::Method {
                id: id_gen.next(),
                inner: method,

                custom_shorthand: common.shorthand.unwrap_or_default(),
                count_range: OptionalRangeInclusive::from(common.count_range)
                    .or(default_method_count),
                start_indices,
                end_indices,
                allowed_courses: vec![monument::parameters::CourseSet::from(allowed_courses)],
            });
        }
        Ok(methods)
    }
}

fn parse_row(name: &str, s: &str, stage: Stage) -> Result<RowBuf, anyhow::Error> {
    RowBuf::parse_with_stage(s, stage)
        .map_err(|e| anyhow::Error::msg(format!("Can't parse {} {:?}: {}", name, s, e)))
}

fn parse_masks(mask_kind: &str, strings: &[String], stage: Stage) -> anyhow::Result<Vec<Mask>> {
    let mut masks = Vec::with_capacity(strings.len());
    for s in strings {
        masks
            .push(Mask::parse_with_stage(s, stage).map_err(|e| mask_parse_error(mask_kind, s, e))?);
    }
    Ok(masks)
}

fn parse_mask(mask_kind: &str, string: &str, stage: Stage) -> anyhow::Result<Mask> {
    Mask::parse_with_stage(string, stage).map_err(|e| mask_parse_error(mask_kind, string, e))
}

///////////////
// METHOD(S) //
///////////////

#[derive(Debug, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum CourseWeightPattern {
    Pattern { pattern: String, weight: f32 },
    Patterns { patterns: Vec<String>, weight: f32 },
}

/// The contents of the `[method]` header in the input TOML file
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum TomlMethod {
    JustTitle(String),
    FromCcLib {
        title: String,
        #[serde(flatten)]
        common: MethodCommon,
    },
    Custom {
        name: String,
        place_notation: String,
        stage: Stage,
        #[serde(flatten)]
        common: MethodCommon,
    },
}

/// Common values for all method variants
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MethodCommon {
    shorthand: Option<String>,

    /// Optional override for method count range
    #[serde(default, rename = "count")]
    count_range: OptRangeInclusive,
    /// Maps labels to where in the lead they occur
    labels: Option<HashMap<String, LeadLabels>>,
    /// Deprecated name for `labels` (deprecated since `v0.11.0`)
    lead_locations: Option<HashMap<String, LeadLabels>>,
    /// Custom set of course head masks for this method
    courses: Option<Vec<String>>,
    course_heads: Option<Vec<String>>, // Deprecated spelling of `courses`
    start_indices: Option<Vec<isize>>,
    end_indices: Option<Vec<isize>>,
}

/// The different styles of spliced that can be generated.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize)]
pub enum SpliceStyle {
    /// Splices could happen at any lead label (usually just
    /// [lead ends](bellframe::method::LABEL_LEAD_END)).
    #[serde(rename = "leads")]
    LeadLabels,
    /// Splices only happen at calls.
    #[serde(rename = "calls")]
    Calls,
}

impl From<self::SpliceStyle> for monument::parameters::SpliceStyle {
    fn from(style: self::SpliceStyle) -> Self {
        match style {
            self::SpliceStyle::LeadLabels => monument::parameters::SpliceStyle::LeadLabels,
            self::SpliceStyle::Calls => monument::parameters::SpliceStyle::Calls,
        }
    }
}

impl Default for SpliceStyle {
    fn default() -> Self {
        Self::LeadLabels
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum LeadLabels {
    JustOne(isize),
    Many(Vec<isize>),
}

impl LeadLabels {
    fn into_indices(self) -> Vec<isize> {
        match self {
            Self::JustOne(idx) => vec![idx],
            Self::Many(indices) => indices,
        }
    }
}

const NUM_METHOD_SUGGESTIONS: usize = 10;

impl TomlMethod {
    fn as_bellframe_method(&self, cc_lib: &MethodLib) -> anyhow::Result<bellframe::Method> {
        match self {
            TomlMethod::JustTitle(title) | TomlMethod::FromCcLib { title, .. } => cc_lib
                .get_by_title_with_suggestions(title, NUM_METHOD_SUGGESTIONS)
                .map_err(|error| match error {
                    SearchError::PnParseErr { pn, error } => {
                        panic!("Error parsing {pn} in CCCBR library: {error}")
                    }
                    SearchError::NotFound(suggestions) => {
                        anyhow::Error::msg(method_suggestion_message(title, suggestions))
                    }
                }),
            TomlMethod::Custom {
                name,
                place_notation,
                stage,
                common: _,
            } => bellframe::Method::from_place_not_string(name.to_owned(), *stage, place_notation)
                .map_err(|error| anyhow::Error::msg(pn_parse_err_msg(name, place_notation, error))),
        }
    }

    fn common(&self) -> MethodCommon {
        match self {
            TomlMethod::JustTitle(_) => MethodCommon::default(),
            TomlMethod::FromCcLib { common, .. } => common.clone(),
            TomlMethod::Custom { common, .. } => common.clone(),
        }
    }
}

/////////////////
// NON DUFFERS //
/////////////////

#[derive(Debug, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
#[allow(dead_code)] // TODO: Remove this once this is used for course masks
enum CourseSet {
    OneMask(String),
    WithOptions {
        courses: Vec<String>,
        #[serde(default)]
        any_stroke: bool,
        #[serde(default)]
        any_bells: bool,
    },
}

impl CourseSet {
    #[allow(dead_code)] // TODO: Remove this once this is used for course masks
    fn as_monument_course_set(
        &self,
        mask_kind: &str,
        stage: Stage,
    ) -> anyhow::Result<monument::parameters::CourseSet> {
        Ok(match self {
            CourseSet::OneMask(mask_str) => {
                monument::parameters::CourseSet::from(parse_mask(mask_kind, mask_str, stage)?)
            }
            CourseSet::WithOptions {
                courses: masks,
                any_stroke,
                any_bells,
            } => monument::parameters::CourseSet {
                masks: parse_masks(mask_kind, masks, stage)?,
                any_stroke: *any_stroke,
                any_bells: *any_bells,
            },
        })
    }
}

////////////
// LENGTH //
////////////

mod length {
    use std::{
        fmt,
        ops::{Range, RangeInclusive},
    };

    use monument::utils::TotalLength;
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
        pub(super) range: RangeInclusive<usize>,
    }

    impl Length {
        pub(super) fn as_total_length_range(&self) -> RangeInclusive<TotalLength> {
            let start = TotalLength::new(*self.range.start());
            let end = TotalLength::new(*self.range.end());
            start..=end
        }
    }

    /////////////
    // PARSING //
    /////////////

    impl From<usize> for Length {
        #[inline(always)]
        fn from(v: usize) -> Length {
            Length { range: v..=v }
        }
    }

    impl From<Range<usize>> for Length {
        #[inline(always)]
        fn from(r: Range<usize>) -> Length {
            Length {
                range: r.start..=r.end - 1,
            }
        }
    }

    impl From<RangeInclusive<usize>> for Length {
        #[inline(always)]
        fn from(range: RangeInclusive<usize>) -> Length {
            Length { range }
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

////////////////////
// ERROR MESSAGES //
////////////////////

/// Constructs an error message for an unknown method, complete with suggestions and pretty diffs.
/// The message looks something like the following:
/// ```text
/// Can't find "{title}" in the Central Council method library.  Did you mean:
///      "{suggestions[0]}" ({diff for suggestions[0]})
///   or "{suggestions[1]}" ({diff for suggestions[1]})
///   or ...
/// ```
fn method_suggestion_message(title: &str, mut suggestions: Vec<(String, usize)>) -> String {
    // Sort suggestions by edit distance, **then alphabetically**.  This makes the error messages
    // deterministic (and, by extension, keeps the tests deterministic).
    suggestions.sort_by_key(|(name, edit_dist)| (*edit_dist, name.clone()));

    let mut message = format!(
        "Can't find {:?} in the Central Council method library.",
        title
    );
    message.push_str("  Did you mean:");
    // Only suggest the suggestions which (perhaps jointly) have the best edit
    // distance
    let best_edit_dist = suggestions[0].1;
    let mut is_first_suggestion = true;
    for (suggested_title, edit_dist) in suggestions {
        if edit_dist > best_edit_dist {
            break;
        }
        message.push('\n');
        message.push_str(if is_first_suggestion {
            "     "
        } else {
            "  or "
        });
        write!(
            message,
            "{:?} ({})",
            suggested_title,
            difference::Changeset::new(title, &suggested_title, "")
        )
        .unwrap();
        is_first_suggestion = false;
    }
    message
}

/// Construct a human-friendly error message for a PN parse error, like:
/// ```text
/// Can't parse place notation for method "{name}":
///       {method place notation}
///             ^^^^ {error message}
/// ```
fn pn_parse_err_msg(name: &str, pn_str: &str, error: PnBlockParseError) -> String {
    let (region, message) = match error {
        PnBlockParseError::EmptyBlock => {
            return format!(
                "Can't have empty place notation block for method {:?}",
                name
            );
        }
        PnBlockParseError::PlusNotAtBlockStart(plus_idx) => (
            plus_idx..plus_idx + 1,
            "`+` must only go at the start of a block (i.e. at the start or directly after a `,`)"
                .to_owned(),
        ),
        PnBlockParseError::PnError(range, err) => (range, err.to_string()),
    };

    let pn_before_error = &pn_str[..region.start];
    let pn_error = &pn_str[region.clone()];
    let pn_after_error = &pn_str[region.end..];
    // Given that we're labelling `region` with some `message`, construct the full string.
    let mut msg = format!("Can't parse place notation for method {:?}:\n", name);
    // Write the place notation string, with the offending 'region' bold.  Also use 5 spaces of
    // margin, plus one `"`
    writeln!(
        msg,
        "     \"{}{}{}\"",
        pn_before_error,
        pn_error.bright_red().bold(),
        pn_after_error
    )
    .unwrap();
    // Add the error message, with carets under the offending region
    let chars_before_plus = pn_before_error.chars().count();
    let caret_string = std::iter::repeat('^')
        .take(pn_error.chars().count())
        .join("")
        .bright_red()
        .bold();
    msg.extend(std::iter::repeat(' ').take(6 + chars_before_plus)); // 6 extra for the margin
    write!(msg, "{} {}", caret_string, message.bright_red().bold()).unwrap();
    msg
}

/// Construct a human-friendly error message when a mask fails to parse.
fn mask_parse_error(
    mask_kind: &str,
    string: &str,
    e: bellframe::mask::ParseError,
) -> anyhow::Error {
    anyhow::Error::msg(format!("Can't parse {mask_kind} {string:?}: {e}"))
}

/////////////
// HELPERS //
/////////////

fn default_num_comps() -> usize {
    100
}
