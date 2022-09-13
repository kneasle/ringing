use std::{collections::HashMap, fmt::Write, path::PathBuf, sync::Arc};

use bellframe::{place_not::PnBlockParseError, Bell, Mask, RowBuf, Stage, Stroke};
use colored::Colorize;
use itertools::Itertools;
use monument::{
    builder::{
        CallDisplayStyle, EndIndices, Method, SpliceStyle, DEFAULT_BOB_WEIGHT,
        DEFAULT_SINGLE_WEIGHT,
    },
    Config, Search, SearchBuilder,
};
use serde::Deserialize;

use crate::{
    calls::{BaseCalls, CustomCall},
    music::{BaseMusic, MusicDisplay, MusicSpec},
    utils::OptRangeInclusive,
    Source,
};

use self::length::Length;

#[allow(unused_imports)] // Only used for doc comments
use bellframe::Row;

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
    #[serde(default = "default_num_comps")]
    num_comps: usize,
    /// Allow Monument to ignore falseness and generate false compositions.  Compositions still
    /// won't have internal rounds.
    #[serde(default)]
    allow_false: bool,

    /* CONFIG OPTIONS */
    /// If set, overrides `--graph-size-limit` CLI argument
    graph_size_limit: Option<usize>,

    /* METHODS */
    /// The method who's compositions we are after
    method: Option<MethodSpec>,
    #[serde(default)]
    /// A list of methods to be spliced together
    methods: Vec<MethodSpec>,
    /// At which locations method splices are allowed
    #[serde(default)]
    splice_style: SpliceStyle,
    /// Bounds on how many rows of each method is allowed
    #[serde(default)]
    method_count: OptRangeInclusive,
    /// Score which is applied for every change of method.  Defaults to `0.0`
    #[serde(default)]
    splice_weight: f32,
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

    /* CALLS */
    /// Sets the bell who's position will be used to determine calling positions.  Defaults to the
    /// heaviest bell
    calling_bell: Option<u8>,
    /// Which calls should be used by default
    #[serde(default)] // Default to near calls
    base_calls: BaseCalls,
    /// If `true`, then singles are excluded from `base_calls`.  This is mutually exclusive with
    /// `singles_only`
    #[serde(default)]
    bobs_only: bool,
    /// If `true`, then bobs are excluded from `base_calls`.  This is mutually exclusive with
    /// `bobs_only`.
    #[serde(default)]
    singles_only: bool,
    /// The weight given to each bob from `base_calls`
    bob_weight: Option<f32>,
    /// The weight given to each single from `base_calls`
    single_weight: Option<f32>,
    /// Which calls to use in the compositions
    #[serde(default)]
    calls: Vec<CustomCall>,

    /* MUSIC */
    /// Adds preset music patterns to the scoring.  If you truly want no music (e.g. to search for
    /// handbell-friendly comps), set `base_music = "none"`.
    #[serde(default)]
    base_music: BaseMusic,
    /// Path to a file containing a music definition, relative to **this** TOML file
    music_file: Option<PathBuf>,
    /// Specification of which classes of music Monument should consider
    #[serde(default)]
    music: Vec<MusicSpec>,
    /// The [`Stroke`] of the first row of the composition
    #[serde(default = "crate::utils::handstroke")]
    start_stroke: Stroke,

    /* COURSES */
    /// The [`Row`] which starts the composition.  When computing falseness and music, this **is**
    /// considered included in the composition.
    #[serde(default)] // The default/empty string parses to rounds on any stage
    start_row: String,
    /// The [`Row`] which ends the composition.  When computing falseness and music, this is
    /// **not** considered part of the composition; the composition stops just before this is
    /// reached.
    #[serde(default)] // The default/empty string parses to rounds on any stage
    end_row: String,
    /// A [`Row`] which generates the part heads of this composition
    #[serde(default)]
    part_head: String,
    /// If set, allows arbitrary splitting of the tenors (warning: this blows up the search size on
    /// large stages)
    #[serde(default)]
    split_tenors: bool,
    /// Which course heads masks are allowed (overrides `split_tenors`)
    course_heads: Option<Vec<String>>,
    /// Score applied to every row with a given CH patterns
    #[serde(default)]
    ch_weights: Vec<ChWeightPattern>,
    /// Weight given to every row in a course, for every handbell pair that's coursing
    #[serde(default)]
    handbell_coursing_weight: f32,

    /* NO-DUFFERS */
    /// Courses which should not be considered a 'duffer'.  If nothing is specified, all courses
    /// are 'non-duffers'.
    non_duffer_courses: Option<Vec<String>>,
    /// The most contiguous [`Row`]s of duffer courses that are allowed between two non-duffer
    /// courses.
    max_contiguous_duffer: Option<usize>,
    /// The most total [`Row`]s of duffer courses that can exist in the composition *in its
    /// entirety*.
    max_total_duffer: Option<usize>,
}

impl Spec {
    /// Read a `Spec` from a TOML file
    pub fn from_source(source: &Source) -> anyhow::Result<Self> {
        let toml_buf;
        let toml_string: &str = match source {
            Source::Path(p) => {
                toml_buf = crate::utils::read_file_to_string(p)?;
                &toml_buf
            }
            Source::Str { spec, .. } => spec,
        };
        crate::utils::parse_toml(toml_string)
    }

    /// 'Lower' this `Spec`ification into a [`Search`].
    pub fn lower(
        &self,
        source: &Source,
        opts: &crate::args::Options,
        leak_search_memory: bool,
    ) -> anyhow::Result<(Arc<Search>, Vec<MusicDisplay>)> {
        log::debug!("Generating query");

        // Build the methods first so that we can compute the overall `Stage` *before* parsing
        // everything else.
        let method_builders = self
            .methods
            .iter()
            .chain(self.method.as_ref())
            .cloned()
            .map(MethodSpec::into_builder)
            .collect::<anyhow::Result<Vec<_>>>()?;
        let length = monument::builder::Length::Range(self.length.range.clone());
        let mut search_builder =
            SearchBuilder::with_methods(method_builders, length).map_err(improve_error_message)?;
        let stage = search_builder.get_stage();

        // Lower `MethodSpec`s into `bellframe::Method`s.
        let calling_bell = match self.calling_bell {
            Some(v) => Bell::from_number(v).ok_or_else(|| {
                anyhow::Error::msg("Invalid calling bell: bell number 0 doesn't exist.")
            })?,
            None => stage.tenor(),
        };

        // Parse the CH mask and calls, and combine these with the `bellframe::Method`s to get
        // `layout::new::Method`s
        let part_head = parse_row("part head", &self.part_head, stage)?;

        // TODO: Make this configurable
        // TODO: Move this into `lib/`
        let call_display_style = if part_head.is_fixed(calling_bell) {
            CallDisplayStyle::CallingPositions(calling_bell)
        } else {
            CallDisplayStyle::Positional
        };

        // General params
        search_builder.num_comps = self.num_comps;
        search_builder.require_truth = !self.allow_false;
        // Methods
        search_builder.default_method_count = self.method_count.into();
        search_builder.default_start_indices = match &self.start_indices {
            Some(indices) => indices.clone(),
            // TODO: Compute actual snaps for multi-treble-dodge methods
            None if self.snap_start => vec![2],
            None => vec![0],
        };
        search_builder.default_end_indices = match &self.end_indices {
            Some(idxs) => EndIndices::Specific(idxs.to_vec()),
            None => EndIndices::Any,
        };
        search_builder.splice_style = self.splice_style;
        search_builder.splice_weight = self.splice_weight;
        // Calls
        search_builder.base_calls = self.base_calls()?;
        search_builder.custom_calls = self
            .calls
            .iter()
            .cloned()
            .map(|specific_call| specific_call.into_call_builder(stage))
            .collect::<anyhow::Result<_>>()?;
        search_builder.call_display_style = call_display_style;
        // Courses
        search_builder.start_row = parse_row("start row", &self.start_row, stage)?;
        search_builder.end_row = parse_row("end row", &self.end_row, stage)?;
        search_builder.part_head = part_head;
        search_builder.courses = match &self.course_heads {
            // If the user specifies some courses, use them
            Some(ch_strings) => Some(parse_masks(ch_strings, stage)?),
            // If the user specifies no courses but sets `split_tenors` then force every course
            None if self.split_tenors => Some(vec![Mask::empty(stage)]),
            // If the user doesn't specify anything, leave it at Monument's default (i.e. fix any
            // 'tenors' which are unaffected by the part head).
            None => None,
        };
        search_builder.course_weights = self.parse_ch_weights(stage)?;
        search_builder = search_builder.handbell_coursing_weight(self.handbell_coursing_weight);
        // Music
        let music_displays = self.music(source, &mut search_builder)?;
        search_builder.start_stroke = self.start_stroke;
        // Non-duffer
        if let Some(strings) = &self.non_duffer_courses {
            search_builder.non_duffer_courses = Some(parse_masks(strings, stage)?);
        }
        search_builder.max_contiguous_duffer = self.max_contiguous_duffer;
        search_builder.max_total_duffer = self.max_total_duffer;

        // Build the search
        let search = search_builder.build(self.config(opts, leak_search_memory))?;

        // Warn when using plain bob calls in Stedman or Grandsire
        for (_id, method, _shorthand) in search.methods() {
            if self.base_calls != BaseCalls::None {
                match method.name() {
                    // TODO: More precisely, we should check for Grandsire-like methods
                    "Grandsire" | "Stedman" => log::warn!(
                        "It looks like you're using Plain Bob calls in {}.  Try `base_calls = \"none\"`?",
                        method.name()
                    ),
                    _ => {}
                }
            }
        }

        Ok((Arc::new(search), music_displays))
    }

    pub fn duffers_specified(&self) -> bool {
        self.non_duffer_courses.is_some()
    }

    fn config(&self, opts: &crate::args::Options, leak_search_memory: bool) -> Config {
        let mut config = Config {
            thread_limit: opts.num_threads,
            leak_search_memory,
            ..Default::default()
        };
        if let Some(limit) = opts.graph_size_limit.or(self.graph_size_limit) {
            config.graph_size_limit = limit;
        }
        if let Some(limit) = opts.mem_limit {
            config.mem_limit = limit;
        }
        config
    }

    /// Convert [`crate::calls::BaseCalls`] into an [`Option`]`<`[`monument::query::BaseCalls`]`>`.
    /// Also check that `bobs_only` and `singles_only` aren't set at the same time.
    fn base_calls(&self) -> anyhow::Result<Option<monument::builder::BaseCalls>> {
        /* Suggest `{bobs,singles}_only` if the user gives calls an extreme negative weight */

        /// Any value of `{bob,single}_weight` smaller than this will trigger a warning to set
        /// `{single,bob}s_only = true`.
        const BIG_NEGATIVE_WEIGHT: f32 = -100.0;

        if let Some(w) = self.bob_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a singles only composition; consider using `singles_only = true` explicitly.");
            }
        }
        if let Some(w) = self.single_weight {
            if w <= BIG_NEGATIVE_WEIGHT {
                log::warn!("It looks like you're trying to make a bobs only composition; consider using `bobs_only = true` explicitly.");
            }
        }

        /* Check that `{bobs,singles}_only` aren't set at the same time */
        if self.bobs_only && self.singles_only {
            return Err(anyhow::Error::msg(
                "Composition can't be both `bobs_only` and `singles_only`",
            ));
        }

        /* Convert the base_calls */
        Ok(self
            .base_calls
            .as_monument_type()
            .map(|ty| monument::builder::BaseCalls {
                ty,
                bob_weight: (!self.singles_only)
                    .then_some(self.bob_weight.unwrap_or(DEFAULT_BOB_WEIGHT)),
                single_weight: (!self.bobs_only)
                    .then_some(self.single_weight.unwrap_or(DEFAULT_SINGLE_WEIGHT)),
            }))
    }

    fn music(
        &self,
        source: &Source,
        search: &mut SearchBuilder,
    ) -> anyhow::Result<Vec<MusicDisplay>> {
        // Load TOML for the music file
        let music_file_buffer;
        let music_file_str = match (&self.music_file, source) {
            (Some(relative_music_path), Source::Path(spec_path)) => {
                // If `self` was loaded from a file (as would be the case when using the CLI in
                // the wild), then we also load the music file from a file
                let mut music_path = spec_path
                    .parent()
                    .expect("files should always have a parent")
                    .to_owned();
                music_path.push(relative_music_path);
                music_file_buffer = crate::utils::read_file_to_string(&music_path)?;
                Some(music_file_buffer.as_str())
            }
            (Some(_), Source::Str { music_file, .. }) => {
                let music_file_str = music_file.ok_or_else(|| {
                    anyhow::Error::msg(
                        "Spec string requires `music_file`, but no music file string was given",
                    )
                })?;
                Some(music_file_str)
            }
            (None, _) => None,
        };
        // Generate `MusicDisplay`s necessary to display all the `MusicTypes` we've generated
        crate::music::generate_music(&self.music, self.base_music, music_file_str, search)
    }

    fn parse_ch_weights(&self, stage: Stage) -> anyhow::Result<Vec<(Mask, f32)>> {
        let mut weights = Vec::new();
        // Add explicit patterns from the `ch_weights` parameter
        for pattern in &self.ch_weights {
            // Extract a (slice of patterns, weight)
            use ChWeightPattern::*;
            let (ch_masks, weight) = match pattern {
                Pattern { pattern, weight } => (std::slice::from_ref(pattern), weight),
                Patterns { patterns, weight } => (patterns.as_slice(), weight),
            };
            // Add the patterns
            for mask_str in ch_masks {
                let mask = Mask::parse_with_stage(mask_str, stage)
                    .map_err(|e| mask_parse_error("course head weight", mask_str, e))?;
                weights.push((mask, *weight));
            }
        }
        Ok(weights)
    }
}

fn parse_row(name: &str, s: &str, stage: Stage) -> Result<RowBuf, anyhow::Error> {
    RowBuf::parse_with_stage(s, stage)
        .map_err(|e| anyhow::Error::msg(format!("Can't parse {} {:?}: {}", name, s, e)))
}

fn parse_masks(strings: &[String], stage: Stage) -> anyhow::Result<Vec<Mask>> {
    let mut masks = Vec::with_capacity(strings.len());
    for s in strings {
        masks.push(
            Mask::parse_with_stage(s, stage)
                .map_err(|e| mask_parse_error("course head mask", s, e))?,
        );
    }
    Ok(masks)
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
    course_heads: Option<Vec<String>>,
    start_indices: Option<Vec<isize>>,
    end_indices: Option<Vec<isize>>,
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

impl MethodSpec {
    fn into_builder(self) -> anyhow::Result<Method> {
        let (mut method_builder, common) = match self {
            MethodSpec::JustTitle(title) => (Method::with_title(title), None),
            MethodSpec::FromCcLib { title, common } => (Method::with_title(title), Some(common)),
            MethodSpec::Custom {
                name,
                place_notation,
                stage,
                common,
            } => (
                Method::with_custom_pn(name, place_notation, stage),
                Some(common),
            ),
        };
        if let Some(common) = common {
            if common.lead_locations.is_some() {
                return Err(anyhow::Error::msg(
                    "`methods.lead_locations` has been renamed to `labels`",
                ));
            }

            method_builder = method_builder
                .count_range(common.count_range.into())
                .courses(common.course_heads)
                .shorthand(common.shorthand)
                .start_indices(common.start_indices)
                .end_indices(common.end_indices);
            if let Some(labels) = common.labels {
                method_builder = method_builder.lead_labels(
                    labels
                        .into_iter()
                        .map(|(label, locs)| (label, locs.into_indices()))
                        .collect(),
                );
            }
        }
        Ok(method_builder)
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

/// Take a [`monument::Error`] and convert it to an [`anyhow::Error`], possibly overwriting the
/// error message with a more verbose coloured one.
fn improve_error_message(error: monument::Error) -> anyhow::Error {
    let message = match error {
        monument::Error::MethodNotFound { title, suggestions } => {
            method_suggestion_message(&title, suggestions)
        }
        monument::Error::MethodPnParse {
            name,
            place_notation_string,
            error,
        } => pn_parse_err_msg(&name, &place_notation_string, error),
        /*
        monument::Error::CustomCourseMaskParse {
            method_title,
            mask_str,
            error,
        } => mask_parse_error(mask_kind, string, e),
        */
        monument::Error::NoMethods => {
            "No methods specified.  Try something like `method = \"Bristol Surprise Major\"`."
                .to_owned()
        }
        e => e.to_string(),
    };
    anyhow::Error::msg(message)
}

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
