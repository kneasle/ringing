use std::{collections::HashMap, fmt::Write, ops::Deref, path::PathBuf, time::Duration};

use bellframe::{
    method::LABEL_LEAD_END,
    method_lib::QueryError,
    music::{Elem, Pattern},
    place_not::PnBlockParseError,
    Bell, Mask, MethodLib, Row, RowBuf, Stage, Stroke,
};
use colored::Colorize;
use itertools::Itertools;
use monument::{
    music::MusicType,
    utils::{group::PartHeadGroup, TotalLength},
    Call, CallDisplayStyle, CallVec, MethodVec, MusicTypeVec, OptRange, Query, SpliceStyle,
};
use serde::Deserialize;

use crate::{
    calls::{check_for_duplicate_call_names, BaseCalls, SpecificCall},
    music::{BaseMusic, MusicSpec},
    utils::OptRangeInclusive,
    Source,
};

use self::length::Length;

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
    calls: Vec<SpecificCall>,

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
    /// The value for `non_duffer` given to music types when none is explicitly given
    // TODO: Uncomment these when implementing non-duffers
    // #[serde(default = "get_true")]
    // default_non_duffer: bool,
    /// The most consecutive rows of duffer chunks.
    max_duffer_rows: Option<usize>,

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

    /* STARTS/ENDS */
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

    /// 'Lower' this `Spec`ification into a [`Query`]
    pub fn lower(&self, source: &Source) -> anyhow::Result<Query> {
        // Lower `MethodSpec`s into `bellframe::Method`s.
        let mut loaded_methods: Vec<(bellframe::Method, MethodCommon)> = self
            .methods
            .iter()
            .map(|m_spec| MethodSpec::gen_method(m_spec, self.base_calls))
            .collect::<anyhow::Result<_>>()?;
        if let Some(m) = &self.method {
            loaded_methods.push(m.gen_method(self.base_calls)?);
        }
        // Combine the methods' stages to get the overall stage
        let stage = loaded_methods
            .iter()
            .map(|(m, _)| m.stage())
            .max()
            .ok_or_else(|| {
                anyhow::Error::msg(
                    "No methods specified.  Try e.g. `method = \"Bristol Surprise Major\"`.",
                )
            })?;
        let calling_bell = match self.calling_bell {
            Some(v) => Bell::from_number(v).ok_or_else(|| {
                anyhow::Error::msg("Invalid calling bell: bell number 0 doesn't exist.")
            })?,
            None => stage.tenor(),
        };

        // Parse the CH mask and calls, and combine these with the `bellframe::Method`s to get
        // `layout::new::Method`s
        let part_head = parse_row("part head", &self.part_head, stage)?;
        let ch_masks = self.ch_masks(stage, calling_bell, &part_head)?;
        let calls = self.calls(stage)?;

        // Generate extra method data
        let shorthands = monument::utils::default_shorthands(
            loaded_methods
                .iter()
                .map(|(m, common)| (m.title(), common.shorthand.as_deref())),
        );
        let global_method_count = OptRange::from(self.method_count);
        // Combine method data
        let mut methods = MethodVec::new();
        #[allow(clippy::or_fun_call)] // `{start,end}_indices.as_ref()` is really cheap
        for ((method, common), shorthand) in loaded_methods.into_iter().zip_eq(shorthands) {
            let override_ch_masks = common
                .course_heads
                .map(|chs| parse_ch_masks(&chs, stage))
                .transpose()?;
            // Determine the start_indices
            let custom_start_indices = common
                .start_indices
                .as_ref()
                .or(self.start_indices.as_ref());
            let start_indices = match custom_start_indices {
                Some(custom) => custom.clone(),
                None if self.snap_start => vec![2],
                None => vec![0],
            };

            methods.push(monument::Method {
                inner: method,
                ch_masks: override_ch_masks.unwrap_or_else(|| ch_masks.clone()),
                shorthand,
                count_range: OptRange::from(common.count_range).or(global_method_count),
                start_indices,
                end_indices: common
                    .end_indices
                    .as_ref()
                    .or(self.end_indices.as_ref())
                    .cloned(),
            });
        }

        let ch_weights = self.ch_weights(stage, methods.iter().map(Deref::deref))?;
        let (music_types, music_displays) = self.music(source, stage)?;
        // TODO: Make this configurable
        let call_display_style = if part_head.is_fixed(calling_bell) {
            CallDisplayStyle::CallingPositions(calling_bell)
        } else {
            CallDisplayStyle::Positional
        };

        // Build this layout into a `Graph`
        Ok(Query {
            len_range: (&self.length).into(),
            num_comps: self.num_comps,
            allow_false: self.allow_false,
            stage,

            methods,
            call_display_style,
            calls: calls.into_iter().map(Call::from).collect(),
            splice_style: self.splice_style,

            start_row: parse_row("start row", &self.start_row, stage)?,
            end_row: parse_row("end row", &self.end_row, stage)?,
            part_head_group: PartHeadGroup::new(&part_head),
            ch_weights,
            splice_weight: self.splice_weight,

            music_types,
            music_displays,
            start_stroke: self.start_stroke,
            max_duffer_rows: self.max_duffer_rows.map(TotalLength::new),
        })
    }

    fn music(
        &self,
        source: &Source,
        stage: Stage,
    ) -> anyhow::Result<(MusicTypeVec<MusicType>, Vec<monument::music::MusicDisplay>)> {
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
        crate::music::generate_music(&self.music, self.base_music, music_file_str, stage)
    }

    fn calls(&self, stage: Stage) -> anyhow::Result<CallVec<Call>> {
        // Generate a full set of calls
        let mut call_specs = self.base_calls.create_calls(
            stage,
            self.bobs_only,
            self.singles_only,
            self.bob_weight,
            self.single_weight,
        )?;
        for specific_call in &self.calls {
            call_specs.push(specific_call.create_call(stage)?);
        }
        // Check for duplicate debug or display symbols
        check_for_duplicate_call_names(&call_specs, "display", |call| &call.display_symbol)?;
        check_for_duplicate_call_names(&call_specs, "debug", |call| &call.debug_symbol)?;
        Ok(call_specs)
    }

    fn ch_masks(
        &self,
        stage: Stage,
        calling_bell: Bell,
        part_head: &Row,
    ) -> anyhow::Result<Vec<Mask>> {
        Ok(if let Some(ch_mask_strings) = &self.course_heads {
            // Always use custom masks if set
            parse_ch_masks(ch_mask_strings, stage)?
        } else if self.split_tenors || !part_head.is_fixed(calling_bell) {
            // If either the part head doesn't preserve the calling bell (e.g. in cyclic spliced) or
            // `split_tenors = true`, then allow any course
            vec![Mask::empty(stage)]
        } else {
            // If `split_tenors = false` and no custom masks are set, then default to
            // tenors-together
            vec![tenors_together_mask(stage)]
        })
    }

    fn ch_weights<'m>(
        &self,
        stage: Stage,
        methods: impl IntoIterator<Item = &'m bellframe::Method>,
    ) -> anyhow::Result<Vec<(Mask, f32)>> {
        // Get the set of lead heads, so we can expand each CH lead mask to every matching CH mask
        // (i.e. compute each lead head within that course).  For example, CH lead mask `*34` would
        // expand into:
        // `[xxxxxx34, xxxxx4x3, xxx4x3xx, x4x3xxxx, x34xxxxx, xx3x4xxx, xxxx3x4x, xxxxx3x4]`
        let lead_heads = methods
            .into_iter()
            .flat_map(|m| m.lead_head().closure())
            .unique()
            .collect_vec();
        let mut weights = Vec::new();
        // Closure to a weight pattern for every CH which contains a lead which matches `mask`
        let mut add_ch_pattern = |mask: &Mask, weight: f32| {
            for lh in &lead_heads {
                weights.push((mask * lh, weight));
            }
        };

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
                add_ch_pattern(&mask, *weight);
            }
        }
        // Add patterns from `handbell_coursing_weight`
        if self.handbell_coursing_weight != 0.0 {
            for right_bell in stage.bells().step_by(2) {
                let left_bell = right_bell + 1;
                // For every handbell pair, we need patterns for `*<left><right>` and `*<right><left>`
                for (b1, b2) in [(left_bell, right_bell), (right_bell, left_bell)] {
                    let pattern =
                        Pattern::from_elems([Elem::Star, Elem::Bell(b1), Elem::Bell(b2)], stage)
                            .expect("Handbell patterns should always be valid regexes");
                    let mask = Mask::from_pattern(&pattern)
                        .expect("Handbell patterns should only have one `*`");
                    add_ch_pattern(&mask, self.handbell_coursing_weight);
                }
            }
        }
        Ok(weights)
    }
}

fn parse_row(name: &str, s: &str, stage: Stage) -> Result<RowBuf, anyhow::Error> {
    RowBuf::parse_with_stage(s, stage)
        .map_err(|e| anyhow::Error::msg(format!("Can't parse {} {:?}: {}", name, s, e)))
}

fn parse_ch_masks(ch_mask_strings: &[String], stage: Stage) -> anyhow::Result<Vec<Mask>> {
    ch_mask_strings
        .iter()
        .map(|s| {
            Mask::parse_with_stage(s, stage).map_err(|e| mask_parse_error("course head mask", s, e))
        })
        .collect()
}

/// Generate the course head mask representing the tenors together.  This corresponds to
/// `xxxxxx7890ET...` (or just the tenor for [`Stage`]s below Triples).
fn tenors_together_mask(stage: Stage) -> Mask {
    let mut fixed_bells = vec![];
    if stage <= Stage::MINOR {
        // On Minor or below, only fix the tenor
        fixed_bells.push(stage.tenor());
    } else {
        // On Triples and above, fix >=7 (i.e. skip the first 6 bells)
        fixed_bells.extend(stage.bells().skip(6));
    }
    Mask::fix_bells(stage, fixed_bells)
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
    lead_locations: Option<HashMap<String, LeadLocations>>,
    /// Custom set of course head masks for this method
    course_heads: Option<Vec<String>>,
    start_indices: Option<Vec<isize>>,
    end_indices: Option<Vec<isize>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum LeadLocations {
    JustOne(isize),
    Many(Vec<isize>),
}

impl LeadLocations {
    fn as_slice(&self) -> &[isize] {
        match self {
            Self::JustOne(idx) => std::slice::from_ref(idx),
            Self::Many(indices) => indices,
        }
    }
}

impl MethodSpec {
    fn gen_method(
        &self,
        base_calls: BaseCalls,
    ) -> anyhow::Result<(bellframe::Method, MethodCommon)> {
        let mut method = self.get_method_without_lead_locations()?;

        if base_calls != BaseCalls::None {
            match method.name() {
                // TODO: More precisely, we should check for Grandsire-like methods
                "Grandsire" | "Stedman" => {
                    log::warn!(
                        "It looks like you're using Plain Bob calls in {}.  Try `base_calls = \"none\"`?",
                        method.name()
                    );
                    log::warn!("In general, Grandsire and Stedman aren't very well supported yet.");
                    log::warn!("You're welcome to try, but be wary that weird things may happen.");
                    // Pause so that the user can register the warnings
                    std::thread::sleep(Duration::from_secs_f32(2.0));
                }
                _ => {}
            }
        }

        let lead_len = method.lead_len() as isize;
        for (name, locations) in self.get_lead_locations() {
            for l in locations.as_slice() {
                let wrapped_index = ((l % lead_len) + lead_len) % lead_len;
                // This cast is OK because we used % twice to guarantee a positive index
                method.add_label(wrapped_index as usize, name.clone());
            }
        }

        let common = self
            .common()
            .map_or_else(MethodCommon::default, MethodCommon::clone);
        Ok((method, common))
    }

    fn common(&self) -> Option<&MethodCommon> {
        match self {
            Self::JustTitle(_) => None,
            Self::FromCcLib { common, .. } => Some(common),
            Self::Custom { common, .. } => Some(common),
        }
    }

    fn get_lead_locations(&self) -> HashMap<String, LeadLocations> {
        self.common()
            .and_then(|c| c.lead_locations.clone())
            .unwrap_or_else(default_lead_labels)
    }

    /// Attempt to generate a new [`Method`] with no lead location annotations
    fn get_method_without_lead_locations(&self) -> anyhow::Result<bellframe::Method> {
        match self {
            MethodSpec::FromCcLib { title, .. } | MethodSpec::JustTitle(title) => {
                let lib = MethodLib::cc_lib().ok_or_else(|| {
                    anyhow::Error::msg("Central Council method library can't be loaded.")
                })?;
                lib.get_by_title_with_suggestions(title, 5)
                    .map_err(|err| match err {
                        QueryError::PnParseErr { .. } => {
                            panic!("Method lib contains invalid place notation")
                        }
                        QueryError::NotFound(suggestions) => {
                            anyhow::Error::msg(method_suggestion_message(title, suggestions))
                        }
                    })
            }
            MethodSpec::Custom {
                name,
                stage,
                place_notation,
                ..
            } => bellframe::Method::from_place_not_string(name.clone(), *stage, place_notation)
                .map_err(|e| anyhow::Error::msg(pn_parse_err_msg(name, place_notation, e))),
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
fn method_suggestion_message(title: &str, mut suggestions: Vec<(&str, usize)>) -> String {
    // Sort suggestions by edit distance, **then alphabetically**.  This makes the error messages
    // deterministic (and, by extension, keeps the tests deterministic).
    suggestions.sort_by_key(|&(name, edit_dist)| (edit_dist, name));

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
            difference::Changeset::new(title, suggested_title, "")
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
    anyhow::Error::msg(format!("Can't parse {} {:?}: {}", mask_kind, string, e))
}

/////////////
// HELPERS //
/////////////

fn default_num_comps() -> usize {
    100
}

/// By default, add a lead location "LE" on the 0th row (i.e. when the place notation repeats,
/// usually the lead end).
fn default_lead_labels() -> HashMap<String, LeadLocations> {
    let mut labels = HashMap::new();
    labels.insert(LABEL_LEAD_END.to_owned(), LeadLocations::JustOne(0));
    labels
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

    // Convert Length to a ranges
    impl From<&Length> for RangeInclusive<TotalLength> {
        #[inline(always)]
        fn from(l: &Length) -> RangeInclusive<TotalLength> {
            let start = TotalLength::new(*l.range.start());
            let end = TotalLength::new(*l.range.end());
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
