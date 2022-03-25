use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::Write,
    ops::{Deref, Range},
    path::{Path, PathBuf},
    time::Duration,
};

use bellframe::{
    method::LABEL_LEAD_END,
    method_lib::QueryError,
    music::{Regex, RegexElem},
    place_not::PnBlockParseError,
    Bell, Mask, MethodLib, Row, RowBuf, Stage, Stroke,
};
use colored::Colorize;
use itertools::Itertools;
use monument::{
    layout::{
        self,
        new::{Call, CourseHeadMaskPreset, SpliceStyle},
        Layout,
    },
    music::{MusicType, StrokeSet},
    CallIdx, CallType, CallVec, OptRange, Query,
};
use serde::Deserialize;

use crate::{
    calls::{BaseCalls, SpecificCall},
    Source,
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
    method_count: OptRange,
    /// Score which is applied for every change of method.  Defaults to `0.0`
    #[serde(default)]
    splice_weight: f32,

    /* CALLS */
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
    /// The most consecutive rows of duffer chunks.
    max_duffer_rows: Option<usize>,

    /* COURSES */
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
    /// If `true`, generate compositions lead-wise, rather than course-wise.  This is useful for
    /// cases like cyclic comps where no course heads are preserved across parts.
    ///
    /// If left unset, Monument will determine the best value - i.e. leadwise iff the part head
    /// affects the tenor
    leadwise: Option<bool>,

    /* STARTS/ENDS */
    /// Set to `true` to allow comps to not start at the lead head.
    #[serde(default)]
    snap_start: bool,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed.  All indices are taken modulo each method's lead length (so
    /// 2, -30 and 34 are all equivalent for Treble Dodging Major).
    #[serde(default = "default_start_indices")]
    start_indices: Option<Vec<isize>>,
    /// Which indices within a lead should the composition be allowed to start.  If unspecified,
    /// then all locations are allowed.  All indices are taken modulo each method's lead length (so
    /// 2, -30 and 34 are all equivalent for Treble Dodging Major).
    end_indices: Option<Vec<isize>>,
}

impl Spec {
    /// Read a `Spec` from a TOML file
    pub fn from_source(source: Source) -> anyhow::Result<Self> {
        let toml_buf;
        let toml_string: &str = match source {
            Source::Path(p) => {
                toml_buf = read_file_to_string(p)?;
                &toml_buf
            }
            Source::Str { spec, .. } => spec,
        };
        parse_toml(toml_string)
    }

    /// 'Lower' this `Spec`ification into a [`Query`]
    pub fn lower(&self, source: Source) -> anyhow::Result<Query> {
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
        // Parse the CH mask and calls, and combine these with the `bellframe::Method`s to get
        // `layout::new::Method`s
        let ch_masks = self.ch_mask_preset(stage)?.into_masks(stage);
        let calls = self.calls(stage)?;
        let part_head = RowBuf::parse_with_stage(&self.part_head, stage).map_err(|e| {
            anyhow::Error::msg(format!("Can't parse part head {:?}: {}", self.part_head, e))
        })?;
        let (start_indices, end_indices) = self.start_end_indices(&part_head);

        // Convert the methods
        let is_spliced = loaded_methods.len() > 1;
        let shorthands = monument::utils::default_shorthands(
            loaded_methods
                .iter()
                .map(|(m, common)| (m.title(), common.shorthand.as_deref())),
        );
        let method_count = self.method_count.or_range(&default_method_count(
            loaded_methods.len(),
            &self.length.range,
        ));
        if is_spliced {
            log::info!("method count range: {:?}", method_count);
        }
        let mut methods = Vec::new();
        #[allow(clippy::or_fun_call)] // `{start,end}_indices.as_deref()` is really cheap
        for ((method, common), shorthand) in loaded_methods.into_iter().zip(shorthands) {
            let override_ch_masks = common
                .course_heads
                .map(|chs| parse_ch_masks(&chs, stage))
                .transpose()?;
            let count_range = common.count_range.or_range(&method_count);
            if is_spliced && common.count_range.is_set() {
                log::info!("count range for {:<2}: {:?}", shorthand, count_range);
            }
            methods.push(layout::new::Method::new(
                method,
                // TODO: Apply all calls to all methods, and control calls exclusively using lead
                // locations
                (0..calls.len()).map(CallIdx::new).collect_vec(),
                override_ch_masks.unwrap_or_else(|| ch_masks.clone()),
                shorthand,
                count_range,
                common.start_indices.as_deref().or(start_indices.as_deref()),
                common.end_indices.as_deref().or(end_indices.as_deref()),
            ));
        }

        let ch_weights = self.ch_weights(stage, methods.iter().map(Deref::deref))?;

        // Generate the `Layout`
        let layout = Layout::new(
            methods,
            &calls,
            self.splice_style,
            &part_head,
            self.leadwise,
        )?;

        let music_types = self.music_types(source, stage)?;
        if music_types.is_empty() {
            // TODO: Add default music
            log::warn!("No music patterns specified.  Are you sure you don't care about music?");
        }

        // Build this layout into a `Graph`
        Ok(Query {
            layout,
            len_range: self.length.range.clone(),
            num_comps: self.num_comps,
            allow_false: self.allow_false,

            calls: calls.into_iter().map(CallType::from).collect(),
            part_head,
            ch_weights,
            splice_weight: self.splice_weight,

            music_types,
            start_stroke: self.start_stroke,
            max_duffer_rows: self.max_duffer_rows,
        })
    }

    fn music_types(&self, source: Source, stage: Stage) -> anyhow::Result<Vec<MusicType>> {
        // Compute the explicitly mentioned music types
        let mut music_types = self
            .music
            .iter()
            .flat_map(|spec| spec.to_music_types(stage, self.default_non_duffer))
            .collect_vec();
        // Load the music from the `music_file`
        if let Some(relative_music_path) = &self.music_file {
            let toml_buf;
            let music_file_toml_string: &str = match source {
                Source::Path(spec_path) => {
                    // If `self` was loaded from a file (as would be the case when using the CLI in
                    // the wild), then we also load the music file from a file
                    let mut music_path = spec_path
                        .parent()
                        .expect("files should always have a parent")
                        .to_owned();
                    music_path.push(relative_music_path);
                    toml_buf = read_file_to_string(&music_path)?;
                    &toml_buf
                }
                Source::Str { music_file, .. } => music_file.ok_or_else(|| {
                    anyhow::Error::msg(
                        "Spec string requires `music_file`, but no music file string was given",
                    )
                })?,
            };

            // Parse the TOML
            let music_file: MusicFile = parse_toml(music_file_toml_string)?;
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

    fn calls(&self, stage: Stage) -> anyhow::Result<CallVec<Call>> {
        // Generate a full set of calls
        let mut call_specs = self.base_calls.to_call_specs(
            stage,
            self.bobs_only,
            self.singles_only,
            self.bob_weight,
            self.single_weight,
        )?;
        for specific_call in &self.calls {
            call_specs.push(specific_call.to_call_spec(stage)?);
        }
        // Check for duplicate debug or display symbols
        Self::check_for_duplicate_call_names(&call_specs, "display", |call| &call.display_symbol)?;
        Self::check_for_duplicate_call_names(&call_specs, "debug", |call| &call.debug_symbol)?;
        Ok(CallVec::from(call_specs))
    }

    /// Check for duplicate calls.  Calls are a 'duplicate' if assign the same symbol at the same
    /// lead location but to **different** place notations.  If they're the same, then Monument
    /// will de-duplicate them.
    fn check_for_duplicate_call_names<'calls>(
        call_specs: &'calls [layout::new::Call],
        symbol_name: &str,
        get_symbol: impl Fn(&'calls layout::new::Call) -> &'calls str,
    ) -> anyhow::Result<()> {
        let sorted_calls = call_specs
            .iter()
            .map(|call| (get_symbol(call), &call.lead_location, &call.place_not))
            .sorted_by_key(|&(sym, lead_loc, _pn)| (sym, lead_loc));
        for ((sym1, lead_location1, pn1), (sym2, lead_location2, pn2)) in
            sorted_calls.tuple_windows()
        {
            if sym1 == sym2 && lead_location1 == lead_location2 && pn1 != pn2 {
                return Err(anyhow::Error::msg(format!(
                    "Call {} symbol {:?} (at {:?}) is used for both {} and {}",
                    symbol_name, sym1, lead_location1, pn1, pn2
                )));
            }
        }
        Ok(())
    }

    fn ch_mask_preset(&self, stage: Stage) -> anyhow::Result<CourseHeadMaskPreset> {
        Ok(if let Some(ch_mask_strings) = &self.course_heads {
            // If masks are specified, parse all the course head mask strings into `Mask`s,
            // defaulting to the tenor as calling bell
            CourseHeadMaskPreset::Custom(parse_ch_masks(ch_mask_strings, stage)?)
        } else if self.split_tenors {
            CourseHeadMaskPreset::SplitTenors
        } else {
            CourseHeadMaskPreset::TenorsTogether // Default to tenors together
        })
    }

    fn start_end_indices(&self, part_head: &Row) -> (Option<Vec<isize>>, Option<Vec<isize>>) {
        let mut start_indices = self.start_indices.clone().or_else(|| {
            if self.snap_start {
                None // If just `snap_start`, then allow any start
            } else {
                Some(vec![0]) // If nothings specified, only search standard starts
            }
        });
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
                    let regex = Regex::from_elems([
                        RegexElem::Glob,
                        RegexElem::Bell(b1),
                        RegexElem::Bell(b2),
                    ]);
                    let mask = Mask::from_regex(&regex, stage)
                        .expect("Handbell patterns should always be valid");
                    add_ch_pattern(&mask, self.handbell_coursing_weight);
                }
            }
        }
        Ok(weights)
    }
}

fn parse_ch_masks(ch_mask_strings: &[String], stage: Stage) -> anyhow::Result<Vec<(Mask, Bell)>> {
    ch_mask_strings
        .iter()
        .map(|s| match Mask::parse_with_stage(s, stage) {
            Ok(mask) => Ok((mask, stage.tenor())),
            Err(e) => Err(mask_parse_error("course head mask", s, e)),
        })
        .collect()
}

/// Determine a suitable default range in which method counts must lie, thus enforcing decent
/// method balance.
fn default_method_count(num_methods: usize, len_range: &Range<usize>) -> Range<usize> {
    let min_f32 = len_range.start as f32 / num_methods as f32 * (1.0 - METHOD_BALANCE_ALLOWANCE);
    let max_f32 = len_range.end as f32 / num_methods as f32 * (1.0 + METHOD_BALANCE_ALLOWANCE);
    // + 1 because the `method_count` is an **inclusive** range
    (min_f32.floor() as usize)..(max_f32.ceil() as usize + 1)
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
    count_range: OptRange,
    /// The inputs to this map are numerical strings representing sub-lead indices, and the
    /// outputs are the lead location names
    lead_locations: Option<HashMap<String, String>>,
    /// Custom set of course head masks for this method
    course_heads: Option<Vec<String>>,
    start_indices: Option<Vec<isize>>,
    end_indices: Option<Vec<isize>>,
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

        for (index_str, name) in self.get_lead_locations() {
            let index = index_str.parse::<isize>().map_err(|_| {
                anyhow::Error::msg(format!(
                    "Lead location {:?} (for label {:?} in {:?}) is not a valid integer",
                    index_str,
                    name,
                    method.title()
                ))
            })?;
            let lead_len = method.lead_len() as isize;
            let wrapped_index = ((index % lead_len) + lead_len) % lead_len;
            // This cast is OK because we used % twice to guarantee a positive index
            method.set_label(wrapped_index as usize, Some(name.clone()));
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

    fn get_lead_locations(&self) -> HashMap<String, String> {
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
    use bellframe::mask::ParseError as PE;
    let mut s = format!("Can't parse {} {:?}: ", mask_kind, string);
    match e {
        PE::MultipleGlobs => {
            s.push_str("too many `*`s.  Masks can only have one region with `x` or `*`.");
        }
        PE::BellExceedsStage(bell, stage) => {
            write!(s, "bell {} is out of stage {}", bell, stage).unwrap();
        }
        PE::MismatchedLength(len, stage) => match len.cmp(&stage.num_bells()) {
            Ordering::Less => write!(
                s,
                "mask is too short; did you mean `{}*` or `{}{}`?",
                string,
                string,
                stage
                    .bells()
                    .skip(len)
                    .map(|b| b.to_char().unwrap())
                    .collect::<String>()
            )
            .unwrap(),
            Ordering::Equal => unreachable!(),
            Ordering::Greater => write!(
                s,
                "mask is too long, needing at least {} bells (too many for {})",
                len, stage
            )
            .unwrap(),
        },
    }
    anyhow::Error::msg(s)
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
        length: u8,
        #[serde(default)]
        internal: bool,
        #[serde(flatten)]
        common: MusicCommon,
    },
    RunLengths {
        #[serde(rename = "run_lengths")]
        lengths: Vec<u8>,
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
    /// If `true`, then any chunks containing this music will be marked as 'non-duffer'
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
            Runs(&'_self [u8], &'_self bool),
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

//////////////////////
// HELPER FUNCTIONS //
//////////////////////

/// Attempt to read a file as a [`String`], returning a helpful error message on failure
fn read_file_to_string(path: &Path) -> anyhow::Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::Error::msg(format!("Can't open {:?}: {}", path, e)))
}

/// Attempt to read a file as a [`String`], returning a helpful error message on failure
fn parse_toml<'de, T: Deserialize<'de>>(s: &'de str) -> anyhow::Result<T> {
    toml::from_str(s).map_err(|e| anyhow::Error::msg(format!("Error parsing spec file: {}", e)))
}

fn get_one() -> f32 {
    1.0
}

fn default_num_comps() -> usize {
    100
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

fn default_start_indices() -> Option<Vec<isize>> {
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
