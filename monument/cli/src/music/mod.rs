use std::{cmp::Ordering, fmt::Write};

use bellframe::{
    music::{Elem, Pattern},
    Bell, RowBuf, Stage,
};
use itertools::Itertools;
use monument::{
    parameters::{IdGenerator, MusicType, MusicTypeId, MusicTypeVec, OptionalRangeInclusive},
    Search,
};
use serde::Deserialize;

use crate::utils::OptRangeInclusive;

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BaseMusic {
    /// No music specified
    None,
    /// A default music profile is used
    Default,
    /// Complib music profile is used
    Complib,
}

/// The specification for a music file
#[derive(Debug, Clone, Deserialize)]
struct MusicFile {
    music: Vec<TomlMusic>,
}

/// The specification for one type of music
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum TomlMusic {
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
        count_each: OptRangeInclusive,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Patterns {
        patterns: Vec<String>,
        /// For each pattern, which music counts are allowed
        #[serde(default)]
        count_each: OptRangeInclusive,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Preset {
        preset: MusicPreset,
        #[serde(flatten)]
        common: MusicCommon,
    },
}

/// Values common to all enum variants of [`TomlMusic`]
#[derive(Debug, Clone, Deserialize)]
pub struct MusicCommon {
    #[serde(default = "crate::utils::get_one")]
    weight: f32,
    /// Possibly unbounded range of counts which are allowed in this music type
    #[serde(rename = "count", default)]
    count_range: OptRangeInclusive,
    /// Which strokes this music can apply to
    #[serde(rename = "stroke", default)]
    strokes: StrokeSet,

    /// If `true`, the count of this will be displayed in the composition summary line.  Defaults
    /// to `true`.
    #[serde(default = "crate::utils::get_true")]
    show: bool,
    /// If `show = true` and `name = Some(n)`, this will override the default name with a custom
    /// one.
    name: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub enum MusicPreset {
    #[serde(rename = "5678 combinations")]
    Combinations5678s,
    #[serde(rename = "near misses")]
    NearMisses,
    #[serde(rename = "crus")]
    Crus,
}

////////////////////////////////////////////////
// DETERMINING WHICH `TomlMusic`S TO GENERATE //
////////////////////////////////////////////////

pub fn generate_music(
    toml_musics: &[TomlMusic],
    base_music: BaseMusic,
    music_file_str: Option<&str>,
    stage: Stage,
) -> anyhow::Result<(Vec<MusicDisplay>, MusicTypeVec<MusicType>)> {
    let mut music_builder = MusicTypeFactory::new();

    // Base music
    if let Some(base_music_toml) = base_music.toml(stage) {
        music_builder
            .add_music_toml(base_music_toml, stage)
            .expect("Loading base music should not cause errors");
    }
    // Music file
    if let Some(music_file_toml) = music_file_str {
        music_builder.add_music_toml(music_file_toml, stage)?;
    }
    // Explicit music types
    music_builder.add_toml_musics(toml_musics, stage)?;

    // Generate `MusicDisplay`s necessary to display all the `MusicTypes` we've generated
    Ok(music_builder.finish())
}

/// Struct to build a single set of `MusicType`s
struct MusicTypeFactory {
    music_types_with_displays: MusicTypeVec<(MusicType, Option<MusicTypeDisplay>)>,
    id_gen: IdGenerator<MusicTypeId>,
}

impl MusicTypeFactory {
    fn new() -> Self {
        Self {
            music_types_with_displays: MusicTypeVec::new(),
            id_gen: IdGenerator::starting_at_zero(),
        }
    }

    fn add_music_toml(&mut self, toml_str: &str, stage: Stage) -> anyhow::Result<()> {
        let music_file: MusicFile = crate::utils::parse_toml(toml_str)?;
        self.add_toml_musics(&music_file.music, stage)
    }

    fn add_toml_musics<'m>(
        &mut self,
        toml_musics: impl IntoIterator<Item = &'m TomlMusic>,
        stage: Stage,
    ) -> anyhow::Result<()> {
        for s in toml_musics {
            self.music_types_with_displays
                .extend(s.to_music_types(&mut self.id_gen, stage)?);
        }
        Ok(())
    }

    fn finish(self) -> (Vec<MusicDisplay>, MusicTypeVec<MusicType>) {
        compute_music_displays(self.music_types_with_displays)
    }
}

impl BaseMusic {
    fn toml(self, stage: Stage) -> Option<&'static str> {
        match self {
            BaseMusic::None => None,
            BaseMusic::Default => {
                let toml = default_music_toml(stage);
                match toml {
                    Some(_) => log::info!(
                        r#"Using default music.  If you want no music, set `base_music = "none"`."#
                    ),
                    None => log::warn!(
                        "No default music profile for {}.  No music will be scored.",
                        stage
                    ),
                }
                toml
            }
            BaseMusic::Complib => {
                let toml = complib_music_toml(stage);
                if toml.is_none() {
                    log::warn!(
                        "No complib music profile for {}.  No music will be scored.",
                        stage
                    );
                }
                toml
            }
        }
    }
}

impl Default for BaseMusic {
    fn default() -> Self {
        Self::Default
    }
}

#[rustfmt::skip] // So the `=>`s can line up
fn default_music_toml(stage: Stage) -> Option<&'static str> {
    match stage {
        Stage::MINOR   => Some(include_str!("default-music-minor.toml")),
        Stage::TRIPLES => Some(include_str!("default-music-triples.toml")),
        Stage::MAJOR   => Some(include_str!("default-music-major.toml")),
        Stage::ROYAL   => Some(include_str!("default-music-royal.toml")),
        Stage::MAXIMUS => Some(include_str!("default-music-maximus.toml")),
        _ => None,
    }
}

#[rustfmt::skip] // So the `=>`s can line up
fn complib_music_toml(stage: Stage) -> Option<&'static str> {
    match stage {
        Stage::MINOR   => Some(include_str!("complib-music-minor.toml")),
        Stage::TRIPLES => Some(include_str!("complib-music-triples.toml")),
        Stage::MAJOR   => Some(include_str!("complib-music-major.toml")),
        Stage::CATERS  => Some(include_str!("complib-music-caters.toml")),
        Stage::ROYAL   => Some(include_str!("complib-music-royal.toml")),
        Stage::CINQUES => Some(include_str!("complib-music-cinques.toml")),
        Stage::MAXIMUS => Some(include_str!("complib-music-maximus.toml")),
        _ => None,
    }
}

impl Default for MusicCommon {
    fn default() -> Self {
        Self {
            weight: 1.0,
            count_range: OptRangeInclusive::default(),
            strokes: StrokeSet::Both,

            show: true,
            name: None,
        }
    }
}

/// A set of at least one [`Stroke`]
#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case")]
enum StrokeSet {
    Hand,
    Back,
    #[default]
    Both,
}

impl From<StrokeSet> for monument::parameters::StrokeSet {
    fn from(value: StrokeSet) -> Self {
        match value {
            StrokeSet::Hand => Self::Hand,
            StrokeSet::Back => Self::Back,
            StrokeSet::Both => Self::Both,
        }
    }
}

////////////////////////////////
// `TomlMusic` -> `MusicType` //
////////////////////////////////

impl TomlMusic {
    /// Generates a [`MusicType`] representing `self`.
    fn to_music_types(
        &self,
        id_gen: &mut IdGenerator<MusicTypeId>,
        stage: Stage,
    ) -> anyhow::Result<Vec<(MusicType, Option<MusicTypeDisplay>)>> {
        // This function just delegates the work to one of `music_type_runs`,
        // `music_type_patterns` or `music_type_preset`.

        use std::slice::from_ref;
        match self {
            Self::RunLength {
                length,
                internal,
                common,
            } => Ok(music_type_runs(
                from_ref(length),
                *internal,
                common,
                id_gen,
                stage,
            )),
            Self::RunLengths {
                lengths,
                internal,
                common,
            } => Ok(music_type_runs(lengths, *internal, common, id_gen, stage)),
            Self::Pattern {
                pattern,
                count_each,
                common,
            } => music_type_patterns(from_ref(pattern), *count_each, common, id_gen, stage),
            Self::Patterns {
                patterns,
                count_each,
                common,
            } => music_type_patterns(patterns, *count_each, common, id_gen, stage),
            Self::Preset { preset, common } => music_type_preset(*preset, common, id_gen, stage),
        }
    }
}

fn music_type_runs(
    lengths: &[u8],
    internal: bool,
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> Vec<(MusicType, Option<MusicTypeDisplay>)> {
    let mut music_types = Vec::new();

    // Create individual `MusicType`s if needed for the breakdown.  The breakdown will be displayed
    // per run-length
    if common.show && common.name.is_none() {
        for &len in lengths {
            let pattern_name = format!("{}-bell run", len);
            let mut all_patterns = Vec::new(); // Used to create the `MusicType` for the total

            // Closure to add a `MusicType` for runs in some position
            let mut push_runs = |position: PatternPosition| {
                let patterns = match position {
                    PatternPosition::Front => Pattern::runs_front(stage, len),
                    PatternPosition::Internal => Pattern::runs_internal(stage, len),
                    PatternPosition::Back => Pattern::runs_back(stage, len),
                    PatternPosition::Total => unreachable!(),
                };
                // Add `MusicType` for the front/back/internal count
                music_types.push((
                    new_music_type(
                        id_gen,
                        patterns.clone(),
                        common.strokes,
                        0.0,
                        OptionalRangeInclusive::default(),
                    ),
                    Some(MusicTypeDisplay {
                        full_name: String::new(),
                        pattern: Some((pattern_name.clone(), position)),
                    }),
                ));
                // Make sure that the patterns get included in the total
                all_patterns.extend(patterns);
            };
            // Add front/internal/back patterns
            push_runs(PatternPosition::Front);
            if internal {
                push_runs(PatternPosition::Internal);
            }
            push_runs(PatternPosition::Back);
            // Add a single `MusicType` for all the positions, which contributes both the total and
            // the scoring weight
            music_types.push((
                new_music_type(
                    id_gen,
                    all_patterns,
                    common.strokes,
                    common.weight,
                    OptionalRangeInclusive::default(),
                ),
                Some(MusicTypeDisplay {
                    full_name: String::new(),
                    pattern: Some((pattern_name, PatternPosition::Total)),
                }),
            ));
        }
    }

    // If we need to enforce a count, create a single `MusicType` containing all run lengths
    let count_range = OptionalRangeInclusive::from(common.count_range);
    let need_to_add_weight = music_types.is_empty();
    if count_range.is_set() || need_to_add_weight {
        let patterns = lengths
            .iter()
            .flat_map(|length| Pattern::runs(stage, *length, internal))
            .collect_vec();
        let weight = if need_to_add_weight {
            common.weight
        } else {
            0.0
        };
        // Runs can't take the `count_each` parameter, so can all be grouped into one
        // `MusicType`
        music_types.push((
            new_music_type(id_gen, patterns, common.strokes, weight, count_range),
            None,
        ));
    }

    music_types
}

fn music_type_patterns(
    pattern_strings: &[String],
    count_each: OptRangeInclusive,
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> anyhow::Result<Vec<(MusicType, Option<MusicTypeDisplay>)>> {
    let individual_count = OptionalRangeInclusive::from(count_each);
    let combined_count = OptionalRangeInclusive::from(common.count_range);
    // Parse patterns
    let mut patterns = Vec::new();
    for pattern_string in pattern_strings {
        let pattern = Pattern::parse(pattern_string, stage)?;
        patterns.push(pattern);
    }

    let mut types = Vec::new();
    // Create music types for the individual groups
    if individual_count.is_set() || (common.show && common.name.is_none()) {
        types.extend(patterns.iter().cloned().map(|pattern| {
            let name = match (common.show, &common.name) {
                (true, None) => Some(MusicTypeDisplay::from_pattern(&pattern)),
                // If `show = true` and `name` is set, then this group *as a whole* will be named
                (true, Some(_)) => None,
                (false, _) => None, // If `show = false`, then no names are generated
            };

            (
                new_music_type(
                    id_gen,
                    vec![pattern],
                    common.strokes,
                    common.weight,
                    individual_count,
                ),
                name,
            )
        }));
    }
    // Create a single music type for the whole group
    if types.is_empty() || combined_count.is_set() || (common.show && common.name.is_some()) {
        let name = match (common.show, &common.name) {
            (true, Some(name)) => Some(MusicTypeDisplay::with_custom_name(name)),
            // If `common.show = true` but `common.name` is not set, then the patterns will be named
            // individually
            (true, None) => None,
            (false, _) => None, // If `common.show = false`, then no name is given
        };
        types.push((
            new_music_type(
                id_gen,
                patterns,
                common.strokes,
                // If individual `MusicType`s have already been created, then give the combined
                // `MusicType` a weight of 0 so everything isn't counted twice
                if types.is_empty() { common.weight } else { 0.0 },
                combined_count,
            ),
            name,
        ));
    }

    Ok(types)
}

fn music_type_preset(
    preset: MusicPreset,
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> anyhow::Result<Vec<(MusicType, Option<MusicTypeDisplay>)>> {
    // Determine the pattern types
    let (combined_patterns, front_back_patterns, default_name) = match preset {
        MusicPreset::Combinations5678s => match stage {
            // For Triples, `8` is always at the back, so `5678` combinations are any point where
            // `567` are at the back together
            Stage::TRIPLES => {
                let mut patterns = Vec::new();
                for singles_row in &Stage::SINGLES.extent() {
                    let mut pattern = vec![Elem::Star];
                    // Map `123` into `567` by adding 4 to each `Bell`
                    pattern.extend(singles_row.bell_iter().map(|b| b + 4).map(Elem::Bell));
                    patterns.push(
                        Pattern::from_vec(pattern, stage)
                            .expect("567 combination pattern should always be valid"),
                    );
                }
                (patterns, None, "5678 comb")
            }
            // For Major, a 5678 combination can happen at the front or back of a row
            Stage::MAJOR => {
                let mut patterns_front = Vec::new();
                let mut patterns_back = Vec::new();
                for minimus_row in &Stage::MINIMUS.extent() {
                    // Map `1234` to `5678` by adding 4 to each `Bell`
                    let bells_5678 = minimus_row.bell_iter().map(|b| b + 4).map(Elem::Bell);
                    patterns_front.push({
                        let mut pat = bells_5678.clone().collect_vec();
                        pat.push(Elem::Star);
                        Pattern::from_vec(pat, stage)
                            .expect("5678 combination pattern should be valid")
                    });
                    patterns_back.push({
                        let mut pat = vec![Elem::Star];
                        pat.extend(bells_5678.clone());
                        Pattern::from_vec(pat, stage)
                            .expect("5678 combination pattern should be valid")
                    });
                }
                // Combine front/back to create `patterns_both`
                let mut patterns_both = patterns_front.clone();
                patterns_both.extend(patterns_back.iter().cloned());
                (
                    patterns_both,
                    Some((patterns_front, patterns_back)),
                    "5678 comb",
                )
            }
            // 5678 combinations don't make sense for any stage other than Triples and Major
            _ => {
                return Err(anyhow::Error::msg(
                    "5678 combinations only make sense for Triples and Major",
                ));
            }
        },
        MusicPreset::NearMisses => {
            let patterns = (0..stage.num_bells() - 1)
                .map(|swap_idx| {
                    let mut pattern_row = RowBuf::rounds(stage);
                    pattern_row.swap(swap_idx, swap_idx + 1);
                    Pattern::from(pattern_row)
                })
                .collect_vec();
            (patterns, None, "NM")
        }
        MusicPreset::Crus => {
            if stage < Stage::TRIPLES {
                return Err(anyhow::Error::msg("Can't have CRUs on less than 7 bells"));
            }
            // Generate a pattern `*{b1}{b2}7890...` for b1 != b2 in {4,5,6}
            let pat_456 = [(4, 5), (5, 4), (4, 6), (6, 4), (5, 6), (6, 5)];
            let patterns = pat_456
                .into_iter()
                .map(|(b1, b2)| {
                    // "*{b1}{b2}"
                    let mut cru = vec![
                        Elem::Star,
                        Elem::Bell(Bell::from_number(b1).unwrap()),
                        Elem::Bell(Bell::from_number(b2).unwrap()),
                    ];
                    // "7890..."
                    cru.extend(stage.bells().skip(6).map(Elem::Bell));
                    Pattern::from_vec(cru, stage).expect("CRU pattern should be valid")
                })
                .collect_vec();
            (patterns, None, "CRU")
        }
    };

    // Determines the `music_type_display` for this position
    let music_type_display = |position: PatternPosition| -> Option<MusicTypeDisplay> {
        let name = common.name.as_deref().unwrap_or(default_name);
        // Only create `MusicTypeDisplay`s if this music_type is being displayed
        common.show.then(|| MusicTypeDisplay {
            full_name: name.to_owned(),
            pattern: Some((name.to_owned(), position)),
        })
    };

    // Combined `MusicType`
    let mut music_types = vec![(
        new_music_type(
            id_gen,
            combined_patterns,
            common.strokes,
            common.weight, // Add weight only to the combined `MusicType`
            OptionalRangeInclusive::from(common.count_range),
        ),
        music_type_display(PatternPosition::Total),
    )];
    // Front/back `MusicType`s
    if let Some((front_patterns, back_patterns)) = front_back_patterns {
        music_types.push((
            new_music_type(
                id_gen,
                front_patterns,
                common.strokes,
                0.0, // Weight is accounted for by the combined `MusicType`
                OptionalRangeInclusive::default(),
            ),
            music_type_display(PatternPosition::Front),
        ));
        music_types.push((
            new_music_type(
                id_gen,
                back_patterns,
                common.strokes,
                0.0, // Weight is accounted for by the combined `MusicType`
                OptionalRangeInclusive::default(),
            ),
            music_type_display(PatternPosition::Back),
        ));
    }

    Ok(music_types) // TODO: Actually generate music
}

//////////////////////////////////////
// DETERMINING HOW TO DISPLAY MUSIC //
//////////////////////////////////////

/// How music counts can be displayed.
///
/// This could take its counts from multiple [`MusicType`]s, and takes a few forms:
/// 1. Just a total count: e.g. `*5678: 23`
/// 2. Just a breakdown: e.g. `5678s: 24f,81i,24b`
/// 3. A breakdown and a total count: e.g. `4-runs: 312 (73f,132i,107b)`
///
/// Setting all sources to `None` is allowed, but will create an empty column.
#[derive(Debug, Clone)]
pub struct MusicDisplay {
    /// The name used to identify this type of music
    pub name: String,

    /// The index of the [`MusicType`] which provides the total count
    pub source_total: Option<MusicTypeId>,

    /// The index of the [`MusicType`] which provides the count off the front
    pub source_front: Option<MusicTypeId>,
    /// The index of the [`MusicType`] which provides the internal count
    pub source_internal: Option<MusicTypeId>,
    /// The index of the [`MusicType`] which provides the count off the back
    pub source_back: Option<MusicTypeId>,
}

impl MusicDisplay {
    /// Creates a new [`MusicDisplay`] with no corresponding [`MusicType`]s
    pub fn empty(name: String) -> Self {
        Self {
            name,
            source_total: None,
            source_front: None,
            source_internal: None,
            source_back: None,
        }
    }

    /// Return the width of the smallest column large enough to be guaranteed to hold (almost)
    /// every instance of this [`MusicDisplay`] (assuming rows can't be repeated).
    pub fn col_width(&self, search: &Search) -> usize {
        let all_zeros = index_vec::index_vec![0; search.parameters().music_types.len()];
        // We always pad the counts as much as required, so displaying a set of 0s results in a
        // maximum-width string (i.e. all output strings are the same length)
        let max_count_width = self.display_counts(search, &all_zeros).len();
        max_count_width.max(self.name.len())
    }

    /// Generate a compact string representing a given set of music counts
    pub fn display_counts(&self, search: &Search, counts: &MusicTypeVec<usize>) -> String {
        let mut s = String::new();

        // Add total count
        if let Some(total_id) = self.source_total {
            write_music_count(&mut s, search, counts, total_id);
        }

        // Add specific counts (if there are any)
        if self.source_front.is_some()
            || self.source_internal.is_some()
            || self.source_back.is_some()
        {
            // Add brackets if there's a total score
            if self.source_total.is_some() {
                s.push_str(" (");
            }
            // Add every front/internal/back count for which we have a source
            let mut is_first_count = true;
            for (source, position_char) in [
                (self.source_front, 'f'),
                (self.source_internal, 'i'),
                (self.source_back, 'b'),
            ] {
                if let Some(music_type_id) = source {
                    // Add separating comma
                    if !is_first_count {
                        s.push(' ');
                    }
                    is_first_count = false;
                    // Add the number
                    write_music_count(&mut s, search, counts, music_type_id);
                    s.push(position_char);
                }
            }
            if self.source_total.is_some() {
                s.push(')'); // Add closing brackets if there's a total score
            }
        }

        s
    }
}

/// Prints the width of the largest count possible for a [`MusicType`] (assuming that rows can't be
/// repeated).
fn write_music_count(
    s: &mut String,
    search: &Search,
    counts: &MusicTypeVec<usize>,
    id: MusicTypeId,
) {
    // `min(4)` because we don't expect more than 9999 instances of a music type, even
    // if more are theoretically possible
    let max_count_width = search.max_music_count(id).to_string().len().min(4);
    let idx = search.parameters().music_type_id_to_idx(id);
    write!(s, "{:>width$}", counts[idx], width = max_count_width).unwrap();
}

/// The way a [`MusicType`] should be displayed
#[derive(Debug, Clone)]
struct MusicTypeDisplay {
    /// A specific full name given to this [`MusicType`], used when this row isn't part of a
    /// combined pattern.  Usually this is the compressed pattern (e.g. `*5678`) or a specific name,
    /// like `"87s at back"` or `"Queens"`.
    full_name: String,
    /// The [`MusicType`] is a pattern applied to one end of the [`Row`].  For example:
    /// - `5678*`    will be given `Some(("5678", PatternPosition::Front))`
    /// - `*x5678x*` will be given `Some(("5678", PatternPosition::Internal))`
    /// - `*5678`    will be given `Some(("5678", PatternPosition::Back))`
    /// These would then be combined into a single count, like `5678s: 24f,80i,24b`
    pattern: Option<(String, PatternPosition)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PatternPosition {
    Front,
    Internal,
    Back,
    Total,
}

impl MusicTypeDisplay {
    fn with_custom_name(name: &str) -> Self {
        Self {
            full_name: name.to_owned(),
            pattern: None,
        }
    }

    fn from_pattern(pattern: &Pattern) -> Self {
        Self {
            full_name: pattern.to_string(),
            pattern: Self::pattern(pattern),
        }
    }

    /// Given a [`Pattern`], try to determine the underlying pattern, as well as its
    /// [`PatternPosition`] within a [`Row`].
    fn pattern(pattern: &Pattern) -> Option<(String, PatternPosition)> {
        // If a row is all wildcards, then no pattern is possible
        if pattern.elems().iter().all(|elem| elem.is_wildcard()) {
            return None;
        }

        // Split the `Pattern` into 3 sections:
        // 1. A prefix of wildcards
        // 2. A centre chunk, starting and ending with a specific bell, which is used to name the
        //    pattern
        // 3. A suffix of wildcards
        let wildcard_prefix_len = pattern
            .elems()
            .iter()
            .find_position(|elem| elem.is_bell())
            .unwrap() // This will only panic if the pattern contains no bells (which is checked)
            .0; // `find_position(_).unwrap()` returns `(index, element)`, but we just want the index
        let wildcard_suffix_len = pattern
            .elems()
            .iter()
            .rev()
            .find_position(|elem| elem.is_bell())
            .unwrap() // This will only panic if the pattern contains no bells (which is checked)
            .0; // `find_position(_).unwrap()` returns `(index, element)`, but we just want the index
        let wildcard_suffix_start = pattern.elems().len() - wildcard_suffix_len;
        let (head, wildcard_suffix) = pattern.elems().split_at(wildcard_suffix_start);
        let (wildcard_prefix, pattern_elems) = head.split_at(wildcard_prefix_len);

        // Check each section for `*`s.  Pattern normalisation tells us that the prefix and suffix
        // can contain at most one star each
        let star_in_prefix = wildcard_prefix.contains(&Elem::Star);
        let star_in_pattern = pattern_elems.contains(&Elem::Star);
        let star_in_suffix = wildcard_suffix.contains(&Elem::Star);
        // Count the number of `x`s in the suffix or prefix
        let xs_in_prefix = wildcard_prefix.iter().filter(|e| e.is_x()).count();
        let xs_in_suffix = wildcard_suffix.iter().filter(|e| e.is_x()).count();

        // If the internal pattern contains a `*`, then the pattern can't have a position
        if star_in_pattern {
            return None;
        }

        let (xs_front, xs_back, position) = if star_in_prefix && star_in_suffix {
            if xs_in_prefix >= 1 && xs_in_suffix >= 1 {
                // If both prefix and suffix have at least 1 `x`, then the pattern is considered
                // internal (e.g. `*x78x*` is considered an internal `78` and `*xx78xx*` is
                // considered an internal `x78x`).  Note how one `x` is taken off the front and
                // back to exclude the front/back of the row.
                (
                    xs_in_prefix - 1,
                    xs_in_suffix - 1,
                    PatternPosition::Internal,
                )
            } else {
                // If one of the ends doesn't have any `xs`, then the pattern is considered
                // anywhere
                (xs_in_prefix, xs_in_suffix, PatternPosition::Total)
            }
        } else {
            // At this point, we know that `pattern` contains at most one `*`, and that it is in
            // one of the suffix or prefix
            let num_stars = pattern.elems().iter().filter(|e| e.is_star()).count();
            assert!(num_stars <= 1);
            assert!(!star_in_pattern);

            // Since there can be at most one `*`, we can expand that single `*` into a series of
            // `x`s, and use that to compute the exact lengths of the suffix and prefix.
            //
            // The length of the star is the number of bells missing once every other element in the
            // pattern is accounted for.
            let star_len = pattern.stage().num_bells() - (pattern.elems().len() - num_stars);
            let prefix_len = xs_in_prefix + if star_in_prefix { star_len } else { 0 };
            let suffix_len = xs_in_suffix + if star_in_suffix { star_len } else { 0 };

            match prefix_len.cmp(&suffix_len) {
                // If the central pattern is equally far from the front and back, then we can't
                // give it a position
                Ordering::Equal => return None,
                // Otherwise, we assume that the pattern is attached to the closer end of the row,
                // so we assign those `x`s to be part of the pattern.
                //
                // Prefix is shorter, and is included in the pattern
                Ordering::Less => (prefix_len, 0, PatternPosition::Front),
                // Suffix is shorter, and is included in the pattern
                Ordering::Greater => (0, suffix_len, PatternPosition::Back),
            }
        };

        // pattern string is `{xs_in_prefix * 'x'}{pattern}{xs_in_suffix * 'x'}`
        let mut pattern_string = String::new();
        pattern_string.extend(std::iter::repeat('x').take(xs_front));
        for elem in pattern_elems {
            write!(pattern_string, "{}", elem).unwrap();
        }
        pattern_string.extend(std::iter::repeat('x').take(xs_back));
        Some((pattern_string, position))
    }
}

/// Given a list of [`MusicType`]s annotated with how to display each of them (i.e. a
/// [`MusicTypeDisplay`]), provide an independent list of [`monument::music::MusicDisplay`]s, which
/// describe how to display **all** the [`MusicType`]s.
///
/// This generates a more compact output by combining the same pattern in different locations (e.g.
/// 4-bell runs/5678s/6578s can be either front/internal/back).
fn compute_music_displays(
    annot_music_types: MusicTypeVec<(MusicType, Option<MusicTypeDisplay>)>,
) -> (Vec<MusicDisplay>, MusicTypeVec<MusicType>) {
    // Create an initial set of `MusicDisplay`s by grouping equivalent patterns into the same
    // `MusicDisplay`
    let mut music_displays = Vec::<MusicDisplay>::new();
    let mut music_types = MusicTypeVec::<MusicType>::new();
    for (music_type, music_type_display) in annot_music_types {
        // Compute the display if it exists
        if let Some(MusicTypeDisplay { full_name, pattern }) = music_type_display {
            match pattern {
                Some((pattern_name, position)) => {
                    // Display this pattern as `{pattern_name}s` (e.g. a pattern of "5678" will be
                    // displayed as "5678s").
                    let mut display_name = pattern_name;
                    display_name.push('s');
                    // Find or create a `MusicDisplay` with this `pattern_name`
                    let music_display_idx = music_displays
                        .iter()
                        .position(|d| d.name == display_name)
                        .unwrap_or_else(|| {
                            music_displays.push(MusicDisplay::empty(display_name.clone()));
                            music_displays.len() - 1 // Index of the new `MusicDisplay`
                        });
                    let music_display = &mut music_displays[music_display_idx];
                    assert_eq!(music_display.name, display_name);
                    // Depending on this pattern's position in the row, set this `MusicType` to
                    // contribute the corresponding total
                    let source = match position {
                        PatternPosition::Front => &mut music_display.source_front,
                        PatternPosition::Internal => &mut music_display.source_internal,
                        PatternPosition::Back => &mut music_display.source_back,
                        PatternPosition::Total => &mut music_display.source_total,
                    };
                    *source = Some(music_type.id);
                }
                // If this pattern has no position, we always create a new [`MusicDisplay`] with
                // this [`MusicType`] as the total
                None => music_displays.push(MusicDisplay {
                    name: full_name,
                    source_total: Some(music_type.id),
                    source_front: None,
                    source_internal: None,
                    source_back: None,
                }),
            }
        }
        // Add every music type, even if it's not displayed
        music_types.push(music_type);
    }

    // These music types may contain degenerate cases, like patterns which only appear in one
    // position within the row.  Here, we filter those out and replace them with an equivalent
    // `MusicDisplay` with just a total.
    for music_display in &mut music_displays {
        if music_display.source_total.is_some() {
            continue; // Skip any music displays which already have a total
        }

        // Decide which (if any) of the front/internal/back sources need to be replaced
        let (source_id, position) = match (
            &mut music_display.source_front,
            &mut music_display.source_internal,
            &mut music_display.source_back,
        ) {
            (Some(source_id), None, None) => (source_id, PatternPosition::Front),
            (None, Some(source_id), None) => (source_id, PatternPosition::Internal),
            (None, None, Some(source_id)) => (source_id, PatternPosition::Back),
            _ => continue,
        };

        // Move this front/internal/back source to become the total
        music_display.source_total = Some(*source_id);
        // Clear all the other sources (including the one we took the source from)
        music_display.source_back = None;
        music_display.source_internal = None;
        music_display.source_back = None;
        // Modify the name
        let mut pattern = std::mem::take(&mut music_display.name);
        assert_eq!(pattern.pop(), Some('s')); // The name was the pattern with 's' at the end
        music_display.name = match position {
            PatternPosition::Front => format!("{pattern}*"),
            PatternPosition::Internal => format!("x*{pattern}*x"),
            PatternPosition::Back => format!("*{pattern}"),
            PatternPosition::Total => unreachable!(),
        };
    }

    (music_displays, music_types)
}

fn new_music_type(
    id_gen: &mut IdGenerator<MusicTypeId>,
    patterns: Vec<Pattern>,
    strokes: StrokeSet,
    weight: f32,
    count_range: OptionalRangeInclusive,
) -> MusicType {
    MusicType {
        id: id_gen.next(),

        patterns,
        strokes: strokes.into(),
        weight,
        count_range,
    }
}

#[cfg(test)]
mod tests {
    use bellframe::{music::Pattern, Stage};

    use super::{MusicTypeDisplay, PatternPosition};

    #[test]
    fn extract_pattern_pattern() {
        #[track_caller]
        fn check(
            pattern_str: &str,
            stage: Stage,
            expected_pattern: &str,
            expected_position: PatternPosition,
        ) {
            let pattern = Pattern::parse(pattern_str, stage).unwrap();
            let (actual_pattern, actual_position) = MusicTypeDisplay::pattern(&pattern).unwrap();
            assert_eq!(actual_pattern, expected_pattern);
            assert_eq!(actual_position, expected_position);
        }

        use PatternPosition::*;

        check("5678*", Stage::MAJOR, "5678", Front);
        check("5678xxx*", Stage::MAJOR, "5678", Front);
        check("5678x*x", Stage::MAJOR, "5678", Front);
        check("5678xxxx", Stage::MAJOR, "5678", Front);
        check("5678xxxx*", Stage::MAJOR, "5678", Front);
        check("x78x*", Stage::MAJOR, "x78", Front);
        check("x78xxxxx", Stage::MAJOR, "x78", Front);

        check("*x5678x*", Stage::MAJOR, "5678", Internal);
        check("*xx5678x*", Stage::MAJOR, "x5678", Internal);

        check("*5678*", Stage::MAJOR, "5678", Total);
        check("*xx5678*", Stage::MAJOR, "xx5678", Total);
        check("*5678xx*", Stage::MAJOR, "5678xx", Total);

        check("*5678", Stage::MAJOR, "5678", Back);
        check("*xxxx5678", Stage::MAJOR, "5678", Back);
        check("xxxx5678", Stage::MAJOR, "5678", Back);
        check("x*x5678", Stage::MAJOR, "5678", Back);
        check("*x78x", Stage::MAJOR, "78x", Back);
        check("xxxxx78x", Stage::MAJOR, "78x", Back);
    }

    #[test]
    fn extract_pattern_no_pattern() {
        #[track_caller]
        fn check_no_pattern(pattern_str: &str, stage: Stage) {
            assert_eq!(
                MusicTypeDisplay::pattern(&Pattern::parse(pattern_str, stage).unwrap()),
                None
            );
        }

        check_no_pattern("x2*3x", Stage::MAJOR);
        check_no_pattern("*2*3*", Stage::MAJOR);
        check_no_pattern("x23x", Stage::MINIMUS);
    }
}
