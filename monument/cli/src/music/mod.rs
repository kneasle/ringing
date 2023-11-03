use std::fmt::Write;

use bellframe::{
    music::{AtRowPositions, Pattern, RowPosition},
    Stage,
};
use index_vec::IndexVec;
use monument::parameters::{IdGenerator, MusicType, MusicTypeId, MusicTypeVec, Parameters};
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
        #[serde(flatten)]
        common: MusicCommon,
    },
    RunLengths {
        #[serde(rename = "run_lengths")]
        lengths: Vec<u8>,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Pattern {
        pattern: String,
        #[serde(flatten)]
        common: MusicCommon,
    },
    Patterns {
        patterns: Vec<String>,
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
    #[serde(rename = "weight", default)]
    specified_weight: MusicWeight,
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

impl MusicCommon {
    fn should_show(&self) -> bool {
        self.show || self.name.is_some()
    }
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

#[derive(Debug, Clone, Copy, Deserialize)]
pub struct MusicWeight {
    front: Option<f32>,
    internal: Option<f32>,
    back: Option<f32>,
    wrap: Option<f32>,
}

impl MusicWeight {
    fn get_mut(&mut self, pos: RowPosition) -> &mut Option<f32> {
        match pos {
            RowPosition::Front => &mut self.front,
            RowPosition::Internal => &mut self.internal,
            RowPosition::Back => &mut self.back,
            RowPosition::Wrap => &mut self.wrap,
        }
    }
}

impl Default for MusicWeight {
    fn default() -> Self {
        Self {
            front: Some(1.0),
            internal: None,
            back: Some(1.0),
            wrap: None,
        }
    }
}

impl From<MusicWeight> for AtRowPositions<Option<f32>> {
    fn from(value: MusicWeight) -> Self {
        Self {
            front: value.front,
            internal: value.internal,
            back: value.back,
            wrap: value.wrap,
        }
    }
}

////////////////////////////////////////////////
// DETERMINING WHICH `TomlMusic`S TO GENERATE //
////////////////////////////////////////////////

pub fn generate_music(
    toml_musics: &[TomlMusic],
    base_music: BaseMusic,
    music_file_str: Option<&str>,
    stage: Stage,
) -> anyhow::Result<(MusicTypeVec<MusicType>, Vec<MusicDisplay>)> {
    let mut music_builder = MusicTypeFactory::new();

    // Base music
    if let Some(base_music_toml) = base_music.toml(stage) {
        music_builder
            .add_toml(base_music_toml, stage)
            .expect("Loading base music should not cause errors");
    }
    // Music file
    if let Some(music_file_toml) = music_file_str {
        music_builder.add_toml(music_file_toml, stage)?;
    }
    // Explicit music types
    music_builder.add_musics(toml_musics, stage)?;

    // Generate `MusicDisplay`s necessary to display all the `MusicTypes` we've generated
    Ok(music_builder.finish())
}

/// Struct to build a single set of `MusicType`s
#[derive(Debug)]
struct MusicTypeFactory {
    music_types_with_displays: MusicTypeVec<(MusicType, Option<MusicDisplay>)>,
    id_gen: IdGenerator<MusicTypeId>,
}

impl MusicTypeFactory {
    fn new() -> Self {
        Self {
            music_types_with_displays: MusicTypeVec::new(),
            id_gen: IdGenerator::starting_at_zero(),
        }
    }

    fn add_toml(&mut self, toml_str: &str, stage: Stage) -> anyhow::Result<()> {
        let music_file: MusicFile = crate::utils::parse_toml(toml_str)?;
        self.add_musics(&music_file.music, stage)
    }

    fn add_musics<'m>(
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

    fn finish(self) -> (MusicTypeVec<MusicType>, Vec<MusicDisplay>) {
        let mut music_types = IndexVec::new();
        let mut music_displays = Vec::new();
        for (ty, disp) in self.music_types_with_displays {
            music_types.push(ty);
            music_displays.extend(disp);
        }
        (music_types, music_displays)
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
            specified_weight: MusicWeight::default(),
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

impl From<StrokeSet> for bellframe::StrokeSet {
    fn from(value: StrokeSet) -> Self {
        match value {
            StrokeSet::Hand => bellframe::StrokeSet::Hand,
            StrokeSet::Back => bellframe::StrokeSet::Back,
            StrokeSet::Both => bellframe::StrokeSet::Both,
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
    ) -> anyhow::Result<Vec<(MusicType, Option<MusicDisplay>)>> {
        // This function just delegates the work to one of `music_type_runs`,
        // `music_type_patterns` or `music_type_preset`.

        use std::slice::from_ref;
        match self {
            Self::RunLength { length, common } => {
                Ok(music_type_runs(from_ref(length), common, id_gen, stage))
            }
            Self::RunLengths { lengths, common } => {
                Ok(music_type_runs(lengths, common, id_gen, stage))
            }
            Self::Pattern { pattern, common } => {
                music_type_patterns(from_ref(pattern), common, id_gen, stage)
            }
            Self::Patterns { patterns, common } => {
                music_type_patterns(patterns, common, id_gen, stage)
            }
            Self::Preset { preset, common } => music_type_preset(*preset, common, id_gen, stage),
        }
    }
}

fn music_type_runs(
    lengths: &[u8],
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> Vec<(MusicType, Option<MusicDisplay>)> {
    let mut music_types = Vec::new();
    for &len in lengths {
        music_types.push(new_music_type(
            id_gen.next(),
            format!("{len}-bell runs"),
            bellframe::MusicType::runs(len, stage),
            common,
            true,
        ));
    }
    music_types
}

fn music_type_patterns(
    pattern_strings: &[String],
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> anyhow::Result<Vec<(MusicType, Option<MusicDisplay>)>> {
    let count_range = monument::parameters::OptionalRangeInclusive::from(common.count_range);
    // Parse patterns
    let mut patterns = Vec::new();
    for pattern_string in pattern_strings {
        patterns.push(Pattern::parse_with_stage(pattern_string, stage)?);
    }
    // Create music types
    let mut music_types = Vec::new();
    if common.show && common.name.is_none() {
        // If this is being shown but no custom name is given, we display each pattern separately
        for pattern in &patterns {
            music_types.push(new_music_type(
                id_gen.next(),
                format!("{pattern}s"),
                bellframe::MusicType::from(pattern.clone()),
                common,
                false,
            ));
        }
    }
    if common.name.is_some() || count_range.is_set() {
        // If the user gave a custom name, we combine all the patterns into one [`MusicType`]
        let (mut music_type, mut music_display) = new_music_type(
            id_gen.next(),
            String::new(),
            bellframe::MusicType::new(patterns),
            &common,
            false,
        );
        if !music_types.is_empty() {
            music_type.weights = AtRowPositions::ZERO_F32;
            music_display = None;
        }
        music_types.push((music_type, music_display));
    }
    Ok(music_types)
}

fn music_type_preset(
    preset: MusicPreset,
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> anyhow::Result<Vec<(MusicType, Option<MusicDisplay>)>> {
    // Determine the pattern types
    let (music_type, positions, default_name) = match preset {
        MusicPreset::Combinations5678s => match stage {
            // For Triples, `8` is always at the back, so `5678` combinations are any point where
            // `567` are at the back together
            Stage::TRIPLES => (
                bellframe::MusicType::combination_5678s_triples(),
                AtRowPositions::BACK,
                "5678 combs",
            ),
            // For Major, a 5678 combination can happen at the front or back of a row
            Stage::MAJOR => (
                bellframe::MusicType::combination_5678s_major(),
                AtRowPositions::FRONT_AND_BACK,
                "5678 combs",
            ),
            // 5678 combinations don't make sense for any stage other than Triples and Major
            _ => {
                return Err(anyhow::Error::msg(
                    "5678 combinations only make sense for Triples and Major",
                ));
            }
        },
        MusicPreset::NearMisses => (
            bellframe::MusicType::near_misses(stage),
            AtRowPositions::FRONT,
            "NMs",
        ),
        MusicPreset::Crus => {
            if stage < Stage::TRIPLES {
                return Err(anyhow::Error::msg("Can't have CRUs on less than 7 bells"));
            }
            let positions = if stage.is_even() {
                AtRowPositions::FRONT_AND_BACK
            } else {
                AtRowPositions::BACK
            };
            (bellframe::MusicType::crus(stage), positions, "CRUs")
        }
    };

    // Reset weight for positions which this music type can't use
    let mut common = common.clone();
    for pos in RowPosition::ALL {
        if !positions.get(pos) {
            *common.specified_weight.get_mut(pos) = None;
        }
    }
    // Construct a music type
    Ok(vec![new_music_type(
        id_gen.next(),
        default_name.to_owned(),
        music_type,
        &common,
        false,
    )])
}

fn new_music_type(
    id: MusicTypeId,
    default_name: String,
    music_type: bellframe::MusicType,
    common: &MusicCommon,
    show_total: bool,
) -> (MusicType, Option<MusicDisplay>) {
    let weights = AtRowPositions::<Option<f32>>::from(common.specified_weight);
    // Create music types, etc.
    let music_type = MusicType {
        id,
        inner: music_type.at_stroke(common.strokes.into()),
        weights: weights.map(|x| x.unwrap_or(0.0)),
        count_range: common.count_range.into(),
    };
    let music_display = common.should_show().then(|| MusicDisplay {
        source: music_type.id,
        name: match &common.name {
            Some(name) => name.clone(),
            None => default_name.to_owned(),
        },
        show_total,
        show: weights.map(|x| x.is_some()),
    });
    (music_type, music_display)
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
    pub source: MusicTypeId,

    /// The name used to identify this type of music
    pub name: String,
    pub show_total: bool,
    pub show: AtRowPositions<bool>,
}

impl MusicDisplay {
    /// Return the width of the smallest column large enough to be guaranteed to hold (almost)
    /// every instance of this [`MusicDisplay`] (assuming rows can't be repeated).
    pub fn col_width(&self, params: &Parameters) -> usize {
        let all_zeros = index_vec::index_vec![AtRowPositions::ZERO; params.music_types.len()];
        // We always pad the counts as much as required, so displaying a set of 0s results in a
        // maximum-width string (i.e. all output strings are the same length)
        let max_count_width = self.display_counts(&all_zeros, params).len();
        max_count_width.max(self.name.len())
    }

    /// Generate a compact string representing a given set of music counts
    pub fn display_counts(
        &self,
        all_counts: &MusicTypeVec<AtRowPositions<usize>>,
        params: &Parameters,
    ) -> String {
        let music_type_idx = params.music_type_id_to_idx(self.source);
        let music_type = &params.music_types[music_type_idx];
        let counts = all_counts[music_type_idx];
        let max_counts = music_type.max_possible_count(params.stage);
        let num_items_to_show: usize = self.show.map(|b| b as usize).total();

        let mut s = String::new();
        // Add total count
        if self.show_total || num_items_to_show == 1 {
            write_music_count(
                &mut s,
                counts.masked(!self.show, 0).total(),
                max_counts.masked(!self.show, 0).total(),
            );
        }
        // Add specific counts (if there are any)
        if num_items_to_show > 1 {
            // Add brackets if there's a total score
            if self.show_total {
                s.push_str(" (");
            }
            // Add every front/internal/back count for which we have a source
            let mut is_first_count = true;
            for (position, position_char) in [
                (RowPosition::Front, 'f'),
                (RowPosition::Internal, 'i'),
                (RowPosition::Back, 'b'),
                (RowPosition::Wrap, 'w'),
            ] {
                if !self.show.get(position) {
                    continue; // Skip positions we're not showing
                }
                // Add separating comma
                if !is_first_count {
                    s.push(' ');
                }
                is_first_count = false;
                // Add the number
                write_music_count(&mut s, *counts.get(position), *max_counts.get(position));
                s.push(position_char);
            }
            if self.show_total {
                s.push(')'); // Add closing brackets if there's a total score
            }
        }

        s
    }
}

/// Prints the width of the largest count possible for a [`MusicType`] (assuming that rows can't be
/// repeated).
fn write_music_count(s: &mut String, count: usize, max_possible_count: usize) {
    // `min(4)` because we don't expect more than 9999 instances of a music type, even
    // if more are theoretically possible
    let max_count_width = max_possible_count.to_string().len().min(4);
    write!(s, "{:>width$}", count, width = max_count_width).unwrap();
}

/*
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
*/
