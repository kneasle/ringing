use bellframe::{
    music::{AtRowPositions, Pattern, RowPosition},
    Stage,
};
use monument::parameters::{IdGenerator, MusicType, MusicTypeId, MusicTypeVec};
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
) -> anyhow::Result<MusicTypeVec<MusicType>> {
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
    music_types: MusicTypeVec<MusicType>,
    id_gen: IdGenerator<MusicTypeId>,
}

impl MusicTypeFactory {
    fn new() -> Self {
        Self {
            music_types: MusicTypeVec::new(),
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
            self.music_types
                .extend(s.to_music_types(&mut self.id_gen, stage)?);
        }
        Ok(())
    }

    fn finish(self) -> MusicTypeVec<MusicType> {
        self.music_types
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
    ) -> anyhow::Result<Vec<MusicType>> {
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
) -> Vec<MusicType> {
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
) -> anyhow::Result<Vec<MusicType>> {
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
    if music_types.is_empty() || count_range.is_set() {
        // If the user gave a custom name or is not showing this type at all, we combine all the
        // patterns into one [`MusicType`]
        let mut music_type = new_music_type(
            id_gen.next(),
            String::new(),
            bellframe::MusicType::new(patterns),
            common,
            false,
        );
        if !music_types.is_empty() {
            music_type.weights = AtRowPositions::splat(0.0);
            music_type.show_positions = AtRowPositions::FALSE;
        }
        music_types.push(music_type);
    }
    // Check that a non-empty pattern string always produces a non-empty set of music types
    if !pattern_strings.is_empty() {
        assert!(!music_types.is_empty());
    }
    Ok(music_types)
}

fn music_type_preset(
    preset: MusicPreset,
    common: &MusicCommon,
    id_gen: &mut IdGenerator<MusicTypeId>,
    stage: Stage,
) -> anyhow::Result<Vec<MusicType>> {
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
                anyhow::bail!("5678 combinations only make sense for Triples and Major");
            }
        },
        MusicPreset::NearMisses => (
            bellframe::MusicType::near_misses(stage),
            AtRowPositions::FRONT,
            "NMs",
        ),
        MusicPreset::Crus => {
            if stage < Stage::TRIPLES {
                anyhow::bail!("Can't have CRUs on less than 7 bells");
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
) -> MusicType {
    let optional_weights = AtRowPositions::<Option<f32>>::from(common.specified_weight);
    let num_specified_weights: usize = optional_weights.map(|v| v.is_some() as usize).total();
    // Create music types, etc.
    MusicType {
        id,
        inner: music_type.at_stroke(common.strokes.into()),
        weights: optional_weights.map(|x| x.unwrap_or(0.0)),
        show_positions: optional_weights.map(|x| x.is_some() && common.should_show()),
        show_total: (show_total || num_specified_weights == 1) && common.should_show(),
        count_range: common.count_range.into(),
        name: match &common.name {
            Some(name) => name.clone(),
            None => default_name.to_owned(),
        },
    }
}
