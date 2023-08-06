use std::path::Path;

use bellframe::Stroke;
use monument::parameters::OptionalRangeInclusive;
use serde::Deserialize;

/// A version of [`OptionalRangeInclusive`] which allows for convenient deserialisation from a
/// single number (e.g. `count = 5` is equivalent to `count = { min = 5, max = 5 }`)
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(untagged, deny_unknown_fields)]
pub enum OptRangeInclusive {
    SingleNumber(usize),
    Range {
        min: Option<usize>,
        max: Option<usize>,
    },
}

impl Default for OptRangeInclusive {
    fn default() -> Self {
        Self::Range {
            min: None,
            max: None,
        }
    }
}

impl From<OptRangeInclusive> for OptionalRangeInclusive {
    fn from(r: OptRangeInclusive) -> Self {
        let (min, max) = match r {
            OptRangeInclusive::SingleNumber(n) => (Some(n), Some(n)),
            OptRangeInclusive::Range { min, max } => (min, max),
        };
        OptionalRangeInclusive { min, max }
    }
}

/// Attempt to read a file as a [`String`], returning a helpful error message on failure
pub fn read_file_to_string(path: &Path) -> anyhow::Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| anyhow::Error::msg(format!("Can't open {:?}: {}", path, e)))
}

/// Attempt to read a file as a [`String`], returning a helpful error message on failure
pub fn parse_toml<'de, T: Deserialize<'de>>(s: &'de str) -> anyhow::Result<T> {
    toml::from_str(s)
        .map_err(|e| anyhow::Error::msg(format!("Error parsing composition file: {}", e)))
}

pub fn get_one() -> f32 {
    1.0
}

pub fn get_true() -> bool {
    true
}

pub fn handstroke() -> Stroke {
    Stroke::Hand
}
