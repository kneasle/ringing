#![cfg(feature = "method_lib_serde")]

use std::collections::HashMap;

use itertools::Itertools;
use serde_crate::{Deserialize, Serialize};

use crate::{
    method::{generate_title, Class, FullClass},
    Stage,
};

use super::{CompactMethod, LibraryMap, MethodLib};

/// A version of `MethodLib` which can be serialized into a compact format
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(crate = "serde_crate")]
pub struct MethodLibSerde {
    groups: Vec<MethodGroup>,
}

impl From<&MethodLib> for MethodLibSerde {
    /// Converting from [`MethodLib`] -> `MethodLibSerde` (ready to be serialized)
    fn from(lib: &MethodLib) -> Self {
        let mut groups: HashMap<(Stage, FullClass), Vec<(&str, &CompactMethod)>> = HashMap::new();

        // Group the methods by their stage and class
        for (stage, methods) in &lib.method_map {
            for (title, method) in methods {
                groups
                    .entry((*stage, method.full_class))
                    .or_insert_with(Vec::new)
                    .push((title, method));
            }
        }

        Self {
            groups: groups
                .into_iter()
                .map(|((stage, full_class), methods)| MethodGroup {
                    stage: stage.as_usize(),
                    is_jump: full_class.is_jump(),
                    is_little: full_class.is_little(),
                    is_differential: full_class.is_differential(),
                    class_id: to_class_id(full_class.class()),
                    methods: methods
                        .into_iter()
                        .map(|(title, comp_method)| {
                            // Check if the title follows the standard construction.  If it does,
                            // we don't store the title explicitly, but if it isn't standard (e.g.
                            // for Grandsire) then we need to store it explicitly
                            let override_title =
                                if &generate_title(&comp_method.name, full_class, stage) == title {
                                    None
                                } else {
                                    Some(title.to_owned())
                                };

                            CompactMethodSerde {
                                title: override_title,
                                name: comp_method.name.to_owned(),
                                place_notation: comp_method.place_notation.to_owned(),
                            }
                        })
                        .collect_vec(),
                })
                .collect_vec(),
        }
    }
}

impl From<MethodLibSerde> for MethodLib {
    fn from(m: MethodLibSerde) -> MethodLib {
        let mut unpacked_method_map: LibraryMap = HashMap::with_capacity(25);

        for group in m.groups {
            // Unpack values from the group
            let stage = Stage::from(group.stage);
            let full_class = FullClass::new(
                group.is_jump,
                group.is_little,
                group.is_differential,
                from_class_id(group.class_id).expect("Unexpected type ID found"),
            );

            // Get the group of methods that correspond to this group's stage
            let method_map = unpacked_method_map
                .entry(stage)
                .or_insert_with(HashMap::new);

            // Go through each method and unpack it into the `method_map`
            for m in group.methods {
                // Split the `CompactMethodSerde` struct into its individual fields (which can then
                // be consumed separately without angering the borrow checker)
                let CompactMethodSerde {
                    title,
                    name,
                    place_notation,
                } = m;

                method_map.insert(
                    title.unwrap_or_else(|| generate_title(&name, full_class, stage)),
                    CompactMethod {
                        name,
                        full_class,
                        place_notation,
                    },
                );
            }
        }

        MethodLib {
            method_map: unpacked_method_map,
        }
    }
}

/// A group of methods with shared properties
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(crate = "serde_crate")]
struct MethodGroup {
    #[serde(rename = "s")]
    stage: usize,

    // Classification
    #[serde(default, skip_serializing_if = "is_false", rename = "j")]
    is_jump: bool,
    #[serde(default, skip_serializing_if = "is_false", rename = "l")]
    is_little: bool,
    #[serde(default, skip_serializing_if = "is_false", rename = "d")]
    is_differential: bool,
    #[serde(rename = "c")]
    class_id: u8,

    #[serde(rename = "m")]
    methods: Vec<CompactMethodSerde>,
}

/// A version of `MethodLib` which can be serialized into a compact format
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(crate = "serde_crate")]
struct CompactMethodSerde {
    #[serde(skip_serializing_if = "Option::is_none", rename = "t")]
    title: Option<String>,
    #[serde(rename = "n")]
    name: String,
    #[serde(rename = "p")]
    place_notation: String,
}

/// Serialization helper required to remove tons of `is_*:false` from the saved JSON
fn is_false(b: &bool) -> bool {
    *b == false
}

/// Converts a [`Class`] into a number, which can then be recovered with `from_class_id`.  This
/// will almost certainly get removed by the compiler, because these numbers are used as
/// [`Class`]'s in-memory representation.
fn to_class_id(class: Class) -> u8 {
    match class {
        Class::Principle => 0,

        Class::Place => 1,
        Class::Bob => 2,

        Class::TrebleBob => 3,
        Class::Delight => 4,
        Class::Surprise => 5,

        Class::TreblePlace => 6,
        Class::Alliance => 7,
        Class::Hybrid => 8,
    }
}

fn from_class_id(v: u8) -> Option<Class> {
    Some(match v {
        0 => Class::Principle,

        1 => Class::Place,
        2 => Class::Bob,

        3 => Class::TrebleBob,
        4 => Class::Delight,
        5 => Class::Surprise,

        6 => Class::TreblePlace,
        7 => Class::Alliance,
        8 => Class::Hybrid,

        _ => panic!("Invalid Class ID {}", v),
    })
}
