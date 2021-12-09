//! Module to read the Central Council's XML format into a [`MethodLib`].  This is very unlikely to
//! be used outside of the CI workflow which keeps a copy of the CC library up-to-date.

#![cfg(feature = "cc_lib_gen")]

use crate::method::Class;

use super::*;
use minidom::Element;

const NAMESPACE: &'static str = "http://www.cccbr.org.uk/methods/schemas/2007/05/methods";

/// Parse the CCCBR's XML format into a [`MethodLib`] (removing a large amount of unnecessary
/// information in the process
pub fn parse_cc_lib(xml: &str) -> MethodLib {
    let root: Element = xml.parse().unwrap();

    // Get the file's date
    let date = root.attr("date").expect("Attr 'date' not found in root.");
    println!("File last modified on {}", date);

    MethodLib {
        method_map: read_methods(&root),
    }
}

/// The bits of the XML file that we care about are:
/// ```xml
/// <collection date="{{ date }}">
///     {{ foreach set of methods }}
///     <methodSet>
///         <properties>
///             <stage>{{ stage }}</stage>
///             <classification
///                 little="{{ is_little }}"
///                 differential="{{ is_differential }}"
///                 >{{ classification }}</classification>
///         </properties>
///
///         {{ foreach method }}
///         <method>
///             <title>{{ title }}</title>
///             <name>{{ name }}</name>
///             <notation>{{ place_notation }}</notation>
///         </method>
///         {{ endfor }}
///     </methodSet>
///     {{ endfor }}
/// </collection>
/// ```
fn read_methods(root: &Element) -> LibraryMap {
    let mut methods: LibraryMap = HashMap::new();

    // Iterate over all the `methodSet` elements
    for method_set in root.children().filter(|e| e.name() == "methodSet") {
        // Read the `properties` element
        let properties = method_set
            .get_child("properties", NAMESPACE)
            .expect("Couldn't find `properties` element");
        let (stage, full_class) = read_properties(properties);

        // The map from titles to `CompactMethod`s (which all share the same stage)
        let method_map = methods.entry(stage).or_insert_with(HashMap::new);

        // Read the methods
        for method in method_set.children().filter(|e| e.name() == "method") {
            // Read the XML for the method
            let title = method
                .get_child("title", NAMESPACE)
                .expect("Couldn't find `title` element")
                .text();
            let name = method
                .get_child("name", NAMESPACE)
                .expect("Couldn't find `name` element")
                .text();
            let place_notation = method
                .get_child("notation", NAMESPACE)
                .expect("Couldn't find `notation` element")
                .text();

            // Push the newly parsed method onto the map corresponding to the correct stage
            method_map.insert(
                title,
                CompactMethod {
                    name,
                    full_class,
                    place_notation,
                },
            );
        }
    }

    methods
}

/// Read the `<properties>` element
fn read_properties(properties: &Element) -> (Stage, FullClass) {
    let stage: Stage = properties
        .get_child("stage", NAMESPACE)
        .expect("Couldn't find `stage` element")
        .text()
        .parse::<usize>()
        .expect("Stage wasn't a valid number")
        .into();

    let classification_elem = properties
        .get_child("classification", NAMESPACE)
        .expect("Couldn't find `classification` element");
    let is_little = classification_elem.attr("little") == Some("true");
    let is_differential = classification_elem.attr("differential") == Some("true");
    let class = match classification_elem.text().as_str() {
        "" => Class::Principle,

        "Place" => Class::Place,
        "Bob" => Class::Bob,

        "Treble Bob" => Class::TrebleBob,
        "Delight" => Class::Delight,
        "Surprise" => Class::Surprise,

        "Treble Place" => Class::TreblePlace,
        "Alliance" => Class::Alliance,
        "Hybrid" => Class::Hybrid,

        x => panic!("Unknown classification {:?}", x),
    };

    // `is_jump` is always false, because the CCCBR doesn't contain jump methods yet
    let full_class = FullClass::new(false, is_little, is_differential, class);

    (stage, full_class)
}
