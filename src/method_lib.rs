#![cfg(feature = "cc_lib")]

use std::{cmp::Ordering, collections::HashMap};

use edit_distance::edit_distance;
use itertools::Itertools;
use shortlist::Shortlist;

use crate::{method::FullClass, place_not::PnBlockParseError, Method, PnBlock, Stage};

type LibraryMap = HashMap<Stage, HashMap<String, CompactMethod>>;

/// A library of [`Methods`], usually that provided by the Central Council.
#[derive(Debug, Clone)]
pub struct MethodLib {
    methods: LibraryMap,
}

impl MethodLib {
    /// Searches this `MethodLib` for a [`Method`] with a title, returning the [`Method`] if found
    /// and `None` otherwise.  The failure state for this function is not very useful - if you want
    /// to provide useful suggestions for your user, then consider using
    /// [`MethodLib::get_by_title_with_suggestions`].
    pub fn get_by_title<'s>(&'s self, title: &str) -> QueryResult<'s, ()> {
        match self._get_by_title_option(title) {
            Some(Ok(method)) => QueryResult::Success(method),
            Some(Err((pn, error))) => QueryResult::PnParseErr { pn, error },
            None => QueryResult::NotFound(()),
        }
    }

    /// A version of `get_by_title` which expresses its return type as an [`Option`] rather than a
    /// [`QueryResult`].  Until the [`Try`](std::ops::Try) trait is stabilised, I think this is a
    /// good balance - the user of bellframe gets an ergonomic result type and the developers of
    /// bellframe get to use the `?` operator.
    fn _get_by_title_option<'s>(
        &'s self,
        title: &str,
    ) -> Option<Result<Method, (&'s str, PnBlockParseError)>> {
        // Firstly, we extract the stage name from the title.  If the stage can't be extracted,
        // then the title must be invalid and therefore can't correspond to a method.
        //
        // This unwrap is safe, because `rsplit` always yields at least one value (even if that
        // value is just the empty string).
        let stage_name = title.rsplit(' ').next().unwrap().to_lowercase();
        let stage = Stage::from_lower_case_name(&stage_name)?;

        // Once we know the stage, we can directly look up the method
        let method = self
            .methods
            .get(&stage)?
            .get(title)?
            .to_method(stage, title.to_owned());
        Some(method)
    }

    /// Searches this `MethodLib` for a [`Method`] with a title.  If this title is found in the
    /// library, then `Ok(Method)` is returned.  Otherwise, a list of similar titles are returned,
    /// along with their [Levenstein edit
    /// distance](https://en.wikipedia.org/wiki/Levenshtein_distance) from the requested title.
    /// These are sorted with the closest results first
    pub fn get_by_title_with_suggestions<'s>(
        &'s self,
        title: &str,
        num_suggestions: usize,
    ) -> QueryResult<Vec<(&'s str, usize)>> {
        self.get_by_title(title)
            .map_not_found(|()| self.generate_suggestions(title, num_suggestions))
    }

    /// Generate a list of method title suggestions based on the Levenstein edit from a given title
    fn generate_suggestions<'lib>(
        &'lib self,
        title: &str,
        num_suggestions: usize,
    ) -> Vec<(&'lib str, usize)> {
        /// A new-type over the suggestions, which is ordered by the edit distance
        #[derive(Debug, Clone, Copy)]
        // Forcing `repr` transparent will make sure that the memory layout is identical to `(&str,
        // usize)` which will usually let LLVM optimise away the allocation in
        // `Shortlist::into_sorted_vec`
        #[repr(transparent)]
        struct Suggestion<'s>((&'s str, usize));

        impl<'s> Suggestion<'s> {
            fn new(actual: &str, suggestion: &'s str) -> Self {
                Suggestion((suggestion, edit_distance(actual, suggestion)))
            }
        }

        impl<'s> PartialOrd for Suggestion<'s> {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<'s> Ord for Suggestion<'s> {
            fn cmp(&self, other: &Self) -> Ordering {
                // Make sure to sort them in reverse order, because the best suggestions have the
                // smallest edit distance
                self.0 .1.cmp(&other.0 .1).reverse()
            }
        }

        impl<'s> PartialEq for Suggestion<'s> {
            fn eq(&self, other: &Self) -> bool {
                self.0 .1 == other.0 .1
            }
        }

        impl<'s> Eq for Suggestion<'s> {}

        // Test each method as a suggestion, pushing the suggestions into a shortlist
        let mut suggestion_shortlist = Shortlist::new(num_suggestions);
        for methods in self.methods.values() {
            suggestion_shortlist.append(
                methods
                    .keys()
                    .map(|stored_title| Suggestion::new(title, stored_title)),
            );
        }

        let mut best_suggestions = suggestion_shortlist.into_sorted_vec();
        // Reverse the sorting so that the best suggestions are first
        best_suggestions.reverse();
        best_suggestions
            .into_iter()
            .map(|Suggestion(vs)| vs)
            .collect_vec()
    }
}

/// A light-weight version of [`Method`] that can be easily stored in a method library.  This is
/// not intended to be used outside of [`MethodLib`]
#[derive(Debug, Clone)]
struct CompactMethod {
    name: String,
    class: FullClass,
    place_notation: String,
}

impl CompactMethod {
    fn to_method(&self, stage: Stage, title: String) -> Result<Method, (&str, PnBlockParseError)> {
        Ok(Method::new(
            title,
            self.name.to_owned(),
            self.class,
            PnBlock::parse(&self.place_notation, stage)
                .map_err(|e| (self.place_notation.as_str(), e))?
                .to_block(),
        ))
    }
}

#[derive(Debug, Clone)]
#[must_use]
pub enum QueryResult<'lib, T> {
    Success(Method),
    PnParseErr {
        pn: &'lib str,
        error: PnBlockParseError,
    },
    NotFound(T),
}

impl<'lib, T> QueryResult<'lib, T> {
    /// Converts a `QueryResult` directly into a [`Method`], panicking if `self` is anything other
    /// than [`QueryResult::Success`]
    pub fn unwrap(self) -> Method {
        match self {
            Self::Success(method) => method,
            Self::PnParseErr { pn, error } => panic!("Error parsing {:?}: {}", pn, error),
            Self::NotFound(_) => panic!("Unwrap called on a `QueryResult::NotFound`"),
        }
    }

    /// Unwraps the `PnParseErr` part of a [`QueryResult`], expecting the Method's place notation
    /// to have parsed correctly and panicking if it didn't
    pub fn unwrap_parse_err(self) -> Result<Method, T> {
        match self {
            Self::Success(method) => Ok(method),
            Self::PnParseErr { pn, error } => panic!("Error parsing {:?}: {}", pn, error),
            Self::NotFound(v) => Err(v),
        }
    }

    /// Passes the value contained in the `NotFound` part of `self` through an arbitrary function.
    pub fn map_not_found<U>(self, f: impl FnOnce(T) -> U) -> QueryResult<'lib, U> {
        match self {
            QueryResult::Success(method) => QueryResult::Success(method),
            QueryResult::PnParseErr { pn, error } => QueryResult::PnParseErr { pn, error },
            QueryResult::NotFound(v) => QueryResult::NotFound(f(v)),
        }
    }
}

/// Module to read the Central Council's XML format into a [`MethodLib`].  This is very unlikely to
/// be used outside of the CI workflow which keeps a copy of the CC library up-to-date.
#[cfg(feature = "cc_lib_gen")]
pub(crate) mod parse_cc_lib {
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
            methods: read_methods(&root),
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
                        class: full_class,
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
}
