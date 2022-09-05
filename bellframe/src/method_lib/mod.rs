use std::{cmp::Ordering, collections::HashMap};

use edit_distance::edit_distance;
use itertools::Itertools;
use shortlist::Shortlist;

use crate::{method::class::FullClass, place_not::PnBlockParseError, Method, PnBlock, Stage};

mod lib_serde;
pub(crate) mod parse_cc_lib;

/// Convenient type alias for the nested [`HashMap`] type used to store methods in the library
type LibraryMap = HashMap<Stage, HashMap<String, CompactMethod>>;

/// A library of [`Method`]s, usually that provided by the Central Council.
#[derive(Debug, Clone)]
pub struct MethodLib {
    method_map: LibraryMap,
}

impl MethodLib {
    /// Searches this `MethodLib` for a [`Method`] with a title, returning the [`Method`] if found
    /// and `None` otherwise.  The failure state for this function is not very useful - if you want
    /// to provide useful suggestions for your user, then consider using
    /// [`MethodLib::get_by_title_with_suggestions`].
    pub fn get_by_title(&self, title: &str) -> Result<Method, QueryError<()>> {
        match self.get_by_title_option(&title.to_lowercase()) {
            Some(Ok(method)) => Ok(method),
            Some(Err((pn, error))) => Err(QueryError::PnParseErr { pn, error }),
            None => Err(QueryError::NotFound(())),
        }
    }

    /// A version of `get_by_title` which expresses its return type as an [`Option`] rather than a
    /// [`Result`].  Until the [`Try`](std::ops::Try) trait is stabilised, I think this is a good
    /// balance - the user of bellframe gets an ergonomic result type and bellframe gets to use the
    /// `?` operator.
    fn get_by_title_option(
        &self,
        lower_case_title: &str,
    ) -> Option<Result<Method, (String, PnBlockParseError)>> {
        // Firstly, we extract the stage name from the title.  If the stage can't be extracted,
        // then the title must be invalid and therefore can't correspond to a method.
        //
        // This unwrap is safe, because `rsplit` always yields at least one value (even if that
        // value is just the empty string).
        let stage_name = lower_case_title.rsplit(' ').next().unwrap();
        let stage = Stage::from_lower_case_name(stage_name)?;

        // Once we know the stage, we can directly look up the method
        let method = self
            .method_map
            .get(&stage)?
            .get(lower_case_title)?
            .to_method(stage);
        Some(method)
    }

    /// Searches this `MethodLib` for a [`Method`] with a title.  If this title is found in the
    /// library, then `Ok(Method)` is returned.  Otherwise, a list of similar titles are returned,
    /// along with their [Levenstein edit
    /// distance](https://en.wikipedia.org/wiki/Levenshtein_distance) from the requested title.
    /// These are sorted with the closest results first
    pub fn get_by_title_with_suggestions(
        &self,
        title: &str,
        num_suggestions: usize,
    ) -> Result<Method, QueryError<Vec<(String, usize)>>> {
        let lower_case_title = title.to_lowercase();
        self.get_by_title(&lower_case_title).map_err(|e| {
            e.map_not_found(|()| self.generate_suggestions(&lower_case_title, num_suggestions))
        })
    }

    /// Generate a list of method title suggestions based on the Levenstein edit from a given title
    fn generate_suggestions(
        &self,
        lower_case_title: &str,
        num_suggestions: usize,
    ) -> Vec<(String, usize)> {
        /// A new-type over the suggestions, which is ordered by the edit distance
        #[derive(Debug, Clone, Copy)]
        // Forcing `repr` transparent will make sure that the memory layout is identical to `(&str,
        // usize)` which will usually let LLVM optimise away the allocation in
        // `Shortlist::into_sorted_vec`
        #[repr(transparent)]
        struct Suggestion<'s>((&'s str, usize));

        impl<'s> Suggestion<'s> {
            fn new(
                actual_title: &str,
                suggestion_title_lower: &str,
                suggestion_title: &'s str,
            ) -> Self {
                Suggestion((
                    suggestion_title,
                    edit_distance(actual_title, suggestion_title_lower),
                ))
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
        for methods in self.method_map.values() {
            suggestion_shortlist.append(methods.iter().map(|(stored_title, method)| {
                Suggestion::new(lower_case_title, stored_title, &method.title)
            }));
        }

        let mut best_suggestions = suggestion_shortlist.into_sorted_vec();
        // Reverse the sorting so that the best suggestions are first
        best_suggestions.reverse();
        best_suggestions
            .into_iter()
            .map(|Suggestion((title, edit_distance))| (title.to_owned(), edit_distance))
            .collect_vec()
    }

    // This method is only used by the method classification test suite
    #[cfg(test)]
    pub(crate) fn all_pns_and_classes(&self) -> Vec<(&str, PnBlock, FullClass)> {
        let mut v = Vec::new();
        for (stage, meths) in &self.method_map {
            for m in meths.values() {
                v.push((
                    m.title.as_str(),
                    PnBlock::parse(&m.place_notation, *stage).unwrap(),
                    m.full_class,
                ));
            }
        }
        v
    }
}

/// (De)serialising libraries to and from JSON
#[cfg(feature = "method_lib_serde")]
impl MethodLib {
    /// Serialize this `MethodLib` to a compact JSON format
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(&lib_serde::MethodLibSerde::from(self))
    }

    /// Serialize this `MethodLib` from the compact JSON format generated by
    /// [`MethodLib::to_json`].
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str::<lib_serde::MethodLibSerde>(json).map(Self::from)
    }
}

#[cfg(feature = "cc_lib")]
use std::path::PathBuf;

/// Fetch the CCCBR method library, or load it from a cached file
#[cfg(feature = "cc_lib")]
impl MethodLib {
    /// Create a `MethodLib` containing the latest version of the Central Council method library
    pub fn cc_lib() -> Option<MethodLib> {
        // Load the CCCBR library from the cache, if it exists
        if let Some(lib_from_cache) = Self::load_cc_lib_from_cache() {
            return Some(lib_from_cache);
        }
        // If the cached version couldn't be loaded, then fetch it from the `kneasle/cc-method-lib`
        // repository.  This also saves it to a file
        Self::fetch_cc_lib()
    }

    /// Try to load a cached copy of the CC method library, returning `None` if it couldn't be
    /// loaded.
    fn load_cc_lib_from_cache() -> Option<MethodLib> {
        let cache_path = Self::cache_file_path()?;
        let json = std::fs::read_to_string(cache_path).ok()?;
        Self::from_json(&json).ok()
    }

    /// Fetch the CC library from the web
    fn fetch_cc_lib() -> Option<MethodLib> {
        let response = reqwest::blocking::get(
            "https://raw.githubusercontent.com/kneasle/cc-method-lib/master/cccbr-methods.json",
        )
        .ok()?;
        let json = response.text().ok()?;
        let lib = Self::from_json(&json).ok()?;
        // Save the JSON **after** creating the library, so we don't cache an invalid method
        // library
        if let Some(path) = Self::cache_file_path() {
            let _ = std::fs::write(path, &json);
        }
        Some(lib)
    }

    /// Returns the expected location of the CC library's cache file
    fn cache_file_path() -> Option<PathBuf> {
        // Look for the cache directory in `$CACHE_DIR/cccbr-methods.json`
        let mut path = dirs::cache_dir()?;
        path.push("cccbr-methods.json");
        Some(path)
    }
}

/// A light-weight version of [`Method`] that can be easily stored in a method library.  This is
/// not intended to be used outside of [`MethodLib`]
#[derive(Debug, Clone)]
struct CompactMethod {
    name: String,
    title: String,
    full_class: FullClass,
    place_notation: String,
}

impl CompactMethod {
    fn to_method(&self, stage: Stage) -> Result<Method, (String, PnBlockParseError)> {
        Ok(Method::new(
            self.title.to_owned(),
            self.name.to_owned(),
            self.full_class,
            PnBlock::parse(&self.place_notation, stage)
                .map_err(|e| (self.place_notation.clone(), e))?
                .to_block_from_rounds(),
        ))
    }
}

#[derive(Debug, Clone)]
pub enum QueryError<T> {
    PnParseErr {
        pn: String,
        error: PnBlockParseError,
    },
    NotFound(T),
}

impl<T> QueryError<T> {
    /// Unwraps the `PnParseErr` part of a `QueryError`, expecting the Method's place notation to
    /// have parsed correctly and panicking if it didn't
    pub fn unwrap_parse_err(self) -> Result<Method, T> {
        match self {
            Self::PnParseErr { pn, error } => panic!("Error parsing {:?}: {}", pn, error),
            Self::NotFound(v) => Err(v),
        }
    }

    /// Passes the value contained in the `NotFound` part of `self` through an arbitrary function.
    pub fn map_not_found<U>(self, f: impl FnOnce(T) -> U) -> QueryError<U> {
        match self {
            QueryError::PnParseErr { pn, error } => QueryError::PnParseErr { pn, error },
            QueryError::NotFound(v) => QueryError::NotFound(f(v)),
        }
    }
}
