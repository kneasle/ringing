use monument::{Comp, Spec};
use serde::Deserialize;

/// The data required to turn this input file into a test case for the test/benchmark harness
#[derive(Debug, Clone, Deserialize)]
pub struct TestData {
    /// Number of seconds that this test is expected to run for
    pub runtime_estimate: f32,
    /// A list of the expected composition results
    pub results: Option<Vec<CompResult>>,
}

/// An easily testable form of a [`Comp`] which can be easily compared
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct CompResult {
    pub call_string: String,
    pub length: usize,
    pub score: f32,
    pub ranking_score: f32,
}

impl CompResult {
    pub fn from_comp(comp: Comp, spec: &Spec) -> Self {
        Self {
            call_string: comp.call_string(spec),
            length: comp.length,
            score: comp.score,
            ranking_score: comp.ranking_score,
        }
    }
}
