use monument::spec::Layout;
use monument::Comp;
use monument::Score;
use serde::Deserialize;

/// The data required to turn this input file into a test case for the test/benchmark harness
#[derive(Debug, Clone, Deserialize)]
pub struct TestData {
    /// Number of seconds that this test is expected to run for
    pub runtime_estimate: f32,
    /// A list of the expected composition results
    pub results: Option<Vec<CompResultI64>>,
}

/// An easily testable form of a [`Comp`] which can be easily compared
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct CompResultI64 {
    call_string: String,
    length: usize,
    score: i64,
    ranking_score: i64,
}

/// An easily testable form of a [`Comp`] which can be easily compared
#[derive(Debug, Clone, PartialEq)]
pub struct CompResult {
    pub call_string: String,
    pub length: usize,
    pub score: Score,
    pub ranking_score: Score,
}

impl CompResult {
    pub fn from_comp(comp: &Comp, layout: &Layout) -> Self {
        Self {
            call_string: comp.call_string(layout),
            length: comp.length,
            score: comp.score,
            ranking_score: comp.ranking_score,
        }
    }
}

impl From<CompResultI64> for CompResult {
    fn from(v: CompResultI64) -> Self {
        CompResult {
            call_string: v.call_string,
            length: v.length,
            score: Score::from_numerator(v.score),
            ranking_score: Score::from_numerator(v.ranking_score),
        }
    }
}
