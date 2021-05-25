use std::{
    iter::repeat,
    ops::{Range, RangeInclusive},
    time::{Duration, Instant},
};

use itertools::Itertools;
use proj_core::{place_not::PnBlockParseError, Bell, Method, PnBlock, Row, Stage};
use separator::Separatable;

use crate::{
    engine::{CompRow, Results, Table as TableTrait},
    single_meth::{tenors_together_fixed_bells, Table},
};

pub const QP: RangeInclusive<usize> = 1250..=1300;
pub const HALF_PEAL: RangeInclusive<usize> = 2500..=2600;
pub const PEAL: RangeInclusive<usize> = 5000..=5200;

pub struct Outcome<R: CompRow> {
    pub results: Results<R, Table<R>>,
    pub table: Table<R>,
    pub table_build_time: Duration,
    pub composing_time: Duration,
}

impl<R: CompRow> Outcome<R> {
    pub fn pretty_print(&self) {
        self.pretty_print_comps();
        self.pretty_print_perf();
    }

    pub fn pretty_print_perf(&self) {
        println!(
            "{:>15} nodes considered ({:>10}/s)",
            self.results.nodes_expanded.separated_string(),
            (self.results.nodes_expanded as f64 / self.composing_time.as_secs_f64())
                .separated_string()
        );
        println!(
            "{:>15} comps found      ({:>10}/s)",
            self.results.comps_found.separated_string(),
            (self.results.comps_found as f64 / self.composing_time.as_secs_f64())
                .separated_string()
        );
    }

    pub fn pretty_print_comps(&self) {
        let mut comps = self.results.comps.iter().collect_vec();
        comps.sort();
        for comp in comps {
            println!("{}", comp.to_string(&self.table));
        }
    }
}

/// The full specification for a single method composition.  This is what will be parsed from an
/// input TOML file.
#[derive(Debug, Clone)]
pub struct Spec {
    method: Method,
    length: RangeInclusive<usize>,
    fixed_bells: Vec<Bell>,
    calls: Vec<CallSpec>,
    shortlist_size: usize,
}

impl Spec {
    pub fn tenors_together_from_pn(
        stage: Stage,
        pn: &str,
        calls: Vec<CallSpec>,
        length: RangeInclusive<usize>,
        shortlist_size: usize,
    ) -> Result<Self, PnBlockParseError> {
        Ok(Self {
            method: Method::with_lead_end(String::new(), &PnBlock::parse(pn, stage)?),
            // Fix the treble and >=7th
            fixed_bells: tenors_together_fixed_bells(stage),
            calls,
            length,
            shortlist_size,
        })
    }

    pub fn to_table(&self) -> Table<Row> {
        Table::from_method(
            &self.method,
            self.fixed_bells.clone(),
            &self.calls,
            // TODO: Compute this from the method
            plain_calling_pos(self.method.stage()),
        )
    }

    pub fn len_range(&self) -> Range<usize> {
        *self.length.start()..*self.length.end() + 1
    }

    /// Use this to generate a composition
    pub fn compose<R: CompRow + From<Row>>(&self) -> Outcome<R> {
        // Generate the table
        print!("Generating table... ");
        let table_start_time = Instant::now();
        let table = self.to_table().change_row_type::<R>();
        let table_build_time = Instant::now() - table_start_time;
        println!("DONE in {:?}", table_build_time);

        // Run the tree search
        print!("Running tree search... ");
        let comp_start_time = Instant::now();
        let results = table.compose(self.length.clone(), self.shortlist_size);
        let composing_time = Instant::now() - comp_start_time;
        println!("DONE in {:?}", composing_time);

        Outcome {
            results,
            table,
            table_build_time,
            composing_time,
        }
    }
}

fn plain_calling_pos(stage: Stage) -> &'static str {
    match stage {
        Stage::MAJOR => "LBFIVMWH",
        Stage::ROYAL => "LBFIVXSMWH",
        Stage::MAXIMUS => "LBFIVXSENMWH",
        _ => unimplemented!(),
    }
}

/// The specification of a single call type used in a composition.
#[derive(Debug, Clone)]
pub struct CallSpec {
    pub place_not_str: String,
    pub symbol: char,
    pub calling_positions: Vec<char>,
}

impl CallSpec {
    pub fn new(place_not_str: String, symbol: char, calling_positions: Vec<char>) -> Self {
        Self {
            place_not_str,
            symbol,
            calling_positions,
        }
    }

    fn from_borrowed(pn: &str, symbol: char, calling_positions: &str) -> Self {
        CallSpec {
            place_not_str: pn.to_owned(),
            symbol,
            calling_positions: calling_positions.chars().collect_vec(),
        }
    }

    /// 4ths place calls for any [`Stage`]
    pub fn near(stage: Stage) -> Vec<Self> {
        vec![
            Self::new("14".to_owned(), '-', calling_positions_near_bob(stage)),
            Self::new("1234".to_owned(), 's', calling_positions_near_single(stage)),
        ]
    }

    /// (n-2)nds place calls for any [`Stage`]
    pub fn far(stage: Stage) -> Vec<Self> {
        let (bob_pos, single_pos, bob, single) = match stage {
            Stage::MAJOR => ("LIOFVMWH", "LIOFVMWH", "16", "1678"),
            Stage::ROYAL => ("LIOFVXSMWH", "LIOFVXSMWH", "18", "1890"),
            Stage::MAXIMUS => ("LIOFVXSENMWH", "LIOFVXSENMWH", "10", "10ET"),
            _ => unimplemented!(),
        };

        vec![
            Self::from_borrowed(bob, '-', bob_pos),
            Self::from_borrowed(single, 's', single_pos),
        ]
    }
}

/// Generates calling positions for a `14` bob on a given [`Stage`]
pub fn calling_positions_near_bob(stage: Stage) -> Vec<char> {
    add_mwh(stage, "LIBFVXSENT")
}

/// Generates calling positions for a `1234` single on a given [`Stage`]
pub fn calling_positions_near_single(stage: Stage) -> Vec<char> {
    add_mwh(stage, "LBTFVXSENT")
}

fn add_mwh(stage: Stage, base_str: &str) -> Vec<char> {
    let num_bells = stage.as_usize();
    // Fill in all the calling positions that aren't MWH (extending with '?'s where there aren't
    // well defined names)
    let mut all_calls = base_str
        .chars()
        .chain(repeat('?'))
        .take(num_bells)
        .collect_vec();
    // W and M are the wrong way round for odd-bell methods
    all_calls[num_bells - 3] = if stage.is_even() { 'M' } else { 'W' };
    all_calls[num_bells - 2] = if stage.is_even() { 'W' } else { 'M' };
    // H is always at the end
    all_calls[num_bells - 1] = 'H';

    all_calls
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calling_pos_14_bob() {
        for &(stage, s) in &[
            (Stage::TRIPLES, "LIBFWMH"),
            (Stage::MAJOR, "LIBFVMWH"),
            (Stage::ROYAL, "LIBFVXSMWH"),
            (Stage::MAXIMUS, "LIBFVXSENMWH"),
            (Stage::FOURTEEN, "LIBFVXSENT?MWH"),
            (Stage::SIXTEEN, "LIBFVXSENT???MWH"),
        ] {
            assert_eq!(calling_positions_near_bob(stage).iter().join(""), s);
        }
    }

    #[test]
    fn calling_pos_1234_single() {
        for &(stage, s) in &[
            (Stage::TRIPLES, "LBTFWMH"),
            (Stage::MAJOR, "LBTFVMWH"),
            (Stage::ROYAL, "LBTFVXSMWH"),
            (Stage::MAXIMUS, "LBTFVXSENMWH"),
            (Stage::FOURTEEN, "LBTFVXSENT?MWH"),
            (Stage::SIXTEEN, "LBTFVXSENT???MWH"),
        ] {
            assert_eq!(calling_positions_near_single(stage).iter().join(""), s);
        }
    }
}
