use std::{
    iter::once,
    ops::{Range, RangeInclusive},
    time::Instant,
};

use itertools::Itertools;

use proj_core::{place_not::PnBlockParseError, Bell, Method, PnBlock, Row, Stage};

use crate::{
    engine::{CompRow, Table as TableTrait},
    single_meth::{tenors_together_fixed_bells, Table},
};

pub const QP: RangeInclusive<usize> = 1250..=1300;
pub const HALF_PEAL: RangeInclusive<usize> = 2500..=2600;
pub const PEAL: RangeInclusive<usize> = 5000..=5200;

/// The full specification for a single method composition.  This is what will be parsed from the input
/// TOML file.
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
    pub fn compose<R: CompRow + From<Row>>(&self) {
        // Generate the table
        print!("Generating table... ");
        let table_start_time = Instant::now();
        let table = self.to_table().change_row_type::<R>();
        println!("DONE in {:?}", Instant::now() - table_start_time);

        // Run the tree search
        print!("Running tree search... ");
        let comp_start_time = Instant::now();
        let results = table.compose(self.length.clone(), self.shortlist_size);
        let composing_time = Instant::now() - comp_start_time;
        println!("DONE in {:?}", composing_time);

        // Print the comps
        let mut comps = results.comps.iter().collect_vec();
        comps.sort();
        for comp in comps {
            println!("{}", comp.to_string(&table));
        }

        // Print the stats
        println!("---###---###---                   #---###---");
        println!(
            "{:>15} nodes considered ({:>10.0}/s)",
            results.nodes_expanded,
            results.nodes_expanded as f64 / composing_time.as_secs_f64()
        );
        println!(
            "{:>15} comps found      ({:>10.0}/s)",
            results.comps_found,
            results.comps_found as f64 / composing_time.as_secs_f64()
        );
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
        let (bob_pos, single_pos) = match stage {
            Stage::MAJOR => ("LIBFVMWH", "LBTFVMWH"),
            Stage::ROYAL => ("LIBFVXSMWH", "LBTFVXSMWH"),
            Stage::MAXIMUS => ("LIBFVXSENMWH", "LBTFVXSENMWH"),
            _ => unimplemented!(),
        };

        vec![
            Self::from_borrowed("14", '-', bob_pos),
            Self::from_borrowed("1234", 's', single_pos),
            // Self::from_borrowed("16", 'x', bob_pos),
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
