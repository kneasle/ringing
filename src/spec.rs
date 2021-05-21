use std::{
    iter::once,
    ops::{Range, RangeInclusive},
};

use itertools::Itertools;

use proj_core::{place_not::PnBlockParseError, Bell, Method, PnBlock, Row, Stage};

use crate::{
    engine::{CompRow, Table as TableTrait},
    single_meth::Table,
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
}

impl Spec {
    pub fn tenors_together_from_pn(
        stage: Stage,
        pn: &str,
        calls: Vec<CallSpec>,
        length: RangeInclusive<usize>,
    ) -> Result<Self, PnBlockParseError> {
        Ok(Self {
            method: Method::with_lead_end(String::new(), &PnBlock::parse(pn, stage)?),
            // Fix the treble and >=7th
            fixed_bells: once(Bell::TREBLE)
                .chain((6..stage.as_usize()).map(Bell::from_index))
                .collect_vec(),
            calls,
            length,
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
        self.to_table()
            .change_row_type::<R>()
            .compose(self.length.clone())
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
