#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

mod engine;
mod music;
mod set;
mod single_meth;

use engine::Table;
use proj_core::SimdRow;
use proj_core::{Row, Stage};

use crate::single_meth::near_calls;

mod tables {
    #![allow(dead_code)]
    use crate::single_meth::{far_calls, Table as SingleMethTable};

    use super::*;

    pub fn bristol_s8() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::MAJOR,
            "x58x14.58x58.36.14x14.58x14x18,18",
            "178",
            &near_calls(Stage::MAJOR),
            "LIBMFHVW",
        )
        .unwrap()
    }

    pub fn cambs_s8() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::MAJOR,
            "-38-14-1258-36-14-58-16-78,12",
            "178",
            &near_calls(Stage::MAJOR)[..1],
            "LBTFVMWH",
        )
        .unwrap()
    }

    pub fn cooktown_d8() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::MAJOR,
            "-38-14-1256-18-12-58-16-78,12",
            "178",
            &near_calls(Stage::MAJOR),
            "LBTFVMWH",
        )
        .unwrap()
    }

    pub fn pb_8() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::MAJOR,
            "-18-18-18-18,12",
            "178",
            &near_calls(Stage::MAJOR),
            "LBTFVMWH",
        )
        .unwrap()
    }

    pub fn yorkshire_s8() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::MAJOR,
            "x38x14x58x16x12x38x14x78,12",
            "178",
            &near_calls(Stage::MAJOR),
            "LBTFVMWH",
        )
        .unwrap()
    }

    pub fn yorkshire_s10() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::ROYAL,
            "x30x14x50x16x1270x38x14x50x16x90,12",
            // Fix the treble and all the tenors
            "17890",
            &near_calls(Stage::ROYAL),
            "LBTFVXSMWH",
        )
        .unwrap()
    }

    pub fn bristol_s10() -> SingleMethTable<Row> {
        SingleMethTable::from_place_not(
            Stage::ROYAL,
            "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
            "17890",
            &far_calls(Stage::ROYAL),
            "LIO?VM?HVW",
        )
        .unwrap()
    }
}

fn main() {
    let table = tables::yorkshire_s8();
    let len = 1250..=1300;
    // let len = 5000..=5184;

    // table.print_falseness();

    if SimdRow::are_cpu_features_enabled() {
        println!("Can use SIMD!");
        single_meth::Table::compose(&table.change_row_type::<SimdRow>(), len);
    } else {
        println!("Can't use SIMD.");
        single_meth::Table::compose(&table, len);
    }
}
