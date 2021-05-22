#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

mod engine;
mod music;
mod set;
mod single_meth;
mod spec;

use engine::Table;
use proj_core::{Row, SimdRow, Stage};
use spec::Spec;

mod tables {
    #![allow(dead_code)]

    use crate::spec::CallSpec;

    use super::*;

    pub fn bristol_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "x58x14.58x58.36.14x14.58x14x18,18",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn cambs_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (Stage::MAJOR, "-38-14-1258-36-14-58-16-78,12", {
            let mut calls = CallSpec::near(Stage::MAJOR);
            calls.pop();
            calls
        })
    }

    pub fn cooktown_d8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "-38-14-1256-18-12-58-16-78,12",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn pb_8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "-18-18-18-18,12",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn yorkshire_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "x38x14x58x16x12x38x14x78,12",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn yorkshire_s10() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::ROYAL,
            "x30x14x50x16x1270x38x14x50x16x90,12",
            CallSpec::near(Stage::ROYAL),
        )
    }

    pub fn bristol_s10() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::ROYAL,
            "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
            CallSpec::far(Stage::ROYAL),
        )
    }
}

fn main() {
    let (stage, pn, calls) = tables::yorkshire_s8();

    let spec = Spec::tenors_together_from_pn(stage, pn, calls, spec::QP, 10).unwrap();

    if SimdRow::are_cpu_features_enabled() {
        println!("Using SimdRow!");
        spec.compose::<SimdRow>();
    } else {
        println!("Can't use SimdRow, defaulting to standard Row.");
        spec.compose::<Row>();
    }
}
