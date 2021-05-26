#![cfg(any(target_arch = "x86", target_arch = "x86_64"))]

use comp_engine::spec::Spec;
use itertools::Itertools;
use proj_core::{Row, SimdRow, Stage};

mod methods {
    #![allow(dead_code)]

    use comp_engine::spec::CallSpec;

    use super::*;

    pub fn bristol_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "x58x14.58x58.36.14x14.58x14x18,18",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn cambs_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "-38-14-1258-36-14-58-16-78,12",
            CallSpec::near(Stage::MAJOR),
        )
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

    pub fn kiveton_s8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "38-58.14-12-36-78-58.14-16.78,12",
            CallSpec::near(Stage::MAJOR),
        )
    }

    pub fn carolina_reaper_tb8() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAJOR,
            "38-38.18-56-18-34-18.16-16.78,12",
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
        (Stage::ROYAL, "x30x14x50x16x1270x38x14x50x16x90,12", {
            let mut calls = CallSpec::near(Stage::ROYAL);
            calls.push(CallSpec::new(
                "16".to_owned(),
                'x',
                "LIBFVXSMWH".chars().collect_vec(),
            ));
            calls
        })
    }

    pub fn bristol_s10() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::ROYAL,
            "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
            CallSpec::far(Stage::ROYAL),
        )
    }

    pub fn bristol_s12() -> (Stage, &'static str, Vec<CallSpec>) {
        (
            Stage::MAXIMUS,
            "-5T-14.5T-5T.36.14-7T.58.16-9T.70.18-18.9T-18-1T,1T",
            CallSpec::near(Stage::MAXIMUS),
        )
    }
}

fn main() {
    let (stage, pn, calls) = methods::bristol_s10();

    let spec =
        Spec::tenors_together_from_pn(stage, pn, calls, comp_engine::spec::HALF_PEAL, 10).unwrap();

    if SimdRow::are_cpu_features_enabled() {
        println!("Using SimdRow!");
        spec.compose::<SimdRow>().pretty_print();
    } else {
        println!("Can't use SimdRow, defaulting to standard Row.");
        spec.compose::<Row>().pretty_print();
    }
}
