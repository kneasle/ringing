use comp_engine::{
    engine::Table,
    spec::{CallSpec, Spec, HALF_PEAL, PEAL, QP},
};
use itertools::Itertools;
use proj_core::{SimdRow, Stage};

#[derive(Debug, Clone)]
struct TestCase {
    name: String,
    spec: Spec,
    exp_comps: Vec<(usize, f32, String)>,
}

impl TestCase {
    fn new(name: String, spec: Spec, exp_comps: Vec<(usize, f32, String)>) -> Self {
        Self {
            name,
            spec,
            exp_comps,
        }
    }
}

fn main() {
    // Run the test cases
    for test in test_cases() {
        // Print some nice pre-amble
        println!("\n\n");
        println!("Running `{}`", test.name);

        // Run the benchmark/test to generate some compositions
        let outcome = test.spec.compose::<SimdRow>();
        let mut comps = outcome.results.comps.iter().collect_vec();
        comps.sort_by(|c1, c2| c1.score.partial_cmp(&c2.score).unwrap());

        // Print the perf stats
        outcome.pretty_print_perf();

        /* Check that the compositions are as expected.  This is more difficult than simply sorting
         * and checking equality, since if there's a tie-break then the order is non-deterministic.
         * Therefore, the worst comps that made it into the shortlist may change depending on
         * threading, etc. */

        let mut error_messages: Vec<String> = Vec::new();

        // Get the worst music score that made it into the shortlist
        let worst_comp_score = match test.exp_comps.first() {
            Some(comp) => comp.1,
            None => {
                // If there aren't any example comps, then we probably haven't written the tests
                // yet, so just dump the code required to generate the test cases
                println!("");
                println!("FAILED TEST!");
                println!("");
                println!("It looks like you don't have any example comps.");
                println!("You might want to copy this:");
                println!(
                    "{}",
                    comps
                        .iter()
                        .enumerate()
                        .map(|(i, c)| format!(
                            "({}, {:.1}{}, \"{}\".to_owned()),",
                            c.length,
                            c.score,
                            if i == 0 { "f32" } else { "" },
                            outcome.table.comp_string(&c.calls)
                        ))
                        .join("\n")
                );
                panic!();
            }
        };
        // First up, check that there are the right number of comps with the lowest score
        let num_worst_comps = comps.iter().filter(|c| c.score == worst_comp_score).count();
        let num_exp_worst_comps = test
            .exp_comps
            .iter()
            .filter(|(_, s, _)| *s == worst_comp_score)
            .count();
        if num_exp_worst_comps != num_worst_comps {
            error_messages.push(format!(
                "Expected {} comps with score {}, got {}",
                num_exp_worst_comps, worst_comp_score, num_worst_comps
            ));
        }

        // Next up, check that every expected comp is found somewhere within the output
        let non_worst_comp_set: Vec<(usize, f32, String)> = comps
            .iter()
            .map(|c| (c.length, c.score, outcome.table.comp_string(&c.calls)))
            .collect_vec();
        for exp_comp in test
            .exp_comps
            .iter()
            .filter(|(_, s, _)| *s != worst_comp_score)
        {
            if non_worst_comp_set.iter().find(|c| *c == exp_comp).is_none() {
                error_messages.push(format!(
                    "Didn't find comp '{}' (score {})",
                    exp_comp.2, exp_comp.1
                ));
            }
        }

        // Print error messages, or an OK message
        if error_messages.is_empty() {
            println!("Results OK ({} comps)", comps.len());
        } else {
            println!("");
            println!("FAILED TEST!");
            println!("");
            for m in error_messages {
                println!("ERROR: {}", m);
            }
            println!("");
            println!("These comps were generated:");
            outcome.pretty_print();
        }
    }
}

fn test_cases() -> Vec<TestCase> {
    vec![
        TestCase::new(
            "Bristol s12 QPs".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::MAXIMUS,
                "-5T-14.5T-5T.36.14-7T.58.16-9T.70.18-18.9T-18-1T,1T",
                CallSpec::near(Stage::MAXIMUS),
                1250..=1440,
                30,
            )
            .unwrap(),
            vec![
                (1296, 99.0f32, "MsWHMWHsH".to_owned()),
                (1296, 100.0, "sMWsHHMWH".to_owned()),
                (1296, 102.0, "sMsWHsMWsHM".to_owned()),
                (1296, 104.0, "HMWHsMWsH".to_owned()),
                (1344, 104.0, "HsHHsH".to_owned()),
                (1296, 104.0, "MWHsMWsHH".to_owned()),
                (1296, 106.0, "HMsWHsMWH".to_owned()),
                (1344, 108.0, "HMHM".to_owned()),
                (1344, 108.0, "HsHsMM".to_owned()),
                (1344, 108.0, "HsMsHM".to_owned()),
                (1296, 109.0, "sMsWWsHMsWH".to_owned()),
                (1296, 109.0, "MsMsWHsMWsH".to_owned()),
                (1344, 111.0, "sHMHsM".to_owned()),
                (1296, 111.0, "MsMWHsMWH".to_owned()),
                (1344, 112.0, "MsMMsM".to_owned()),
                (1344, 112.0, "WsHsWH".to_owned()),
                (1296, 112.0, "MWHsHMsWH".to_owned()),
                (1344, 112.0, "sHHsHH".to_owned()),
                (1296, 113.0, "MsWHMsWWH".to_owned()),
                (1344, 114.0, "sWHWsH".to_owned()),
                (1344, 116.0, "WsWsHH".to_owned()),
                (1296, 116.0, "sHMsWHMWH".to_owned()),
                (1344, 120.0, "HWsWsH".to_owned()),
                (1344, 122.0, "sHsMMH".to_owned()),
                (1344, 125.0, "MHMH".to_owned()),
                (1344, 127.0, "MHsMsH".to_owned()),
                (1344, 141.0, "sHsMsHsM".to_owned()),
                (1344, 142.0, "WHWH".to_owned()),
                (1344, 144.0, "MHsHsM".to_owned()),
                (1344, 144.0, "sWsHsWsH".to_owned()),
            ],
        ),
        TestCase::new(
            "Yorkshire s10 QPs (with big bobs)".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::ROYAL,
                "x30x14x50x16x1270x38x14x50x16x90,12",
                {
                    let mut calls = CallSpec::near(Stage::ROYAL);
                    calls.push(CallSpec::new(
                        "16".to_owned(),
                        'x',
                        "LIBFVXSMWH".chars().collect_vec(),
                    ));
                    calls
                },
                QP,
                16,
            )
            .unwrap(),
            vec![
                (1280, 112.0f32, "MHsMsWsHsWxBxH".to_owned()),
                (1280, 112.0, "MHsMsWxBWMsWsH".to_owned()),
                (1280, 113.0, "MHWxBsMsHW".to_owned()),
                (1280, 113.0, "MsWsHxBMHW".to_owned()),
                (1280, 113.0, "MHsMsWxBWsHsMW".to_owned()),
                (1280, 113.0, "MHWxBsHsMW".to_owned()),
                (1280, 113.0, "MHWxBMsHsW".to_owned()),
                (1280, 113.0, "sMsHWxBMHW".to_owned()),
                (1280, 113.0, "MHsMsWxBMxHW".to_owned()),
                (1280, 113.0, "MsHsWxBMHW".to_owned()),
                (1280, 114.0, "MHWxHsMWxBMsWxH".to_owned()),
                (1280, 114.0, "MHsMsWHMWxBsMsWxH".to_owned()),
                (1280, 114.0, "xHsWxBsMWxHxH".to_owned()),
                (1280, 118.0, "MxBWsHsMWxHxH".to_owned()),
                (1280, 122.0, "MHsMsWHWxBxH".to_owned()),
                (1280, 123.0, "MHWxBMHW".to_owned()),
            ],
        ),
        TestCase::new(
            "Cambridge s8 QPs".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::MAJOR,
                "-38-14-1258-36-14-58-16-78,12",
                CallSpec::near(Stage::MAJOR),
                QP,
                20,
            )
            .unwrap(),
            vec![
                (1280, 46.0f32, "HWHsMsHBsHsMWHBHBH".to_owned()),
                (1280, 47.0, "MHsMsWHBHHBHWB".to_owned()),
                (1280, 47.0, "HBHsMsWHBHsMsWHBHH".to_owned()),
                (1280, 47.0, "HHBMBMHBH".to_owned()),
                (1280, 47.0, "BBHWBWHHH".to_owned()),
                (1280, 47.0, "HBHMHHBBMH".to_owned()),
                (1280, 47.0, "BBsHsWsHsMsWsHBsHMWHMsH".to_owned()),
                (1280, 47.0, "MHWBMHsMsWsHBsHB".to_owned()),
                (1280, 47.0, "BBsHMWMsWHBsHMsWHMsWsH".to_owned()),
                (1280, 48.0, "MHsMHBHsMsHsMHBHBH".to_owned()),
                (1280, 48.0, "HWBsHsMHBBMH".to_owned()),
                (1280, 48.0, "BsHBsHsMsWHWBMHW".to_owned()),
                (1280, 48.0, "MHWBMHsMsWHBHB".to_owned()),
                (1280, 48.0, "BsHBsHBMWHMWWHW".to_owned()),
                (1280, 49.0, "HWHBHHBBMH".to_owned()),
                (1280, 49.0, "BHBHsMsWHWBMHW".to_owned()),
                (1280, 49.0, "BHBHBMWHMWWHW".to_owned()),
                (1280, 51.0, "BBBMHWHHH".to_owned()),
                (1280, 52.0, "MBWHsMsWHHBHBH".to_owned()),
                (1280, 52.0, "HWBMHHBHBH".to_owned()),
            ],
        ),
        TestCase::new(
            "Yorkshire s8 QPs".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::MAJOR,
                "x38x14x58x16x12x38x14x78,12",
                CallSpec::near(Stage::MAJOR),
                QP,
                10,
            )
            .unwrap(),
            vec![
                (1280, 74.0f32, "sHsWHBHsMsMHBsHBsH".to_owned()),
                (1280, 74.0, "sHsMsMsWHBHBBMsWsH".to_owned()),
                (1280, 74.0, "sHBsMBsMsWBsHMsH".to_owned()),
                (1280, 74.0, "BBsWsHBWsHsWsWsH".to_owned()),
                (1280, 74.0, "BBsWsHBsHsHWsHsH".to_owned()),
                (1280, 75.0, "MHsMBsMBHBHW".to_owned()),
                (1280, 75.0, "sHsWBsHsMBBMsWsH".to_owned()),
                (1280, 75.0, "sHsWHBHBsWBsHMsWsH".to_owned()),
                (1280, 76.0, "sHsMsMsWHBHsHBHBsH".to_owned()),
                (1280, 77.0, "sHsWHBHsMsMsHBHBsH".to_owned()),
            ],
        ),
        TestCase::new(
            "Cambridge s8 Bobs-only peals".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::MAJOR,
                "-38-14-1258-36-14-58-16-78,12",
                {
                    let mut cs = CallSpec::near(Stage::MAJOR);
                    cs.pop();
                    cs
                },
                PEAL,
                10,
            )
            .unwrap(),
            vec![
                (
                    5184,
                    112.0f32,
                    "HHMMWWHHHMMWWHHHMMWWHBMWMHMMWWHHHBH".to_owned(),
                ),
                (
                    5184,
                    112.0,
                    "MMWWHWMWBHMMWWHHHMMWWHHHMMWWHHHBHHH".to_owned(),
                ),
                (
                    5184,
                    113.0,
                    "MMWWHHHMMWWHHHBHHHMMWWHWMWBHMMWWHHH".to_owned(),
                ),
                (5056, 114.0, "HHMMWWHHHMMWWHHHMMWWHHHBHHHMMWWH".to_owned()),
                (
                    5184,
                    114.0,
                    "HHMMWWHHHMMWWHHHBHHHMMWWHWMWBHMMWWH".to_owned(),
                ),
                (5152, 114.0, "MMWWHHHMMWWHHHMMWWMMWWHHHMMWWHHH".to_owned()),
                (5152, 115.0, "MMWWHHHMMWWHHHMMWWHHHMMWWMMWWHHH".to_owned()),
                (5152, 116.0, "MMWWHHHMMWWMMWWHHHMMWWHHHMMWWHHH".to_owned()),
                (5152, 117.0, "MMWWMMWWHHHMMWWHHHMMWWHHHMMWWHHH".to_owned()),
                (
                    5184,
                    117.0,
                    "MMWWHBMWMHMMWWHHHBHHHMMWWHHHMMWWHHH".to_owned(),
                ),
            ],
        ),
        TestCase::new(
            "Bristol s10 half-peals".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::ROYAL,
                "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
                CallSpec::far(Stage::ROYAL),
                HALF_PEAL,
                30,
            )
            .unwrap(),
            vec![
                (2520, 296.0f32, "sVsIOVIsIsVIOV".to_owned()),
                (2520, 296.0, "IOOIVIVsIsVIOI".to_owned()),
                (2520, 296.0, "sVsOsOsIOsOsIsVsIOsOsI".to_owned()),
                (2520, 296.0, "sOsIVIsOsVVIIsVsO".to_owned()),
                (2520, 296.0, "VIVOOOIIsVsIsOsI".to_owned()),
                (2520, 297.0, "sOsIsVsIVsIsVIVIsOsIsVsO".to_owned()),
                (2520, 297.0, "sVsOsVsIVIsVsOVIsOsIV".to_owned()),
                (2520, 297.0, "OIVsVsIOsVsIsVsIsVsO".to_owned()),
                (2520, 298.0, "sVsIOOIIOVsOsIsOsI".to_owned()),
                (2520, 298.0, "OIVIOVIsIsVIsVsO".to_owned()),
                (2520, 298.0, "sVsOsOsIOVVsIsVIOI".to_owned()),
                (2520, 298.0, "IsVsOIsVsOVIIsVsIsOsI".to_owned()),
                (2520, 299.0, "VIVOOVsVsIsVsIsOsI".to_owned()),
                (2520, 299.0, "sVsIOVsVsIOVOIOI".to_owned()),
                (2520, 299.0, "VIVIOIVsVsIOV".to_owned()),
                (2520, 299.0, "VIVIOIVsVsOsOsIsVsO".to_owned()),
                (2520, 299.0, "sVsIVOVIIsVsOIOsOsI".to_owned()),
                (2520, 300.0, "IOVIVOVsIsVIOI".to_owned()),
                (2520, 300.0, "OIsVsIVOOOIIsVsO".to_owned()),
                (2520, 300.0, "sOsIVIVOVsIVOsIIsVsO".to_owned()),
                (2520, 301.0, "sVsIOVsVsIOVsOsIsOsI".to_owned()),
                (2520, 301.0, "sVsIsVsOsVsIOVIOsOsI".to_owned()),
                (2520, 302.0, "sOsIsVsIsVsOOOIIsVsO".to_owned()),
                (2520, 303.0, "OIsVsIVOOVsVsIsVsO".to_owned()),
                (2520, 303.0, "VIVIIOVIOsOsI".to_owned()),
                (2520, 303.0, "VIVOsOsIOVIOsOsI".to_owned()),
                (2520, 304.0, "VIVOOOIIOV".to_owned()),
                (2520, 305.0, "sVsIOOIIsVsOIOsOsI".to_owned()),
                (2520, 307.0, "VIVOOVsVsIOV".to_owned()),
                (2520, 308.0, "sVsIOVsVsIsVsOIOsOsI".to_owned()),
            ],
        ),
        TestCase::new(
            "Lessness s8 QPs".to_owned(),
            Spec::tenors_together_from_pn(
                Stage::MAJOR,
                "-38-14-56-16-12-58-14-58,12",
                CallSpec::near(Stage::MAJOR),
                QP,
                20,
            )
            .unwrap(),
            vec![
                (1280, 74.0f32, "sHBsWsHsMWBMWMsWsH".to_owned()),
                (1280, 74.0, "sHBsMsHsMWsHMBWsHMsWsH".to_owned()),
                (1280, 74.0, "BsHsMWsHMWsMHHBW".to_owned()),
                (1280, 74.0, "sHsMBHMBsHsWsHsMWsHWsH".to_owned()),
                (1280, 74.0, "BMsWsHMsWsMHHBW".to_owned()),
                (1280, 74.0, "sHsMsWsHMBHsWHBHMWsH".to_owned()),
                (1280, 74.0, "MsWsHBMsWBsMWM".to_owned()),
                (1280, 74.0, "BMsWsHsMsMsHsMWsHMBWsH".to_owned()),
                (1280, 74.0, "BsHsMWMsWsMHHBW".to_owned()),
                (1280, 74.0, "BMsWMWsMHHBW".to_owned()),
                (1280, 75.0, "sHsMBHsMBsWMWMHsMWsH".to_owned()),
                (1280, 75.0, "sHsMWBsMBsWMWsHWsH".to_owned()),
                (1280, 75.0, "MBWsHsMsWsHMBWsHsMWMWsHWsH".to_owned()),
                (1280, 76.0, "BMsWsHMsWsMWBsHsMW".to_owned()),
                (1280, 76.0, "sHsMsWsHMBHsWsHBMsWsH".to_owned()),
                (1280, 76.0, "BMsWsMWsHsMWBMsWsH".to_owned()),
                (1280, 76.0, "sHsMsWsHMBHsWsHBsHMWsH".to_owned()),
                (1280, 76.0, "sHsMBsWHsMWBMWMsWsH".to_owned()),
                (1280, 76.0, "sHsMBsWHsMWBMWsHMWsH".to_owned()),
                (1280, 76.0, "sHsMsWsHMBHBsHsMsHMWsH".to_owned()),
            ],
        ),
    ]
}
