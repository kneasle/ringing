mod engine;
mod single_meth;

use proj_core::{Bell, Method, PnBlock, Row, Stage};

use crate::single_meth::Table;

fn main() {
    let yorkshire_royal = Method::with_lead_end(
        "Yorkshire".to_owned(),
        &PnBlock::parse("x30x14x50x16x1270x38x14x50x16x90,12", Stage::ROYAL).unwrap(),
    );

    let table = Table::new(
        &yorkshire_royal,
        // Split each course into H-M, M-W, W-H
        vec![0..4, 4..5, 5..9].as_slice(),
        // vec![0..9].as_slice(),
        // Fix the treble and all the tenors
        ['1', '7', '8', '9', '0']
            .iter()
            .map(|c| Bell::from_name(*c).unwrap())
            .collect::<Vec<Bell>>()
            .as_slice(),
        // Which courses can be reached from the plain course
        vec![
            vec![
                ("", Row::parse("1234567890").unwrap(), 1),
                ("-M", Row::parse("1436527890").unwrap(), 1),
                ("sM", Row::parse("1634527890").unwrap(), 1),
            ],
            vec![
                ("", Row::parse("1234567890").unwrap(), 2),
                ("-W", Row::parse("1524367890").unwrap(), 2),
                ("sW", Row::parse("1534267890").unwrap(), 2),
            ],
            vec![
                ("", Row::parse("1234567890").unwrap(), 0),
                ("-H", Row::parse("1423567890").unwrap(), 0),
                ("sH", Row::parse("1243567890").unwrap(), 0),
            ],
        ]
        .as_slice(),
    );

    table
        .falseness
        .iter()
        .enumerate()
        .for_each(|(i, t)| println!("{}: {:?}", i, t));
}
