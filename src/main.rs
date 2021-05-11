mod engine;
mod set;
mod single_meth;

use proj_core::{Bell, Method, PnBlock, Row, Stage};

use engine::Section;

fn cooktown() -> single_meth::Table {
    let s = Stage::MAJOR;

    single_meth::Table::new(
        &Method::with_lead_end(
            "Cooktown Orchid".to_owned(),
            &PnBlock::parse("-38-14-1256-18-12-58-16-78,12", s).unwrap(),
        ),
        // Split each course into H-M, M-W, W-H
        vec![0..1, 1..6, 6..7].as_slice(),
        // Fix the treble and all the tenors
        ['1', '7', '8']
            .iter()
            .map(|c| Bell::from_name(*c).unwrap())
            .collect::<Vec<Bell>>()
            .as_slice(),
        // Which courses can be reached from the plain course
        vec![
            vec![
                ("", Row::parse_with_stage("123456", s).unwrap(), 1),
                ("-W", Row::parse_with_stage("152436", s).unwrap(), 1),
                ("sW", Row::parse_with_stage("153426", s).unwrap(), 1),
            ],
            vec![
                ("", Row::parse_with_stage("123456", s).unwrap(), 2),
                ("-M", Row::parse_with_stage("143652", s).unwrap(), 2),
                ("sM", Row::parse_with_stage("163452", s).unwrap(), 2),
            ],
            vec![
                ("", Row::parse_with_stage("123456", s).unwrap(), 0),
                ("-H", Row::parse_with_stage("142356", s).unwrap(), 0),
                ("sH", Row::parse_with_stage("124356", s).unwrap(), 0),
            ],
        ]
        .as_slice(),
    )
}

fn yorkshire() -> single_meth::Table {
    let s = Stage::ROYAL;

    single_meth::Table::new(
        &Method::with_lead_end(
            "Yorkshire".to_owned(),
            // &PnBlock::parse("-30-14-1250-36-1470-58-16-70-18-90,12", s).unwrap(),
            &PnBlock::parse("x30x14x50x16x1270x38x14x50x16x90,12", s).unwrap(),
        ),
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
                ("", Row::parse_with_stage("123456", s).unwrap(), 1),
                ("-M", Row::parse_with_stage("143652", s).unwrap(), 1),
                ("sM", Row::parse_with_stage("163452", s).unwrap(), 1),
            ],
            vec![
                ("", Row::parse_with_stage("123456", s).unwrap(), 2),
                ("-W", Row::parse_with_stage("152436", s).unwrap(), 2),
                ("sW", Row::parse_with_stage("153426", s).unwrap(), 2),
            ],
            vec![
                ("", Row::parse_with_stage("123456", s).unwrap(), 0),
                ("-H", Row::parse_with_stage("142356", s).unwrap(), 0),
                ("sH", Row::parse_with_stage("124356", s).unwrap(), 0),
            ],
        ]
        .as_slice(),
    )
}

fn main() {
    let table = yorkshire();

    // table.print_falseness();
    single_meth::Section::compose(&table, 1..2501);
}
