mod engine;
mod single_meth;

use proj_core::{Bell, Method, PnBlock, Stage};

use crate::single_meth::SingleMethodTable;

fn main() {
    let yorkshire_royal = Method::with_lead_end(
        "Yorkshire".to_owned(),
        &PnBlock::parse("x30x14x50x16x1270x38x14x50x16x90,12", Stage::ROYAL).unwrap(),
    );

    let table = SingleMethodTable::new(
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
    );

    table
        .falseness
        .iter()
        .enumerate()
        .for_each(|(i, t)| println!("{}: {:?}", i, t));
}
