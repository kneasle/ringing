mod engine;
mod set;
mod single_meth;

use proj_core::{Row, Stage};

use engine::Section;

use crate::single_meth::near_calls;

fn yorkshire() -> single_meth::Table<Row> {
    single_meth::Table::from_place_not(
        Stage::ROYAL,
        "x30x14x50x16x1270x38x14x50x16x90,12",
        // Fix the treble and all the tenors
        "17890",
        &[("14", '-', "LIBFVXSMWH"), ("1234", 's', "LBTFVXSMWH")],
        "LBTFVXSMWH",
    )
    .unwrap()
}

fn main() {
    let table = single_meth::Table::<Row>::from_place_not(
        Stage::MAJOR,
        "-38-14-1258-36-14-58-16-78,12",
        "178",
        &near_calls(Stage::MAJOR)[..1],
        "LBTFVMWH",
    )
    .unwrap();

    // table.print_falseness();
    if is_x86_feature_detected!("ssse3") && is_x86_feature_detected!("sse4.1") {
        println!("Can use SIMD!");
        single_meth::Section::compose(&table.change_row_type::<proj_core::SimdRow>(), 5000..=5184);
    } else {
        println!("Can't use SIMD.");
        single_meth::Section::compose(&table, 5000..=5184);
    }
}
