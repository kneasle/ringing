mod engine;
mod set;
mod single_meth;

use proj_core::Stage;

use engine::Section;

fn yorkshire() -> single_meth::Table {
    single_meth::Table::from_place_not(
        Stage::ROYAL,
        "x30x14x50x16x1270x38x14x50x16x90,12",
        // Fix the treble and all the tenors
        &['1', '7', '8', '9', '0'],
        &[("14", '-', "LIBFVXSMWH"), ("1234", 's', "LBTFVXSMWH")],
        "LBTFVXSMWH",
    )
    .unwrap()
}

fn main() {
    let table = single_meth::Table::from_place_not(
        Stage::MAJOR,
        "-58-14.58-58.36.14-14.58-14-18,18",
        &['1', '7', '8'],
        single_meth::NEAR_CALLS_MAJOR,
        "LIBMFHVW",
    )
    .unwrap();

    // table.print_falseness();
    single_meth::Section::compose(&table, 5000..5185);
}
