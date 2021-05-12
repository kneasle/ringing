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
        "17890",
        &[("14", '-', "LIBFVXSMWH"), ("1234", 's', "LBTFVXSMWH")],
        "LBTFVXSMWH",
    )
    .unwrap()
}

fn main() {
    let table = single_meth::Table::from_place_not(
        Stage::MAJOR,
        "-38-14-1258-36-14-58-16-78,12",
        "178",
        &single_meth::NEAR_CALLS_MAJOR[..1],
        "LBTFVMWH",
    )
    .unwrap();

    // table.print_falseness();
    single_meth::Section::compose(&table, 5000..5185);
}
