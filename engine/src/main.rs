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
        Stage::ROYAL,
        "-50-14.50-50.36.14-70.58.16-16.70-16-10,10",
        "17890",
        &single_meth::far_calls(Stage::ROYAL),
        "LIO?VM?HVW",
    )
    .unwrap();

    // table.print_falseness();
    single_meth::Section::compose(&table, 5000..=5184);
}
