mod engine;
mod set;
mod single_meth;

use proj_core::Stage;

use engine::Section;

fn cooktown() -> single_meth::Table {
    single_meth::Table::from_place_not(
        Stage::MAJOR,
        "-38-14-1256-18-12-58-16-78,12",
        // Fix the treble and all the tenors
        &['1', '7', '8'],
        &[("14", '-', "LIBFVMWH"), ("1234", 's', "LBTFVMWH")],
        "LBTFVMWH",
    )
    .unwrap()
}

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
    let table = cooktown();

    // table.print_falseness();
    single_meth::Section::compose(&table, 1250..1300);
}
