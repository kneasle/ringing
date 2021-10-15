use bellframe::{Bell, Mask, Method, MethodLib};
use monument_graph::{
    layout::{self, Call, Layout},
    music::MusicType,
    Graph,
};

fn main() {
    let cc_lib = MethodLib::cc_lib().expect("Couldn't load CC lib");
    let mut bristol = cc_lib
        .get_by_title("Bristol Surprise Major")
        .expect("CC lib doesn't contain Bristol");
    bristol.set_lead_end_label();
    let stage = bristol.stage();

    let calls = Call::near_calls(stage).unwrap();
    let runs = MusicType::all_4_bell_runs(stage, 1.0).unwrap();

    let layout = tenors_together_layout(bristol, &calls).expect("Couldn't build layout");

    dbg!(&layout);

    let graph = Graph::from_layout(&layout, &[runs], 1280);

    println!("Hello World!");
}

fn tenors_together_layout(
    method: Method,
    calls: &[Call],
) -> Result<Layout, layout::single_method::Error> {
    let tenor = Bell::tenor(method.stage());
    let tt_course_head = Mask::fix_bells(method.stage(), [tenor, tenor - 1].iter().cloned());
    Layout::single_method(&method, calls, vec![(tt_course_head, tenor)], None, None)
}
