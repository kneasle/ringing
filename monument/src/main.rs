use monument::spec::Spec;

fn main() {
    // let file_name = "test-cases/bristol-s10-lb4s.toml";
    // let file_name = "test-cases/bristol-s12-qps.toml";
    let file_name = "test-cases/yorkshire-s10-qps.toml";

    let spec_toml = std::fs::read_to_string(file_name).unwrap();
    let spec: Spec = toml::from_str(&spec_toml).unwrap();
    println!("{:#?}", spec);

    let engine = spec.create_engine().unwrap();

    engine.compose();
}
