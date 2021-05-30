use zimmer::spec::Spec;

fn main() {
    let spec_toml = std::fs::read_to_string("test-cases/bristol-s10-lb4s.toml").unwrap();

    let spec: Spec = toml::from_str(&spec_toml).unwrap();

    println!("{:#?}", spec);
}
