use std::time::Duration;

mod examples;

fn main() {
    // We want the tests to run quickly, so we assert that the tests finish in about 10s
    examples::main(Duration::from_secs(10)).unwrap();
}
