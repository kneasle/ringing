use std::time::Duration;

mod examples;

fn main() {
    // If we're benchmarking, then we can be prepared to run some pretty long tests, so allow up to
    // an hour for the benchmarks
    examples::main(Duration::from_secs(1 * 60 * 60)).unwrap();
}
