//! Runner for test cases (not benchmarks)

mod common;

/// Collect and run all the test cases, but no benchmarks (which would take too long)
fn main() -> anyhow::Result<()> {
    // Read the args to determine what to do
    let mut args = std::env::args();
    args.next(); // Skip the first arg, which always the path to the test's binary
    if args.next().as_deref() == Some("bless") {
        match args.next().as_deref() {
            // `cargo bless`: bless only unspecified
            None => common::bless_tests(common::BlessLevel::OnlyUnspecified),
            // `cargo bless --fails ...`
            Some("--fails") => match args.next() {
                // `cargo bless --fails {something} ...`
                Some(arg) => {
                    println!("Unknown additional arg '{}'", arg);
                    print_bless_usage();
                    Ok(())
                }
                // `cargo bless --fails`: bless everything
                None => common::bless_tests(common::BlessLevel::Fails),
            },
            // `cargo bless {something not --fails} ...`
            Some(arg) => {
                println!("Unknown arg '{}'", arg);
                print_bless_usage();
                Ok(())
            }
        }
    } else {
        // If no args were given, just run the tests
        match common::run(common::RunType::Test)? {
            common::Outcome::Fail => Err(anyhow::Error::msg("Tests failed")),
            common::Outcome::Pass => Ok(()),
        }
    }
}

fn print_bless_usage() {
    println!();
    println!("Possible options:");
    println!("    `cargo bless`       : Bless only unspecified/new test cases");
    println!("    `cargo bless --fails`: Bless everything, even failed test cases");
}
