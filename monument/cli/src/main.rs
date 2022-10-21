#![deny(rustdoc::broken_intra_doc_links)]

use colored::Colorize;
use monument_cli::{args::CliArgs, Environment, Source};
use structopt::StructOpt;

fn main() {
    let args = CliArgs::from_args();
    monument_cli::init_logging(args.log_level());
    let result = monument_cli::run(
        Source::Path(args.input_file.clone()),
        &args.config,
        Environment::Cli,
    );
    match result {
        Ok(Some(query_result)) => query_result.print(),
        Ok(None) => assert!(args.config.debug_option.is_some()),
        Err(e) => {
            // In the case of an error, print the error message nicely then terminate the program
            // with code -1 without causing a panic message.
            println!("{} {:?}", "Error:".bright_red().bold(), e);
            drop(args);
            std::process::exit(-1);
        }
    }
}
