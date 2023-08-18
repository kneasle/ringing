#![deny(rustdoc::broken_intra_doc_links)]

use colored::Colorize;
use monument_cli::{args::CliArgs, Environment};
use structopt::StructOpt;

fn main() {
    // Run Monument with the CLI args
    let args = CliArgs::from_args();
    monument_cli::init_logging(args.log_level());
    let result = monument_cli::run(&args.input_file, &args.options, Environment::Cli);

    // Handle the results, either by doing a debug print or printing an error message
    match result {
        Ok(Some(mut query_result)) => {
            if !args.options.only_display_update_line {
                query_result.print();
            }
        }
        Ok(None) => assert!(args.options.debug_option.is_some()),
        Err(e) => {
            // In the case of an error, print the error message nicely then terminate the program
            // with code -1 without causing a panic message.
            println!("{} {:?}", "Error:".bright_red().bold(), e);
            drop(args);
            std::process::exit(-1);
        }
    }
}
