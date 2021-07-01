use monument::Config;
use monument_cli::{cli_args::CliArgs, spec::AbstractSpec};
use structopt::StructOpt;

/// Entry point for the CLI command `monument`
fn main() {
    // Parse the CLI args
    let args = CliArgs::from_args();
    let config = Config::from(&args);

    // Parse the TOML file into an 'abstract' specification
    let abstr_spec = AbstractSpec::read_from_file(&args.input_file).unwrap();
    println!("{:#?}", abstr_spec);

    // Convert the abstract specification into a concrete specification accepted by Monument's
    // engine
    let comp_spec = abstr_spec.to_spec(&config).unwrap();

    // Use this concrete specification to generate compositions
    let comps = monument::compose(&comp_spec, &config);

    for c in comps {
        println!("{}", c.to_string(&comp_spec));
    }
}
