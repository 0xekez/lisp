use clap::{App, Arg};
use lustc::timer;

fn main() {
    let _t = timer::timeit("total");

    let cli_opts = {
        App::new("lustc")
            .version("0.1.0")
            .author("Zeke M. <zekemedley@gmail.com>")
            .about("Compiles and runs lust programs.")
            .arg(
                Arg::with_name("file")
                    .required(true)
                    .index(1)
                    .help("the file to run"),
            )
            .arg(
                Arg::with_name("timeit")
                    .short("t")
                    .long("timeit")
                    .required(false)
                    .takes_value(false)
                    .help("show execution time information"),
            )
            .get_matches()
    };

    let file = cli_opts.value_of("file").unwrap();

    timer::init(cli_opts.is_present("timeit"));

    match lustc::roundtrip_file(file) {
        Ok(e) => println!("=> {:?}", e),
        Err(s) => eprintln!("error: {}", s),
    }
}
