use std::env;

use lust::interpreter::Interpreter;

fn show_usage() {
    println!("usage: lust <file>?");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 3 {
        show_usage();
        return;
    }

    if args.len() == 2 {
        match lust::interpret_file(&args[1]) {
            Err(e) => println!("error: {}", e),
            Ok(_) => (),
        }
    } else {
        lust::do_repl(&mut Interpreter::new())
    }
}
