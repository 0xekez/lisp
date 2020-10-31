use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::io;

use lust::{
    errors::{Error, Printable},
    interpreter::Interpreter,
    parser::Parser,
};

fn repl(evaluator: &mut Interpreter) {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("lust> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line == "(exit)" {
                    break;
                }
                let mut parser = Parser::new(&line);
                while parser.has_more() {
                    let res = parser.parse_expr();

                    for e in &res.errors {
                        e.show(&line, "repl");
                    }
                    if res.errors.is_empty() {
                        let expr = res.expr.unwrap();
                        if let Err(s) = evaluator.eval_print(&expr) {
                            let error = Error::on_expr(&s, &expr);
                            error.show(&line, "repl");
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                println!("lust: use CTRL-D or (exit) to exit");
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("error reading line: {:?}", err);
                break;
            }
        }
    }
}

fn file(path: &str) -> io::Result<String> {
    let file = std::fs::read_to_string(path)?;
    let mut evaluator = Interpreter::new();
    let mut parser = Parser::new(&file);
    while parser.has_more() {
        let res = parser.parse_expr();
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            if let Err(e) = evaluator.eval(&expr) {
                let error = Error::on_expr(&e, &expr);
                error.show(&file, path);
                println!("lust: starting debug repl at error");
                repl(&mut evaluator);
            }
        } else {
            for e in &res.errors {
                e.show(&file, path);
            }
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "an error occured during parsing the input file",
            ));
        }
    }
    repl(&mut evaluator);
    Ok("lust: done :)".to_string())
}

fn show_usage() {
    println!("usage: lust <file>?");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        show_usage();
        return;
    }

    if args.len() == 2 {
        match file(&args[1]) {
            Err(e) => println!("error: {}", e),
            Ok(_) => (),
        }
    } else {
        repl(&mut Interpreter::new())
    }
}
