use std::io;

use lust::{
    errors::{Error, Printable},
    interpreter::Interpreter,
    parser::Parser,
};

fn get_line() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let mut evaluator = Interpreter::new();
    loop {
        let line = get_line();
        let mut parser = Parser::new(&line);
        let res = parser.parse_expr();

        for e in &res.errors {
            e.show(&line, "repl");
        }
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            if let Err(s) = evaluator.eval(&expr) {
                let error = Error::on_expr(&s, &expr);
                error.show(&line, "repl");
            }
        }
    }
}
