use std::io;

use lust::{
    errors::{Error, Printable},
    interpreter::Interpreter,
    parser::ExprVal,
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
    let evaluator = Interpreter::new();
    loop {
        let line = get_line();
        let mut parser = Parser::new(&line);
        let res = parser.parse_expr();

        for e in &res.errors {
            e.show(&line, "repl");
        }
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            evaluator.eval(expr);
        }
    }
}
