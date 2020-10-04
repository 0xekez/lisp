use std::io;

use lust::parser::Parser;

fn get_line() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    loop {
        let source = get_line();
        let mut parser = Parser::new(&source);
        let res = parser.parse_expr();
        if res.errors.is_empty() {
            if let Some(e) = res.expr {
                println!("{:?}", e);
            }
        } else {
            for err in res.errors {
                err.show_error(&source);
            }
            println!();
        }
    }
}
