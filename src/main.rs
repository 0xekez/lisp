use std::io;

use lust::parser::ExprVal;
use lust::parser::Parser;
use lust::{
    errors::{Error, Printable},
    jit::JIT,
};

fn get_line() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let mut jit = JIT::new();
    loop {
        let source = get_line();
        let mut parser = Parser::new(&source);
        let res = parser.parse_expr();

        for e in &res.errors {
            e.show(&source, "repl");
        }
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            let code = jit.jit_expr(&expr).unwrap();
            let func = unsafe { std::mem::transmute::<_, fn() -> f32>(code) };
            println!("  => {}", func());
        }
    }
}
