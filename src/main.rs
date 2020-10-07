use lust::tokenizer::Tokenizer;
use std::io;

// use lust::errors::Printable;
// use lust::parser::Parser;

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
        let mut tokenizer = Tokenizer::new(&source);
        println!("{:?}", tokenizer.next_token());
    }
}
