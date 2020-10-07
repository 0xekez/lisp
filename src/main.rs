use lust::errors::{Error, Printable};
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
    // loop {
    //     let source = get_line();
    //     let mut tokenizer = Tokenizer::new(&source);
    //     if let Some(t) = tokenizer.next_token() {
    //         let error = Error::on_tok("foobar!", &t);
    //         error.show(&source, "repl");
    //     }
    //     println!("{:?}", tokenizer.next_token());
    // }
    let source = "hello
this is a string with multiple
     lines";
    let error = Error::from_raw(0, 0, 2, 10, "malformed string");
    error.show(&source, "repl");
}
