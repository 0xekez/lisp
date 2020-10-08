// use lust::errors::{Error, Printable};
use std::io;

use lust::errors::Printable;
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

        for e in res.errors {
            e.show(&source, "repl");
        }
        if let Some(e) = res.expr {
            println!("{:?}", e);
        }
        // let mut tokenizer = Tokenizer::new(&source);
        // if let Some(t) = tokenizer.next_token() {
        //     let error = Error::on_tok("foobar!", &t);
        //     error.show(&source, "repl");
        // }
        // println!("{:?}", tokenizer.next_token());
    }
    // let source = "(def list= (fn (a b)
    //  (if (nil? a)
    //  	 (if (nil? b) 1 0)
    // 	 (if (nil? b) 0
    // 	     (if (= (first a) (first b))
    //              (= (rest a) (rest b))
    // 	     	 0)))))";
    // let error = Error::from_raw(0, 5, 5, 28, "nothing wrong just a test");
    // error.show(&source, "repl");
}
