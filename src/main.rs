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
            match expr.val {
                ExprVal::List(ref v) => match v[0].val {
                    ExprVal::Id(ref s) => {
                        if s == "defun" {
                            jit.compile_defun(expr);
                        } else {
                            let foo = jit.compile_call(expr).unwrap();
                            let foo = unsafe { std::mem::transmute::<_, fn() -> f32>(foo) };
                            println!("res: {}", foo());
                        }
                    }
                    _ => continue,
                },
                _ => continue,
            }
            // let foo = jit.compile_defun(res.expr.unwrap()).unwrap();
            // let foo = unsafe { std::mem::transmute::<_, fn(f32, f32) -> f32>(foo) };
            // println!("hello {}!", foo(1.0, 2.0));
        }
    }
    // let mut tokenizer = Tokenizer::new(&source);
    // if let Some(t) = tokenizer.next_token() {
    //     let error = Error::on_tok("foobar!", &t);
    //     error.show(&source, "repl");
    // }
    // println!("{:?}", tokenizer.next_token());
    // }
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
