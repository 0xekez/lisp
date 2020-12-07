pub mod builtins;
pub mod errors;
pub mod interpreter;
pub mod location;
pub mod lustvec;
pub mod parser;
pub mod reader;
pub mod repl;
pub mod symboltable;
pub mod tokenbuffer;
pub mod tokenizer;

extern crate colored;

use repl::REPLHelper;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use rustyline::Config;

use crate::errors::{Error, Printable};
use crate::interpreter::Interpreter;
use crate::parser::Parser;

pub fn do_repl(evaluator: &mut Interpreter) {
    let mut rl = Editor::<REPLHelper>::new();
    let helper = repl::REPLHelper::new();
    rl.set_helper(Some(helper));

    let indent = rustyline::KeyEvent::new('\t', rustyline::Modifiers::NONE);
    rl.bind_sequence(indent, rustyline::Cmd::Insert(1, "    ".to_string()));

    loop {
        let p = ">> ";
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{}\x1b[0m", p);
        let readline = rl.readline(&p);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.trim() == "(exit)" {
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

pub fn interpret_file(path: &str) -> Result<Interpreter, String> {
    let contents = match std::fs::read_to_string(path).map_err(|e| e.to_string()) {
        Ok(s) => s,
        Err(e) => return Err(format!("failed to read file {}: {}", path, e)),
    };
    let mut evaluator = Interpreter::new();
    let mut parser = Parser::new(&contents);

    while parser.has_more() {
        let res = parser.parse_expr();
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            if let Err(e) = evaluator.eval(&expr) {
                let error = Error::on_expr(&e, &expr);
                error.show(&contents, path);
                return Err(e);
            }
        } else {
            for e in &res.errors {
                e.show(&contents, path);
            }
            return Err("an error occured parsing the input file".to_string());
        }
    }
    Ok(evaluator)
}
