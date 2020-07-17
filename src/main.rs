use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, Write};
use std::num::ParseFloatError;
use std::rc::Rc;

#[derive(Clone)]
enum LustExpr {
    Symbol(String),
    Number(f64),
    List(Vec<LustExpr>),
    Func(fn(&[LustExpr]) -> Result<LustExpr, LustErr>),
    Lambda(LustLambda),
}

#[derive(Clone)]
struct LustLambda {
    // Rc is a reference counted pointer in Rust.
    params_expr: Rc<LustExpr>,
    body_expr: Rc<LustExpr>,
}

#[derive(Debug)]
enum LustErr {
    Reason(String),
}

#[derive(Clone)]
struct LustEnv<'a> {
    data: HashMap<String, LustExpr>,
    outer: Option<&'a LustEnv<'a>>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(LustExpr, &'a [String]), LustErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(LustErr::Reason("Couldn't get token.".to_string()))?;

    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(LustErr::Reason("Unexpected ')'.".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(LustExpr, &'a [String]), LustErr> {
    let mut res: Vec<LustExpr> = vec![];
    let mut xs = tokens;

    loop {
        let (next_tok, rest) = xs
            .split_first()
            .ok_or(LustErr::Reason("Could not find closing ')'".to_string()))?;
        if next_tok == ")" {
            return Ok((LustExpr::List(res), rest));
        }
        let (expr, _xs) = parse(&xs)?;
        xs = _xs;
        res.push(expr);
    }
}

fn parse_atom(token: &str) -> LustExpr {
    let poss_float: Result<f64, ParseFloatError> = token.parse();
    match poss_float {
        Ok(v) => LustExpr::Number(v),
        Err(_) => LustExpr::Symbol(token.to_string().clone()),
    }
}

/*
One of our goals with this is that we minimize the number of things
here where it is at all reasonable.
*/
fn get_default_env<'a>() -> LustEnv<'a> {
    let mut data: HashMap<String, LustExpr> = HashMap::new();
    data.insert(
        "+".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(LustExpr::Number(sum))
        }),
    );
    data.insert(
        ">".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            let items = parse_list_of_floats(args)?;
            // Check every pair of adjacent numbers.
            match items.windows(2).all(|w| w[0] > w[1]) {
                true => Ok(LustExpr::Number(1.0)),
                false => Ok(LustExpr::Number(0.0)),
            }
        }),
    );
    data.insert(
        "<".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            let items = parse_list_of_floats(args)?;
            // Check every pair of adjacent numbers.
            match items.windows(2).all(|w| w[0] < w[1]) {
                true => Ok(LustExpr::Number(1.0)),
                false => Ok(LustExpr::Number(0.0)),
            }
        }),
    );
    data.insert(
        "list".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            Ok(LustExpr::List(args.to_vec()))
        }),
    );
    data.insert(
        "nil?".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            if args.len() > 1 {
                return Err(LustErr::Reason("nil? expects only 1 argument.".to_string()));
            }
            let ex = args
                .first()
                .ok_or(LustErr::Reason("nil? got no arguments.".to_string()))?;
            match ex {
                LustExpr::List(l) => {
                    if l.len() == 0 {
                        Ok(LustExpr::Number(1.0))
                    } else {
                        Ok(LustExpr::Number(0.0))
                    }
                }
                _ => Ok(LustExpr::Number(0.0)),
            }
        }),
    );
    data.insert(
        "first".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            if args.len() > 1 {
                return Err(LustErr::Reason(
                    "first expects only 1 argument.".to_string(),
                ));
            }
            let ex = args
                .first()
                .ok_or(LustErr::Reason("first got no arguments.".to_string()))?;
            match ex {
                LustExpr::List(l) => match l.first() {
                    Some(ex) => Ok(ex.clone()),
                    None => Err(LustErr::Reason("first called on empty list.".to_string())),
                },
                _ => Err(LustErr::Reason(
                    "first called on non-list type.".to_string(),
                )),
            }
        }),
    );
    data.insert(
        "rest".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            if args.len() > 1 {
                return Err(LustErr::Reason("rest expects only 1 argument.".to_string()));
            }
            let ex = args
                .first()
                .ok_or(LustErr::Reason("rest got no arguments.".to_string()))?;
            match ex {
                LustExpr::List(l) => match l.len() {
                    0 => Err(LustErr::Reason("rest called on empty list".to_string())),
                    _ => Ok(LustExpr::List(l[1..].to_vec())),
                },
                _ => Err(LustErr::Reason(
                    "first called on non-list type.".to_string(),
                )),
            }
        }),
    );
    data.insert(
        "print".to_string(),
        LustExpr::Func(|args: &[LustExpr]| -> Result<LustExpr, LustErr> {
            if args.len() > 1 {
                return Err(LustErr::Reason(
                    "print expects only 1 argument.".to_string(),
                ));
            }
            let ex = args
                .first()
                .ok_or(LustErr::Reason("print got no arguments.".to_string()))?;
            println!("{}", format!("{}", ex.to_string()));
            Ok(LustExpr::Number(1.0))
        }),
    );
    LustEnv { data, outer: None }
}

fn parse_list_of_floats(args: &[LustExpr]) -> Result<Vec<f64>, LustErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(expr: &LustExpr) -> Result<f64, LustErr> {
    match expr {
        LustExpr::Number(num) => Ok(*num),
        _ => Err(LustErr::Reason("Expected a number.".to_string())),
    }
}

fn env_get(key: &str, env: &LustEnv) -> Option<LustExpr> {
    match env.data.get(key) {
        Some(expr) => Some(expr.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(key, &outer_env),
            None => None,
        },
    }
}

fn eval(expr: &LustExpr, env: &mut LustEnv) -> Result<LustExpr, LustErr> {
    match expr {
        LustExpr::Symbol(k) => env_get(k, env)
            .ok_or(LustErr::Reason(format!("Unexpected symbol {}", k)))
            .map(|x| x.clone()),
        LustExpr::Number(_n) => Ok(expr.clone()),
        LustExpr::List(list) => {
            let first_form = list
                .first()
                .ok_or(LustErr::Reason("Expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];

            // Handle builtins.
            match eval_builtin_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        LustExpr::Func(f) => f(&eval_forms(arg_forms, env)?),
                        LustExpr::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_expr, arg_forms, env)?;
                            eval(&lambda.body_expr, new_env)
                        }
                        _ => Err(LustErr::Reason(
                            "First expresson in list must be a function".to_string(),
                        )),
                    }
                }
            } // eval_builtin_form
        } // LustExpr::List
        // Functions and lambdas just evalutate to themselves.
        LustExpr::Func(_f) => Ok(expr.clone()),
        LustExpr::Lambda(_l) => Ok(expr.clone()),
    }
}

fn eval_forms(arg_forms: &[LustExpr], env: &mut LustEnv) -> Result<Vec<LustExpr>, LustErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

// Makes a new env with all the params installed.
fn env_for_lambda<'a>(
    params: Rc<LustExpr>,
    arg_forms: &[LustExpr],
    outer_env: &'a mut LustEnv,
) -> Result<LustEnv<'a>, LustErr> {
    let par = parse_list_of_symbol_strings(params)?;
    if par.len() != arg_forms.len() {
        return Err(LustErr::Reason(
            "Wrong number of arguments to lambda expression.".to_string(),
        ));
    }

    let args = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, LustExpr> = HashMap::new();
    for (p, a) in par.iter().zip(args.iter()) {
        data.insert(p.clone(), a.clone());
    }

    Ok(LustEnv {
        data,
        outer: Some(outer_env),
    })
}

fn parse_list_of_symbol_strings(form: Rc<LustExpr>) -> Result<Vec<String>, LustErr> {
    let list = match form.as_ref() {
        LustExpr::List(s) => Ok(s.clone()),
        _ => Err(LustErr::Reason(
            "Expected arguments to be a list.".to_string(),
        )),
    }?;

    list.iter()
        .map(|x| match x {
            LustExpr::Symbol(s) => Ok(s.clone()),
            _ => Err(LustErr::Reason(
                "Only symbols are allowed in argument list".to_string(),
            )),
        })
        .collect()
}

fn eval_builtin_form(
    exp: &LustExpr,
    arg_forms: &[LustExpr],
    env: &mut LustEnv,
) -> Option<Result<LustExpr, LustErr>> {
    match exp {
        LustExpr::Symbol(s) => match s.as_ref() {
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            "if" => Some(eval_if_args(arg_forms, env)),
            "eval" => Some(eval_eval_args(arg_forms, env)),
            "err?" => match eval_eval_args(arg_forms, env) {
                Ok(_) => Some(Ok(LustExpr::Number(0.0))),
                Err(_) => Some(Ok(LustExpr::Number(1.0))),
            },
            _ => None,
        },
        _ => None,
    }
}

// I am no expert on how Lisp works, so it is entirely possible that I
// do not understand eval well enough to implement it
// correctly. Here is how I imagine it working though:
//    1. Evaluate all the arguments (resolve them and etc).
//    2. Do a second eval pass on those arguments.
fn eval_eval_args(arg_forms: &[LustExpr], env: &mut LustEnv) -> Result<LustExpr, LustErr> {
    if arg_forms.len() > 1 {
        return Err(LustErr::Reason("eval only takes one argument.".to_string()));
    }
    let ex = arg_forms
        .first()
        .ok_or(LustErr::Reason("Expected expression in eval.".to_string()))?;

    match eval(ex, env) {
        Ok(resolved) => match eval(&resolved, env) {
            Ok(evaluated) => Ok(evaluated),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}

fn eval_lambda_args(arg_forms: &[LustExpr]) -> Result<LustExpr, LustErr> {
    if arg_forms.len() > 2 {
        return Err(LustErr::Reason(
            "fn expression can only have two arguments.".to_string(),
        ));
    }

    let params_expr = arg_forms
        .first()
        .ok_or(LustErr::Reason("Expected lambda arguments.".to_string()))?;
    let body_expr = arg_forms
        .get(1)
        .ok_or(LustErr::Reason("Expected lambda body".to_string()))?;

    Ok(LustExpr::Lambda(LustLambda {
        body_expr: Rc::new(body_expr.clone()),
        params_expr: Rc::new(params_expr.clone()),
    }))
}

fn eval_def_args(arg_forms: &[LustExpr], env: &mut LustEnv) -> Result<LustExpr, LustErr> {
    if arg_forms.len() > 2 {
        return Err(LustErr::Reason(
            "def expression can only have two arguments.".to_string(),
        ));
    }

    let first_form = arg_forms
        .first()
        .ok_or(LustErr::Reason("Expected first form in def".to_string()))?;

    let first_str = match first_form {
        LustExpr::Symbol(s) => Ok(s.clone()),
        _ => Err(LustErr::Reason(
            "Expected first form to be a symbol.".to_string(),
        )),
    }?;

    let second_form = arg_forms
        .get(1)
        .ok_or(LustErr::Reason("Expected second form in def".to_string()))?;

    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_if_args(arg_forms: &[LustExpr], env: &mut LustEnv) -> Result<LustExpr, LustErr> {
    let test_form = arg_forms
        .first()
        .ok_or(LustErr::Reason("Expected test form.".to_string()))?;
    let test_eval = eval(test_form, env)?;
    match test_eval {
        LustExpr::Number(n) => {
            let form_idx = if n == 0.0 { 2 } else { 1 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or(LustErr::Reason(format!("Expected form idx={}", form_idx)))?;
            let res_eval = eval(res_form, env);
            res_eval
        }
        _ => Err(LustErr::Reason(format!(
            "Unexpected test form='{}'",
            test_form.to_string()
        ))),
    }
}

impl fmt::Display for LustExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            LustExpr::Symbol(s) => s.clone(),
            LustExpr::Number(n) => n.to_string(),
            LustExpr::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(", "))
            }
            LustExpr::Func(_) => "builtin".to_string(),
            LustExpr::Lambda(l) => format!("(fn {} {})", l.params_expr, l.body_expr), //"Lambda {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut LustEnv) -> Result<LustExpr, LustErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn read_file(filename: &String) -> std::io::Result<String> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn slurp_file(buffer: &String, env: &mut LustEnv) -> Result<(), LustErr> {
    let mut tokens = tokenize(buffer.to_string());
    loop {
        if tokens.len() == 0 {
            return Ok(());
        }
        let (parsed_expr, tokens_) = parse(&tokens)?;
        tokens = tokens_.to_vec();
        eval(&parsed_expr, env)?;
    }
}

fn main() {
    let mut env = &mut get_default_env();

    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        match read_file(&args[1]) {
            Ok(buff) => match slurp_file(&buff, &mut env) {
                Ok(_) => println!("Read in source file."),
                Err(e) => match e {
                    LustErr::Reason(msg) => println!("//   => {}", msg),
                },
            },
            Err(_) => println!("Failed to read file"),
        }
    }
    if args.len() > 2 {
        println!("Usage: lust <filemane>");
    }

    loop {
        print!("lust > ");
        // Need to flush so output doesn't get line buffered.
        io::stdout().flush().unwrap();
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("//   => {}", res),
            Err(e) => match e {
                LustErr::Reason(msg) => println!("//   => {}", msg),
            },
        }
    }
}
