use std::collections::HashMap;
use std::num::ParseFloatError;
use std::fmt;
use std::io::{self, Write};
use std::rc::Rc;

#[derive(Clone)]
enum RispExpr
{
    Symbol(String),
    Number(f64),
    List(Vec<RispExpr>),
    Func(fn(&[RispExpr]) -> Result<RispExpr, RispErr>),
    Lambda(RispLambda),
}

#[derive(Clone)]
struct RispLambda
{
    // Rc is a reference counted pointer in Rust.
    params_expr : Rc<RispExpr>,
    body_expr : Rc<RispExpr>,
}

#[derive(Debug)]
enum RispErr
{
    Reason(String),
}

#[derive(Clone)]
struct RispEnv<'a>
{
    data: HashMap<String, RispExpr>,
    outer: Option<&'a RispEnv<'a>>,
}

/*
Tokenizes a Risp program.
*/
fn tokenize(expr: String) -> Vec<String>
{
    expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map( |x| x.to_string() )
        .collect()
}

/*
Parses an expression from a string.
 */
fn parse<'a>(tokens: &'a[String]) -> Result<(RispExpr, &'a[String]), RispErr>
{
    let (token, rest) = tokens.split_first()
        .ok_or(
           RispErr::Reason("Couldn't get token.".to_string())
        )?;

    match &token[..]
    {
        "(" => read_seq(rest),
        ")" => Err(RispErr::Reason("Unexpected ')'.".to_string())),
        _   => Ok((parse_atom(token), rest)),
    }    
}

/*
Parses a sequence.
*/
fn read_seq<'a>(tokens: &'a[String]) -> Result<(RispExpr, &'a[String]), RispErr>
{
    let mut res : Vec<RispExpr> = vec![];
    let mut xs = tokens;

    loop
    {
        let (next_tok, rest) = xs
            .split_first()
            .ok_or(RispErr::Reason("Could not find closing ')'".to_string()))?;
        if next_tok == ")"
        {
            return Ok((RispExpr::List(res), rest))
        }
        let (expr, _xs) = parse(&xs)?;
        xs = _xs;
        res.push(expr);
    }
}

/*
Parses an atom.
*/
fn parse_atom(token: &str) -> RispExpr
{
    let poss_float: Result<f64, ParseFloatError> = token.parse();
    match poss_float
    {
        Ok(v) => RispExpr::Number(v),
        Err(_) => RispExpr::Symbol(token.to_string().clone())
    }
}

/*
One of our goals with this is that we minimize the number of things
here where it is at all reasonable.
*/
fn get_default_env<'a>() -> RispEnv<'a>
{
    let mut data: HashMap<String, RispExpr> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExpr::Func(
            |args: &[RispExpr]| -> Result<RispExpr, RispErr>
            {
                let sum = parse_list_of_floats(args)?
                    .iter().fold(0.0, |sum, a| sum + a);
                Ok(RispExpr::Number(sum))
            }
        )
    );
    data.insert(
        ">".to_string(),
        RispExpr::Func(
            |args: &[RispExpr]| -> Result<RispExpr, RispErr>
            {
                let items = parse_list_of_floats(args)?;
                // Check every pair of adjacent numbers.
                match items.windows(2).all(|w| w[0] > w[1])
                {
                    true => Ok(RispExpr::Number(1.0)),
                    false => Ok(RispExpr::Number(0.0)),
                }
            }
        )
    );
    data.insert(
        "<".to_string(),
        RispExpr::Func(
            |args: &[RispExpr]| -> Result<RispExpr, RispErr>
            {
                let items = parse_list_of_floats(args)?;
             	// Check every pair of adjacent numbers.                                                                                                                                                   
                match items.windows(2).all(|w| w[0] < w[1])
                {
                    true => Ok(RispExpr::Number(1.0)),
                    false => Ok(RispExpr::Number(0.0)),
                }
            }
        )
    );    
    RispEnv {data, outer: None}
}

fn parse_list_of_floats(args: &[RispExpr]) -> Result<Vec<f64>, RispErr>
{
    args
        .iter()
        .map( |x| parse_single_float(x) )
        .collect()
}

fn parse_single_float(expr: &RispExpr) -> Result<f64, RispErr>
{
    match expr
    {
        RispExpr::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("Expected a number.".to_string())),
    }
}

fn env_get(key: &str, env: &RispEnv) -> Option<RispExpr>
{
    match env.data.get(key)
    {
        Some(expr) => Some(expr.clone()),
        None =>
        {
            match &env.outer
            {
                Some(outer_env) => env_get(key, &outer_env),
                None => None,
            }
        }
    }
}

fn eval(expr: &RispExpr, env: &mut RispEnv) -> Result<RispExpr, RispErr>
{
    match expr
    {
        RispExpr::Symbol(k) =>
            env_get(k, env)
            .ok_or(
                RispErr::Reason(
                    format!("Unexpected symbol {}", k)
                )
            )
            .map(|x| x.clone() ),
        RispExpr::Number(_n) => Ok(expr.clone()),
        RispExpr::List(list) =>
        {
            let first_form = list.first()
            .ok_or(
                RispErr::Reason("Expected a non-empty list".to_string())
            )?;
            let arg_forms = &list[1..];

            // Handle builtins.
            match eval_builtin_form(first_form, arg_forms, env)
            {
                Some(res) => res,
                None =>
                {
                    let first_eval = eval(first_form, env)?;
                    match first_eval
                    {
                        RispExpr::Func(f) =>
                        {
                            f(&eval_forms(arg_forms, env)?)
                        },
                        RispExpr::Lambda(lambda) =>
                        {
                            let new_env = &mut env_for_lambda(lambda.params_expr, arg_forms, env)?;
                            eval(&lambda.body_expr, new_env)
                        }
                        _ => Err(
                            RispErr::Reason("First expresson in list must be a function".to_string())
                        ),
                    }
                }
            } // eval_builtin_form
        }, // RispExpr::List
        _ => Err(
            RispErr::Reason("Unexpected form.".to_string())
        ),
    }
}

fn eval_forms(arg_forms: &[RispExpr], env: &mut RispEnv) -> Result<Vec<RispExpr>, RispErr> {
    arg_forms
        .iter()
        .map(|x| eval(x, env))
        .collect()
}

// Makes a new env with all the params installed.
fn env_for_lambda<'a>(params: Rc<RispExpr>, arg_forms: &[RispExpr], outer_env: &'a mut RispEnv)
                      -> Result<RispEnv<'a>, RispErr>
{
    let par = parse_list_of_symbol_strings(params)?;
    if par.len() != arg_forms.len()
    {
        return Err(
            RispErr::Reason("Wrong number of arguments to lambda expression.".to_string())
        );
    }

    let args = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, RispExpr> = HashMap::new();
    for (p, a) in par.iter().zip(args.iter())
    {
        data.insert(p.clone(), a.clone());
    }

    Ok(
        RispEnv {
            data,
            outer: Some(outer_env),
        }
    )
}

fn parse_list_of_symbol_strings(form: Rc<RispExpr>) -> Result<Vec<String>, RispErr>
{
    let list = match form.as_ref()
    {
        RispExpr::List(s) => Ok(s.clone()),
        _ => Err(
            RispErr::Reason("Expected arguments to be a list.".to_string())
        ),
    }?;

    list
        .iter()
        .map(
            |x|
            {
                match x
                {
                    RispExpr::Symbol(s) => Ok(s.clone()),
                    _ => Err(
                        RispErr::Reason("Only symbols are allowed in argument list".to_string())
                    ),
                }
            }
        ).collect()
}

fn eval_builtin_form(
    exp: &RispExpr, arg_forms: &[RispExpr], env: &mut RispEnv
) -> Option<Result<RispExpr, RispErr>>
{
    match exp
    {
        RispExpr::Symbol(s) => 
            match s.as_ref()
            {
                "def" => Some(eval_def_args(arg_forms, env)),
                "fn"  => Some(eval_lambda_args(arg_forms)),
                "if"  => Some(eval_if_args(arg_forms, env)),
                _ => None,
            },
        _ => None,
    }
}

fn eval_lambda_args(arg_forms: &[RispExpr]) -> Result<RispExpr, RispErr>
{
    if arg_forms.len() > 2
    {
        return Err(
            RispErr::Reason("fn expression can only have two arguments.".to_string())
        )
    }

    let params_expr = arg_forms.first().ok_or(
        RispErr::Reason("Expected lambda arguments.".to_string())
    )?;
    let body_expr = arg_forms.get(1).ok_or(
        RispErr::Reason("Expected lambda body".to_string())
    )?;

    Ok(
        RispExpr::Lambda(
            RispLambda {
                body_expr: Rc::new(body_expr.clone()),
                params_expr: Rc::new(params_expr.clone()),
            }
        )
    )
}

fn eval_def_args(arg_forms: &[RispExpr], env: &mut RispEnv) -> Result<RispExpr, RispErr>
{
    if arg_forms.len() > 2
    {
        return Err(
            RispErr::Reason("def expression can only have two arguments.".to_string())
        )
    }
    
    let first_form = arg_forms.first()
        .ok_or(
            RispErr::Reason("Expected first form in def".to_string()),
        )?;

    let first_str = match first_form
    {
        RispExpr::Symbol(s) => Ok(s.clone()),
        _ => Err(
            RispErr::Reason("Expected first form to be a symbol.".to_string()))
    }?;

    let second_form = arg_forms.get(1)
        .ok_or(
            RispErr::Reason("Expected second form in def".to_string())
        )?;

    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_if_args(arg_forms: &[RispExpr], env: &mut RispEnv) -> Result<RispExpr, RispErr>
{
    let test_form = arg_forms.first().ok_or(
        RispErr::Reason(
            "Expected test form.".to_string(),
        )
    )?;
    let test_eval = eval(test_form, env)?;
    match test_eval
    {
        RispExpr::Number(n) =>
        {
            let form_idx = if n == 0.0 { 2 } else { 1 };
            let res_form = arg_forms.get(form_idx)
                .ok_or(RispErr::Reason(
                    format!("Expected form idx={}", form_idx)
                ))?;
            let res_eval = eval(res_form, env);
            res_eval
        },
        _ => Err(
            RispErr::Reason(format!("Unexpected test form='{}'", test_form.to_string()))
        )
    }
}

impl fmt::Display for RispExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let str = match self
        {
            RispExpr::Symbol(s) => s.clone(),
            RispExpr::Number(n) => n.to_string(),
            RispExpr::List(list) => {
                let xs: Vec<String> = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            },
            RispExpr::Func(_) => "Function {}".to_string(),
            RispExpr::Lambda(_) => "Lambda {}".to_string(),
        };
    
        write!(f, "{}", str)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExpr, RispErr>
{
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;
    
    Ok(evaled_exp)
}

fn slurp_expr() -> String
{
    let mut expr = String::new();
  
    io::stdin().read_line(&mut expr)
        .expect("Failed to read line");
    
    expr
}

fn main()
{
    let env = &mut get_default_env();
    loop
    {
        print!("risp > ");
        // Need to flush so output doesn't get line buffered.
        io::stdout().flush().unwrap();
        let expr = slurp_expr();
        match parse_eval(expr, env)
        {
            Ok(res) => println!("//   => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("//   => {}", msg),
            },
        }
    }
}
