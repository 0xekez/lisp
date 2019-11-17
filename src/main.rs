use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::boxed::Box;
use std::num::ParseFloatError;
use std::fmt;
use std::io::{self, Write};

/*
Expressions.
*/

// Risp has symbols, pairs, numbers, functions, and streams (not
// yet implemented).
#[derive(Clone)]
enum RispExpr
{
    Nil,
    Symbol(String),
    Pair(Box<RispExpr>, Box<RispExpr>),
    Number(f32),
    Func(fn(&[RispExpr]) -> Result<RispExpr, RispErr>),
    Lambda(RispLambda)
}

impl fmt::Display for RispExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let str = match self
        {
            RispExpr::Number(n) => n.to_string(),
            RispExpr::Symbol(s) => s.clone(),
            RispExpr::Pair(first, second) =>
                format!("({} . {})", *first, *second),
            RispExpr::Func(_) => "Function {}".to_string(),
            RispExpr::Lambda(_) => "Lambda {}".to_string(),
            RispExpr::Nil => "nil".to_string(),
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone)]
struct RispLambda
{
    params_expr: Rc<RispExpr>,
    body_expr:   Rc<RispExpr>,
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
    parent: Option<&'a RispEnv<'a>>,
}

/*
Parsing.
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

fn parse_expr<'a>(tokens: &'a[String]) -> Result<(RispExpr, &'a[String]), RispErr>
{

    let (token, rest) = tokens.split_first()
        .ok_or(
           RispErr::Reason("Couldn't get token.".to_string())
        )?;


    match &token[..]
    {
        "("  => parse_pair(rest),
        ")"  => Err(RispErr::Reason("Unexpected closing ')'.".to_string())),
        "."  => Err(RispErr::Reason("Unexpected '.'.".to_string())),
        "\\" => Err(RispErr::Reason("Char type not implemented".to_string())),
        _    => Ok((parse_atom(token), rest)),
    }
}

// ( expr . expr )
// (expr expr)
fn parse_pair<'a>(tokens: &'a[String])
                 -> Result<(RispExpr, &'a[String]), RispErr>
{    
    let mut xs = tokens;

    // Parse the first expression.
    let (first, rest) = parse_expr(&xs)?;
    xs = rest;
    
    match xs.first()
    {
        None => Err(
            RispErr::Reason("Could not find closing ')'.".to_string())),
        Some(tok)  => match &tok[..]
        {
            "." =>
            {
                // Pop the '.' and parse the other half.
                let (_dot, xs) = xs
                    .split_first()
                    .ok_or(RispErr::Reason("Unexpected parse end.".to_string()))?;
                let (second, rest) = parse_expr(&xs)?;

                // Parse the closing parenthesis.
                let (closer, rest) = rest.split_first()
                    .ok_or(
                        RispErr::Reason("Unexpected EOF, expected ')'".to_string())
                    )?;
                if closer != ")"
                {
                    return Err(RispErr::Reason("Expected closing ')'".to_string()))
                }
                    
                return Ok((RispExpr::Pair(Box::new(first), Box::new(second)), rest))
            },
            _ => // Well formed list.
            {
                // This is almost without a doubt not the most
                // effective way to do this. Here we build a vector
                // and then assemble it into a linked list. Given time
                // and interest in speeding up parsing this might be a
                // good place to look for easy performance inprovements.
                let mut res: VecDeque<RispExpr> = VecDeque::new();
                res.push_back(first);

                loop
                {
                    let (next_tok, rest) = xs
                        .split_first()
                        .ok_or(
                            RispErr::Reason("Expected closing ')'".to_string()))?;
                    if next_tok == ")"
                    {
                        // done. Assemble into regular list.
                        // Would like to do this recursively. Probably
                        // should define a helper function that takes
                        // a vector and outputs a linked list.
                        fn vec_to_wellformed_pairs (v: &mut VecDeque<RispExpr>) -> RispExpr
                        {
                            match v.pop_front()
                            {
                                None => RispExpr::Nil,
                                Some(e) =>
                                {
                                    RispExpr::Pair(
                                        Box::new(e),
                                        Box::new(vec_to_wellformed_pairs(v)))
                                },
                            }
                        };
                        return Ok((vec_to_wellformed_pairs(&mut res), rest))
                    }
                    let (expr, _xs) = parse_expr(&xs)?;
                    xs = _xs;
                    res.push_back(expr);
                }
            }
        },
    }
}

// Symbol or float. Not char.
fn parse_atom(token: &str) -> RispExpr
{
    let poss_float: Result<f32, ParseFloatError> = token.parse();
    match poss_float
    {
        Ok(v) => RispExpr::Number(v),
        Err(_) => RispExpr::Symbol(token.to_string().clone())
    }
}

/*
I/O
*/
fn slurp_expr() -> String
{
    let mut expr = String::new();
    io::stdin().read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

fn parse_eval(expr: String) -> Result<RispExpr, RispErr>
{
    let (parsed, _) = parse_expr(&tokenize(expr))?;
    Ok(parsed)
}

fn main()
{
    loop
    {
        print!("lst > ");
        io::stdout().flush().unwrap();
        let io = slurp_expr();
        match parse_eval(io)
        {
            Ok(res) => println!("//  => {}", res),
            Err(e)  => match e {
                RispErr::Reason(msg) => println!("//  => {}", msg),
            },
        }
    }
}
