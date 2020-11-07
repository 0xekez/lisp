// Builtin functions for Lust.

use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter::{CallResult, Interpreter, LustData, LustEnv, LustFn};

/// Quotes its argument. The result of evaluating a quoted argument is
/// the argument.
pub fn quote(args: &[LustData], _env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("quote", 1, args)?;
    Ok(CallResult::Ret(args[0].clone()))
}

/// Returns the first item in a list or () if the list is empty.
pub fn car(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("car", 1, args)?;
    let expr = Interpreter::eval_in_env(&args[0], env)?;
    let l = LustData::expect_list(&expr)?;
    Ok(CallResult::Ret(match l.first().take() {
        Some(d) => d.clone(),
        None => LustData::get_empty_list(),
    }))
}

/// Takes a list and returns a new list containing all but the first
/// item in the list.
pub fn cdr(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("cdr", 1, args)?;
    let expr = Interpreter::eval_in_env(&args[0], env)?;
    let l = LustData::expect_list(&expr)?;
    Ok(CallResult::Ret(match l.split_first() {
        Some((_, rest)) => LustData::List(rest.to_vec()),
        None => LustData::get_empty_list(),
    }))
}

/// Prepends its first argument to its second argument where the
/// second argument is a list.
pub fn cons(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("cons", 2, args)?;
    let prepend = Interpreter::eval_in_env(&args[0], env.clone())?;
    let expr = Interpreter::eval_in_env(&args[1], env)?;
    let l = LustData::expect_list(&expr)?;
    let mut ret = l.clone();
    ret.insert(0, prepend);
    Ok(CallResult::Ret(LustData::List(ret)))
}

/// Takes arguments COND THEN ELSE. If COND is true evaluates and
/// returns the result of THEN, otherwise evaluates and returns the
/// result of ELSE.
pub fn if_(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("if", 3, args)?;
    let cond = Interpreter::eval_in_env(&args[0], env.clone())?;
    let nenv = LustEnv::new();
    nenv.borrow_mut().outer = Some(env);
    Ok(if truthy(&cond) {
        CallResult::Call(nenv, args[1].clone())
    } else {
        CallResult::Call(nenv, args[2].clone())
    })
}

/// Calls back into the interpreter to evaluate its argument.
pub fn eval(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("eval", 1, args)?;
    let arg = Interpreter::eval_in_env(&args[0], env.clone())?;
    Ok(CallResult::Ret(Interpreter::eval_in_env(&arg, env)?))
}

/// Takes two arguments TARGET and VALUE. Evaluates target and then
/// assigns VAUE to that symbol in the global enviroment.
pub fn set(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("set", 2, args)?;
    let target = Interpreter::eval_in_env(&args[0], env.clone())?;
    let target = LustData::expect_symbol(&target)?;
    let val = Interpreter::eval_in_env(&args[1], env.clone())?;
    env.borrow_mut().set_global(target.clone(), &val);
    Ok(CallResult::Ret(val))
}

/// Same as set above but binds the value in the local enviroment.
pub fn let_(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("let", 2, args)?;
    let target = Interpreter::eval_in_env(&args[0], env.clone())?;
    let target = LustData::expect_symbol(&target)?;
    let val = Interpreter::eval_in_env(&args[1], env.clone())?;
    env.borrow_mut().data.insert(target.clone(), val.clone());
    Ok(CallResult::Ret(val))
}

/// Takes two arguments PARAMS and BODY. PARAMS is a list of symbols
/// that will be bound to arguments when the function is called and
/// BODY is an expression to evaluate and return the result of when
/// the function is called. PARAMS can optionally include an `&`
/// symbol followed by one additional argument. When this function is
/// called the additional argument will be bound to a list containing
/// any remaining arguments after the first arguments have been bound
/// to values.
pub fn fn_(args: &[LustData], _env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("fn", 2, args)?;
    let params = collect_param_list(&args[0])?;
    let body = args[1].clone();
    Ok(CallResult::Ret(LustData::Fn(Rc::new(LustFn {
        params,
        body,
    }))))
}

/// Declares a macro. This has the same syntax and semantics as
/// declaring a function but the evaluation rules are the same as Lisp
/// macros.
pub fn macro_(args: &[LustData], _env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("macro", 2, args)?;
    let params = collect_param_list(&args[0])?;
    let body = args[1].clone();
    Ok(CallResult::Ret(LustData::Mac(Rc::new(LustFn {
        params,
        body,
    }))))
}

/// Expands but does not evaluate a macro. Very useful for debugging
/// macros.
pub fn macroexpand(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("macroexpand", 1, args)?;
    Ok(CallResult::Ret(Interpreter::macroexpand(
        args[0].clone(),
        env,
    )?))
}

pub fn error(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("error", 1, args)?;
    let message = Interpreter::eval_in_env(&args[0], env)?;
    Err(format!("{}", message))
}

/// Takes on argument and prints it to stdout followed by a newline.
pub fn println_(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("println", 1, args)?;
    let val = Interpreter::eval_in_env(&args[0], env)?;
    println!("{}", val);
    Ok(CallResult::Ret(LustData::get_empty_list()))
}

/// Evaluates and imports the global symbol table from another
/// file. For example, to add the stdlib to a project: `(import
/// 'std)`. Takes the relative path to the file as an argument and
/// appends .lisp before reading the file.
pub fn import(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("import", 1, args)?;
    let target = Interpreter::eval_in_env(&args[0], env.clone())?;
    let mut target = LustData::expect_symbol(&target)?.clone();
    target.push_str(".lisp");
    let evaluator = crate::interpret_file(&target)?;
    env.borrow_mut()
        .data
        // Sadly need to clone here. Would be unsafe to move out of a
        // reference counted value because we'd be liable to give
        // other owners a headache.
        .extend((*evaluator.global_env.borrow_mut()).data.clone());
    Ok(CallResult::Ret(LustData::get_empty_list()))
}

/// Takes one numeric argument and negates it.
pub fn negate(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("negate", 1, args)?;
    let val = Interpreter::eval_in_env(&args[0], env)?;
    let val = LustData::expect_num(&val)?;
    Ok(CallResult::Ret(LustData::Number(-val)))
}

/// Takes two arguments and adds them together.
pub fn add(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("add", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(LustData::Number(l + r)))
}

/// Takes two arguments and subtracts the second from the first.
pub fn sub(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("sub", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(LustData::Number(l - r)))
}

/// Takes two arguments and multiplies them together.
pub fn mul(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("mul", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(LustData::Number(l * r)))
}

/// Takes two arguments and divides the first by the second.
pub fn div(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("div", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(LustData::Number(l / r)))
}

/// Takes two numeric arguments LEFT and RIGHT and returns if LEFT is
/// less than RIGHT.
pub fn lt(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("lt", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(get_truthy_equiv(l < r)))
}

/// Takes two numeric arguments LEFT and RIGHT and returns if LEFT is
/// greater than RIGHT.
pub fn gt(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("gt", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let l = LustData::expect_num(&l)?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    let r = LustData::expect_num(&r)?;
    Ok(CallResult::Ret(get_truthy_equiv(l > r)))
}

/// Takes two numeric arguments LEFT and RIGHT and returns if LEFT is
/// equal to RIGHT.
pub fn eq(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("gt", 2, args)?;
    let l = Interpreter::eval_in_env(&args[0], env.clone())?;
    let r = Interpreter::eval_in_env(&args[1], env.clone())?;
    Ok(CallResult::Ret(get_truthy_equiv(l == r)))
}

// Evaluate each argument in a comma expression, ignore all others.
pub fn quaziquote(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
    check_arg_len("quaziquote", 1, args)?;
    let l = args[0].expect_list()?;
    let mut res = Vec::with_capacity(l.len());
    for item in l {
        res.push(eval_commas(item, env.clone())?);
    }
    Ok(CallResult::Ret(LustData::List(res)))
}

fn eval_commas(data: &LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
    // If it's a comma, evaluate and return its argument. If it's a
    // non-list type return it. If it's a list return a new list that
    // is the result of calling eval_commas on each of its items.
    match data {
        LustData::List(ref l) => {
            if is_comma(l) {
                // We know that we have at least one element because
                // is_comma returned true.
                eval_comma(&l[1..], env)
            } else {
                let mut res = Vec::with_capacity(l.len());
                for d in l {
                    res.push(eval_commas(d, env.clone())?);
                }
                Ok(LustData::List(res))
            }
        }
        _ => Ok(data.clone()),
    }
}

fn is_comma(data: &[LustData]) -> bool {
    match data.first() {
        Some(ref d) => {
            if let Ok(s) = d.expect_symbol() {
                s == "comma"
            } else {
                false
            }
        }
        None => false,
    }
}

fn eval_comma(args: &[LustData], env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
    check_arg_len("<comma builtin>", 1, args)?;
    Ok(Interpreter::eval_in_env(&args[0], env)?)
}

/// Verifies that the function called NAME has received the expected
/// number of arguments.
fn check_arg_len(name: &str, expected: usize, args: &[LustData]) -> Result<(), String> {
    if args.len() != expected {
        Err(format!(
            "{} expected {} arguments but got {}",
            name,
            expected,
            args.len()
        ))
    } else {
        Ok(())
    }
}

/// Get's the Lust truthy equivalent to Rust boolean value.
fn get_truthy_equiv(cond: bool) -> LustData {
    if cond {
        LustData::Symbol("#t".to_string())
    } else {
        LustData::get_empty_list()
    }
}

/// Collects a list of function paramaters or errors.
fn collect_param_list(expr: &LustData) -> Result<Vec<String>, String> {
    let v = LustData::expect_list(expr)?;
    let mut res = Vec::with_capacity(v.len());
    for (i, e) in v.iter().enumerate() {
        let name = LustData::expect_symbol(e)?;
        res.push(name.clone());
        if name == "&" {
            if i + 2 != v.len() {
                return Err(
                    "invalid varadic function. & symbol must occur before last argument"
                        .to_string(),
                );
            }
        }
    }
    Ok(res)
}

/// Converts some data to a Rust boolean.
fn truthy(expr: &LustData) -> bool {
    match LustData::expect_list(expr) {
        Ok(ref v) => !(v.len() == 0),
        Err(_) => true,
    }
}
