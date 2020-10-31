use crate::parser::{Expr, ExprVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct Interpreter {
    global_env: Rc<RefCell<LustEnv>>,
}

enum CallResult {
    Ret(LustData),
    Call(Rc<RefCell<LustEnv>>, LustData),
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            global_env: LustEnv::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        Self::eval_in_env(&data, self.global_env.clone())?;
        Ok(())
    }

    pub fn eval_print(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        println!("=> {}", Self::eval_in_env(&data, self.global_env.clone())?);
        Ok(())
    }

    fn eval_in_env(expr: &LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
        let mut currentenv = env;
        let mut currexpr = expr.clone();
        loop {
            match currexpr {
                LustData::Symbol(ref s) => break currentenv.borrow().resolve(s),
                LustData::List(ref v) => {
                    // Empty list does not result in function call.
                    if v.len() == 0 {
                        break Ok(currexpr);
                    }
                    let fnres = Self::eval_list(v, currentenv)?;
                    match fnres {
                        CallResult::Ret(v) => break Ok(v),
                        CallResult::Call(env, expr) => {
                            currentenv = env;
                            currexpr = expr;
                        }
                    }
                }
                LustData::Builtin(_) => break Err("unexpected builtin".to_string()),
                _ => break Ok(currexpr),
            }
        }
    }

    fn eval_list(list: &Vec<LustData>, env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
        let pred = Self::eval_in_env(list.first().unwrap(), env.clone())?;
        match pred {
            LustData::Builtin(ref f) => f(&list[1..], env),
            LustData::Fn(ref lf) => Self::eval_funcall(lf, &list[1..], env),
            LustData::Mac(ref lm) => Self::eval_macinv(lm, &list[1..], env),
            _ => Err(format!("invalid list predicate: {}", pred)),
        }
    }

    fn eval_funcall(
        func: &LustFn,
        args: &[LustData],
        env: Rc<RefCell<LustEnv>>,
    ) -> Result<CallResult, String> {
        if args.len() != func.params.len() {
            Err(format!(
                "wrong number of arguments for function call. got {} and expected {}",
                args.len(),
                func.params.len()
            ))
        } else {
            let fnenv = LustEnv::new();
            for (arg, param) in args.iter().zip(&func.params) {
                fnenv
                    .borrow_mut()
                    .data
                    .insert(param.clone(), Self::eval_in_env(arg, env.clone())?);
            }
            fnenv.borrow_mut().outer = Some(env);
            Ok(CallResult::Call(fnenv, func.body.clone()))
        }
    }

    fn eval_macinv(
        func: &LustFn,
        args: &[LustData],
        env: Rc<RefCell<LustEnv>>,
    ) -> Result<CallResult, String> {
        if args.len() != func.params.len() {
            Err(format!(
                "wrong number of arguments for macro involcation. got {} and expected {}",
                args.len(),
                func.params.len()
            ))
        } else {
            let fnenv = LustEnv::new();
            for (arg, param) in args.iter().zip(&func.params) {
                fnenv.borrow_mut().data.insert(param.clone(), arg.clone());
            }
            // Now that we are done mutating the enviroment, make it
            // the outer enviroment for the function.
            fnenv.borrow_mut().outer = Some(env);
            Ok(CallResult::Call(fnenv, func.body.clone()))
        }
    }
}

impl Expr {
    fn to_data(&self) -> Result<LustData, String> {
        match &self.val {
            ExprVal::Number(f) => Ok(LustData::Number(*f)),
            ExprVal::List(ref v) => {
                let mut res = Vec::with_capacity(v.len());
                for e in v {
                    let data = e.to_data()?;
                    res.push(data);
                }
                Ok(LustData::List(res))
            }
            ExprVal::Id(s) => Ok(LustData::Symbol(s.clone())),
            _ => Err("unsuported form".to_string()),
        }
    }
}

#[derive(Clone)]
enum LustData {
    Number(f32),
    List(Vec<LustData>),
    Symbol(String),
    Builtin(fn(&[LustData], Rc<RefCell<LustEnv>>) -> Result<CallResult, String>),
    Fn(Box<LustFn>),
    Mac(Box<LustFn>),
}

#[derive(Clone, PartialEq)]
struct LustFn {
    params: Vec<String>,
    body: LustData,
}

struct LustEnv {
    data: HashMap<String, LustData>,
    outer: Option<Rc<RefCell<LustEnv>>>,
}

impl LustEnv {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::new_with_defaults()))
    }

    fn new_with_defaults() -> Self {
        let mut me = Self {
            data: HashMap::new(),
            outer: None,
        };

        me.data.insert(
            "quote".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        Err(format!("quote expects 1 arg, got {}", args.len()))
                    } else {
                        Ok(CallResult::Ret(args[0].clone()))
                    }
                },
            ),
        );

        me.data.insert(
            "eval".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        Err(format!("eval expects 1 arg, got {}", args.len()))
                    } else {
                        Ok(CallResult::Ret(Interpreter::eval_in_env(&args[0], env)?))
                    }
                },
            ),
        );

        me.data.insert(
            "set".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err(format!("def expects 2 args, got {}", args.len()))
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        match target {
                            LustData::Symbol(ref s) => {
                                let val = Interpreter::eval_in_env(&args[1], env.clone())?;
                                env.borrow_mut().data.insert(s.clone(), val.clone());
                                Ok(CallResult::Ret(val))
                            }
                            _ => Err("target of def expression must be a symbol".to_string()),
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "fn".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err("fn expects two paramaters, a param list and a body".to_string())
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body = args[1].clone();
                        Ok(CallResult::Ret(LustData::Fn(Box::new(LustFn {
                            params,
                            body,
                        }))))
                    }
                },
            ),
        );

        me.data.insert(
            "macro".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err("fn expects two paramaters, a param list and a body".to_string())
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body = args[1].clone();
                        Ok(CallResult::Ret(LustData::Mac(Box::new(LustFn {
                            params,
                            body,
                        }))))
                    }
                },
            ),
        );

        me.data.insert(
            "if".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 3 {
                        Err("if expects three paramaters condition, then, and else".to_string())
                    } else {
                        let cond = Interpreter::eval_in_env(&args[0], env.clone())?;
                        // The empty list is the only false value.
                        let cond = match cond {
                            LustData::List(ref v) => !v.is_empty(),
                            _ => true,
                        };
                        // TODO: if should create a new enviroment
                        // with its new children.
                        if cond {
                            Ok(CallResult::Call(env, args[1].clone()))
                        } else {
                            Ok(CallResult::Call(env, args[2].clone()))
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "println".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        Err("println expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env)?;
                        print!("{}\n", target);
                        Ok(CallResult::Ret(LustData::List(vec![])))
                    }
                },
            ),
        );

        me.data.insert(
            "negate".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        Err("negate expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env)?;
                        match target {
                            LustData::Number(n) => Ok(CallResult::Ret(LustData::Number(-n))),
                            _ => Err("non numeric argument to negate".to_string()),
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "add".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(CallResult::Ret(LustData::Number(lhs + rhs)))
                    }
                },
            ),
        );

        me.data.insert(
            "sub".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() < 1 {
                        Err("sub expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to sub".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to sub".to_string()),
                        };
                        Ok(CallResult::Ret(LustData::Number(lhs - rhs)))
                    }
                },
            ),
        );

        me.data.insert(
            "lt".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(CallResult::Ret(if lhs < rhs {
                            LustData::Symbol("#t".to_string())
                        } else {
                            LustData::List(vec![])
                        }))
                    }
                },
            ),
        );

        me.data.insert(
            "eq".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err("eq expects two arguments".to_string())
                    } else {
                        let lhs = Interpreter::eval_in_env(&args[0], env.clone())?;
                        let rhs = Interpreter::eval_in_env(&args[1], env)?;
                        Ok(CallResult::Ret(if lhs == rhs {
                            LustData::Symbol("#t".to_string())
                        } else {
                            LustData::List(vec![])
                        }))
                    }
                },
            ),
        );

        me
    }

    fn collect_param_list(data: &LustData) -> Result<Vec<String>, String> {
        match data {
            LustData::List(ref v) => {
                let mut res = Vec::new();
                for data in v {
                    if let LustData::Symbol(s) = data {
                        res.push(s.clone());
                    } else {
                        return Err(
                            "unexpected non-symbol type in function decl param list".to_string()
                        );
                    }
                }
                Ok(res)
            }
            _ => Err("unexpected non-list type as function decl param list".to_string()),
        }
    }

    pub fn resolve(&self, id: &str) -> Result<LustData, String> {
        match self.data.get(id) {
            Some(data) => Ok(data.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow().resolve(id),
                None => Err(format!("failed to resolve identifier {}", id)),
            },
        }
    }
}

impl PartialEq for LustData {
    fn eq(&self, other: &Self) -> bool {
        match (&self, other) {
            (LustData::Number(l), LustData::Number(r)) => l == r,
            (LustData::Symbol(ref l), LustData::Symbol(ref r)) => l == r,
            (LustData::List(ref l), LustData::List(ref r)) => {
                l.iter().zip(r.iter()).all(|(lhs, rhs)| lhs == rhs)
            }
            (LustData::Fn(l), LustData::Fn(r)) => l == r,
            (LustData::Mac(l), LustData::Mac(r)) => l == r,
            (_, _) => false,
        }
    }
}
// number -> number
// symbol -> symbol
// if -> if cond { then } else { otherwise }
// (set 'name (fn (a))) -> fn name (a, b) -> (return) { body }
impl fmt::Display for LustData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::List(l) => {
                if l.is_empty() {
                    return write!(f, "()");
                }
                write!(f, "(")?;
                for e in &l[..(l.len() - 1)] {
                    write!(f, "{} ", e)?;
                }
                write!(f, "{})", l[l.len() - 1])
            }
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Builtin(_) => write!(f, "<builtin anonymous fn>"),
            Self::Fn(func) => {
                write!(f, "(fn ")?;
                if func.params.is_empty() {
                    write!(f, "()")?;
                } else {
                    write!(f, "(")?;
                    for e in &func.params[..(func.params.len() - 1)] {
                        write!(f, "{} ", e)?;
                    }
                    write!(f, "{})", func.params[func.params.len() - 1])?;
                }
                write!(f, " {}", func.body)?;
                write!(f, ")")
            }
            Self::Mac(func) => {
                write!(f, "(mac ")?;
                if func.params.is_empty() {
                    write!(f, "()")?;
                } else {
                    write!(f, "(")?;
                    for e in &func.params[..(func.params.len() - 1)] {
                        write!(f, "{} ", e)?;
                    }
                    write!(f, "{})", func.params[func.params.len() - 1])?;
                }
                write!(f, " {}", func.body)?;
                write!(f, ")")
            }
        }
    }
}
