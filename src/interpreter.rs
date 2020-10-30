use crate::parser::{Expr, ExprVal};
use std::collections::HashMap;
use std::fmt;

pub struct Interpreter<'a> {
    global_env: LustEnv<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            global_env: LustEnv::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        Self::eval_in_env(&data, &mut self.global_env)?;
        Ok(())
    }

    pub fn eval_print(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        println!("=> {}", Self::eval_in_env(&data, &mut self.global_env)?);
        Ok(())
    }

    fn eval_in_env(expr: &LustData, env: &mut LustEnv) -> Result<LustData, String> {
        match expr {
            LustData::Symbol(ref s) => env.resolve(s),
            LustData::List(ref v) => Self::eval_list(v, env),
            LustData::Builtin(_) => Err("unexpected builtin".to_string()),
            _ => Ok(expr.clone()),
        }
    }

    fn eval_list(list: &Vec<LustData>, env: &mut LustEnv) -> Result<LustData, String> {
        if list.len() == 0 {
            // Empty list evaluates to itself.
            Ok(LustData::List(list.clone()))
        } else {
            if let LustData::Symbol(s) = list.first().unwrap() {
                let pred = env.resolve(s)?;
                return match pred {
                    LustData::Builtin(ref f) => f(&list[1..], env),
                    LustData::Fn(ref lf) => Self::eval_funcall(lf, &list[1..], env),
                    LustData::Mac(ref lm) => Self::eval_macinv(lm, &list[1..], env),
                    _ => Err(format!("invalid list predicate: {}", pred)),
                };
            }
            Err("internal error retreiving predicate from list".to_string())
        }
    }

    fn is_tail_call(body: &Vec<LustData>, env: &LustEnv) -> Result<bool, String> {
        let last = body.last().unwrap();
        Ok(if let LustData::List(ref v) = last {
            match &v[..] {
                [a, ..] => {
                    if let LustData::Symbol(ref s) = a {
                        let val = env.resolve(s)?;
                        if let LustData::Fn(ref f) = val {
                            &f.body == body
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => false,
            }
        } else {
            false
        })
    }

    fn eval_fnbody(body: &Vec<LustData>, mut env: &mut LustEnv) -> Result<LustData, String> {
        for d in &body[0..(body.len() - 1)] {
            Self::eval_in_env(d, &mut env)?;
        }
        if Self::is_tail_call(body, env)? {
            println!("tail call");
        }
        Self::eval_in_env(body.last().unwrap(), &mut env)
    }

    fn eval_funcall(
        func: &LustFn,
        args: &[LustData],
        env: &mut LustEnv,
    ) -> Result<LustData, String> {
        if args.len() != func.params.len() {
            Err(format!(
                "wrong number of arguments for function call. got {} and expected {}",
                args.len(),
                func.params.len()
            ))
        } else {
            let mut fnenv = LustEnv::new();
            for (arg, param) in args.iter().zip(&func.params) {
                fnenv
                    .data
                    .insert(param.clone(), Self::eval_in_env(arg, env)?);
            }
            fnenv.outer = Some(&env);
            Self::eval_fnbody(&func.body, &mut fnenv)
        }
    }

    fn eval_macinv(
        func: &LustFn,
        args: &[LustData],
        env: &mut LustEnv,
    ) -> Result<LustData, String> {
        if args.len() != func.params.len() {
            Err(format!(
                "wrong number of arguments for macro involcation. got {} and expected {}",
                args.len(),
                func.params.len()
            ))
        } else {
            let mut fnenv = LustEnv::new();
            for (arg, param) in args.iter().zip(&func.params) {
                fnenv.data.insert(param.clone(), arg.clone());
            }
            // Now that we are done mutating the enviroment, make it
            // the outer enviroment for the function.
            fnenv.outer = Some(env);
            Self::eval_fnbody(&func.body, &mut fnenv)
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
    Builtin(fn(&[LustData], &mut LustEnv) -> Result<LustData, String>),
    Fn(LustFn),
    Mac(LustFn),
}

#[derive(Clone, PartialEq)]
struct LustFn {
    params: Vec<String>,
    body: Vec<LustData>,
}

struct LustEnv<'a> {
    data: HashMap<String, LustData>,
    outer: Option<&'a LustEnv<'a>>,
}

impl<'a> LustEnv<'a> {
    pub fn new() -> Self {
        Self::new_with_defaults()
    }

    fn new_with_defaults() -> Self {
        let mut me = Self {
            data: HashMap::new(),
            outer: None,
        };
        // TODO: I think that append is more generic than this.
        // (set 'join (fn (a b) (append b (append () a))))
        me.data.insert(
            "join".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    let mut res = Vec::new();
                    for arg in args {
                        let val = Interpreter::eval_in_env(&arg, env)?;
                        res.push(val);
                    }
                    Ok(LustData::List(res))
                },
            ),
        );

        me.data.insert(
            "quote".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 1 {
                        Err(format!("quote expects 1 arg, got {}", args.len()))
                    } else {
                        Ok(args[0].clone())
                    }
                },
            ),
        );

        me.data.insert(
            "set".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 2 {
                        Err(format!("def expects 2 args, got {}", args.len()))
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env)?;
                        match target {
                            LustData::Symbol(ref s) => {
                                let val = Interpreter::eval_in_env(&args[1], env)?;
                                env.data.insert(s.clone(), val.clone());
                                Ok(val)
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
                |args: &[LustData], _env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() < 2 {
                        Err(
                            "fn expects at least two paramaters, a param list and a body"
                                .to_string(),
                        )
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body: Vec<LustData> = args[1..].iter().cloned().collect();
                        Ok(LustData::Fn(LustFn { params, body }))
                    }
                },
            ),
        );

        me.data.insert(
            "macro".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() < 2 {
                        Err(
                            "macro expects at least two paramaters, a param list and a body"
                                .to_string(),
                        )
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body: Vec<LustData> = args[1..].iter().cloned().collect();
                        Ok(LustData::Mac(LustFn { params, body }))
                    }
                },
            ),
        );

        me.data.insert(
            "if".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 3 {
                        Err("if expects three paramaters condition, then, and else".to_string())
                    } else {
                        let cond = Interpreter::eval_in_env(&args[0], env)?;
                        // The empty list is the only false value.
                        let cond = match cond {
                            LustData::List(ref v) => !v.is_empty(),
                            _ => true,
                        };
                        // TODO: if should create a new enviroment
                        // with its new children.
                        if cond {
                            Interpreter::eval_in_env(&args[1], env)
                        } else {
                            Interpreter::eval_in_env(&args[2], env)
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "println".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 1 {
                        Err("println expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env)?;
                        print!("{}\n", target);
                        Ok(LustData::List(vec![]))
                    }
                },
            ),
        );

        me.data.insert(
            "negate".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 1 {
                        Err("negate expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env)?;
                        match target {
                            LustData::Number(n) => Ok(LustData::Number(-n)),
                            _ => Err("non numeric argument to negate".to_string()),
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "add".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(LustData::Number(lhs + rhs))
                    }
                },
            ),
        );

        me.data.insert(
            "lt".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env)? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(if lhs < rhs {
                            LustData::Symbol("#t".to_string())
                        } else {
                            LustData::List(vec![])
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "eq".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() != 2 {
                        Err("eq expects two arguments".to_string())
                    } else {
                        let lhs = Interpreter::eval_in_env(&args[0], env)?;
                        let rhs = Interpreter::eval_in_env(&args[1], env)?;
                        Ok(if lhs == rhs {
                            LustData::Symbol("#t".to_string())
                        } else {
                            LustData::List(vec![])
                        })
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
                Some(ref outer) => outer.resolve(id),
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
                for e in &func.body {
                    write!(f, " {}", e)?;
                }
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
                for e in &func.body {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            }
        }
    }
}
