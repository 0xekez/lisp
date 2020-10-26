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

        println!("=> {}", Self::eval_in_env(&data, &mut self.global_env)?);
        Ok(())
    }

    fn eval_in_env(expr: &LustData, env: &mut LustEnv) -> Result<LustData, String> {
        match expr {
            LustData::Symbol(ref s) => env.resolve(s),
            LustData::List(ref v) => Self::eval_list(v, env),
            LustData::Builtin(_) => Err("unexpected builtin".to_string()),
            LustData::Number(_) => Ok(expr.clone()),
            LustData::Fn(_) => Ok(expr.clone()),
            LustData::Mac(_) => Ok(expr.clone()),
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
            // Now that we are done mutating the enviroment, make it
            // the outer enviroment for the function.
            fnenv.outer = Some(env);
            let mut res = Vec::new();
            for d in &func.body {
                res.push(Self::eval_in_env(d, &mut fnenv)?);
            }
            Ok(res.last().unwrap().clone())
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
            let mut res = Vec::new();
            for d in &func.body {
                res.push(Self::eval_in_env(d, &mut fnenv)?);
            }
            Ok(res.last().unwrap().clone())
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

#[derive(Clone)]
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
            "mac".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: &mut LustEnv| -> Result<LustData, String> {
                    if args.len() < 2 {
                        Err(
                            "mac expects at least two paramaters, a param list and a body"
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
                        if cond {
                            Interpreter::eval_in_env(&args[1], env)
                        } else {
                            Interpreter::eval_in_env(&args[2], env)
                        }
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
