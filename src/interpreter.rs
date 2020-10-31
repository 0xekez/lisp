use crate::parser::{Expr, ExprVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct Interpreter {
    global_env: Rc<RefCell<LustEnv>>,
}

struct FnResult {
    env: Rc<RefCell<LustEnv>>,
    ret: Option<LustData>,
    ast: Vec<LustData>,
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
        let mut currentast = vec![expr.clone()];
        loop {
            let expr = currentast.remove(0);
            match expr {
                LustData::Symbol(ref s) => break currentenv.borrow().resolve(s),
                LustData::List(ref v) => {
                    // Empty list does not result in function call.
                    if v.len() == 0 {
                        break Ok(LustData::List(v.clone()));
                    }
                    let fnres = Self::eval_list(v, currentenv.clone())?;
                    if let Some(d) = fnres.ret {
                        break Ok(d.clone());
                    }
                    currentenv = fnres.env;
                    currentast = fnres.ast;
                }
                LustData::Builtin(_) => break Err("unexpected builtin".to_string()),
                _ => break Ok(expr.clone()),
            }
        }
    }

    fn eval_list(list: &Vec<LustData>, env: Rc<RefCell<LustEnv>>) -> Result<FnResult, String> {
        if let LustData::Symbol(s) = list.first().unwrap() {
            let pred = env.borrow().resolve(s)?;
            return match pred {
                LustData::Builtin(ref f) => f(&list[1..], env),
                LustData::Fn(ref lf) => Self::eval_funcall(lf, &list[1..], env),
                LustData::Mac(ref lm) => Self::eval_macinv(lm, &list[1..], env),
                _ => Err(format!("invalid list predicate: {}", pred)),
            };
        }
        Err("internal error retreiving predicate from list".to_string())
    }

    fn eval_funcall(
        func: &LustFn,
        args: &[LustData],
        env: Rc<RefCell<LustEnv>>,
    ) -> Result<FnResult, String> {
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
            Ok(FnResult {
                env: fnenv,
                ret: None,
                ast: func.body.clone(),
            })
        }
    }

    fn eval_macinv(
        func: &LustFn,
        args: &[LustData],
        env: Rc<RefCell<LustEnv>>,
    ) -> Result<FnResult, String> {
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
            Ok(FnResult {
                env: fnenv,
                ast: func.body.clone(),
                ret: None,
            })
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
    Builtin(fn(&[LustData], Rc<RefCell<LustEnv>>) -> Result<FnResult, String>),
    Fn(LustFn),
    Mac(LustFn),
}

#[derive(Clone, PartialEq)]
struct LustFn {
    params: Vec<String>,
    body: Vec<LustData>,
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
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() != 1 {
                        Err(format!("quote expects 1 arg, got {}", args.len()))
                    } else {
                        Ok(FnResult {
                            env,
                            ret: Some(args[0].clone()),
                            ast: vec![],
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "set".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() != 2 {
                        Err(format!("def expects 2 args, got {}", args.len()))
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        match target {
                            LustData::Symbol(ref s) => {
                                let val = Interpreter::eval_in_env(&args[1], env.clone())?;
                                env.borrow_mut().data.insert(s.clone(), val.clone());
                                Ok(FnResult {
                                    env: env,
                                    ret: Some(val.clone()),
                                    ast: vec![],
                                })
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
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() < 2 {
                        Err(
                            "fn expects at least two paramaters, a param list and a body"
                                .to_string(),
                        )
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body: Vec<LustData> = args[1..].iter().cloned().collect();
                        Ok(FnResult {
                            env,
                            ret: Some(LustData::Fn(LustFn { params, body })),
                            ast: vec![],
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "macro".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() < 2 {
                        Err(
                            "macro expects at least two paramaters, a param list and a body"
                                .to_string(),
                        )
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body: Vec<LustData> = args[1..].iter().cloned().collect();
                        Ok(FnResult {
                            env,
                            ret: Some(LustData::Mac(LustFn { params, body })),
                            ast: vec![],
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "if".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
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
                            Ok(FnResult {
                                env,
                                ret: None,
                                ast: vec![args[1].clone()],
                            })
                        } else {
                            Ok(FnResult {
                                env,
                                ret: None,
                                ast: vec![args[2].clone()],
                            })
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "println".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() != 1 {
                        Err("println expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        print!("{}\n", target);
                        Ok(FnResult {
                            env,
                            ret: Some(LustData::List(vec![])),
                            ast: vec![],
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "negate".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() != 1 {
                        Err("negate expects one paramater".to_string())
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        match target {
                            LustData::Number(n) => Ok(FnResult {
                                env,
                                ret: Some(LustData::Number(-n)),
                                ast: vec![],
                            }),
                            _ => Err("non numeric argument to negate".to_string()),
                        }
                    }
                },
            ),
        );

        me.data.insert(
            "add".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(FnResult {
                            env,
                            ret: Some(LustData::Number(lhs + rhs)),
                            ast: vec![],
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "lt".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() < 1 {
                        Err("add expects two paramaters".to_string())
                    } else {
                        let lhs = match Interpreter::eval_in_env(&args[0], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        let rhs = match Interpreter::eval_in_env(&args[1], env.clone())? {
                            LustData::Number(n) => n,
                            _ => return Err("non numeric argument to add".to_string()),
                        };
                        Ok(FnResult {
                            env,
                            ast: vec![],
                            ret: Some(if lhs < rhs {
                                LustData::Symbol("#t".to_string())
                            } else {
                                LustData::List(vec![])
                            }),
                        })
                    }
                },
            ),
        );

        me.data.insert(
            "eq".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<FnResult, String> {
                    if args.len() != 2 {
                        Err("eq expects two arguments".to_string())
                    } else {
                        let lhs = Interpreter::eval_in_env(&args[0], env.clone())?;
                        let rhs = Interpreter::eval_in_env(&args[1], env.clone())?;
                        Ok(FnResult {
                            env,
                            ast: vec![],
                            ret: Some(if lhs == rhs {
                                LustData::Symbol("#t".to_string())
                            } else {
                                LustData::List(vec![])
                            }),
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
