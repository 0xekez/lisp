use crate::parser::{Expr, ExprVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// An interpreter for Lust code.
pub struct Interpreter {
    /// The global enviroment in which functions are evlauted.
    global_env: Rc<RefCell<LustEnv>>,
}

/// The result of calling a function. If the function is a builtin the
/// result will be a return value, if it is a user defined function
/// then the result will be a new enviroment and expression to
/// evaluate in that enviroment.
enum CallResult {
    /// A returned value.
    Ret(LustData),
    /// A new enviroment and data to evalute in it.
    Call(Rc<RefCell<LustEnv>>, LustData),
}

impl Interpreter {
    /// Builds a new interpreter with all of Lust's builtin functions
    /// installed.
    pub fn new() -> Self {
        Self {
            global_env: LustEnv::new(),
        }
    }

    /// Evlalutes an expression from the parser. The expression is
    /// first stripped of location data and then evaluated.
    pub fn eval(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        Self::eval_in_env(&data, self.global_env.clone())?;
        Ok(())
    }

    /// Evaluates an expression and then prints the result. Used by the
    /// repl.
    pub fn eval_print(&mut self, expr: &Expr) -> Result<(), String> {
        let data = expr.to_data()?;

        println!("=> {}", Self::eval_in_env(&data, self.global_env.clone())?);
        Ok(())
    }

    /// Evaluates an expression in the given enviroment.
    fn eval_in_env(expr: &LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
        // The current enviroment we're evaluating in.
        let currentenv = env;
        let currexpr = Self::macroexpand(expr.clone(), currentenv.clone())?;
        Self::eval_without_expansion(currexpr, currentenv)
    }

    /// Evaluates an expression witout performing macro expansion.
    fn eval_without_expansion(
        mut currexpr: LustData,
        mut currentenv: Rc<RefCell<LustEnv>>,
    ) -> Result<LustData, String> {
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
                        // If this is a call of a user-defined
                        // expression we perform a tail call by
                        // replacing the enviroment and expression
                        // that we're evlauting with the returned
                        // ones.
                        CallResult::Call(env, expr) => {
                            currentenv = env;
                            currexpr = expr;
                        }
                    }
                }
                _ => break Ok(currexpr),
            }
        }
    }

    /// Determines if an expression is a call to a macro.
    fn is_macro_call(ast: &LustData, env: Rc<RefCell<LustEnv>>) -> bool {
        if let LustData::List(ast) = ast {
            if ast.len() == 0 {
                return false;
            }
            let pred = &ast[0];
            match pred {
                LustData::Symbol(ref s) => match env.borrow().resolve(s) {
                    Ok(data) => {
                        if let LustData::Mac(_) = data {
                            true
                        } else {
                            false
                        }
                    }
                    Err(_) => false,
                },
                LustData::Mac(_) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Expands an expression if it is a macro.
    fn macroexpand(mut ast: LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
        loop {
            if !Self::is_macro_call(&ast, env.clone()) {
                break Ok(ast.clone());
            }
            ast = Self::eval_without_expansion(ast, env.clone())?;
        }
    }

    /// Evaluates a list.
    fn eval_list(list: &Vec<LustData>, env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
        let pred = Self::eval_in_env(list.first().unwrap(), env.clone())?;
        match pred {
            LustData::Builtin(ref f) => f(&list[1..], env),
            LustData::Fn(ref lf) => Self::eval_funcall(lf, &list[1..], env),
            LustData::Mac(ref lm) => Self::eval_funcall(lm, &list[1..], env),
            _ => Err(format!("invalid list predicate: {}", pred)),
        }
    }

    /// Evaluates a function call. This pretty much just ammounts to
    /// installing its arguments in the enviroment.
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
    Fn(Rc<LustFn>),
    Mac(Rc<LustFn>),
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

        // Quotes the next expression. The result of evaluating a
        // quoted expression is the expression. For example, (eval
        // (quote foo)) yields the value of foo in the current scope.
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

        // Creates a new list that is the result of evaluating all of
        // its arguments. For example (list 1 2 3) => (1 2 3)
        me.data.insert(
            "list".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    let mut res = Vec::new();
                    for arg in args {
                        res.push(Interpreter::eval_in_env(arg, env.clone())?);
                    }
                    Ok(CallResult::Ret(LustData::List(res)))
                },
            ),
        );

        // Returns the first item in a list if it exists otherwise
        // returns ().
        me.data.insert(
            "first".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        return Err("first expects one argument".to_string());
                    }
                    match Interpreter::eval_in_env(&args[0], env.clone())? {
                        LustData::List(ref v) => {
                            let val = match v.first() {
                                Some(d) => d.clone(),
                                None => LustData::List(vec![]),
                            };
                            Ok(CallResult::Ret(val))
                        }
                        _ => Err("first called on non list type".to_string()),
                    }
                },
            ),
        );

        // Returns all but the first element in a list an () if there
        // are no more elements.
        me.data.insert(
            "rest".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        return Err("rest expects one argument".to_string());
                    }
                    match Interpreter::eval_in_env(&args[0], env.clone())? {
                        LustData::List(ref v) => {
                            let val = LustData::List(match v.split_first() {
                                Some((_, rest)) => rest.to_vec(),
                                None => vec![],
                            });
                            Ok(CallResult::Ret(val))
                        }
                        _ => Err("first called on non list type".to_string()),
                    }
                },
            ),
        );

        // Evaluates each expression in its arguments in sequence.
        me.data.insert(
            "do".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    let mut res = LustData::List(vec![]);
                    for arg in args {
                        res = Interpreter::eval_in_env(arg, env.clone())?;
                    }
                    Ok(CallResult::Ret(res))
                },
            ),
        );

        // Evaluates its first argument. If that evaluates to the
        // empty list evlautes its third argument and returns the
        // result, otherwise evaluates its second argument and returns
        // the result.
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
                        if cond {
                            Ok(CallResult::Call(env, args[1].clone()))
                        } else {
                            Ok(CallResult::Call(env, args[2].clone()))
                        }
                    }
                },
            ),
        );

        // Evaluates its argument. This can be used to unwrap quoted forms.
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

        // Set's a variable in the global scope.
        me.data.insert(
            "set".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err(format!("set expects 2 args, got {}", args.len()))
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        match target {
                            LustData::Symbol(ref s) => {
                                let val = Interpreter::eval_in_env(&args[1], env.clone())?;
                                env.borrow_mut().set_global(s.clone(), &val);
                                Ok(CallResult::Ret(val))
                            }
                            _ => Err("target of set expression must be a symbol".to_string()),
                        }
                    }
                },
            ),
        );

        // Binds its the result of evaluating its first argument to
        // the result of evaluating its second argument in the current
        // scope. New scopes are created when functons are called.
        me.data.insert(
            "let".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err(format!("let expects 2 args, got {}", args.len()))
                    } else {
                        let target = Interpreter::eval_in_env(&args[0], env.clone())?;
                        match target {
                            LustData::Symbol(ref s) => {
                                let val = Interpreter::eval_in_env(&args[1], env.clone())?;
                                env.borrow_mut().data.insert(s.clone(), val.clone());
                                Ok(CallResult::Ret(val))
                            }
                            _ => Err("target of set expression must be a symbol".to_string()),
                        }
                    }
                },
            ),
        );

        // Creates a new function where the first argument is a list
        // of paramaters and the second its body. When a function is
        // called it creates a new enviroment with its arguments
        // installed with a parent enviroment of the calling
        // enviroment.
        me.data.insert(
            "fn".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err("fn expects two paramaters, a param list and a body".to_string())
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body = args[1].clone();
                        Ok(CallResult::Ret(LustData::Fn(Rc::new(LustFn {
                            params,
                            body,
                        }))))
                    }
                },
            ),
        );

        // Creates a macro function. Macro functions run before
        // evaluation and can be used to add special syntatic forms.
        me.data.insert(
            "macro".to_string(),
            LustData::Builtin(
                |args: &[LustData], _env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 2 {
                        Err("fn expects two paramaters, a param list and a body".to_string())
                    } else {
                        let params = Self::collect_param_list(&args[0])?;
                        let body = args[1].clone();
                        Ok(CallResult::Ret(LustData::Mac(Rc::new(LustFn {
                            params,
                            body,
                        }))))
                    }
                },
            ),
        );

        // Expands its argument. If the argument is not a macro
        // invocation returns its argument, if it is a macro
        // invocation, invokes the macro and displays the result
        // without evaluating it like it regularally would be. This
        // can be very handy for debugging macros.
        me.data.insert(
            "macroexpand".to_string(),
            LustData::Builtin(
                |args: &[LustData], env: Rc<RefCell<LustEnv>>| -> Result<CallResult, String> {
                    if args.len() != 1 {
                        Err("macroexpand only expects one argument".to_string())
                    } else {
                        Ok(CallResult::Ret(Interpreter::macroexpand(
                            args[0].clone(),
                            env,
                        )?))
                    }
                },
            ),
        );

        // Prints its first argument to the console followed by a
        // newline.
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

        // Negates its first argument. This will make a numeric
        // argument negative and error if its argument is non-numeric.
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

        // Adds its first argument to its second argument and returns
        // the result.
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

        // Subtracts its first argument from its second argument and
        // returns the result.
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

        // Compares its first argument to its second argument. If its
        // first argument is smaller, returns a truthy value. Returns
        // a falsy one otherwise.
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

        // Compares its first argument to its second argument. If they
        // are the same returns a truthy value, otherwise returns a
        // falsey one.
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

    pub fn set_global(&mut self, id: String, val: &LustData) -> Option<LustData> {
        match self.outer {
            Some(ref outer) => outer.borrow_mut().set_global(id, val),
            None => self.data.insert(id, val.clone()),
        }
    }
}

impl PartialEq for LustData {
    fn eq(&self, other: &Self) -> bool {
        match (&self, other) {
            (LustData::Number(l), LustData::Number(r)) => l == r,
            (LustData::Symbol(ref l), LustData::Symbol(ref r)) => l == r,
            (LustData::List(ref l), LustData::List(ref r)) => {
                l.len() == r.len() && l.iter().zip(r.iter()).all(|(lhs, rhs)| lhs == rhs)
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
                write!(f, "(macro ")?;
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
