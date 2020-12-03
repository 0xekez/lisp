use crate::builtins;
use crate::lustvec::LustVec;
use crate::parser::{Expr, ExprVal};
use rev_slice::RevSlice;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

/// An interpreter for Lust code.
pub struct Interpreter {
    /// The global enviroment in which functions are evlauted.
    pub global_env: Rc<RefCell<LustEnv>>,
}

/// The result of calling a function. If the function is a builtin the
/// result will be a return value, if it is a user defined function
/// then the result will be a new enviroment and expression to
/// evaluate in that enviroment.
pub enum CallResult {
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
    pub fn eval_in_env(expr: &LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
        // The current enviroment we're evaluating in.
        let currentenv = env;
        let currexpr = Self::macroexpand(expr.clone(), currentenv.clone())?;

        Self::eval_expanded(currexpr, currentenv)
    }

    /// Evaluates an expanded expression. Expanded meaning that
    /// macroexpand has already been called on it.
    fn eval_expanded(
        mut currexpr: LustData,
        mut currentenv: Rc<RefCell<LustEnv>>,
    ) -> Result<LustData, String> {
        loop {
            match currexpr {
                LustData::Symbol(ref s) => break currentenv.borrow().resolve(s),
                LustData::List(ref v) => {
                    // Empty list does not result in function call.
                    if v.borrow().len() == 0 {
                        break Ok(currexpr);
                    }
                    let fnres = Self::eval_list(&v.borrow(), currentenv)?;
                    match fnres {
                        CallResult::Ret(v) => break Ok(v),
                        // If this is a call of a user-defined
                        // expression we perform a tail call by
                        // replacing the enviroment and expression
                        // that we're evlauting with the returned
                        // ones.
                        CallResult::Call(env, expr) => {
                            currentenv = env;
                            // Need to expand if the new expression is
                            // a macro
                            currexpr = Self::macroexpand(expr, currentenv.clone())?;
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
            if ast.borrow().len() == 0 {
                return false;
            }
            let pred = &ast.borrow()[0];
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
    pub fn macroexpand(mut ast: LustData, env: Rc<RefCell<LustEnv>>) -> Result<LustData, String> {
        loop {
            if !Self::is_macro_call(&ast, env.clone()) {
                break Ok(ast);
            }
            ast = Self::eval_expanded(ast, env.clone())?;
        }
    }

    /// Evaluates a list.
    fn eval_list(
        list: &LustVec<LustData>,
        env: Rc<RefCell<LustEnv>>,
    ) -> Result<CallResult, String> {
        let pred = Self::eval_in_env(list.first().unwrap(), env.clone())?;
        match pred {
            LustData::Builtin(ref f) => f(list.remove_first_view(), env),
            LustData::Fn(ref lf) => Self::eval_funcall(lf, list.remove_first_view(), env, true),
            LustData::Mac(ref lm) => Self::eval_funcall(lm, list.remove_first_view(), env, false),
            _ => Err(format!("invalid list predicate: {}", pred)),
        }
    }

    /// Evaluates a function call. This pretty much just ammounts to
    /// installing its arguments in the enviroment.
    fn eval_funcall(
        func: &LustFn,
        args: &RevSlice<LustData>,
        env: Rc<RefCell<LustEnv>>,
        eval_args: bool,
    ) -> Result<CallResult, String> {
        if (func.is_varadic() && args.len() < func.get_min_param_count())
            || (!func.is_varadic() && args.len() != func.params.len())
        {
            if func.is_varadic() {
                Err(format!(
                    "wrong number of arguments for function call. got {} and expected at least {}",
                    args.len(),
                    func.params.len() - 1 // Minus one to offset for & argument
                ))
            } else {
                Err(format!(
                    "wrong number of arguments for function call. got {} and expected {}",
                    args.len(),
                    func.get_min_param_count()
                ))
            }
        } else {
            let fnenv = LustEnv::new();

            for (i, param) in func.params.iter().enumerate() {
                if param == "&" {
                    let bind = func.params[i + 1].clone();
                    let val = if i >= args.len() {
                        LustData::get_empty_list()
                    } else {
                        let res = Rc::new(RefCell::new(LustVec::with_len(args.len() - i)));
                        for j in i..args.len() {
                            let e = &args[j];
                            let arg = if eval_args {
                                Self::eval_in_env(e, env.clone())?
                            } else {
                                e.clone()
                            };
                            // Honestly not totally sure why we have
                            // to install backwards here but works for
                            // me.
                            let index = res.borrow().len() - 1 - (j - i);
                            (&mut res.borrow_mut())[index] = arg;
                        }
                        LustData::List(res)
                    };
                    fnenv.borrow_mut().insert(bind, val);
                    break;
                }
                let arg = if eval_args {
                    Self::eval_in_env(&args[i], env.clone())?
                } else {
                    args[i].clone()
                };
                fnenv.borrow_mut().insert(param.clone(), arg);
            }

            fnenv.borrow_mut().outer = Some(func.env.clone());
            Ok(CallResult::Call(fnenv, func.body.clone()))
        }
    }
}

impl Expr {
    fn to_data(&self) -> Result<LustData, String> {
        match &self.val {
            ExprVal::Number(f) => Ok(LustData::Number(*f)),
            ExprVal::List(ref v) => {
                let res = Rc::new(RefCell::new(LustVec::with_capacity(v.len())));
                for e in v.iter().rev() {
                    let data = e.to_data()?;
                    res.borrow_mut().push_front(data);
                }
                Ok(LustData::List(res))
            }
            ExprVal::String(s) => Ok(LustData::from_string(s)),
            ExprVal::Id(s) => Ok(LustData::Symbol(Box::new(s.clone()))),
        }
    }
}

// Thinking that List, Symbol, Fn, and Mac should be garbage
// collected. Other things are fine to copy around.

#[derive(Clone)]
pub enum LustData {
    /// A floating point number
    Number(f32),
    /// A list.
    List(Rc<RefCell<LustVec<LustData>>>),
    /// A symbol. Used to represent IDs and files in import
    /// expressions.
    Symbol(Box<String>),
    /// A character. The building block of a string.
    Char(char),
    /// A builtin function.
    Builtin(fn(&RevSlice<LustData>, Rc<RefCell<LustEnv>>) -> Result<CallResult, String>),
    /// A user defined function.
    Fn(Box<LustFn>),
    /// A user defined macro. Macros differ from functions in that
    /// their arguments are implicitly quoted and that they are
    /// evlauted at compile time.
    Mac(Box<LustFn>),
}

impl Default for LustData {
    fn default() -> Self {
        LustData::Number(0.0)
    }
}

#[derive(Clone)]
pub struct LustFn {
    pub params: Vec<String>,
    pub body: LustData,
    pub env: Rc<RefCell<LustEnv>>,
}

pub struct LustEnv {
    data: Vec<(String, LustData)>,
    outer: Option<Rc<RefCell<LustEnv>>>,
}

impl LustData {
    pub fn from_string(s: &str) -> LustData {
        let res = Rc::new(RefCell::new(LustVec::with_capacity(s.len())));
        for c in s.chars().rev() {
            res.borrow_mut().push_front(LustData::Char(c))
        }
        LustData::List(res)
    }

    /// Extracts a list from some data or returns an error.
    pub fn expect_list(&self) -> Result<Rc<RefCell<LustVec<LustData>>>, String> {
        match self {
            LustData::List(ref v) => Ok(v.clone()),
            _ => Err(format!("expected list, got {}", self)),
        }
    }

    /// Extracts a symbol from some data or returns an error.
    pub fn expect_symbol<'a>(&'a self) -> Result<&'a String, String> {
        match self {
            LustData::Symbol(ref s) => Ok(s),
            _ => Err(format!("expected symbol, got {}", self)),
        }
    }

    /// Extracts a number from some data or returns an error.
    pub fn expect_num(&self) -> Result<f32, String> {
        match self {
            LustData::Number(f) => Ok(*f),
            _ => Err(format!("expected number, got {}", self)),
        }
    }

    pub fn expect_char(&self) -> Result<char, String> {
        match self {
            LustData::Char(c) => Ok(*c),
            _ => Err(format!("expected number, got {}", self)),
        }
    }

    /// Gets an empty list.
    pub fn get_empty_list() -> LustData {
        LustData::List(Rc::new(RefCell::new(LustVec::new())))
    }

    pub fn deep_clone(&self) -> LustData {
        match self {
            LustData::List(l) => {
                let res = Rc::new(RefCell::new(LustVec::with_capacity(l.borrow().len())));
                if l.borrow().mutable {
                    res.borrow_mut().mutable = true;
                }
                for e in l.borrow().iter().rev() {
                    res.borrow_mut().push_front(e.deep_clone());
                }
                LustData::List(res)
            }
            _ => self.clone(),
        }
    }

    pub fn make_imutable(&self) {
        if let LustData::List(l) = self {
            l.borrow_mut().mutable = false;
            for expr in l.borrow().iter() {
                expr.make_imutable();
            }
        }
    }
    pub fn make_mutable(&self) {
        if let LustData::List(l) = self {
            l.borrow_mut().mutable = true;
            for expr in l.borrow().iter() {
                expr.make_mutable();
            }
        }
    }

    pub fn is_imutable(&self) -> bool {
        if let LustData::List(l) = self {
            !l.borrow().mutable
        } else {
            false
        }
    }

    pub fn stringify(&self) -> Option<String> {
        match self {
            LustData::List(l) => {
                if l.borrow().len() == 0 {
                    return None;
                }
                let mut res = String::with_capacity(l.borrow().len());
                for d in l.borrow().iter() {
                    let c = match d.expect_char() {
                        Ok(c) => c,
                        Err(_) => return None,
                    };
                    res.push(c);
                }
                Some(res)
            }
            _ => None,
        }
    }
}

impl LustFn {
    pub fn get_min_param_count(&self) -> usize {
        if self.is_varadic() {
            self.params.len() - 2
        } else {
            self.params.len()
        }
    }

    pub fn is_varadic(&self) -> bool {
        self.params.iter().rev().any(|i| *i == "&")
    }
}

impl LustEnv {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::new_with_defaults()))
    }

    fn install_builtin(
        &mut self,
        name: &str,
        func: fn(&RevSlice<LustData>, Rc<RefCell<LustEnv>>) -> Result<CallResult, String>,
    ) {
        self.data.push((name.to_string(), LustData::Builtin(func)));
    }

    fn new_with_defaults() -> Self {
        let mut me = Self {
            data: Vec::new(),
            outer: None,
        };

        me.install_builtin("dis", builtins::dis);
        me.install_builtin("quote", builtins::quote);
        me.install_builtin("quaziquote", builtins::quaziquote);
        me.install_builtin("car", builtins::car);
        me.install_builtin("cdr", builtins::cdr);
        me.install_builtin("cons", builtins::cons);
        me.install_builtin("if", builtins::if_);
        me.install_builtin("eval", builtins::eval);
        me.install_builtin("let", builtins::let_);
        me.install_builtin("fn", builtins::fn_);
        me.install_builtin("error", builtins::error);
        me.install_builtin("macro", builtins::macro_);
        me.install_builtin("macroexpand", builtins::macroexpand);
        me.install_builtin("println", builtins::println_);
        me.install_builtin("print", builtins::print_);
        me.install_builtin("import", builtins::import);
        me.install_builtin("negate", builtins::negate);
        me.install_builtin("add", builtins::add);
        me.install_builtin("sub", builtins::sub);
        me.install_builtin("mul", builtins::mul);
        me.install_builtin("div", builtins::div);
        me.install_builtin("lt", builtins::lt);
        me.install_builtin("gt", builtins::gt);
        me.install_builtin("eq", builtins::eq);

        me
    }

    // These functions don't remove old definitions from the
    // enviroment if a symbol is redefined. Instead, symbols are added
    // to the back of the enviroment and when resolving something we
    // resolve back to front.
    //
    // This is all based on the assumption that most enviroments are
    // small and short lived so we're best off keeping overhead for
    // their creation as small as possible.

    pub fn resolve(&self, id: &str) -> Result<LustData, String> {
        match self.data.iter().rev().find(|x| x.0 == id) {
            Some(data) => Ok(data.1.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow().resolve(id),
                None => Err(format!("failed to resolve identifier {}", id)),
            },
        }
    }

    pub fn insert(&mut self, id: String, val: LustData) {
        self.data.push((id, val.clone()));
    }

    pub fn extend(&mut self, other: &Self) {
        self.data.extend(other.data.clone())
    }
}

impl PartialEq for LustData {
    fn eq(&self, other: &Self) -> bool {
        match (&self, other) {
            (LustData::Number(l), LustData::Number(r)) => l == r,
            (LustData::Symbol(ref l), LustData::Symbol(ref r)) => l == r,
            (LustData::List(ref l), LustData::List(ref r)) => {
                l.borrow().len() == r.borrow().len()
                    && l.borrow()
                        .iter()
                        .zip(r.borrow().iter())
                        .all(|(lhs, rhs)| lhs == rhs)
            }
            (LustData::Char(l), LustData::Char(r)) => l == r,
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
        if let Some(s) = self.stringify() {
            write!(f, "\"{}\"", s)
        } else {
            match self {
                Self::Number(n) => write!(f, "{}", n),
                Self::Char(c) => write!(f, "'{}'", c),

                Self::List(l) => {
                    if l.borrow().len() == 0 {
                        return write!(f, "()");
                    }
                    write!(f, "(")?;
                    for i in 0..(l.borrow().len() - 1) {
                        write!(f, "{} ", &l.borrow()[i])?;
                    }
                    write!(f, "{})", (&l.borrow())[l.borrow().len() - 1])
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
}
