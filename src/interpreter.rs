use crate::builtins;
use crate::parser::{Expr, ExprVal};
use std::cell::RefCell;
use std::fmt;
use std::ops::Index;
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
        let res = Self::eval_in_env(&data, self.global_env.clone())?;

        if !res.is_empty_list() {
            println!("=> {}", res);
        }
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

                LustData::Cons(ref c) => {
                    match **c {
                        ConsCell::Nil => break Ok(currexpr),
                        ConsCell::Cons(ref c) => {
                            let fnres = Self::eval_cons(c, currentenv)?;
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
                    }
                }

                _ => break Ok(currexpr),
            }
        }
    }

    /// Determines if an expression is a call to a macro.
    fn is_macro_call(ast: &LustData, env: Rc<RefCell<LustEnv>>) -> bool {
        if let LustData::Cons(c) = ast {
            if c.len() == 0 {
                return false;
            }
            let pred = &c[0];
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

    fn eval_cons(cons: &Cons, env: Rc<RefCell<LustEnv>>) -> Result<CallResult, String> {
        let pred = Self::eval_in_env(&cons.data, env.clone())?;
        match pred {
            LustData::Builtin(ref f) => f(&*cons.next, env),
            LustData::Fn(ref f) => Self::eval_funcall(f, &*cons.next, env, true),
            LustData::Mac(ref f) => Self::eval_funcall(f, &*cons.next, env, false),
            _ => Err(format!("invalid list predicate: {}", pred)),
        }
    }

    /// Evaluates a function call. This pretty much just ammounts to
    /// installing its arguments in the enviroment.
    fn eval_funcall(
        func: &LustFn,
        args: &ConsCell,
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
                        let varadic_args = args.nth_item(i);
                        LustData::Cons(Rc::new(varadic_args.transform_fallible(
                            |item: &LustData| {
                                if eval_args {
                                    Self::eval_in_env(&item, env.clone())
                                } else {
                                    Ok(item.clone())
                                }
                            },
                        )?))
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
            ExprVal::List(ref l) => Self::list_to_cons(l),
            ExprVal::String(s) => Ok(LustData::from_string(s)),
            ExprVal::Id(s) => Ok(LustData::Symbol(Box::new(s.clone()))),
        }
    }

    fn list_to_cons(list: &Vec<Expr>) -> Result<LustData, String> {
        let mut next = Rc::new(ConsCell::Nil);
        for e in list.iter().rev() {
            let data = e.to_data()?;
            let new = Cons {
                data,
                next,
                mutable: true,
            };
            next = Rc::new(ConsCell::Cons(new));
        }
        Ok(LustData::Cons(next))
    }
}

/// A cons cell.
pub struct Cons {
    /// The data I hold.
    pub data: LustData,
    /// The next item in my list.
    pub next: Rc<ConsCell>,
    /// Is this conscell mutable?
    pub mutable: bool,
}

pub enum ConsCell {
    Nil,
    Cons(Cons),
}

// Thinking that List, Symbol, Fn, and Mac should be garbage
// collected. Other things are fine to copy around.

#[derive(Clone)]
pub enum LustData {
    /// A floating point number
    Number(f32),
    /// A cons cell
    Cons(Rc<ConsCell>),
    /// A symbol. Used to represent IDs and files in import
    /// expressions.
    Symbol(Box<String>),
    /// A character. The building block of a string.
    Char(char),
    /// A builtin function.
    Builtin(fn(&ConsCell, Rc<RefCell<LustEnv>>) -> Result<CallResult, String>),
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
        let mut res = Rc::new(ConsCell::Nil);
        for c in s.chars().rev() {
            res = Rc::new(ConsCell::push_front(res, LustData::Char(c)))
        }
        let mut quote = Rc::new(ConsCell::Nil);
        quote = Rc::new(ConsCell::push_front(quote, LustData::Cons(res)));
        quote = Rc::new(ConsCell::push_front(
            quote,
            LustData::Symbol(Box::new("quote".to_string())),
        ));

        LustData::Cons(quote)
    }

    /// Extracts a list from some data or returns an error.
    pub fn expect_cons(&self) -> Result<Rc<ConsCell>, String> {
        match self {
            LustData::Cons(ref r) => Ok(r.clone()),
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
        LustData::Cons(Rc::new(ConsCell::Nil))
    }

    pub fn is_empty_list(&self) -> bool {
        match self {
            LustData::Cons(ref c) => match **c {
                ConsCell::Nil => true,
                ConsCell::Cons(_) => false,
            },
            _ => false,
        }
    }

    pub fn deep_clone(&self, mutable: bool) -> LustData {
        match self {
            LustData::Cons(ref c) => LustData::Cons(Rc::new(
                c.transform_infallible(|item: &LustData| item.deep_clone(mutable)),
            )),
            _ => self.clone(),
        }
    }

    pub fn is_imutable(&self) -> bool {
        if let LustData::Cons(ref c) = self {
            c.is_mutable()
        } else {
            false
        }
    }

    pub fn stringify(&self) -> Option<String> {
        match self {
            LustData::Cons(ref c) => {
                let len = c.len();
                if len == 0 {
                    return None;
                }
                let mut res = String::with_capacity(len);
                for d in c.into_iter() {
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
        func: fn(&ConsCell, Rc<RefCell<LustEnv>>) -> Result<CallResult, String>,
    ) {
        self.data.push((name.to_string(), LustData::Builtin(func)));
    }

    fn new_with_defaults() -> Self {
        let mut me = Self {
            data: Vec::new(),
            outer: None,
        };

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
            (LustData::Cons(ref l), LustData::Cons(ref r)) => {
                l.len() == r.len()
                    && l.into_iter()
                        .zip(r.into_iter())
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

                Self::Cons(c) => write!(f, "({})", c),

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

impl ConsCell {
    pub fn len(&self) -> usize {
        match self {
            ConsCell::Nil => 0,
            ConsCell::Cons(ref c) => 1 + c.next.len(),
        }
    }

    pub fn push_front(target: Rc<ConsCell>, data: LustData) -> Self {
        ConsCell::Cons(Cons {
            data,
            mutable: target.is_mutable(),
            next: target,
        })
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            ConsCell::Nil => true,
            ConsCell::Cons(ref c) => c.mutable,
        }
    }

    pub fn transform_fallible<F>(&self, f: F) -> Result<Self, String>
    where
        F: Fn(&LustData) -> Result<LustData, String>,
    {
        Ok(match self {
            ConsCell::Nil => ConsCell::Nil,
            ConsCell::Cons(ref c) => ConsCell::Cons(Cons {
                data: f(&c.data)?,
                next: Rc::new(c.next.transform_fallible(f)?),
                mutable: true,
            }),
        })
    }

    pub fn transform_infallible<F>(&self, f: F) -> Self
    where
        F: Fn(&LustData) -> LustData,
    {
        match self {
            ConsCell::Nil => ConsCell::Nil,
            ConsCell::Cons(ref c) => ConsCell::Cons(Cons {
                data: f(&c.data),
                next: Rc::new(c.next.transform_infallible(f)),
                mutable: true,
            }),
        }
    }

    pub fn nth_item(&self, n: usize) -> &Self {
        match self {
            ConsCell::Nil => {
                panic!("index out of bounds");
            }
            ConsCell::Cons(ref c) => {
                if n == 0 {
                    &self
                } else {
                    c.next.nth_item(n - 1)
                }
            }
        }
    }
}

impl fmt::Display for ConsCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConsCell::Nil => write!(f, ""),
            ConsCell::Cons(ref cell) => {
                write!(f, "{}", cell.data)?;
                match *cell.next {
                    ConsCell::Cons(_) => {
                        write!(f, " {}", cell.next)
                    }
                    ConsCell::Nil => write!(f, ""),
                }
            }
        }
    }
}

impl<'a> IntoIterator for &'a ConsCell {
    type Item = &'a LustData;
    type IntoIter = ConsCellIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ConsCellIterator { cell: self }
    }
}

pub struct ConsCellIterator<'a> {
    cell: &'a ConsCell,
}

impl<'a> Iterator for ConsCellIterator<'a> {
    type Item = &'a LustData;
    fn next(&mut self) -> Option<Self::Item> {
        match self.cell {
            ConsCell::Nil => None,
            ConsCell::Cons(ref c) => {
                let data = &c.data;
                self.cell = &*c.next;
                Some(data)
            }
        }
    }
}

impl Index<usize> for ConsCell {
    type Output = LustData;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            ConsCell::Nil => {
                panic!("index out of bounds");
            }
            ConsCell::Cons(ref c) => {
                if index == 0 {
                    &c.data
                } else {
                    &c.next[index - 1]
                }
            }
        }
    }
}
