use std::fmt;

use crate::interpreter::LustData;

pub enum Inst {
    PushConst(LustData),
    StoreLocal(String),
    GetSym(String),
    Call,
    Puts,
    Exit,
}

pub struct Func {
    params: Vec<String>,
    body: Vec<Inst>,
}

pub enum Data {
    Func(Box<Func>),
    List(Vec<Data>),
    Num(f32),
    Char(char),
    Symbol(String),
}

pub struct Executor {
    stack: Vec<LustData>,
}

impl Executor {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn compile(data: &LustData) -> Result<Vec<Inst>, String> {
        match data {
            LustData::Symbol(s) => Ok(vec![Inst::GetSym(*s.clone())]),
            LustData::List(l) => {
                let mut res = Vec::new();
                for item in l.borrow().iter().rev() {
                    res.extend(Self::compile(item)?);
                }
                res.push(Inst::Call);
                return Ok(res);
            }
            LustData::Number(_) => Ok(vec![Inst::PushConst(data.clone())]),
            LustData::Char(_) => Ok(vec![Inst::PushConst(data.clone())]),
            LustData::Builtin(_) => Ok(vec![Inst::PushConst(data.clone())]),
            LustData::Fn(_) => Ok(vec![Inst::PushConst(data.clone())]),
            LustData::Mac(_) => Ok(vec![Inst::PushConst(data.clone())]),
        }
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inst::PushConst(d) => {
                write!(f, "PUSH_CONST {}", d)
            }
            Inst::StoreLocal(s) => {
                write!(f, "STORE_LOCAL {}", s)
            }
            Inst::GetSym(s) => {
                write!(f, "GET_LOCAL {}", s)
            }
            Inst::Puts => {
                write!(f, "PUTS")
            }
            Inst::Exit => {
                write!(f, "EXIT")
            }
            Inst::Call => {
                write!(f, "CALL")
            }
        }
    }
}
