use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::LustData;

pub enum Inst {
    PushConst(Data),
    StoreLocal(String),
    GetLocal(String),
    Puts,
    Exit,
}

pub struct Function {
    body: Vec<Inst>,
    args: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Data {
    Num(f32),
    Char(char),
    List(Rc<Vec<Data>>),
}

// List of enviroments in the order that we've seen them. When an
// enviroment is entered it is pushed to the front of the list, when
// it is done it is removed. If a function depends on the map it will
// be moved into the function.

pub struct Executor {
    stack: Vec<Data>,
    env: HashMap<String, Data>,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            env: HashMap::new(),
        }
    }

    pub fn execute(&mut self, instructions: Vec<Inst>) -> Result<(), String> {
        let mut pc: usize = 0;
        loop {
            let inst = &instructions[pc];
            match inst {
                Inst::PushConst(data) => self.stack.push(data.clone()),
                Inst::StoreLocal(s) => {
                    self.env.insert(
                        s.clone(),
                        self.stack.pop().ok_or("empty stack".to_string())?,
                    );
                }
                Inst::GetLocal(s) => self.stack.push(
                    self.env
                        .get(s)
                        .ok_or(format!("undefined symbol: {}", s))?
                        .clone(),
                ),
                Inst::Puts => println!("{:#?}", self.stack.pop().ok_or("empty stack".to_string())?),
                Inst::Exit => break,
            }
            pc += 1;
        }
        Ok(())
    }
}

impl Data {
    fn expect_list(&self) -> Result<Rc<Vec<Self>>, String> {
        match self {
            Self::List(l) => Ok(l.clone()),
            _ => Err(format!("expected list, got {:#?}", self)),
        }
    }
}
