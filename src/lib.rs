pub mod compiler;
pub mod conversions;
pub mod locals;
pub mod primitives;

pub type Word = i64;
pub type UWord = u64;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Char(char),
    Bool(bool),
    Nil,
    List(Vec<Expr>),
    Symbol(String),
}
