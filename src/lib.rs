pub mod compiler;
pub mod conversions;

pub type Word = i64;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Char(char),
    Bool(bool),
    Nil,
}
