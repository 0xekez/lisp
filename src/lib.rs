pub mod conversions;

pub type Word = i64;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Char(char),
    Bool(bool),
    Nil,
}
