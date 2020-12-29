pub mod compiler;
pub mod conditional;
pub mod conversions;
pub mod errors;
pub mod heap;
pub mod locals;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod reader;
pub mod tokenbuffer;
pub mod tokenizer;

use crate::errors::Printable;
use crate::parser::ExprVal;
use crate::parser::Parser;

pub(crate) type Word = i64;
pub(crate) type UWord = u64;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Char(char),
    Bool(bool),
    Nil,
    List(Vec<Expr>),
    Symbol(String),
}

impl crate::parser::Expr {
    pub(crate) fn into_expr(self) -> Expr {
        match self.val {
            ExprVal::Number(i) => Expr::Integer(i),
            ExprVal::Id(s) => Expr::Symbol(s),
            ExprVal::List(v) => Expr::List(v.into_iter().map(|e| e.into_expr()).collect()),
            ExprVal::String(s) => {
                Expr::List(s.chars().into_iter().map(|c| Expr::Char(c)).collect())
            }
        }
    }
}

pub fn roundtrip_string(input: &str) -> Result<Expr, String> {
    let mut parser = Parser::new(input);
    let mut exprs = Vec::new();
    while parser.has_more() {
        let res = parser.parse_expr();

        for e in &res.errors {
            e.show(input, "anonymous");
        }
        if res.errors.is_empty() {
            let expr = res.expr.unwrap();
            exprs.push(expr.into_expr());
        } else {
            return Err("parse error!".to_string());
        }
    }

    crate::compiler::roundtrip_exprs(&exprs)
}

/// Some more general tests that test the entire pipeline.
#[cfg(test)]
mod tests {
    use super::*;

    fn test_evaluation(input: &str, expected: Expr) {
        assert_eq!(roundtrip_string(input).unwrap(), expected)
    }

    #[test]
    fn variables() {
        let input = r#"
(let ten 10)
(let eleven (add1 ten))
(let twelve (add1 eleven))

(let six 6)
(let also-six (sub twelve six))

(eq six also-six)
"#;
        let expected = Expr::Bool(true);
        test_evaluation(input, expected);
    }

    #[test]
    fn unicode() {
        let input = r#"
(let 🚨 41)
(add1 🚨)
"#;
        let expected = Expr::Integer(42);
        test_evaluation(input, expected);
    }
}
