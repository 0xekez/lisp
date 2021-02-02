pub mod compiler;
pub mod conditional;
pub mod conversions;
pub mod data;
pub mod errors;
pub mod escape;
pub mod fatal;
pub mod foreign;
pub mod heap;
pub mod locals;
pub mod location;
pub mod parser;
pub mod primitives;
pub mod procedures;
pub mod reader;
pub mod renamer;
pub mod tokenbuffer;
pub mod tokenizer;

use crate::errors::Printable;
use crate::parser::ExprVal;
use crate::parser::Parser;

pub(crate) type Word = i64;
pub(crate) type UWord = u64;

/// An expression as understood by the Lust compiler.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Char(char),
    Bool(bool),
    Nil,
    List(Vec<Expr>),
    Symbol(String),
    String(std::ffi::CString),
}

impl crate::parser::Expr {
    /// Converts an Expr as understood by the parser into an Expr as
    /// understood by the compiler.
    pub(crate) fn into_expr(self) -> Result<Expr, String> {
        Ok(match self.val {
            ExprVal::Number(i) => Expr::Integer(i),
            ExprVal::Id(s) => Expr::Symbol(s),
            ExprVal::List(v) => {
                if v.is_empty() {
                    Expr::Nil
                } else {
                    Expr::List(
                        v.into_iter()
                            .map(|e| e.into_expr())
                            .collect::<Result<Vec<Expr>, String>>()?,
                    )
                }
            }
            ExprVal::String(s) => Expr::String(
                std::ffi::CString::new(s).map_err(|e| format!("unsupported string: {} ", e))?,
            ),
        })
    }
}

/// Used to indicate to the preorder traversal function rather or not
/// it ought to traverse the rest of the list.
pub(crate) enum PreorderStatus {
    /// Indicates that the traversal should continue as expected.
    Continue,
    /// Indicates that the traversal should skip the rest of the
    /// current node.
    Skip,
}

impl Expr {
    /// Performs a postorder traversal of the expr calling F on each
    /// item it encounters.
    pub(crate) fn postorder_traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&Expr),
    {
        match self {
            Expr::List(v) => {
                for e in v {
                    e.postorder_traverse(f);
                }
                f(self);
            }
            _ => f(self),
        }
    }

    /// Performs a postorder traversal of the expr calling F on each
    /// item it encounters. F may mutate the underlying expr.
    pub(crate) fn postorder_traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expr),
    {
        if let Expr::List(v) = self {
            for e in v {
                e.postorder_traverse_mut(f);
            }
        }
        f(self);
    }

    pub(crate) fn preorder_traverse_mut_res<F, E>(&mut self, f: &mut F) -> Result<PreorderStatus, E>
    where
        F: FnMut(&mut Expr) -> Result<PreorderStatus, E>,
    {
        let status = f(self)?;
        if let PreorderStatus::Skip = status {
            return Ok(status);
        }
        if let Expr::List(v) = self {
            for e in v {
                e.preorder_traverse_mut_res(f)?;
            }
        }
        Ok(PreorderStatus::Continue)
    }

    pub(crate) fn preorder_traverse<F>(&self, f: &mut F) -> PreorderStatus
    where
        F: FnMut(&Expr) -> PreorderStatus,
    {
        let status = f(self);
        if let PreorderStatus::Skip = status {
            return status;
        }
        if let Expr::List(v) = self {
            for e in v {
                e.preorder_traverse(f);
            }
        }
        PreorderStatus::Continue
    }

    pub(crate) fn preorder_traverse_mut<F>(&mut self, f: &mut F) -> PreorderStatus
    where
        F: FnMut(&mut Expr) -> PreorderStatus,
    {
        let status = f(self);
        if let PreorderStatus::Skip = status {
            return status;
        }
        if let Expr::List(v) = self {
            for e in v {
                e.preorder_traverse_mut(f);
            }
        }
        PreorderStatus::Continue
    }
}

/// Roundtrips a string by spinning up a JIT and executing it. Returns
/// the result.
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
            exprs.push(expr.into_expr()?);
        } else {
            return Err("parse error!".to_string());
        }
    }

    crate::compiler::roundtrip_program(&mut exprs)
}

/// Roundtrips a file by spinning up a JIT and executing it.
pub fn roundtrip_file(name: &str) -> Result<Expr, String> {
    let contents = std::fs::read_to_string(name).map_err(|e| e.to_string())?;
    roundtrip_string(&contents)
}

/// Some more general tests that test the entire pipeline.
#[cfg(test)]
mod tests {
    use super::*;

    fn test_string_evaluation(input: &str, expected: Expr) {
        assert_eq!(roundtrip_string(input).unwrap(), expected)
    }

    fn test_file_evaluation(filename: &str, expected: Expr) {
        assert_eq!(roundtrip_file(filename).unwrap(), expected)
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
        test_string_evaluation(input, expected);
    }

    #[test]
    fn unicode() {
        let input = r#"
(let 🚨 41)
(add1 🚨)
"#;
        let expected = Expr::Integer(42);
        test_string_evaluation(input, expected);
    }

    #[test]
    fn primitives_file() {
        let expected = Expr::Bool(true);
        test_file_evaluation("examples/primitives.lisp", expected)
    }

    #[test]
    fn cons_file() {
        let expected = Expr::Integer(10);
        test_file_evaluation("examples/cons.lisp", expected)
    }
}
