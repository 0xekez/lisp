//! Some constants that have values that can be determined at compile
//! time. For those we can determine their values at runtime and store
//! them in the programs data.

use crate::conversions::list_to_immediate;
use crate::Expr;
use crate::Word;

impl Expr {
    pub fn is_complex_const(&self) -> Option<Word> {
        match self {
            Expr::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    if s == "quote" {
                        Some(list_to_immediate(&v[1..]))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub struct LustData {
    pub name: String,
    pub data: Word,
}

pub(crate) fn collect_data(program: &[Expr]) -> Vec<LustData> {
    let mut res = Vec::new();

    for e in program {
        e.depth_first_traverse(&mut |e: &Expr| {
            if let Some(repr) = e.is_complex_const() {
                res.push(LustData {
                    name: format!("__anon_data_{}", res.len()),
                    data: repr,
                })
            }
        })
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parser;

    #[test]
    fn test_data_collection() {
        let source = r#"
(let foo (quote 1 2 3))

(if 1 (quote 1 2) (quote 2 3))
"#;
        let mut parser = Parser::new(source);
        let mut exprs = Vec::new();
        while parser.has_more() {
            let res = parser.parse_expr();

            if res.errors.is_empty() {
                let expr = res.expr.unwrap();
                exprs.push(expr.into_expr());
            } else {
                panic!("parse error!".to_string());
            }
        }
        let data = collect_data(&exprs);

        assert_eq!(data.len(), 3);

        assert_eq!(
            Expr::from_immediate(data[2].data),
            Expr::List(vec![
                Expr::Integer(2),
                Expr::List(vec![Expr::Integer(3), Expr::Nil])
            ])
        )
    }
}
