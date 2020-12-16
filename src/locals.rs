use cranelift::prelude::*;
use std::collections::HashMap;

use crate::compiler::emit_expr;
use crate::Expr;

impl Expr {
    // Nice lifetime inference!
    pub fn is_let(&self) -> Option<(&String, &Expr)> {
        match self {
            Self::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    if s == "let" && v.len() == 3 {
                        if let Expr::Symbol(s) = &v[1] {
                            Some((s, &v[2]))
                        } else {
                            None
                        }
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

pub fn emit_let(
    name: &str,
    val: &Expr,
    builder: &mut FunctionBuilder,
    word: Type,
    env: &mut HashMap<String, Variable>,
) -> Result<Value, String> {
    if let Some(_) = env.get(name) {
        return Err(format!("variable {} is declared more than once", name));
    }

    let val = emit_expr(val, builder, word, env)?;
    let index = env.len();

    let var = Variable::new(index);
    // Declare its type.
    builder.declare_var(var, word);
    // Set its value.
    builder.def_var(var, val);
    env.insert(name.to_string(), var);

    Ok(builder.use_var(var))
}

pub fn emit_var_access(
    name: &str,
    builder: &mut FunctionBuilder,
    env: &mut HashMap<String, Variable>,
) -> Result<Value, String> {
    match env.get(name) {
        Some(v) => Ok(builder.use_var(*v)),
        None => Err(format!("use of undeclared variable: {}", name)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_evaluation(exprs: &[Expr], expected: Expr) {
        assert_eq!(crate::compiler::roundtrip_exprs(exprs).unwrap(), expected)
    }

    #[test]
    fn let_return() {
        let ast = [Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::Symbol("tel".to_string()),
            Expr::Integer(10),
        ])];
        let expected = Expr::Integer(10);
        test_evaluation(&ast, expected);

        let ast = [Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::Symbol("tel".to_string()),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("ttel".to_string()),
                Expr::Char('🥺'),
            ]),
        ])];
        let expected = Expr::Char('🥺');
        test_evaluation(&ast, expected);
    }

    #[test]
    fn naked_var() {
        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::Symbol("tel".to_string()),
        ];
        let expected = Expr::Integer(10);
        test_evaluation(&ast, expected);

        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::List(vec![
                    Expr::Symbol("let".to_string()),
                    Expr::Symbol("🚨".to_string()),
                    Expr::Char('🥺'),
                ]),
            ]),
            Expr::Symbol("🚨".to_string()),
        ];
        let expected = Expr::Char('🥺');
        test_evaluation(&ast, expected);
    }

    #[test]
    fn clothed_var() {
        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Integer(1),
                Expr::Symbol("tel".to_string()),
            ]),
        ];
        let expected = Expr::Integer(11);
        test_evaluation(&ast, expected);
    }

    #[test]
    fn var_double_use() {
        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Symbol("tel".to_string()),
            ]),
        ];
        let expected = Expr::Integer(20);
        test_evaluation(&ast, expected);
    }

    #[test]
    fn lots_of_vars() {
        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel1".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel2".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel3".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel4".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel5".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel6".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel7".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel8".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel9".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Symbol("tel8".to_string()),
                Expr::Symbol("tel9".to_string()),
            ]),
        ];
        let expected = Expr::Integer(20);
        test_evaluation(&ast, expected);
    }

    #[test]
    #[should_panic(expected = "variable twice is declared more than once")]
    fn illegal_redef() {
        let ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("twice".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("twice".to_string()),
                Expr::Integer(10),
            ]),
        ];
        crate::compiler::roundtrip_exprs(&ast).unwrap();
    }
}
