use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::Type;
use cranelift::prelude::Value;

use crate::compiler::emit_expr;
use crate::Expr;

impl Expr {
    pub fn is_primcall(&self) -> bool {
        match self {
            Self::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    string_is_primitive(s)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn primcall_op<'a>(&'a self) -> &'a str {
        debug_assert!(self.is_primcall());
        match self {
            Self::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    &s
                } else {
                    panic!("unreachable")
                }
            }
            _ => panic!("unreachable"),
        }
    }
}

pub fn emit_primcall(
    name: &str,
    args: &[Expr],
    builder: &mut FunctionBuilder,
    word: Type,
) -> Result<Value, String> {
    debug_assert!(string_is_primitive(name));
    Ok(match name {
        "add1" => {
            check_arg_len("add1", args, 1)?;
            let res = emit_expr(&args[0], builder, word)?;
            builder
                .ins()
                .iadd_imm(res, Expr::Integer(1).immediate_rep())
        }
        "integer->char" => {
            check_arg_len("integer->char", args, 1)?;

            // To convert an integer to a character we left shift by 6
            // and then tag it with the character tag.
            let res = emit_expr(&args[0], builder, word)?;
            let res = builder.ins().ishl_imm(res, 6);
            let res = builder.ins().bor_imm(res, crate::conversions::CHAR_TAG);
            res
        }
        "char->integer" => {
            check_arg_len("char->integer", args, 1)?;

            // To convert a char to an integer we right shift by 6 and
            // then tag it with the integer tag.
            //
            // NOTE: We're skipping some of the work here because
            // we're assuming the input is an integer and as such
            // there is no need to tag after the right shift.
            let res = emit_expr(&args[0], builder, word)?;
            let res = builder.ins().ushr_imm(res, 6);
            res
        }
        _ => panic!("non primitive in emit_primcall"),
    })
}

fn string_is_primitive(s: &str) -> bool {
    s == "add1" || s == "integer->char" || s == "char->integer"
}

fn check_arg_len(name: &str, args: &[Expr], expected: usize) -> Result<(), String> {
    if args.len() != expected {
        Err(format!(
            "{} expected {} args and got {}",
            name,
            expected,
            args.len()
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_evaluation(expr: Expr, expected: Expr) {
        assert_eq!(expected, crate::compiler::roundtrip_expr(expr).unwrap())
    }

    #[test]
    fn add1() {
        let ast = Expr::List(vec![Expr::Symbol("add1".to_string()), Expr::Integer(1)]);
        let expected = Expr::Integer(2);
        test_evaluation(ast, expected);
    }

    #[test]
    fn nested_add1() {
        // (add1 (add1 (add1 1)))
        let ast = Expr::List(vec![
            Expr::Symbol("add1".to_string()),
            Expr::List(vec![
                Expr::Symbol("add1".to_string()),
                Expr::List(vec![Expr::Symbol("add1".to_string()), Expr::Integer(1)]),
            ]),
        ]);
        let expected = Expr::Integer(4);
        test_evaluation(ast, expected);
    }

    #[test]
    fn add1_comprehensive() {
        for i in -50..50 {
            let ast = Expr::List(vec![Expr::Symbol("add1".to_string()), Expr::Integer(i)]);
            let expected = Expr::Integer(i + 1);
            test_evaluation(ast, expected);
        }
    }

    #[test]
    fn integer_to_char() {
        let ast = Expr::List(vec![
            Expr::Symbol("integer->char".to_string()),
            Expr::Integer(0x2764),
        ]);
        let expected = Expr::Char('❤');
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("integer->char".to_string()),
            Expr::Integer(128175),
        ]);
        let expected = Expr::Char('💯');
        test_evaluation(ast, expected);
    }

    #[test]
    fn char_to_integer() {
        for c in 'a'..'😀' {
            let ast = Expr::List(vec![
                Expr::Symbol("integer->char".to_string()),
                Expr::List(vec![
                    Expr::Symbol("char->integer".to_string()),
                    Expr::Char(c),
                ]),
            ]);
            let expected = Expr::Char(c);
            test_evaluation(ast, expected);
        }
    }
}
