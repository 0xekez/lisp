use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::*;

use crate::compiler::emit_expr;
use crate::compiler::Context;
use crate::conversions;
use crate::heap::emit_alloc;
use crate::Expr;

// TODO: refactor to look like is_let in locals.rs
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

pub(crate) fn emit_primcall(name: &str, args: &[Expr], ctx: &mut Context) -> Result<Value, String> {
    debug_assert!(string_is_primitive(name));
    Ok(match name {
        "add1" => {
            check_arg_len("add1", args, 1)?;
            let accum = emit_expr(&args[0], ctx)?;
            ctx.builder
                .ins()
                .iadd_imm(accum, Expr::Integer(1).immediate_rep())
        }
        "integer->char" => {
            check_arg_len("integer->char", args, 1)?;

            // To convert an integer to a character we left shift by 6
            // and then tag it with the character tag.
            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().ishl_imm(accum, 6);
            let accum = ctx.builder.ins().bor_imm(accum, conversions::CHAR_TAG);
            accum
        }
        "char->integer" => {
            check_arg_len("char->integer", args, 1)?;

            // To convert a char to an integer we right shift by 6 and
            // then tag it with the integer tag.
            //
            // NOTE: We're skipping some of the work here because
            // we're assuming the input is an integer and as such
            // there is no need to tag after the right shift.
            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().ushr_imm(accum, 6);
            accum
        }
        "null?" => {
            check_arg_len("null?", args, 1)?;
            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::NIL_VALUE);
            // The result of this comparason is a boolean value so we
            // need to convert it back to a ctx.word before working on it.
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "zero?" => {
            check_arg_len("zero?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum =
                ctx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, accum, Expr::Integer(0).immediate_rep());
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "not" => {
            check_arg_len("not", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;

            // To get the not of a boolean, subtract one from it and
            // then take the absolute value.
            let accum = ctx.builder.ins().sshr_imm(accum, conversions::BOOL_SHIFT);
            let accum = ctx.builder.ins().iadd_imm(accum, -1);
            // FIXME: there is some serious black magic surrounding
            // why we don't need to take the absolute value
            // here. Taking the absolute value causes a compilation
            // error when cranelift is verifying things.
            // let accum = ctx.builder.ins().iabs(accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "integer?" => {
            check_arg_len("integer?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;

            let accum = ctx.builder.ins().band_imm(accum, conversions::FIXNUM_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::FIXNUM_TAG);
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "boolean?" => {
            check_arg_len("boolean?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;

            let accum = ctx.builder.ins().band_imm(accum, conversions::BOOL_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::BOOL_TAG);
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "pair?" => {
            check_arg_len("pair?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;

            let accum = ctx
                .builder
                .ins()
                .band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::PAIR_TAG);
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "add" => {
            check_arg_len("add", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            ctx.builder.ins().iadd(left, right)
        }
        "sub" => {
            check_arg_len("sub", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;
            let right = ctx.builder.ins().ineg(right);

            ctx.builder.ins().iadd(left, right)
        }
        "mul" => {
            check_arg_len("mul", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let accum = ctx.builder.ins().imul(left, right);

            // At this point we've picked up an extra 2^2 so we need
            // to right shift it out.
            //
            // NOTE: It is possible that it would be more reasonable
            // to shift things out first. I'm worried here that this
            // will cause integer overflows where we wouldn't normally
            // expect them.
            ctx.builder.ins().sshr_imm(accum, 2)
        }
        "eq" => {
            check_arg_len("eq", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let accum = ctx.builder.ins().icmp(IntCC::Equal, left, right);
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "cons" => {
            check_arg_len("cons", args, 2)?;

            let data = emit_expr(&args[0], ctx)?;
            let next = emit_expr(&args[1], ctx)?;

            let storage = emit_alloc((ctx.word.bytes() * 2).into(), ctx)?;

            ctx.builder.ins().store(MemFlags::new(), data, storage, 0);
            ctx.builder
                .ins()
                .store(MemFlags::new(), next, storage, ctx.word.bytes() as i32);

            ctx.builder.ins().bor_imm(storage, conversions::PAIR_TAG)
        }
        "car" => {
            check_arg_len("car", args, 1)?;

            let pair = emit_expr(&args[0], ctx)?;
            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            ctx.builder
                .ins()
                .load(ctx.word, MemFlags::new(), address, 0)
        }
        "cdr" => {
            check_arg_len("cdr", args, 1)?;

            let pair = emit_expr(&args[0], ctx)?;
            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            ctx.builder
                .ins()
                .load(ctx.word, MemFlags::new(), address, ctx.word.bytes() as i32)
        }
        _ => panic!("non primitive in emit_primcall: {}", name),
    })
}

fn emit_word_to_bool(accum: Value, builder: &mut FunctionBuilder) -> Value {
    let accum = builder.ins().ishl_imm(accum, conversions::BOOL_SHIFT);
    let accum = builder.ins().bor_imm(accum, conversions::BOOL_TAG);
    accum
}

fn string_is_primitive(s: &str) -> bool {
    s == "add1"
        || s == "integer->char"
        || s == "char->integer"
        || s == "null?"
        || s == "zero?"
        || s == "not"
        || s == "boolean?"
        || s == "integer?"
        || s == "pair?"
        || s == "add"
        || s == "sub"
        || s == "mul"
        || s == "eq"
        || s == "cons"
        || s == "car"
        || s == "cdr"
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
        assert_eq!(crate::compiler::roundtrip_expr(expr).unwrap(), expected)
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
        for c in 'a'..'z' {
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

    #[test]
    fn is_null() {
        let ast = Expr::List(vec![Expr::Symbol("null?".to_string()), Expr::Nil]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("null?".to_string()), Expr::Integer(0)]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);
    }

    #[test]
    fn is_zero() {
        let ast = Expr::List(vec![Expr::Symbol("zero?".to_string()), Expr::Nil]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("zero?".to_string()), Expr::Integer(0)]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);
    }

    #[test]
    fn not() {
        let ast = Expr::List(vec![Expr::Symbol("not".to_string()), Expr::Bool(false)]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("not".to_string()), Expr::Bool(true)]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);
    }

    #[test]
    fn is_boolean() {
        let ast = Expr::List(vec![Expr::Symbol("boolean?".to_string()), Expr::Nil]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("boolean?".to_string()), Expr::Integer(1)]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("boolean?".to_string()), Expr::Char('a')]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("boolean?".to_string()),
            Expr::Bool(false),
        ]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("boolean?".to_string()), Expr::Bool(true)]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);
    }

    #[test]
    fn is_integer() {
        let ast = Expr::List(vec![Expr::Symbol("integer?".to_string()), Expr::Nil]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("integer?".to_string()), Expr::Char('a')]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("integer?".to_string()),
            Expr::Bool(false),
        ]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![Expr::Symbol("integer?".to_string()), Expr::Bool(true)]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        for i in -10..10 {
            let ast = Expr::List(vec![Expr::Symbol("integer?".to_string()), Expr::Integer(i)]);
            let expected = Expr::Bool(true);
            test_evaluation(ast, expected);
        }
    }

    #[test]
    fn add() {
        let ast = Expr::List(vec![
            Expr::Symbol("add".to_string()),
            Expr::Integer(1),
            Expr::Integer(1),
        ]);
        let expected = Expr::Integer(2);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("add".to_string()),
            Expr::Integer(-1000),
            Expr::Integer(10),
        ]);
        let expected = Expr::Integer(-990);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("add".to_string()),
            Expr::Integer(1),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Integer(-1),
                Expr::Integer(-1),
            ]),
        ]);
        let expected = Expr::Integer(-1);
        test_evaluation(ast, expected);
    }

    #[test]
    fn mul() {
        let ast = Expr::List(vec![
            Expr::Symbol("mul".to_string()),
            Expr::Integer(1),
            Expr::Integer(1),
        ]);
        let expected = Expr::Integer(1);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("mul".to_string()),
            Expr::Integer(-1000),
            Expr::Integer(10),
        ]);
        let expected = Expr::Integer(-10000);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("mul".to_string()),
            Expr::Integer(1),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Integer(-1),
                Expr::Integer(-1),
            ]),
        ]);
        let expected = Expr::Integer(-2);
        test_evaluation(ast, expected);
    }

    #[test]
    fn sub() {
        let ast = Expr::List(vec![
            Expr::Symbol("sub".to_string()),
            Expr::Integer(1),
            Expr::Integer(1),
        ]);
        let expected = Expr::Integer(0);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("sub".to_string()),
            Expr::Integer(-1000),
            Expr::Integer(10),
        ]);
        let expected = Expr::Integer(-1010);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("sub".to_string()),
            Expr::Integer(1),
            Expr::List(vec![
                Expr::Symbol("sub".to_string()),
                Expr::Integer(-1),
                Expr::Integer(-1),
            ]),
        ]);
        let expected = Expr::Integer(1);
        test_evaluation(ast, expected);
    }

    #[test]
    fn eq() {
        let ast = Expr::List(vec![
            Expr::Symbol("eq".to_string()),
            Expr::Integer(1),
            Expr::Integer(1),
        ]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("eq".to_string()),
            Expr::Char('🚨'),
            Expr::Integer(1),
        ]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);

        let ast = Expr::List(vec![
            Expr::Symbol("eq".to_string()),
            Expr::Nil,
            Expr::List(vec![
                Expr::Symbol("sub".to_string()),
                Expr::Integer(-1),
                Expr::Integer(-1),
            ]),
        ]);
        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);
    }

    #[test]
    fn is_pair() {
        let ast = Expr::List(vec![
            Expr::Symbol("pair?".to_string()),
            Expr::List(vec![
                Expr::Symbol("cons".to_string()),
                Expr::Integer(-1),
                Expr::Integer(-1),
            ]),
        ]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);
    }

    #[test]
    fn car() {
        let ast = Expr::List(vec![
            Expr::Symbol("car".to_string()),
            Expr::List(vec![
                Expr::Symbol("cons".to_string()),
                Expr::Integer(-1),
                Expr::Integer(2),
            ]),
        ]);

        let expected = Expr::Integer(-1);
        test_evaluation(ast, expected);
    }

    #[test]
    fn cdr() {
        let ast = Expr::List(vec![
            Expr::Symbol("cdr".to_string()),
            Expr::List(vec![
                Expr::Symbol("cons".to_string()),
                Expr::Integer(-1),
                Expr::Integer(2),
            ]),
        ]);

        let expected = Expr::Integer(2);
        test_evaluation(ast, expected);
    }
}
