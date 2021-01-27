//! Handles primitive functions in Lust.

use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_module::Module;
use cranelift_simplejit::SimpleJITModule;

use crate::compiler::emit_expr;
use crate::compiler::Context;
use crate::compiler::JIT;
use crate::conversions;
use crate::heap::{emit_alloc, emit_alloc_bare};
use crate::procedures::LustFn;
use crate::Expr;

impl Expr {
    pub fn is_primcall(&self) -> Option<(&str, &[Expr])> {
        if let Self::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if string_is_primitive(s) {
                    return Some((s, &v[1..]));
                }
            }
        }
        None
    }
}

pub(crate) fn emit_primitive<F>(
    name: &str,
    arity: usize,
    jit: &mut JIT,
    mut body_builder: F,
) -> Result<LustFn, String>
where
    F: FnMut(&mut FunctionBuilder, Block, &mut SimpleJITModule) -> Value,
{
    let word = jit.module.target_config().pointer_type();

    // Arguments
    for _ in 0..arity {
        jit.context.func.signature.params.push(AbiParam::new(word));
    }
    // Additional closure argument
    jit.context.func.signature.params.push(AbiParam::new(word));
    // Return value
    jit.context.func.signature.returns.push(AbiParam::new(word));

    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);

    let res = body_builder(&mut builder, entry_block, &mut jit.module);

    builder.ins().return_(&[res]);

    builder.seal_all_blocks();
    builder.finalize();

    let id = jit
        .module
        .declare_function(
            name,
            cranelift_module::Linkage::Export,
            &jit.context.func.signature,
        )
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.context, &mut NullTrapSink {})
        .map_err(|e| e.to_string())?;

    jit.module.clear_context(&mut jit.context);

    Ok(LustFn {
        name: name.to_string(),
        params: (0..arity).map(|n| n.to_string()).collect(),
        body: vec![],
        free_variables: vec![],
    })
}

pub(crate) fn emit_primitives(jit: &mut JIT) -> Result<Vec<LustFn>, String> {
    let mut res = Vec::new();

    let word = jit.module.target_config().pointer_type();

    res.push(emit_primitive(
        "add1",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            builder
                .ins()
                .iadd_imm(accum, Expr::Integer(1).immediate_rep())
        },
    )?);

    res.push(emit_primitive(
        "integer->char",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            let accum = builder.ins().ishl_imm(accum, 6);
            let accum = builder.ins().bor_imm(accum, conversions::CHAR_TAG);
            accum
        },
    )?);

    res.push(emit_primitive(
        "char->integer",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            let accum = builder.ins().ushr_imm(accum, 6);
            accum
        },
    )?);

    res.push(emit_primitive(
        "null?",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            let accum = builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::NIL_VALUE);
            // The result of this comparason is a boolean value so we
            // need to convert it back to a ctx.word before working on it.
            let accum = builder.ins().bint(word, accum);
            emit_word_to_bool(accum, builder)
        },
    )?);

    res.push(emit_primitive(
        "zero?",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            let accum =
                builder
                    .ins()
                    .icmp_imm(IntCC::Equal, accum, Expr::Integer(0).immediate_rep());
            let accum = builder.ins().bint(word, accum);
            emit_word_to_bool(accum, builder)
        },
    )?);

    res.push(emit_primitive("not", 1, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let accum = args[0];
        // To get the not of a boolean, subtract one from it and
        // then take the absolute value.
        let accum = builder.ins().sshr_imm(accum, conversions::BOOL_SHIFT);
        let accum = builder.ins().iadd_imm(accum, -1);
        // FIXME: there is some serious black magic surrounding
        // why we don't need to take the absolute value
        // here. Taking the absolute value causes a compilation
        // error when cranelift is verifying things.
        // let accum = builder.ins().iabs(accum);
        emit_word_to_bool(accum, builder)
    })?);

    res.push(emit_primitive(
        "integer?",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];
            let accum = builder.ins().band_imm(accum, conversions::FIXNUM_MASK);
            let accum = builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::FIXNUM_TAG);
            let accum = builder.ins().bint(word, accum);
            emit_word_to_bool(accum, builder)
        },
    )?);

    res.push(emit_primitive(
        "boolean?",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];

            let accum = builder.ins().band_imm(accum, conversions::BOOL_MASK);
            let accum = builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::BOOL_TAG);
            let accum = builder.ins().bint(word, accum);
            emit_word_to_bool(accum, builder)
        },
    )?);

    res.push(emit_primitive(
        "pair?",
        1,
        jit,
        |builder, block, _module| {
            let args = builder.block_params(block);
            let accum = args[0];

            let accum = builder.ins().band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::PAIR_TAG);
            let accum = builder.ins().bint(word, accum);
            emit_word_to_bool(accum, builder)
        },
    )?);

    res.push(emit_primitive("add", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];

        builder.ins().iadd(left, right)
    })?);

    res.push(emit_primitive("sub", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];
        let right = builder.ins().ineg(right);

        builder.ins().iadd(left, right)
    })?);

    res.push(emit_primitive("mul", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];

        let accum = builder.ins().imul(left, right);

        // At this point we've picked up an extra 2^2 so we need
        // to right shift it out.
        //
        // NOTE: It is possible that it would be more reasonable
        // to shift things out first. I'm worried here that this
        // will cause integer overflows where we wouldn't normally
        // expect them.
        builder.ins().sshr_imm(accum, 2)
    })?);

    res.push(emit_primitive("eq", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];

        let accum = builder.ins().icmp(IntCC::Equal, left, right);
        let accum = builder.ins().bint(word, accum);
        emit_word_to_bool(accum, builder)
    })?);

    res.push(emit_primitive("lt", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];

        let accum = builder.ins().icmp(IntCC::SignedLessThan, left, right);
        let accum = builder.ins().bint(word, accum);
        emit_word_to_bool(accum, builder)
    })?);

    res.push(emit_primitive("gt", 2, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let left = args[0];
        let right = args[1];

        let accum = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
        let accum = builder.ins().bint(word, accum);
        emit_word_to_bool(accum, builder)
    })?);

    res.push(emit_primitive("cons", 2, jit, |builder, block, module| {
        let args = builder.block_params(block);
        let data = args[0];
        let next = args[1];

        let storage = emit_alloc_bare((word.bytes() * 2).into(), builder, module);

        builder.ins().store(MemFlags::new(), data, storage, 0);
        builder
            .ins()
            .store(MemFlags::new(), next, storage, word.bytes() as i32);

        builder.ins().bor_imm(storage, conversions::PAIR_TAG)
    })?);

    res.push(emit_primitive("car", 1, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let pair = args[0];

        let address = builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

        builder.ins().load(word, MemFlags::new(), address, 0)
    })?);

    res.push(emit_primitive("cdr", 1, jit, |builder, block, _module| {
        let args = builder.block_params(block);
        let pair = args[0];

        let address = builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

        builder
            .ins()
            .load(word, MemFlags::new(), address, word.bytes() as i32)
    })?);

    Ok(res)
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
        "lt" => {
            check_arg_len("lt", args, 2)?;
            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let accum = ctx.builder.ins().icmp(IntCC::SignedLessThan, left, right);
            let accum = ctx.builder.ins().bint(ctx.word, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "gt" => {
            check_arg_len("gt", args, 2)?;
            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let accum = ctx
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, left, right);
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

pub(crate) fn string_is_builtin(s: &str) -> bool {
    string_is_primitive(s) || s == "if" || s == "quote" || s == "let" || s == "fn" || s == "set"
}

pub(crate) fn string_is_primitive(s: &str) -> bool {
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
        || s == "lt"
        || s == "gt"
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
    use crate::roundtrip_string;

    use super::*;

    fn test_evaluation(expr: Expr, expected: Expr) {
        assert_eq!(
            crate::compiler::roundtrip_program(&mut [expr]).unwrap(),
            expected
        )
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
        for i in -10..10 {
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
        for c in 'm'..'q' {
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

    #[test]
    fn car_cdr_car() {
        let ast = Expr::List(vec![
            Expr::Symbol("car".to_string()),
            Expr::List(vec![
                Expr::Symbol("cdr".to_string()),
                Expr::List(vec![
                    Expr::Symbol("cons".to_string()),
                    Expr::Integer(1),
                    Expr::List(vec![
                        Expr::Symbol("cons".to_string()),
                        Expr::Integer(2),
                        Expr::Integer(3),
                    ]),
                ]),
            ]),
        ]);
        let expected = Expr::Integer(2);
        test_evaluation(ast, expected);
    }

    #[test]
    fn car_cdr_car_mix() {
        let ast = Expr::List(vec![
            Expr::Symbol("car".to_string()),
            Expr::List(vec![
                Expr::Symbol("cdr".to_string()),
                Expr::List(vec![
                    Expr::Symbol("cons".to_string()),
                    Expr::Char('a'),
                    Expr::List(vec![
                        Expr::Symbol("cons".to_string()),
                        Expr::Bool(true),
                        Expr::Integer(2),
                    ]),
                ]),
            ]),
        ]);
        let expected = Expr::Bool(true);
        test_evaluation(ast, expected);
    }

    #[test]
    fn tree() {
        let ast = Expr::List(vec![
            Expr::Symbol("car".to_string()),
            Expr::List(vec![
                Expr::Symbol("car".to_string()),
                Expr::List(vec![
                    Expr::Symbol("cons".to_string()),
                    Expr::List(vec![
                        Expr::Symbol("cons".to_string()),
                        Expr::Bool(false),
                        Expr::Integer(2),
                    ]),
                    Expr::List(vec![
                        Expr::Symbol("cons".to_string()),
                        Expr::Bool(true),
                        Expr::Integer(2),
                    ]),
                ]),
            ]),
        ]);

        let expected = Expr::Bool(false);
        test_evaluation(ast, expected);
    }

    #[test]
    fn higher_order_builtin_assignment() {
        let source = r#"
(let addd add)
(addd 10 10)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Integer(20), res)
    }

    #[test]
    fn higher_order_builtin_in_list() {
        let source = r#"
(let pair (cons add sub))
((car pair) ((cdr pair) 5 4) 41)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Integer(42), res)
    }

    #[test]
    fn higher_order_builtin_as_arg() {
        let source = r#"
(let call (fn (f a) (f a)))
(call add1 1)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Integer(2), res)
    }
}
