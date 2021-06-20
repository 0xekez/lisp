//! Handles primitive functions in Lust. For primitives that are only
//! used in the head position of a list we don't need to create a
//! higher order version of them and can just emit them inline. For
//! that we use the emit_primcall function. For primitives that are
//! used in a higher order context we don't have such a luxury and
//! need to actually make a function.

use std::collections::HashMap;
use std::collections::HashSet;

use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_module::Module;

use crate::compiler::emit_expr;
use crate::compiler::Context;
use crate::compiler::JIT;
use crate::conversions;
use crate::fatal;
use crate::fatal::emit_check_arg_count;
use crate::heap::emit_alloc;
use crate::heap::emit_free;
use crate::procedures::LustFn;
use crate::Expr;
use crate::PreorderStatus;

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
    F: FnMut(&mut Context) -> Result<Value, String>,
{
    let reftype = jit.reference_type();
    let wordtype = jit.module.target_config().pointer_type();

    // Additional closure argument
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(reftype));
    // Additional arg count argument
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(reftype));
    // A pointer to where arguments are stored on the heap
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(reftype));

    // Return value
    jit.context
        .func
        .signature
        .returns
        .push(AbiParam::new(reftype));

    let builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);

    let mut ctx = Context::new(
        builder,
        &mut jit.module,
        reftype,
        wordtype,
        HashMap::new(),
        HashMap::new(),
        Vec::new(),
    );

    let entry_block = ctx.builder.create_block();

    ctx.builder
        .append_block_params_for_function_params(entry_block);
    ctx.builder.switch_to_block(entry_block);
    ctx.builder.seal_block(entry_block);

    let res = body_builder(&mut ctx)?;
    let res = ctx.builder.ins().raw_bitcast(ctx.reftype, res);

    let argloc = ctx.builder.block_params(entry_block)[2];
    emit_free(argloc, &mut ctx)?;

    ctx.builder.ins().return_(&[res]);

    ctx.builder.seal_all_blocks();
    ctx.builder.finalize();

    let id = jit
        .module
        .declare_function(
            name,
            cranelift_module::Linkage::Export,
            &jit.context.func.signature,
        )
        .unwrap();

    jit.module
        .define_function(
            id,
            &mut jit.context,
            &mut NullTrapSink {},
            &mut cranelift_codegen::binemit::NullStackMapSink {},
        )
        .unwrap();

    // println!("{}", jit.context.func.display(jit.module.isa()));

    jit.module.clear_context(&mut jit.context);

    Ok(LustFn {
        name: name.to_string(),
        params: (0..arity).map(|n| n.to_string()).collect(),
        body: vec![],
        // Filled later by build_fn_map,
        id: 0,
        free_variables: vec![],
        varadic_symbol: None,
    })
}

/// Collects all of the primitive functions that are used in higher
/// order contexts.
pub(crate) fn collect_higher_order_primitives(program: &[Expr]) -> Result<HashSet<String>, String> {
    let mut res = HashSet::new();

    for e in program {
        e.preorder_traverse_res(&mut |e| {
            if let Expr::List(v) = e {
                for ex in &v[1..] {
                    if let Expr::Symbol(s) = ex {
                        if string_is_primitive(s) {
                            res.insert(s.clone());
                        } else if string_is_builtin(s) {
                            // Functions like let and set and if can't be used in higher order contexts.
                            return Err(format!(
                                "builtin function ({}) can not be used in a higher order context",
                                s
                            ));
                        }
                    }
                }
            }
            Ok(PreorderStatus::Continue)
        })?;
    }

    Ok(res)
}

/// Collects the arguments for a function with ARITY number of
/// arguments.
fn get_primitive_args(ctx: &mut Context, block: Block, arity: usize) -> Vec<Value> {
    let args = ctx.builder.block_params(block);
    let argloc = args[2];
    (0..arity)
        .map(|i| {
            ctx.builder.ins().load(
                ctx.reftype,
                MemFlags::new(),
                argloc,
                (i * ctx.reftype.bytes() as usize) as i32,
            )
        })
        .collect()
}

pub(crate) fn emit_primitives(
    jit: &mut JIT,
    higher_order_primitives: HashSet<String>,
) -> Result<Vec<LustFn>, String> {
    let _t = crate::timer::timeit("emit primitives");

    let mut res = Vec::new();

    let wordtype = jit.module.target_config().pointer_type();

    if higher_order_primitives.contains("add1") {
        res.push(emit_primitive("add1", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            // Need to reassign args because we mutably borrowed context
            // above.
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let accum = fatal::emit_check_int(accum, ctx)?;

            Ok(ctx
                .builder
                .ins()
                .iadd_imm(accum, Expr::Integer(1).word_rep()))
        })?);
    }

    if higher_order_primitives.contains("print") {
        res.push(emit_primitive("print", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let mut sig = ctx.module.make_signature();
            sig.params.push(AbiParam::new(ctx.reftype));
            sig.returns.push(AbiParam::new(ctx.reftype));

            let callee = ctx
                .module
                .declare_function("print_lustc_word", cranelift_module::Linkage::Import, &sig)
                .map_err(|e| e.to_string())?;

            let local_callee = ctx
                .module
                .declare_func_in_func(callee, &mut ctx.builder.func);

            let args = vec![accum];

            let call = ctx.builder.ins().call(local_callee, &args);
            let res = ctx.builder.inst_results(call)[0];

            Ok(res)
        })?);
    }

    if higher_order_primitives.contains("println") {
        res.push(emit_primitive("println", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let mut sig = ctx.module.make_signature();
            sig.params.push(AbiParam::new(ctx.reftype));
            sig.returns.push(AbiParam::new(ctx.reftype));

            let callee = ctx
                .module
                .declare_function(
                    "println_lustc_word",
                    cranelift_module::Linkage::Import,
                    &sig,
                )
                .map_err(|e| e.to_string())?;

            let local_callee = ctx
                .module
                .declare_func_in_func(callee, &mut ctx.builder.func);

            let args = vec![accum];

            let call = ctx.builder.ins().call(local_callee, &args);
            let res = ctx.builder.inst_results(call)[0];

            Ok(res)
        })?);
    }

    if higher_order_primitives.contains("integer->char") {
        res.push(emit_primitive("integer->char", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let accum = fatal::emit_check_int(accum, ctx)?;

            let accum = ctx.builder.ins().ishl_imm(accum, 6);
            let accum = ctx.builder.ins().bor_imm(accum, conversions::CHAR_TAG);
            Ok(accum)
        })?);
    }

    if higher_order_primitives.contains("char->integer") {
        res.push(emit_primitive("char->integer", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let accum = fatal::emit_check_char(accum, ctx)?;

            let accum = ctx.builder.ins().ushr_imm(accum, 6);
            Ok(accum)
        })?);
    }

    if higher_order_primitives.contains("null?") {
        res.push(emit_primitive("null?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::NIL_VALUE);
            // The result of this comparason is a boolean value so we
            // need to convert it back to a ctx.word before working on it.
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("zero?") {
        res.push(emit_primitive("zero?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum =
                ctx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, accum, Expr::Integer(0).word_rep());
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("not") {
        res.push(emit_primitive("not", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];

            let accum = fatal::emit_check_bool(accum, ctx)?;

            // To get the not of a boolean, subtract one from it and
            // then take the absolute value.
            let accum = ctx.builder.ins().sshr_imm(accum, conversions::BOOL_SHIFT);
            let accum = ctx.builder.ins().icmp_imm(IntCC::Equal, accum, 0);
            let accum = ctx.builder.ins().bint(ctx.reftype, accum);

            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("integer?") {
        res.push(emit_primitive("integer?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx.builder.ins().band_imm(accum, conversions::FIXNUM_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::FIXNUM_TAG);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("boolean?") {
        res.push(emit_primitive("boolean?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx.builder.ins().band_imm(accum, conversions::BOOL_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::BOOL_TAG);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("pair?") {
        res.push(emit_primitive("pair?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx
                .builder
                .ins()
                .band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::PAIR_TAG);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("closure?") {
        res.push(emit_primitive("closure?", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(1, args[1], ctx, false)?;
            let args = get_primitive_args(ctx, block, 1);
            let accum = args[0];
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx
                .builder
                .ins()
                .band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::CLOSURE_TAG);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("add") {
        res.push(emit_primitive("add", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            Ok(ctx.builder.ins().iadd(left, right))
        })?);
    }

    if higher_order_primitives.contains("sub") {
        res.push(emit_primitive("sub", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let right = ctx.builder.ins().ineg(right);

            Ok(ctx.builder.ins().iadd(left, right))
        })?);
    }

    if higher_order_primitives.contains("mul") {
        res.push(emit_primitive("mul", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let accum = ctx.builder.ins().imul(left, right);

            // At this point we've picked up an extra 2^2 so we need
            // to right shift it out.
            //
            // NOTE: It is possible that it would be more reasonable
            // to shift things out first. I'm worried here that this
            // will cause integer overflows where we wouldn't normally
            // expect them.
            Ok(ctx.builder.ins().sshr_imm(accum, 2))
        })?);
    }

    if higher_order_primitives.contains("eq") {
        res.push(emit_primitive("eq", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = ctx.builder.ins().raw_bitcast(ctx.wordtype, left);
            let right = ctx.builder.ins().raw_bitcast(ctx.wordtype, right);

            let accum = ctx.builder.ins().icmp(IntCC::Equal, left, right);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("lt") {
        res.push(emit_primitive("lt", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let accum = ctx.builder.ins().icmp(IntCC::SignedLessThan, left, right);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("gt") {
        res.push(emit_primitive("gt", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let left = args[0];
            let right = args[1];

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let accum = ctx
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, left, right);
            let accum = ctx.builder.ins().bint(wordtype, accum);
            Ok(emit_word_to_bool(accum, &mut ctx.builder))
        })?);
    }

    if higher_order_primitives.contains("cons") {
        res.push(emit_primitive("cons", 2, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 2);
            let data = args[0];
            let next = args[1];

            let storage = emit_alloc((ctx.reftype.bytes() * 2).into(), ctx)?;

            ctx.builder.ins().store(MemFlags::new(), data, storage, 0);
            ctx.builder
                .ins()
                .store(MemFlags::new(), next, storage, wordtype.bytes() as i32);

            let storage = ctx.builder.ins().raw_bitcast(ctx.wordtype, storage);
            Ok(ctx.builder.ins().bor_imm(storage, conversions::PAIR_TAG))
        })?);
    }

    if higher_order_primitives.contains("car") {
        res.push(emit_primitive("car", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 1);
            let pair = args[0];

            let pair = fatal::emit_check_pair(pair, ctx)?;

            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            Ok(ctx
                .builder
                .ins()
                .load(wordtype, MemFlags::new(), address, 0))
        })?);
    }

    if higher_order_primitives.contains("cdr") {
        res.push(emit_primitive("cdr", 1, jit, |ctx| {
            let block = ctx.builder.current_block().unwrap();
            let args = ctx.builder.block_params(block);
            emit_check_arg_count(2, args[1], ctx, false)?;

            let args = get_primitive_args(ctx, block, 1);
            let pair = args[0];

            let pair = fatal::emit_check_pair(pair, ctx)?;

            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            Ok(ctx
                .builder
                .ins()
                .load(wordtype, MemFlags::new(), address, wordtype.bytes() as i32))
        })?);
    }

    Ok(res)
}

pub(crate) fn emit_primcall(name: &str, args: &[Expr], ctx: &mut Context) -> Result<Value, String> {
    debug_assert!(string_is_primitive(name));

    // Emit the code for the primitive. The result is a wordtype which
    // we later cast back into a reftype.
    let wordres = match name {
        "add1" => {
            check_arg_len("add1", args, 1)?;
            let accum = emit_expr(&args[0], ctx)?;

            let accum = fatal::emit_check_int(accum, ctx)?;

            ctx.builder
                .ins()
                .iadd_imm(accum, Expr::Integer(1).word_rep())
        }
        "integer->char" => {
            check_arg_len("integer->char", args, 1)?;

            // To convert an integer to a character we left shift by 6
            // and then tag it with the character tag.
            let accum = emit_expr(&args[0], ctx)?;

            let accum = fatal::emit_check_int(accum, ctx)?;

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

            let accum = fatal::emit_check_char(accum, ctx)?;

            let accum = ctx.builder.ins().ushr_imm(accum, 6);
            accum
        }
        "null?" => {
            check_arg_len("null?", args, 1)?;
            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::NIL_VALUE);
            // The result of this comparason is a boolean value so we
            // need to convert it back to a ctx.word before working on it.
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "zero?" => {
            check_arg_len("zero?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum =
                ctx.builder
                    .ins()
                    .icmp_imm(IntCC::Equal, accum, Expr::Integer(0).word_rep());
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "not" => {
            check_arg_len("not", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;

            let accum = fatal::emit_check_bool(accum, ctx)?;

            // To get the not of a boolean, subtract one from it and
            // then take the absolute value.
            let accum = ctx.builder.ins().sshr_imm(accum, conversions::BOOL_SHIFT);
            let accum = ctx.builder.ins().icmp_imm(IntCC::Equal, accum, 0);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "integer?" => {
            check_arg_len("integer?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx.builder.ins().band_imm(accum, conversions::FIXNUM_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::FIXNUM_TAG);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "boolean?" => {
            check_arg_len("boolean?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx.builder.ins().band_imm(accum, conversions::BOOL_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::BOOL_TAG);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "pair?" => {
            check_arg_len("pair?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx
                .builder
                .ins()
                .band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::PAIR_TAG);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "closure?" => {
            check_arg_len("closure?", args, 1)?;

            let accum = emit_expr(&args[0], ctx)?;
            let accum = ctx.builder.ins().raw_bitcast(ctx.wordtype, accum);

            let accum = ctx
                .builder
                .ins()
                .band_imm(accum, conversions::HEAP_TAG_MASK);
            let accum = ctx
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, accum, conversions::CLOSURE_TAG);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "add" => {
            check_arg_len("add", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            ctx.builder.ins().iadd(left, right)
        }
        "sub" => {
            check_arg_len("sub", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let right = ctx.builder.ins().ineg(right);

            ctx.builder.ins().iadd(left, right)
        }
        "mul" => {
            check_arg_len("mul", args, 2)?;

            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

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

            let left = ctx.builder.ins().raw_bitcast(ctx.wordtype, left);
            let right = ctx.builder.ins().raw_bitcast(ctx.wordtype, right);

            let accum = ctx.builder.ins().icmp(IntCC::Equal, left, right);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "lt" => {
            check_arg_len("lt", args, 2)?;
            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let accum = ctx.builder.ins().icmp(IntCC::SignedLessThan, left, right);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }
        "gt" => {
            check_arg_len("gt", args, 2)?;
            let left = emit_expr(&args[0], ctx)?;
            let right = emit_expr(&args[1], ctx)?;

            let left = fatal::emit_check_int(left, ctx)?;
            let right = fatal::emit_check_int(right, ctx)?;

            let accum = ctx
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, left, right);
            let accum = ctx.builder.ins().bint(ctx.wordtype, accum);
            emit_word_to_bool(accum, &mut ctx.builder)
        }

        "cons" => {
            check_arg_len("cons", args, 2)?;

            let data = emit_expr(&args[0], ctx)?;
            let next = emit_expr(&args[1], ctx)?;

            let storage = emit_alloc((ctx.reftype.bytes() * 2).into(), ctx)?;

            ctx.builder.ins().store(MemFlags::new(), data, storage, 0);
            ctx.builder
                .ins()
                .store(MemFlags::new(), next, storage, ctx.reftype.bytes() as i32);

            let storage = ctx.builder.ins().raw_bitcast(ctx.wordtype, storage);
            ctx.builder.ins().bor_imm(storage, conversions::PAIR_TAG)
        }
        "car" => {
            check_arg_len("car", args, 1)?;

            let pair = emit_expr(&args[0], ctx)?;

            let pair = fatal::emit_check_pair(pair, ctx)?;

            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            ctx.builder
                .ins()
                .load(ctx.reftype, MemFlags::new(), address, 0)
        }
        "cdr" => {
            check_arg_len("cdr", args, 1)?;

            let pair = emit_expr(&args[0], ctx)?;

            let pair = fatal::emit_check_pair(pair, ctx)?;

            let address = ctx.builder.ins().band_imm(pair, conversions::HEAP_PTR_MASK);

            ctx.builder.ins().load(
                ctx.reftype,
                MemFlags::new(),
                address,
                ctx.reftype.bytes() as i32,
            )
        }

        "print" => {
            check_arg_len("print", args, 1)?;
            let arg = emit_expr(&args[0], ctx)?;
            let args = vec![arg];

            let mut sig = ctx.module.make_signature();
            sig.params.push(AbiParam::new(ctx.reftype));
            sig.returns.push(AbiParam::new(ctx.reftype));

            let callee = ctx
                .module
                .declare_function("print_lustc_word", cranelift_module::Linkage::Import, &sig)
                .map_err(|e| e.to_string())?;

            let local_callee = ctx
                .module
                .declare_func_in_func(callee, &mut ctx.builder.func);

            let call = ctx.builder.ins().call(local_callee, &args);
            ctx.builder.inst_results(call)[0]
        }

        "println" => {
            check_arg_len("println", args, 1)?;
            let arg = emit_expr(&args[0], ctx)?;
            let args = vec![arg];

            let mut sig = ctx.module.make_signature();
            sig.params.push(AbiParam::new(ctx.reftype));
            sig.returns.push(AbiParam::new(ctx.reftype));

            let callee = ctx
                .module
                .declare_function(
                    "println_lustc_word",
                    cranelift_module::Linkage::Import,
                    &sig,
                )
                .map_err(|e| e.to_string())?;

            let local_callee = ctx
                .module
                .declare_func_in_func(callee, &mut ctx.builder.func);

            let call = ctx.builder.ins().call(local_callee, &args);
            ctx.builder.inst_results(call)[0]
        }

        _ => panic!("non primitive in emit_primcall: {}", name),
    };
    // Cast the result back into a reference type
    Ok(ctx.builder.ins().raw_bitcast(ctx.reftype, wordres))
}

fn emit_word_to_bool(accum: Value, builder: &mut FunctionBuilder) -> Value {
    let accum = builder.ins().ishl_imm(accum, conversions::BOOL_SHIFT);
    let accum = builder.ins().bor_imm(accum, conversions::BOOL_TAG);
    accum
}

pub(crate) fn string_is_builtin(s: &str) -> bool {
    string_is_primitive(s)
        || s == "if"
        || s == "quote"
        || s == "let"
        || s == "fn"
        || s == "set"
        || s == "foreign-call"
        || s == "error"
}

pub(crate) fn string_is_primitive(s: &str) -> bool {
    s == "add1"
        || s == "print"
        || s == "println"
        || s == "integer->char"
        || s == "char->integer"
        || s == "null?"
        || s == "zero?"
        || s == "not"
        || s == "boolean?"
        || s == "integer?"
        || s == "pair?"
        || s == "closure?"
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

pub(crate) fn emit_contigous_to_list(
    ctx: &mut crate::compiler::Context,
    ptr: Value,
    len: Value,
) -> Result<Value, String> {
    let reftype = ctx.reftype;

    let mut sig = ctx.module.make_signature();

    sig.params.push(AbiParam::new(reftype));
    sig.params.push(AbiParam::new(reftype));

    sig.returns.push(AbiParam::new(reftype));

    let callee = ctx
        .module
        .declare_function(
            "contiguous-to-list",
            cranelift_module::Linkage::Import,
            &sig,
        )
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    let args = vec![ptr, len];

    let call = ctx.builder.ins().call(local_callee, &args);
    let res = ctx.builder.inst_results(call)[0];

    Ok(res)
}

// Defines the function contiguous-to-list which converts a contiguous
// vec of values into a list.
pub(crate) fn define_contiguous_to_list(
    jit: &mut JIT,
    unwind_context: &mut crate::debug::UnwindContext,
) -> Result<(), String> {
    let reftype = jit.reference_type();
    let wordtype = jit.module.target_config().pointer_type();

    // A pointer to the beginning of the contiguous memory
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(reftype));
    // The length of the contiguous memory.
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(reftype));
    // A pointer to the new list
    jit.context
        .func
        .signature
        .returns
        .push(AbiParam::new(reftype));

    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);

    let ptr = builder.block_params(entry_block)[0];
    let len = builder.block_params(entry_block)[1];

    let len = builder.ins().raw_bitcast(wordtype, len);
    let cond = builder.ins().icmp_imm(IntCC::Equal, len, 0);

    let done_block = builder.create_block();
    let more_block = builder.create_block();

    // If there is no length we are done and can return nil.
    builder.ins().brnz(cond, done_block, &[]);
    builder.ins().jump(more_block, &[]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);

    let nil = Expr::Nil.ctxless_immediate_rep(
        &mut builder,
        jit.module.target_config().pointer_type(),
        reftype,
    );
    builder.ins().return_(&[nil]);

    builder.switch_to_block(more_block);
    builder.seal_block(more_block);

    // Make a call to alloc to get some storage
    let mut sig = jit.module.make_signature();
    sig.params.push(AbiParam::new(wordtype));
    sig.returns.push(AbiParam::new(reftype));

    let callee = jit
        .module
        .declare_function("alloc", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = jit.module.declare_func_in_func(callee, &mut builder.func);
    let size = builder.ins().iconst(wordtype, 2);
    let args = vec![size];
    let call = builder.ins().call(local_callee, &args);

    let storage = builder.inst_results(call)[0];
    let data = builder.ins().load(reftype, MemFlags::new(), ptr, 0);

    // Make a call to ourselves to get the rest.
    let mut sig = jit.module.make_signature();
    sig.params.push(AbiParam::new(reftype));
    sig.params.push(AbiParam::new(reftype));
    sig.returns.push(AbiParam::new(reftype));

    let callee = jit
        .module
        .declare_function(
            "contiguous-to-list",
            cranelift_module::Linkage::Import,
            &sig,
        )
        .map_err(|e| e.to_string())?;

    let local_callee = jit.module.declare_func_in_func(callee, &mut builder.func);

    // Cast to a word, add one, and then cast back to a pointer to
    // point to the next item in the contigous list.
    let ptr_word = builder.ins().raw_bitcast(wordtype, ptr);
    let new_ptr = builder.ins().iadd_imm(ptr_word, reftype.bytes() as i64);
    let new_ptr = builder.ins().raw_bitcast(reftype, new_ptr);

    let len_word = builder.ins().raw_bitcast(wordtype, len);
    let new_len = builder.ins().iadd_imm(len_word, -1);
    let new_len = builder.ins().raw_bitcast(reftype, new_len);

    let args = vec![new_ptr, new_len];
    let call = builder.ins().call(local_callee, &args);

    let next = builder.inst_results(call)[0];

    builder.ins().store(MemFlags::new(), data, storage, 0);
    builder
        .ins()
        .store(MemFlags::new(), next, storage, reftype.bytes() as i32);

    let storage_word = builder.ins().raw_bitcast(wordtype, storage);
    let res = builder.ins().bor_imm(storage_word, conversions::PAIR_TAG);
    let res = builder.ins().raw_bitcast(reftype, res);

    builder.ins().return_(&[res]);

    builder.seal_all_blocks();
    builder.finalize();

    let id = jit
        .module
        .declare_function(
            "contiguous-to-list",
            cranelift_module::Linkage::Export,
            &jit.context.func.signature,
        )
        .unwrap();

    jit.module
        .define_function(
            id,
            &mut jit.context,
            &mut NullTrapSink {},
            &mut cranelift_codegen::binemit::NullStackMapSink {},
        )
        .unwrap();

    unwind_context.add_function(id, &jit.context, jit.module.isa())?;

    jit.module.clear_context(&mut jit.context);

    Ok(())
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
                Expr::Integer(1),
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

    #[test]
    fn higher_order_builtin_in_closure() {
        let source = r#"
(let - sub)
(let minus-one (fn (n) (- n 1)))
(minus-one 3)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Integer(2), res)
    }

    #[test]
    fn is_closure() {
        let source = r#"
(closure? ((fn () (fn (n) n))))
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Bool(true), res)
    }

    #[test]
    #[should_panic(expected = "builtin function (let) can not be used in a higher order context")]
    fn bad_builtin_assign() {
        let source = r#"
(let set 10)
(let s set)
(let l let)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Bool(true), res)
    }

    #[test]
    #[should_panic(expected = "builtin function (set) can not be used in a higher order context")]
    fn bad_builtin_use() {
        let source = r#"
(let f (fn (op) (op 1 2)))
(f cons)
(f set)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Bool(true), res)
    }
}
