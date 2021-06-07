use crate::{
    compiler::{self, Context, JIT},
    conversions,
    data::LustData,
    foreign, Expr, Word,
};
use cranelift::prelude::*;

impl Expr {
    // Determines if an expression is an error expression and returns
    // its messsage and return code arguments.
    pub fn is_error(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "error" && v.len() == 3 {
                    return Some((&v[1], &v[2]));
                }
            }
        }
        None
    }
}

pub(crate) fn emit_error_strings(jit: &mut JIT) -> Result<(), String> {
    let error_strings = [
        (
            "__anon_data_bad_call_type",
            "fatal error: non-closure object in head position of list",
        ),
        (
            "__anon_data_bad_arg_type",
            "fatal error: runtime type missmatch",
        ),
        (
            "__anon_data_bad_arg_count",
            "fatal error: wrong number of arguments in function call",
        ),
    ];
    let error_data = error_strings
        .iter()
        .map(|(name, msg)| -> Result<LustData, std::ffi::NulError> {
            Ok(LustData {
                name: name.to_string(),
                // bit of a hack but we tag these as pairs so that
                // they register as heap allocated values elsewhere.
                data: std::ffi::CString::new(*msg)?.into_raw() as Word | conversions::PAIR_TAG,
            })
        })
        .collect::<Result<Vec<LustData>, _>>()
        .map_err(|e| e.to_string())?;

    error_data
        .into_iter()
        .map(|d| crate::data::create_data(d, jit))
        .collect()
}

pub(crate) fn emit_error(
    message: &Expr,
    exit_code: &Expr,
    ctx: &mut Context,
) -> Result<Value, String> {
    foreign::emit_foreign_call("puts", &[message.clone()], ctx)?;
    foreign::emit_foreign_call("exit", &[exit_code.clone()], ctx)
}

pub(crate) fn emit_check_tag(
    query: Value,
    tag: Word,
    mask: Word,
    ctx: &mut Context,
) -> Result<(), String> {
    let tag_val = ctx.builder.ins().band_imm(query, mask);
    let is_tag = ctx.builder.ins().icmp_imm(IntCC::Equal, tag_val, tag);

    let error_block = ctx.builder.create_block();
    let ok_block = ctx.builder.create_block();

    ctx.builder.ins().brz(is_tag, error_block, &[]);
    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(error_block);
    ctx.builder.seal_block(error_block);

    emit_error(
        &Expr::Symbol("__anon_data_bad_arg_type".to_string()),
        &Expr::Integer(-1),
        ctx,
    )?;

    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(ok_block);
    ctx.builder.seal_block(ok_block);

    Ok(())
}

pub(crate) fn emit_check_int(query: Value, ctx: &mut Context) -> Result<(), String> {
    emit_check_tag(
        query,
        conversions::FIXNUM_TAG,
        conversions::FIXNUM_MASK,
        ctx,
    )
}

pub(crate) fn emit_check_char(query: Value, ctx: &mut Context) -> Result<(), String> {
    emit_check_tag(query, conversions::CHAR_TAG, conversions::CHAR_MASK, ctx)
}

pub(crate) fn emit_check_bool(query: Value, ctx: &mut Context) -> Result<(), String> {
    emit_check_tag(query, conversions::BOOL_TAG, conversions::BOOL_MASK, ctx)
}

pub(crate) fn emit_check_pair(query: Value, ctx: &mut Context) -> Result<(), String> {
    emit_check_tag(
        query,
        conversions::PAIR_TAG,
        conversions::HEAP_TAG_MASK,
        ctx,
    )
}

pub(crate) fn emit_check_callable(query: &Expr, ctx: &mut Context) -> Result<Value, String> {
    let closure_ptr = compiler::emit_expr(query, ctx)?;
    let tag = ctx
        .builder
        .ins()
        .band_imm(closure_ptr, conversions::HEAP_TAG_MASK);
    let cond = ctx
        .builder
        .ins()
        .icmp_imm(IntCC::Equal, tag, conversions::CLOSURE_TAG);

    let error_block = ctx.builder.create_block();
    let ok_block = ctx.builder.create_block();

    ctx.builder.ins().brz(cond, error_block, &[]);
    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(error_block);
    ctx.builder.seal_block(error_block);

    emit_error(
        &Expr::Symbol("__anon_data_bad_call_type".to_string()),
        &Expr::Integer(-1),
        ctx,
    )?;

    // This ought to be unreachable but it appeases the code
    // generator.
    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(ok_block);
    ctx.builder.seal_block(ok_block);

    Ok(closure_ptr)
}

pub(crate) fn emit_check_arg_count(
    expected: usize,
    actual: Value,
    ctx: &mut Context,
    is_varadic: bool,
) -> Result<(), String> {
    // If the function is varadic wrong number of arguments only if
    // there are less than the number of regular params.
    let cond = if is_varadic {
        ctx.builder
            .ins()
            .icmp_imm(IntCC::UnsignedGreaterThanOrEqual, actual, expected as i64)
    } else {
        ctx.builder
            .ins()
            .icmp_imm(IntCC::Equal, actual, expected as i64)
    };

    let error_block = ctx.builder.create_block();
    let ok_block = ctx.builder.create_block();

    ctx.builder.ins().brz(cond, error_block, &[]);
    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(error_block);
    ctx.builder.seal_block(error_block);

    emit_error(
        &Expr::Symbol("__anon_data_bad_arg_count".to_string()),
        &Expr::Integer(-1),
        ctx,
    )?;

    // This ought to be unreachable but it appeases the code
    // generator.
    ctx.builder.ins().jump(ok_block, &[]);

    ctx.builder.switch_to_block(ok_block);
    ctx.builder.seal_block(ok_block);

    Ok(())
}
