//! Calls to foreign functions.

use crate::compiler::{emit_expr, Context};
use crate::conversions::*;
use crate::{Expr, Word};
use cranelift::prelude::*;
use cranelift_module::Module;

impl Expr {
    /// Determines if the expression is a foreign call and if it is
    /// returns its name and arguments.
    pub fn is_foreign_call(&self) -> Option<(String, &[Expr])> {
        if let Expr::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "foreign-call" && v.len() >= 2 {
                    if let Expr::String(name) = &v[1] {
                        return Some((name.clone().into_string().unwrap(), &v[2..]));
                    }
                }
            }
        }
        None
    }

    pub fn is_foreign_call_mut(&mut self) -> Option<(String, &mut [Expr])> {
        if let Expr::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "foreign-call" && v.len() >= 2 {
                    if let Expr::String(name) = &v[1] {
                        return Some((name.clone().into_string().unwrap(), &mut v[2..]));
                    }
                }
            }
        }
        None
    }
}

/// Emits the code to call a foreign function.
pub(crate) fn emit_foreign_call(
    name: &str,
    args: &[Expr],
    ctx: &mut Context,
) -> Result<Value, String> {
    let mut sig = ctx.module.make_signature();

    for _ in args {
        sig.params.push(AbiParam::new(ctx.word));
    }
    sig.returns.push(AbiParam::new(ctx.word));

    let callee = ctx
        .module
        .declare_function(name, cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    let args = args
        .iter()
        .map(|e| emit_untag(e, ctx))
        .collect::<Result<Vec<_>, String>>()?;

    let call = ctx.builder.ins().call(local_callee, &args);
    let res = ctx.builder.inst_results(call)[0];

    // For now we just assume that all foreign functions are going to
    // return a fixnum. This could very likely be changed to making
    // them return nil.
    let res = ctx.builder.ins().ishl_imm(res, FIXNUM_SHIFT);
    let res = ctx.builder.ins().bor_imm(res, FIXNUM_TAG);

    Ok(res)
}

/// Emits the code to store VAL is the type represented by TAG using
/// MASK and returning the result.
pub(crate) fn emit_is(val: Value, tag: Word, mask: Word, ctx: &mut Context) -> Value {
    let masked = ctx.builder.ins().band_imm(val, mask);
    ctx.builder.ins().icmp_imm(IntCC::Equal, masked, tag)
}

/// Emits the code to check that VAL is heap allocated.
pub(crate) fn emit_is_heap(val: Value, ctx: &mut Context) -> Value {
    let masked = ctx.builder.ins().band_imm(val, HEAP_TAG_MASK);
    ctx.builder.ins().icmp_imm(IntCC::NotEqual, masked, 0)
}

/// Takes EXPR and emits the code to remove it's tag making it
/// elegible for use as an argument for a foreign function.
pub(crate) fn emit_untag(expr: &Expr, ctx: &mut Context) -> Result<Value, String> {
    let the_val = emit_expr(expr, ctx)?;

    let fixnum_block = ctx.builder.create_block();
    let bool_block = ctx.builder.create_block();
    let nil_block = ctx.builder.create_block();
    let heap_block = ctx.builder.create_block();
    let heap_convert = ctx.builder.create_block();
    let shift_block = ctx.builder.create_block();
    let return_block = ctx.builder.create_block();

    // Merge block takes an argument which is the amount of right
    // shifting to do.
    ctx.builder.append_block_param(shift_block, ctx.word);
    ctx.builder.append_block_param(return_block, ctx.word);

    let is_char = emit_is(the_val, CHAR_TAG, CHAR_MASK, ctx);
    let char_shift = ctx.builder.ins().iconst(ctx.word, CHAR_SHIFT);

    ctx.builder.ins().brnz(is_char, shift_block, &[char_shift]);
    ctx.builder.ins().jump(fixnum_block, &[]);

    ctx.builder.switch_to_block(fixnum_block);
    ctx.builder.seal_block(fixnum_block);

    let is_fixnum = emit_is(the_val, FIXNUM_TAG, FIXNUM_MASK, ctx);
    let fixnum_shift = ctx.builder.ins().iconst(ctx.word, FIXNUM_SHIFT);

    ctx.builder
        .ins()
        .brnz(is_fixnum, shift_block, &[fixnum_shift]);
    ctx.builder.ins().jump(bool_block, &[]);

    ctx.builder.switch_to_block(bool_block);
    ctx.builder.seal_block(bool_block);

    let is_bool = emit_is(the_val, BOOL_TAG, BOOL_MASK, ctx);
    let bool_shift = ctx.builder.ins().iconst(ctx.word, BOOL_SHIFT);

    ctx.builder.ins().brnz(is_bool, shift_block, &[bool_shift]);
    ctx.builder.ins().jump(nil_block, &[]);

    ctx.builder.switch_to_block(nil_block);
    ctx.builder.seal_block(nil_block);

    let is_nil = ctx.builder.ins().icmp_imm(IntCC::Equal, the_val, NIL_VALUE);
    // Nil goes to zero in c land
    let nil_shift = ctx.builder.ins().iconst(ctx.word, ctx.word.bytes() as i64);

    ctx.builder.ins().brnz(is_nil, shift_block, &[nil_shift]);
    ctx.builder.ins().jump(heap_block, &[]);

    ctx.builder.switch_to_block(heap_block);
    ctx.builder.seal_block(heap_block);

    let is_heap = emit_is_heap(the_val, ctx);
    let zero = ctx.builder.ins().iconst(ctx.word, 0);

    ctx.builder.ins().brnz(is_heap, heap_convert, &[]);
    ctx.builder.ins().jump(shift_block, &[zero]);

    ctx.builder.switch_to_block(heap_convert);
    ctx.builder.seal_block(heap_convert);

    let naked_ptr = ctx.builder.ins().band_imm(the_val, HEAP_PTR_MASK);
    ctx.builder.ins().jump(return_block, &[naked_ptr]);

    ctx.builder.switch_to_block(shift_block);
    ctx.builder.seal_block(shift_block);

    let shift = ctx.builder.block_params(shift_block)[0];
    let shifted = ctx.builder.ins().ushr(the_val, shift);
    ctx.builder.ins().jump(return_block, &[shifted]);

    ctx.builder.switch_to_block(return_block);
    ctx.builder.seal_block(return_block);

    let arg = ctx.builder.block_params(return_block)[0];
    Ok(arg)
}

#[cfg(test)]
mod tests {
    use std::ffi::CString;

    use crate::roundtrip_string;

    use super::*;

    #[test]
    fn puts() {
        let source = r#"
(let res (foreign-call "puts" "test of puts"))
"#;
        let res = roundtrip_string(source).unwrap();
        match res {
            Expr::Integer(i) => assert!(i >= 0),
            _ => panic!("expected fixnum"),
        }
    }

    #[test]
    fn strlen() {
        let source = r#"
(let res (foreign-call "strlen" "four"))
"#;
        let res = roundtrip_string(source).unwrap();
        match res {
            Expr::Integer(i) => assert_eq!(i, 4),
            _ => panic!("expected fixnum"),
        }
    }

    #[test]
    fn open_read() {
        let source = r#"
(let fd (foreign-call "open" "examples/file.lisp" 0))
(let bar "hello there")
(foreign-call "read" fd bar 11)
bar
"#;
        let res = roundtrip_string(source).unwrap();
        let file = std::fs::read_to_string("examples/file.lisp").unwrap();
        let expected = &file[0..11];
        let expected = CString::new(expected).unwrap();
        assert_eq!(res, Expr::String(expected))
    }
}
