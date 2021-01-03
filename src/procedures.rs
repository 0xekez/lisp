use std::collections::HashMap;

use crate::compiler::{emit_expr, JIT};
use crate::locals::emit_var_decl;
use crate::Expr;
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};

impl Expr {
    /// Determines whether or not the expression is a function
    /// call. If it is, returns a tuple containing the name of the
    /// function being called and a list of expressions corresponding
    /// to its arguments. Otherwise, returns None.
    pub fn is_fncall(&self) -> Option<(&str, &[Expr])> {
        match self {
            Self::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    Some((s, &v[1..]))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub fn emit_procedure(
    jit: &mut JIT,
    name: &str,
    params: &[String],
    body: &[Expr],
) -> Result<(), String> {
    let word = jit.module.target_config().pointer_type();

    // Indicate that the function takes some number of words as
    // arguments. This says nothing yet of the names of those
    // paramaters.
    for _ in params {
        jit.context.func.signature.params.push(AbiParam::new(word));
    }

    // All lust functions return the result of evaluating their last
    // expression.
    jit.context.func.signature.returns.push(AbiParam::new(word));

    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let mut env = HashMap::new();

    for (i, p) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        emit_var_decl(p, val, &mut env, &mut builder, word)?;
    }

    let mut ctx = crate::compiler::Context::new(builder, &mut jit.module, word, env);

    let vals = body
        .iter()
        .map(|e| emit_expr(e, &mut ctx))
        .collect::<Result<Vec<_>, _>>()?;

    // Emit a return instruction to return the result.
    ctx.builder.ins().return_(&[*vals
        .last()
        .ok_or("expected at least one expression".to_string())?]);

    // Clean up
    ctx.builder.seal_all_blocks();
    ctx.builder.finalize();

    let id = jit
        .module
        .declare_function(name, Linkage::Export, &jit.context.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.context, &mut codegen::binemit::NullTrapSink {})
        .map_err(|e| e.to_string())?;

    // If you want to dump the generated IR this is the way:
    // println!("{}", jit.context.func.display(jit.module.isa()));

    jit.module.clear_context(&mut jit.context);

    jit.module.finalize_definitions();

    Ok(())
}

pub(crate) fn emit_fncall(
    name: &str,
    args: &[Expr],
    ctx: &mut crate::compiler::Context,
) -> Result<Value, String> {
    let word = ctx.module.target_config().pointer_type();

    let mut sig = ctx.module.make_signature();

    for _ in args {
        sig.params.push(AbiParam::new(word));
    }
    sig.returns.push(AbiParam::new(word));

    let callee = ctx
        .module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    let args: Vec<_> = args
        .iter()
        .map(|e| emit_expr(e, ctx))
        .collect::<Result<Vec<_>, _>>()?;

    let call = ctx.builder.ins().call(local_callee, &args);
    let res = ctx.builder.inst_results(call)[0];

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::compiler::Context;
    use crate::errors::Printable;
    use crate::parser::Parser;

    use super::*;

    /// Manually create / parse a function then manually create /
    /// parse an expression that uses said function reusing the JIT
    /// that created the function. Finally, make sure that the
    /// function can be called.
    ///
    /// This test can be removed once we have propper machinery for
    /// defining functions and using them. That will require some
    /// lifting of function definitions and indirect calls though so a
    /// test is worthwhile here.
    #[test]
    fn manual_procedure() {
        let addfour = r#"
(let foo (add1 arg))
(add1 (add1 (add1 foo)))
"#;
        let input = r#"
(addfour 38)
"#;

        let mut parser = Parser::new(addfour);
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
                panic!("parse error!".to_string());
            }
        }

        let mut jit = JIT::default();

        emit_procedure(&mut jit, "addfour", &["arg".to_string()], &exprs).unwrap();

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
                panic!("parse error!".to_string());
            }
        }

        let exprs: &[Expr] = &exprs;

        let word = jit.module.target_config().pointer_type();

        // Signature for the function that we're compiling. This function
        // takes no arguments and returns an integer.
        jit.context.func.signature.returns.push(AbiParam::new(word));

        // Create a new builder for building our function and create a new
        // block to compile into.
        let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
        let entry_block = builder.create_block();

        // Give the paramaters that we set up earlier to this entry block.
        builder.append_block_params_for_function_params(entry_block);
        // Start putting code in the new block.
        builder.switch_to_block(entry_block);

        let env = HashMap::new();

        let mut ctx = Context::new(builder, &mut jit.module, word, env);

        let vals = exprs
            .iter()
            .map(|e| emit_expr(e, &mut ctx))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        // Emit a return instruction to return the result.
        ctx.builder.ins().return_(&[*vals
            .last()
            .ok_or("expected at least one expression".to_string())
            .unwrap()]);

        // Clean up
        ctx.builder.seal_all_blocks();
        ctx.builder.finalize();

        let id = jit
            .module
            .declare_function("lust_entry", Linkage::Export, &jit.context.func.signature)
            .map_err(|e| e.to_string())
            .unwrap();

        jit.module
            .define_function(id, &mut jit.context, &mut codegen::binemit::NullTrapSink {})
            .map_err(|e| e.to_string())
            .unwrap();

        // If you want to dump the generated IR this is the way:
        // println!("{}", jit.context.func.display(jit.module.isa()));

        jit.module.clear_context(&mut jit.context);

        jit.module.finalize_definitions();

        let code_ptr = jit.module.get_finalized_function(id);

        let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };

        let actual = Expr::from_immediate(code_fn());
        let expected = Expr::Integer(42);

        assert_eq!(actual, expected);
    }
}
