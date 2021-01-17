use std::collections::HashMap;

use crate::compiler::{emit_expr, JIT};
use crate::locals::emit_var_access;
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

    /// Determines if the expression is a function definition and if
    /// it is returns a tuple containing its paramaters and its body.
    pub fn is_fndef(&self) -> Option<(Vec<&String>, &[Expr])> {
        if let Self::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "fn" && v.len() >= 3 {
                    let params = Self::collect_list_of_symbols(&v[1])?;
                    let body = &v[2..];
                    return Some((params, body));
                }
            }
        }
        return None;
    }

    fn collect_list_of_symbols(expr: &Expr) -> Option<Vec<&String>> {
        if let Self::List(v) = expr {
            let mut res = Vec::with_capacity(v.len());
            for e in v {
                match e {
                    Expr::Symbol(s) => res.push(s),
                    _ => return None,
                }
            }
            Some(res)
        } else {
            None
        }
    }
}

/// Emits a function into the JIT.
pub fn emit_procedure(
    jit: &mut JIT,
    name: &str,
    params: &[String],
    body: &[Expr],
    argmap: &HashMap<String, u8>,
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

    // FIXME: ugly clone here.
    let mut ctx =
        crate::compiler::Context::new(builder, &mut jit.module, word, env, argmap.clone());

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

/// Emits a call to a function. If the name is the name of an
/// anonymous function emits a direct call. Otherwise, emits an
/// indirect one to the function pointed to by the argument variable.
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

    let args: Vec<_> = args
        .iter()
        .map(|e| emit_expr(e, ctx))
        .collect::<Result<Vec<_>, _>>()?;

    if name.starts_with("__anon_fn_") {
        // This is a direct call. We can emit a regulat call instruction.
        let callee = ctx
            .module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| e.to_string())?;

        let local_callee = ctx
            .module
            .declare_func_in_func(callee, &mut ctx.builder.func);

        let call = ctx.builder.ins().call(local_callee, &args);
        let res = ctx.builder.inst_results(call)[0];

        Ok(res)
    } else {
        // This is an indirect call. We need to load the varaible
        // first and then emit a call_indirect instruction.
        let val = emit_var_access(name, ctx)?;

        let sig_ref = ctx.builder.import_signature(sig);

        let call = ctx.builder.ins().call_indirect(sig_ref, val, &args);
        let res = ctx.builder.inst_results(call)[0];

        Ok(res)
    }
}

/// A descriptor of an anonymous function.
pub(crate) struct LustFn {
    /// The functions name. This is always in the form
    /// __anon_fn_{number}.
    pub name: String,
    /// The param names for the function.
    pub params: Vec<String>,
    /// The body of the function.
    pub body: Vec<Expr>,
}

/// Collects all of the anonymous functions in a program and returns a
/// list of them.
pub(crate) fn collect_functions(program: &[Expr]) -> Vec<LustFn> {
    let mut res = Vec::new();

    for e in program {
        e.depth_first_traverse(&mut |e: &Expr| {
            if let Some((params, body)) = e.is_fndef() {
                res.push(LustFn {
                    name: format!("__anon_fn_{}", res.len()),
                    params: params.iter().map(|&s| s.clone()).collect(),
                    body: body.iter().map(|e| e.clone()).collect(),
                })
            }
        })
    }

    res
}

/// Builds a map between anonymous function names and their argument
/// counts. This is used later to build function signatures when we
/// need to perform indirect function calls as getting a reference to
/// a function that has been defined earlier requires that we know its
/// number of arguments. This is a cranelift constraint and not one
/// that we add ourselves.
pub(crate) fn build_arg_count_map(functions: &[LustFn]) -> HashMap<String, u8> {
    functions
        .iter()
        .map(|f| (f.name.clone(), f.params.len() as u8))
        .collect()
}

/// Replaces functions with their anonymous names. Takes a program and
/// a list of functions from `collect_functions` as arguments. Needs
/// the list of functions because it needs to be sure to replace their
/// collected bodies with versions with the replaced functions inside.
pub(crate) fn replace_functions(program: &mut [Expr], functions: &mut [LustFn]) {
    let mut count = 0;
    for e in program {
        // It is very important that this is a depth first traversal
        // so that we can be sure that the function bodies are
        // traversed before we hit them. At that point we can replace
        // the bodies from the `functions` list with the updated one.
        e.depth_first_traverse_mut(&mut |e: &mut Expr| {
            if let Some(_) = e.is_fndef() {
                if let Expr::List(v) = e {
                    functions[count].body = v[2..].to_vec();
                } else {
                    // This should really never happen
                    panic!("fndef outside of a list")
                }

                *e = Expr::Symbol(format!("__anon_fn_{}", count));
                count += 1;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::Printable;
    use crate::parser::Parser;
    use crate::roundtrip_file;
    use crate::roundtrip_string;

    use super::*;

    #[test]
    fn test_replace_functions() {
        let source = r#"
(let foo (fn (a b)
	     (let bar (add a b))
	     ((fn (c) (add c 1)) bar)))

(if (fn (a) (fn (b) (add a b))) (fn (c) 1) (fn (d) 2))
"#;
        let mut parser = Parser::new(source);
        let mut exprs = Vec::new();
        while parser.has_more() {
            let res = parser.parse_expr();

            for e in &res.errors {
                e.show(source, "anonymous");
            }
            if res.errors.is_empty() {
                let expr = res.expr.unwrap();
                exprs.push(expr.into_expr());
            } else {
                panic!("parse error!".to_string());
            }
        }

        let mut functions = collect_functions(&exprs);
        assert_eq!(functions.len(), 6);

        replace_functions(&mut exprs, &mut functions);

        let functions = collect_functions(&exprs);
        assert_eq!(functions.len(), 0);
    }

    #[test]
    fn test_collect_functions() {
        let source = r#"
(let foo (fn (a b)
	     (let bar (add a b))
	     ((fn (c) (add c 1)) bar)))

(if (fn (a) (fn (b) (add a b))) (fn (c) 1) (fn (d) 2))
"#;
        let mut parser = Parser::new(source);
        let mut exprs = Vec::new();
        while parser.has_more() {
            let res = parser.parse_expr();

            for e in &res.errors {
                e.show(source, "anonymous");
            }
            if res.errors.is_empty() {
                let expr = res.expr.unwrap();
                exprs.push(expr.into_expr());
            } else {
                panic!("parse error!".to_string());
            }
        }

        let functions = collect_functions(&exprs);

        assert_eq!(functions.len(), 6)
    }

    #[test]
    fn direct_function_basic() {
        let source = r#"
((fn (n) (add1 n)) 41)
"#;
        let res = roundtrip_string(source).unwrap();

        assert_eq!(res, Expr::Integer(42))
    }

    #[test]
    fn indirect_function_basic() {
        let source = r#"
(let foo (fn (n) (add1 n)))
(foo 41)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(42))
    }

    #[test]
    fn indirect_functions() {
        let source = r#"
(let fact (fn (n)
              (if (eq n 1)
                  1
                 (mul n (__anon_fn_0 (sub n 1))))))
(fact 14)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(87178291200))
    }

    #[test]
    fn multiple_expr_body() {
        let source = r#"
(let fact (fn (n)
              (let continue (eq n 1))
              (if continue
                  1
                 (mul n (__anon_fn_0 (sub n 1))))))
(fact 14)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(87178291200))
    }

    #[test]
    fn fn_from_file() {
        let res = roundtrip_file("examples/fn.lisp").unwrap();
        assert_eq!(res, Expr::Integer(4))
    }

    /// Variables and function paramaters should not conflict.
    #[test]
    fn argument_scoping() {
        let source = r#"
(let n 10)
(let fact (fn (n)
              (let continue (eq n 1))
              (if continue
                  1
                 (mul n (__anon_fn_0 (sub n 1))))))
(fact 14)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(87178291200))
    }
}
