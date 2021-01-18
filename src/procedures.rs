use std::collections::HashMap;
use std::collections::HashSet;

use crate::compiler::Context;
use crate::compiler::{emit_expr, JIT};
use crate::heap::emit_alloc;
use crate::locals::emit_var_access;
use crate::locals::emit_var_decl_and_assign;
use crate::primitives::string_is_primitive;
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

    /// Collects a list of symbols from an expression. Used for
    /// collecting arguments to a function.
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
    fnmap: &HashMap<String, LustFn>,
) -> Result<(), String> {
    let word = jit.module.target_config().pointer_type();

    // Indicate that the function takes some number of words as
    // arguments. This says nothing yet of the names of those
    // paramaters.
    for _ in params {
        jit.context.func.signature.params.push(AbiParam::new(word));
    }

    // Closure argument
    jit.context.func.signature.params.push(AbiParam::new(word));

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
        emit_var_decl_and_assign(p, val, &mut env, &mut builder, word)?;
    }

    let closure_ptr = builder.block_params(entry_block)[params.len()];
    let free_vars = fnmap
        .get(name)
        .map(|f| &f.free_variables)
        .ok_or(format!("internal error finding free vars for {}", name))?;
    let word_size = word.bytes();

    for (i, free) in free_vars.iter().enumerate() {
        let offset = i + 1;
        let byte_offset = offset * (word_size as usize);

        let val = builder
            .ins()
            .load(word, MemFlags::new(), closure_ptr, byte_offset as i32);

        emit_var_decl_and_assign(free, val, &mut env, &mut builder, word)?;
    }

    // FIXME: ugly clone here.
    let mut ctx = Context::new(
        builder,
        &mut jit.module,
        word,
        env,
        fnmap.clone(),
        Vec::new(),
    );

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
pub(crate) fn emit_fncall(name: &str, args: &[Expr], ctx: &mut Context) -> Result<Value, String> {
    let word = ctx.module.target_config().pointer_type();

    let mut sig = ctx.module.make_signature();

    for _ in args {
        sig.params.push(AbiParam::new(word));
    }
    // Also push an argument which is the closure.
    sig.params.push(AbiParam::new(word));
    sig.returns.push(AbiParam::new(word));

    let mut args: Vec<_> = args
        .iter()
        .map(|e| emit_expr(e, ctx))
        .collect::<Result<Vec<_>, _>>()?;

    let closure_ptr = emit_var_access(name, ctx)?;
    let closure_ptr = ctx
        .builder
        .ins()
        .band_imm(closure_ptr, crate::conversions::HEAP_PTR_MASK);

    let fn_ptr = ctx
        .builder
        .ins()
        .load(ctx.word, MemFlags::new(), closure_ptr, 0);

    args.push(closure_ptr);

    let sig_ref = ctx.builder.import_signature(sig);

    let call = ctx.builder.ins().call_indirect(sig_ref, fn_ptr, &args);
    let res = ctx.builder.inst_results(call)[0];

    Ok(res)
}

/// A descriptor of an anonymous function.
#[derive(Debug, Clone)]
pub struct LustFn {
    /// The functions name. This is always in the form
    /// __anon_fn_{number}.
    pub name: String,
    /// The param names for the function.
    pub params: Vec<String>,
    /// The body of the function.
    pub body: Vec<Expr>,
    /// Variables that need to be captured in this function's closure.
    pub free_variables: Vec<String>,
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
                    free_variables: vec![],
                })
            }
        })
    }

    res
}

pub(crate) fn build_fn_map(functions: Vec<LustFn>) -> HashMap<String, LustFn> {
    functions.into_iter().map(|f| (f.name.clone(), f)).collect()
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

/// Collects the bound and unbound variables in E into two sets and
/// returns them in a tuple (bound, unbound).
fn analyze_variables(e: &Expr) -> (HashSet<&String>, HashSet<&String>) {
    let mut bound = HashSet::new();
    let mut free = HashSet::new();

    if let Some((name, binding)) = e.is_let() {
        bound.insert(name);
        let (newbound, newfree) = analyze_variables(binding);
        bound.extend(newbound);
        // Extend with the free variables found that do not have
        // bindings.
        free.extend(newfree.difference(&bound));
    } else if let Some((params, body)) = e.is_fndef() {
        // Variables that are bound in this function.
        let mut fn_bound: HashSet<&String> = params.into_iter().collect();

        for e in body {
            let (newbound, newfree) = analyze_variables(e);
            fn_bound.extend(newbound);
            let newfree: HashSet<&String> = newfree.difference(&fn_bound).map(|&x| x).collect();
            free.extend(newfree.difference(&bound));
        }
    } else if let Expr::Symbol(s) = e {
        // If isn't really a "primitive" in the string_is_primitive
        // sense so its seperated. If this becomes a common hack then
        // it's probably worthwhile to write some sort of
        // "string_is_builtin" method that also would include `let`
        // and `fn` which ought to be caught above.
        if !string_is_primitive(s) && s != "if" && !bound.contains(s) {
            free.insert(s);
        }
    } else if let Expr::List(v) = e {
        for e in v {
            let (newbound, newfree) = analyze_variables(e);
            bound.extend(newbound);
            // Extend with the free variables found that do not have
            // bindings.
            free.extend(newfree.difference(&bound));
        }
    }

    (bound, free)
}

/// A symbol in a let expression is bound for the remainder of the
/// current scope. A symbol is a function's arguments is bound for the
/// remainder of the current scope.
pub(crate) fn annotate_free_variables(f: &mut LustFn) {
    let mut bound: HashSet<&String> = f.params.iter().collect();
    let mut free = HashSet::<&String>::new();

    for e in &f.body {
        let (newbound, newfree) = analyze_variables(e);
        bound.extend(newbound);
        // Extend with the free variables found that do not have
        // bindings.
        free.extend(newfree.difference(&bound));
    }

    f.free_variables = free.into_iter().cloned().collect();
}

/// Emits code to allocate a closure and returns a pointer to it.
fn emit_alloc_closure(var_count: usize, ctx: &mut Context) -> Result<Value, String> {
    // Free variables and the function pointer.
    let size = var_count + 1;
    emit_alloc(size as i64, ctx)
}

fn letstack_contains(ctx: &Context, name: &str) -> bool {
    ctx.letstack.contains(&name.to_string())
}
pub fn option_contains<T>(o: &Option<T>, x: T) -> bool
where
    T: PartialEq,
{
    match o {
        Some(y) => *y == x,
        None => false,
    }
}

pub(crate) fn emit_make_closure(
    fn_name: &str,
    free_variables: &[String],
    ctx: &mut Context,
) -> Result<Value, String> {
    let closure_ptr = emit_alloc_closure(free_variables.len(), ctx)?;
    let fn_ptr = emit_get_fn_addr(fn_name, ctx)?;

    // Store the function pointer in the closure.
    ctx.builder
        .ins()
        .store(MemFlags::new(), fn_ptr, closure_ptr, 0);

    let word_size = ctx.word.bytes();

    for (offset, free) in free_variables.iter().enumerate() {
        let val = if letstack_contains(ctx, free) {
            ctx.builder
                .ins()
                .bor_imm(closure_ptr, crate::conversions::CLOSURE_TAG)
        } else {
            emit_var_access(free, ctx)?
        };
        // let val = emit_var_access(free, ctx)?;
        let byte_offset = (1 + (offset as u32)) * word_size;

        // Store the value in the closure.
        ctx.builder
            .ins()
            .store(MemFlags::new(), val, closure_ptr, byte_offset as i32);
    }

    // Tag the closure pointer
    Ok(ctx
        .builder
        .ins()
        .bor_imm(closure_ptr, crate::conversions::CLOSURE_TAG))
}

pub(crate) fn emit_get_fn_addr(name: &str, ctx: &mut Context) -> Result<Value, String> {
    let argcount = ctx
        .fnmap
        .get(name)
        .map(|f| f.params.len() + 1)
        .ok_or(format!("internal error finding arg count for {}", name))?;
    let mut sig = ctx.module.make_signature();
    for _ in 0..argcount {
        sig.params.push(AbiParam::new(ctx.word));
    }
    sig.returns.push(AbiParam::new(ctx.word));

    let callee = ctx
        .module
        .declare_function(name, Linkage::Local, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    Ok(ctx.builder.ins().func_addr(ctx.word, local_callee))
}

#[cfg(test)]
mod tests {
    use crate::errors::Printable;
    use crate::parser::Parser;
    use crate::roundtrip_file;
    use crate::roundtrip_string;

    use super::*;

    #[test]
    fn test_free_annotation() {
        let source = r#"
(let dog 10)
(let cat 11)
(let foo (fn (a b)
            (let bar (fn (c)
                          (foo b (add c a))))
            (if (bar dog) cat dog)))

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
        for mut f in &mut functions {
            annotate_free_variables(&mut f)
        }
        assert_eq!(functions.len(), 2);
        replace_functions(&mut exprs, &mut functions);

        // depth first traversal should mean functions[0] is bar
        let expected = vec!["a".to_string(), "b".to_string(), "foo".to_string()];
        functions[0].free_variables.sort();
        assert_eq!(expected, functions[0].free_variables);

        // depth first traversal should mean functions[1] is foo
        let expected = vec!["cat".to_string(), "dog".to_string(), "foo".to_string()];
        functions[1].free_variables.sort();
        assert_eq!(expected, functions[1].free_variables)
    }

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
    fn multiple_expr_body() {
        let source = r#"
(let fact (fn (n)
              (let n (add1 n))
              n))
(fact 14)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(15))
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
                 (mul n (fact (sub n 1))))))
(fact 14)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(87178291200))
    }

    #[test]
    fn weird_fib() {
        let res = roundtrip_file("examples/fib.lisp").unwrap();
        assert_eq!(res, Expr::Integer(55))
    }

    #[test]
    fn weird_recursion() {
        let res = roundtrip_file("examples/weird_recursion.lisp").unwrap();
        assert_eq!(res, Expr::Integer(55))
    }
}
