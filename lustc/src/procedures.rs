use std::collections::HashMap;
use std::collections::HashSet;

use crate::compiler::{emit_expr, JIT};
use crate::heap::emit_alloc;
use crate::locals::emit_var_decl_and_assign;
use crate::primitives::emit_contigous_to_list;
use crate::primitives::string_is_builtin;
use crate::Expr;
use crate::{compiler::Context, fatal::emit_check_callable};
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};

impl Expr {
    /// Determines whether or not the expression is a function
    /// call. If it is, returns a tuple containing the name of the
    /// function being called and a list of expressions corresponding
    /// to its arguments. Otherwise, returns None.
    pub fn is_fncall(&self) -> Option<(&Expr, &[Expr])> {
        match self {
            Self::List(v) => {
                if let Some(e) = v.first() {
                    Some((e, &v[1..]))
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
        match expr {
            Expr::List(v) => {
                let mut res = Vec::with_capacity(v.len());
                for e in v {
                    match e {
                        Expr::Symbol(s) => res.push(s),
                        _ => return None,
                    }
                }
                Some(res)
            }
            // An exmpty list gets parsed in as Expr::Nil so if that
            // is in the position of a function param list it means
            // that we have no arguments. If we add a dedicated nil
            // symbol this will need to change.
            Expr::Nil => Some(vec![]),
            _ => None,
        }
    }
}

/// Emits a function into the JIT.
pub fn emit_procedure(
    jit: &mut JIT,
    name: &str,
    params: &[String],
    body: &[Expr],
    varadic_symbol: &Option<String>,
    fnmap: &HashMap<String, LustFn>,
) -> Result<(), String> {
    let word = jit.module.target_config().pointer_type();

    // Closure param
    jit.context.func.signature.params.push(AbiParam::new(word));

    // Argument count param.
    jit.context.func.signature.params.push(AbiParam::new(word));

    // Location of arguments on heap
    jit.context.func.signature.params.push(AbiParam::new(word));

    // All lust functions return the result of evaluating their last
    // expression.
    jit.context.func.signature.returns.push(AbiParam::new(word));

    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let mut ctx = Context::new(
        builder,
        &mut jit.module,
        word,
        HashMap::new(),
        fnmap.clone(),
        Vec::new(),
    );

    let closure_ptr = ctx.builder.block_params(entry_block)[0];
    let arg_count = ctx.builder.block_params(entry_block)[1];

    crate::fatal::emit_check_arg_count(
        params.len(),
        arg_count,
        &mut ctx,
        varadic_symbol.is_some(),
    )?;

    let argloc = ctx.builder.block_params(entry_block)[2];

    // Assign regular arguments
    for (i, p) in params.iter().enumerate() {
        let val = ctx.builder.ins().load(
            word,
            MemFlags::new(),
            argloc,
            (i * word.bytes() as usize) as i32,
        );

        // Params that are escaped need to be initialized
        // appropriately.
        let val = if p.starts_with("e_") {
            let location = emit_alloc(ctx.word.bytes().into(), &mut ctx)?;
            ctx.builder.ins().store(MemFlags::new(), val, location, 0);
            location
        } else {
            val
        };
        emit_var_decl_and_assign(p, val, &mut ctx)?;
    }

    // Assign varadic argument
    if let Some(sym) = varadic_symbol {
        let varadic_len = ctx
            .builder
            .ins()
            .iadd_imm(arg_count, -(params.len() as i64));
        let varadic_ptr = ctx
            .builder
            .ins()
            .iadd_imm(argloc, (params.len() * word.bytes() as usize) as i64);
        let varadic_val = emit_contigous_to_list(&mut ctx, varadic_ptr, varadic_len)?;
        emit_var_decl_and_assign(sym, varadic_val, &mut ctx)?;
    }

    // Assign free variables from closure
    let free_vars = fnmap
        .get(name)
        .map(|f| &f.free_variables)
        .ok_or(format!("internal error finding free vars for {}", name))?;
    let word_size = ctx.word.bytes();

    for (i, free) in free_vars.iter().enumerate() {
        let offset = i + 1;
        let byte_offset = offset * (word_size as usize);

        let val =
            ctx.builder
                .ins()
                .load(ctx.word, MemFlags::new(), closure_ptr, byte_offset as i32);

        emit_var_decl_and_assign(free, val, &mut ctx)?;
    }

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

    Ok(())
}

/// Emits a call to a function. If the name is the name of an
/// anonymous function emits a direct call. Otherwise, emits an
/// indirect one to the function pointed to by the argument variable.
pub(crate) fn emit_fncall(head: &Expr, args: &[Expr], ctx: &mut Context) -> Result<Value, String> {
    let closure_ptr = emit_check_callable(head, ctx)?;

    let word = ctx.module.target_config().pointer_type();

    let mut sig = ctx.module.make_signature();

    // Argument which is a pointer to the closure
    sig.params.push(AbiParam::new(word));

    // Argument that is the number of args being passed in. Used for
    // validating the number of arguments and varadic functions.
    sig.params.push(AbiParam::new(word));

    // A pointer to where the arguments are stored on the heap.
    sig.params.push(AbiParam::new(word));

    // We always return a single word
    sig.returns.push(AbiParam::new(word));

    // First argumnet is a pointer to the closure
    let closure_ptr = ctx
        .builder
        .ins()
        .band_imm(closure_ptr, crate::conversions::HEAP_PTR_MASK);

    let fn_ptr = ctx
        .builder
        .ins()
        .load(ctx.word, MemFlags::new(), closure_ptr, 0);

    let mut argsc = vec![closure_ptr];

    // Second argument is the number of arguments we're going to pass in.
    argsc.push(ctx.builder.ins().iconst(word, args.len() as i64));

    // Allocate space for arguments and stash them away.
    let argloc = emit_alloc((args.len() * word.bytes() as usize) as i64, ctx)?;
    for (i, arg) in args.iter().enumerate() {
        let val = emit_expr(arg, ctx)?;
        ctx.builder.ins().store(
            MemFlags::new(),
            val,
            argloc,
            (i * word.bytes() as usize) as i32,
        );
    }
    argsc.push(argloc);

    let sig_ref = ctx.builder.import_signature(sig);

    let call = ctx.builder.ins().call_indirect(sig_ref, fn_ptr, &argsc);
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
    /// The symbol varadic arguments should be bound to if any.
    pub varadic_symbol: Option<String>,
}

fn is_varadic_param(p: &str) -> bool {
    p.len() == 3 && p.chars().last() == Some('&')
}

fn is_varadic_signature(sig: &[&String]) -> bool {
    sig.iter().any(|p| is_varadic_param(p))
}

fn get_and_validate_varadic(sig: &[&String]) -> Result<String, String> {
    if sig.len() < 2 {
        return Err("a varadic signature (one with the & symbol) must have at least one additional symbol to bind the varadic arguments to.".to_string());
    }
    if !is_varadic_param(sig[sig.len() - 2]) {
        return Err("varadic symbol (&) in non tail position".to_string());
    }
    Ok(sig[sig.len() - 1].clone())
}

/// Collects all of the anonymous functions in a program and returns a
/// list of them.
pub(crate) fn collect_functions(program: &[Expr]) -> Result<Vec<LustFn>, String> {
    let _t = crate::timer::timeit("function collection pass");
    let mut res = Vec::new();

    for e in program {
        e.postorder_traverse_res::<_, String>(&mut |e: &Expr| {
            if let Some((params, body)) = e.is_fndef() {
                let (varadic_symbol, params) = if is_varadic_signature(&params) {
                    (Some(get_and_validate_varadic(&params)?), {
                        let mut params: Vec<_> = params
                            .iter()
                            .filter(|p| !is_varadic_param(p))
                            .map(|p| *p)
                            .collect();
                        params.pop();
                        params
                    })
                } else {
                    (None, params)
                };

                let params = params.iter().map(|&s| s.clone()).collect();
                res.push(LustFn {
                    name: format!("__anon_fn_{}", res.len()),
                    params: params,
                    body: body.iter().map(|e| e.clone()).collect(),
                    free_variables: vec![],
                    varadic_symbol,
                });
            }
            Ok(())
        })?;
    }

    Ok(res)
}

pub(crate) fn build_fn_map(functions: Vec<LustFn>) -> HashMap<String, LustFn> {
    functions.into_iter().map(|f| (f.name.clone(), f)).collect()
}

/// Replaces functions with their anonymous names. Takes a program and
/// a list of functions from `collect_functions` as arguments. Needs
/// the list of functions because it needs to be sure to replace their
/// collected bodies with versions with the replaced functions inside.
pub(crate) fn replace_functions(program: &mut [Expr], functions: &mut [LustFn]) {
    let _t = crate::timer::timeit("function replacement pass");
    let mut count = 0;
    for e in program {
        // It is very important that this is a depth first traversal
        // so that we can be sure that the function bodies are
        // traversed before we hit them. At that point we can replace
        // the bodies from the `functions` list with the updated one.
        e.postorder_traverse_mut(&mut |e: &mut Expr| {
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
        let (newbound, newfree) = analyze_variables(binding);
        bound.extend(newbound);
        // If the binding is a function definition then our special
        // rule for recursive anonymous functions applies and we
        // determine that the variable being assigned to is not a free
        // variable after all as it will be bound to the function.
        //
        // Note that this is for the function that sees the internal
        // function declaration. The internal function declaration
        // will still annotate name as a free variable here. We're
        // just saying here that the outer function doesn't need to
        // try and capture the symbol as a free variable. The inner
        // one still does.
        if let Some(_) = binding.is_fndef() {
            bound.insert(name);
        }
        // Extend with the free variables found that do not have
        // bindings.
        free.extend(newfree.difference(&bound));
        bound.insert(name);
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
        if !string_is_builtin(s) && !bound.contains(s) && !s.starts_with("__anon_") {
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
    let _t = crate::timer::timeit("free variable analysis");
    let mut bound: HashSet<&String> = f.params.iter().collect();
    if let Some(s) = &f.varadic_symbol {
        bound.insert(s);
    }
    let mut free = HashSet::<&String>::new();

    for e in &f.body {
        let (newbound, newfree) = analyze_variables(e);
        // Extend with the free variables found that do not have
        // bindings in our scope yet.
        free.extend(newfree.difference(&bound));
        // Once that is done add the new bound variables to our
        // scope. This needs to happen second so variables that are
        // free and bound in analyze_variables are added properly.
        bound.extend(newbound);
    }

    f.free_variables = free.into_iter().cloned().collect();
}

/// Emits code to allocate a closure and returns a pointer to it.
fn emit_alloc_closure(var_count: usize, ctx: &mut Context) -> Result<Value, String> {
    // Free variables and the function pointer.
    let size = (var_count + 1) * (ctx.word.bytes() as usize);
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
            let pointer = ctx
                .builder
                .ins()
                .bor_imm(closure_ptr, crate::conversions::CLOSURE_TAG);
            let location = emit_alloc(ctx.word.bytes().into(), ctx)?;
            ctx.builder
                .ins()
                .store(MemFlags::new(), pointer, location, 0);
            location
        } else {
            // Rather than emit a regular var access here we want to
            // get the address which would give us the value of the
            // variable we're interested in the address of the
            // variable.
            match ctx.env.get(free) {
                Some(v) => Ok(ctx.builder.use_var(*v)),
                None => Err(format!(
                    "internal error: (constructing closure) use of undeclared variable ({})",
                    free
                )),
            }?
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
    let mut sig = ctx.module.make_signature();
    // Clojure
    sig.params.push(AbiParam::new(ctx.word));
    // Argument count
    sig.params.push(AbiParam::new(ctx.word));
    // Heap location of arguments
    sig.params.push(AbiParam::new(ctx.word));

    sig.returns.push(AbiParam::new(ctx.word));

    let callee = ctx
        .module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    Ok(ctx.builder.ins().func_addr(ctx.word, local_callee))
}

#[cfg(test)]
mod tests {
    use crate::parse_string;
    use crate::roundtrip_file;
    use crate::roundtrip_string;

    use super::*;

    #[test]
    fn test_free_annotation() {
        let source = r#"
(let dog 10)
(let cat 11)
(let half 5)
(let foo (fn (a b)
            (let half half)
            (let bar (fn (c)
                          (foo b (add c a))))
            (if (bar dog) cat dog)))

"#;
        let mut exprs = parse_string(source).unwrap();

        let mut functions = collect_functions(&exprs).unwrap();
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
        let expected = vec![
            "cat".to_string(),
            "dog".to_string(),
            "foo".to_string(),
            "half".to_string(),
        ];
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
        let mut exprs = parse_string(source).unwrap();

        let mut functions = collect_functions(&exprs).unwrap();
        assert_eq!(functions.len(), 6);

        replace_functions(&mut exprs, &mut functions);

        let functions = collect_functions(&exprs).unwrap();
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
        let exprs = parse_string(source).unwrap();

        let functions = collect_functions(&exprs).unwrap();

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

    /// Variables and function paramaters should not conflict.
    #[test]
    fn partial_free_var() {
        let source = r#"
(let half 5)
(let foo (fn (n)
             (let half (sub n half))
             (mul half 2)))
(foo 26)
"#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(res, Expr::Integer(42))
    }

    #[test]
    fn fib() {
        let res = roundtrip_file("examples/fib.lisp").unwrap();
        assert_eq!(res, Expr::Integer(102334155))
    }

    #[test]
    fn weird_recursion() {
        let res = roundtrip_file("examples/weird_recursion.lisp").unwrap();
        assert_eq!(res, Expr::Integer(55))
    }

    #[test]
    fn memoized_fib() {
        let res = roundtrip_file("examples/memoized_fib.lisp").unwrap();
        assert_eq!(res, Expr::Integer(102334155))
    }

    #[test]
    fn do_impl() {
        let res = roundtrip_file("examples/do.lisp").unwrap();
        assert_eq!(res, Expr::Integer(11))
    }

    #[test]
    fn list_impl() {
        let res = roundtrip_file("examples/list.lisp").unwrap();
        assert_eq!(
            res,
            Expr::List(vec![
                Expr::Integer(1),
                Expr::List(vec![
                    Expr::Integer(2),
                    Expr::List(vec![
                        Expr::Integer(3),
                        Expr::List(vec![
                            Expr::List(vec![Expr::Integer(1), Expr::Integer(2)]),
                            Expr::Nil
                        ])
                    ])
                ])
            ])
        )
    }

    #[test]
    fn varadic_collection() {
        // Hack here where we prefix the varadic symbol with two
        // characters so that functions that expect processed input
        // handle it correcty.
        let source = r#"
(let half 5)
(let foo (fn (n aa& m)
             (let half (sub n half))
             (mul half 2)))
(foo 26)
"#;
        let exprs = parse_string(source).unwrap();
        let functions = collect_functions(&exprs).unwrap();
        let function = functions.first().unwrap();
        assert_eq!(Some("m".to_string()), function.varadic_symbol);
        assert_eq!(1, function.params.len())
    }

    #[test]
    fn arg_counts() {
        let source = r#"
    (let reg (fn (a b) (add a b)))
    (let v   (fn (a & c) c))
    (reg 1 2)
    (v 2 3 4)
    (v 1)
    (let res (v 1 2 3 4))
    (car (cdr (cdr res)))
    "#;
        let res = roundtrip_string(source).unwrap();
        assert_eq!(Expr::Integer(4), res)
    }
}
