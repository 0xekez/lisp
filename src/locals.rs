//! Just down the street
//! Two stack slots up
//! That's where you can find the Lets

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_module::Module;

use crate::compiler::emit_expr;
use crate::compiler::Context;
use crate::heap::emit_alloc;
use crate::primitives::string_is_primitive;
use crate::procedures::emit_make_closure;
use crate::Expr;

impl Expr {
    /// Determines if the expression is a let expression and if it is
    /// returns the name and expression being bound.
    pub fn is_let(&self) -> Option<(&String, &Expr)> {
        if let Self::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "let" && v.len() == 3 {
                    if let Expr::Symbol(s) = &v[1] {
                        return Some((s, &v[2]));
                    }
                }
            }
        }
        None
    }

    /// Determines if the expression is a set expression and if it is
    /// returns the name and expression being set.
    pub fn is_set(&self) -> Option<(&String, &Expr)> {
        if let Self::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "set" && v.len() == 3 {
                    if let Expr::Symbol(s) = &v[1] {
                        return Some((s, &v[2]));
                    }
                }
            }
        }
        None
    }
}

pub(crate) fn emit_let(name: &str, val: &Expr, ctx: &mut Context) -> Result<Value, String> {
    // The variable is declared but not defined yet so it is on the letstack
    ctx.letstack.push(name.to_string());

    let val = emit_expr(val, ctx)?;

    // If the value is an escaped value then we allocate space for it
    // on the heap and store its value there.
    let val = if name.starts_with("e_") {
        let location = emit_alloc(ctx.word.bytes().into(), ctx)?;
        ctx.builder.ins().store(MemFlags::new(), val, location, 0);
        location
    } else {
        val
    };

    let var = if let Some(_) = ctx.env.get(name) {
        Err(format!(
            "internal error: let expression modifying an already declared variable: {}",
            name
        ))
    } else {
        Ok(emit_declare_var(
            name,
            &mut ctx.env,
            &mut ctx.builder,
            ctx.word,
        )?)
    }?;

    ctx.builder.def_var(var, val);

    // The variable now has a value so it is no longer on the letstack.
    ctx.letstack.pop();

    Ok(ctx.builder.use_var(var))
}

pub(crate) fn emit_set(target: &str, val: &Expr, ctx: &mut Context) -> Result<Value, String> {
    let val = emit_expr(val, ctx)?;
    let var = ctx.env.get(target).ok_or(format!(
        "use of undeclared variable ({}) in set expression",
        target
    ))?;

    if target.starts_with("e_") {
        let location = ctx.builder.use_var(*var);
        ctx.builder.ins().store(MemFlags::new(), val, location, 0);
    } else {
        ctx.builder.def_var(*var, val);
    }
    Ok(val)
}

pub(crate) fn emit_var_access(name: &str, ctx: &mut Context) -> Result<Value, String> {
    if name.starts_with("__anon_fn_") || string_is_primitive(name) {
        // We're dealing with a closure so we'll need to make one.
        let free_variables = ctx
            .fnmap
            .get(name)
            // Need to clone because otherwise we'd have an imutable
            // ref to something inside context here and then procede
            // to pass a utbale reference to context to make_closure.
            .map(|f| f.free_variables.clone())
            .ok_or(format!("internal error: {} not found in argmap", name))?;

        emit_make_closure(name, &free_variables, ctx)
    } else if name.starts_with("__anon_data_") {
        let sym = ctx
            .module
            .declare_data(name, cranelift_module::Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;
        let local_id = ctx.module.declare_data_in_func(sym, ctx.builder.func);

        let data_ptr = ctx.builder.ins().symbol_value(ctx.word, local_id);
        Ok(ctx
            .builder
            .ins()
            .load(ctx.word, MemFlags::new(), data_ptr, 0))
    } else if name.starts_with("e_") {
        let var = ctx.env.get(name).ok_or(format!(
            "internal error: use of undeclared variable ({})",
            name
        ))?;
        let location = ctx.builder.use_var(*var);
        Ok(ctx
            .builder
            .ins()
            .load(ctx.word, MemFlags::new(), location, 0))
    } else {
        match ctx.env.get(name) {
            Some(v) => Ok(ctx.builder.use_var(*v)),
            None => Err(format!(
                "internal error: use of undeclared variable ({})",
                name
            )),
        }
    }
}

pub(crate) fn emit_declare_var(
    name: &str,
    env: &mut HashMap<String, Variable>,
    builder: &mut FunctionBuilder,
    word: Type,
) -> Result<Variable, String> {
    if env.contains_key(name) {
        return Err(format!("variable ({}) is declared more than once", name));
    }
    let index = env.len();
    let var = Variable::new(index);
    builder.declare_var(var, word);
    env.insert(name.to_string(), var);
    Ok(var)
}

pub(crate) fn emit_var_decl_and_assign(
    name: &str,
    val: Value,
    ctx: &mut Context,
) -> Result<Variable, String> {
    let var = emit_declare_var(name, &mut ctx.env, &mut ctx.builder, ctx.word)?;

    ctx.builder.def_var(var, val);
    Ok(var)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_evaluation(exprs: &mut [Expr], expected: Expr) {
        assert_eq!(crate::compiler::roundtrip_program(exprs).unwrap(), expected)
    }

    #[test]
    fn let_return() {
        let mut ast = [Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::Symbol("tel".to_string()),
            Expr::Integer(10),
        ])];
        let expected = Expr::Integer(10);
        test_evaluation(&mut ast, expected);

        let mut ast = [Expr::List(vec![
            Expr::Symbol("let".to_string()),
            Expr::Symbol("tel".to_string()),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("ttel".to_string()),
                Expr::Char('🥺'),
            ]),
        ])];
        let expected = Expr::Char('🥺');
        test_evaluation(&mut ast, expected);
    }

    #[test]
    fn naked_var() {
        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::Symbol("tel".to_string()),
        ];
        let expected = Expr::Integer(10);
        test_evaluation(&mut ast, expected);

        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::List(vec![
                    Expr::Symbol("let".to_string()),
                    Expr::Symbol("🚨".to_string()),
                    Expr::Char('🥺'),
                ]),
            ]),
            Expr::Symbol("🚨".to_string()),
        ];
        let expected = Expr::Char('🥺');
        test_evaluation(&mut ast, expected);
    }

    #[test]
    fn clothed_var() {
        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Integer(1),
                Expr::Symbol("tel".to_string()),
            ]),
        ];
        let expected = Expr::Integer(11);
        test_evaluation(&mut ast, expected);
    }

    #[test]
    fn var_double_use() {
        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Symbol("tel".to_string()),
            ]),
        ];
        let expected = Expr::Integer(20);
        test_evaluation(&mut ast, expected);
    }

    #[test]
    fn lots_of_vars() {
        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel1".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel2".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel3".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel4".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel5".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel6".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel7".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel8".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel9".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("add".to_string()),
                Expr::Symbol("tel8".to_string()),
                Expr::Symbol("tel9".to_string()),
            ]),
        ];
        let expected = Expr::Integer(20);
        test_evaluation(&mut ast, expected);
    }

    /// Let expressions shouldn't redefine variables. There are
    /// variable uniquifying passes that should happen earlier in the
    /// compiler that make let expressions shadow variables instead of
    /// modifying existing ones.
    #[test]
    #[should_panic]
    fn var_redef() {
        let mut ast = [
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("twice".to_string()),
                Expr::Integer(10),
            ]),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("twice".to_string()),
                Expr::Integer(10),
            ]),
        ];
        crate::compiler::roundtrip_exprs(&mut ast).unwrap();
    }
}
