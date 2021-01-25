//! Pass to give all variables unique names. Makes later program
//! analysis easier and ensures that builtin names like __anon_fn and
//! __anon_data don't conflict with user defined names.

use std::collections::HashMap;

use crate::primitives::string_is_builtin;
use crate::Expr;
use crate::PreorderStatus;

impl Expr {
    /// Renames the variable being bound to by the let expression that
    /// this expression is assumed to represent. Returns an error if
    /// the expression is not indeed a let expression.
    fn rename_let_binding(&mut self, count: usize) -> Result<(), String> {
        if let Self::List(v) = self {
            if let Some(Expr::Symbol(s)) = v.first() {
                if s == "let" && v.len() == 3 {
                    if let Expr::Symbol(s) = &mut v[1] {
                        *s = format!("{}_{}", count, s);
                        return Ok(());
                    }
                }
            }
        }
        Err(format!(
            "internal error: rename_let_binding called on non-let expression: ({:?})",
            self
        ))
    }

    fn get_let_name(&mut self) -> Result<String, String> {
        if let Some((name, _)) = self.is_let() {
            Ok(name.clone())
        } else {
            Err(format!(
                "internal error: get_let_name called on non-let expression: ({:?})",
                self
            ))
        }
    }

    fn get_let_value_mut(&mut self) -> Result<&mut Expr, String> {
        if let Some(_) = self.is_let() {
            if let Self::List(v) = self {
                return Ok(&mut v[2]);
            }
        }
        Err(format!(
            "internal error: get_let_value_mut called on non-let expression: ({:?})",
            self
        ))
    }

    fn get_symbol_name(&self) -> Result<String, String> {
        if let Self::Symbol(s) = self {
            Ok(s.clone())
        } else {
            Err(format!(
                "internal error: get_symbol_name called on non-symbol expression: ({:?})",
                self
            ))
        }
    }

    fn rename_fn_params(
        &mut self,
        count: &mut usize,
        env: &mut HashMap<String, String>,
    ) -> Result<(), String> {
        if let Some(_) = self.is_fndef() {
            if let Self::List(v) = self {
                if let Self::List(v) = &mut v[1] {
                    for s in v {
                        let old_name = s.get_symbol_name()?;
                        let new_name = format!("{}_{}", *count, old_name);
                        *s = Expr::Symbol(new_name);
                        *count += 1;
                        env.insert(old_name, s.get_symbol_name()?);
                    }
                    return Ok(());
                } else if let Self::Nil = v[1] {
                    return Ok(());
                }
            }
        }
        Err(format!(
            "internal error: rename_fn_params called on non-fndef expression: ({:?})",
            self
        ))
    }

    fn get_fn_body_mut(&mut self) -> Result<&mut [Expr], String> {
        if let Some(_) = self.is_fndef() {
            if let Self::List(v) = self {
                return Ok(&mut v[2..]);
            }
        }
        Err(format!(
            "internal error: get_fn_body_mut called on non-fndef expression: ({:?})",
            self
        ))
    }
}

fn make_expr_names_unique(
    expr: &mut Expr,
    env: &mut HashMap<String, String>,
    count: &mut usize,
) -> Result<(), String> {
    expr.preorder_traverse_mut_res::<_, String>(&mut |expr| {
        if let Some(_) = expr.is_let() {
            let old_name = expr.get_let_name()?;

            let name_exists = env.contains_key(&old_name); // && !expr.get_let_value_mut()?.is_fndef().is_some();

            if name_exists {
                make_expr_names_unique(expr.get_let_value_mut()?, env, count)?;
            }

            expr.rename_let_binding(*count)?;
            *count += 1;
            // We insert after renaming the body because the body
            // should be evaluated in the old scope so that varaible
            // shadowing works.
            env.insert(old_name, expr.get_let_name()?);

            // If the name does not already exist we need to evaluate
            // the body using the new variable name so that recursion
            // works as expected.
            if !name_exists {
                make_expr_names_unique(expr.get_let_value_mut()?, env, count)?;
            }

            // Because we've already traversed the let expression's
            // body above we indicate to the traversal that it should
            // skip the rest of this expression.
            return Ok(PreorderStatus::Skip);
        } else if let Some(_) = expr.is_fndef() {
            // Make a new enviroment to analyze this function in. It
            // has all of the items in the current enviroment and
            // function arguments will supercede existing names.
            let mut nenv = env.clone();
            expr.rename_fn_params(count, &mut nenv)?;
            for e in expr.get_fn_body_mut()? {
                make_expr_names_unique(e, &mut nenv, count)?;
            }
            // We've already traversed the body so we don't want the
            // traversal to continue on this expr.
            return Ok(PreorderStatus::Skip);
        } else if let Expr::Symbol(s) = expr {
            let newname = env
                .get(s)
                .or_else(|| if string_is_builtin(s) { Some(s) } else { None })
                .ok_or(format!("undefined variable ({})", s))?;
            *s = newname.to_string();
        }

        Ok(PreorderStatus::Continue)
    })?;
    Ok(())
}

pub fn make_names_unique(program: &mut [Expr]) -> Result<(), String> {
    let mut count = 0;
    let mut env = HashMap::new();

    for e in program {
        make_expr_names_unique(e, &mut env, &mut count)?;
    }

    Ok(())
}
