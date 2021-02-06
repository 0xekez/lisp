//! Determines what variables will need to be stored on the heap.

use crate::procedures::LustFn;
use crate::Expr;

fn mark_escaped_variables(free_vars: &[String], scope: &mut [Expr]) {
    for e in scope {
        e.postorder_traverse_mut(&mut |e: &mut Expr| {
            if let Expr::Symbol(s) = e {
                if free_vars.contains(&s) {
                    *s = format!("e_{}", s);
                }
            }
        })
    }
}

fn patch_free_vars(f: &mut LustFn) {
    for s in f.free_variables.iter_mut() {
        *s = format!("e_{}", s);
    }
}

fn patch_params(f: &mut LustFn, free_vars: &[String]) {
    for s in f.params.iter_mut() {
        if free_vars.contains(s) {
            *s = format!("e_{}", s);
        }
    }
}

pub fn annotate_escaped_variables(
    functions: &mut [LustFn],
    program: &mut [Expr],
) -> Result<(), String> {
    let _t = crate::timer::timeit("free variable annotation");
    let mut free_vars = Vec::new();
    for f in functions.iter() {
        free_vars.extend(f.free_variables.iter().cloned())
    }

    for f in functions.iter_mut() {
        mark_escaped_variables(&free_vars, &mut f.body);
        patch_free_vars(f);
        patch_params(f, &free_vars);
    }

    mark_escaped_variables(&free_vars, program);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::roundtrip_file;

    use super::*;

    #[test]
    fn test_objects() {
        let expected = Expr::List(vec![Expr::Integer(11), Expr::Integer(10)]);
        let actual = roundtrip_file("examples/objects.lisp").unwrap();
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_set() {
        let expected = Expr::List(vec![Expr::Integer(11), Expr::Integer(12)]);
        let actual = roundtrip_file("examples/set.lisp").unwrap();
        assert_eq!(actual, expected)
    }
}
