use cranelift::prelude::*;

use crate::compiler::emit_expr;
use crate::compiler::Context;
use crate::Expr;

impl Expr {
    pub fn is_conditional(&self) -> Option<(&Expr, &Expr, &Expr)> {
        match self {
            Self::List(v) => {
                if let Some(Expr::Symbol(s)) = v.first() {
                    if s == "if" && v.len() == 4 {
                        Some((&v[1], &v[2], &v[3]))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub(crate) fn emit_conditional(
    cond: &Expr,
    then: &Expr,
    else_: &Expr,
    ctx: &mut Context,
) -> Result<Value, String> {
    let cond = emit_expr(cond, ctx)?;
    let cond = ctx
        .builder
        .ins()
        .icmp_imm(IntCC::Equal, cond, Expr::Bool(true).immediate_rep());

    let then_block = ctx.builder.create_block();
    let else_block = ctx.builder.create_block();
    let merge_block = ctx.builder.create_block();

    // Our if-else blocks have a return value. Cranelift has no PHI
    // form so instead of using a PHI we use a merge block that each
    // of the then and else blocks jump to with their return value.
    ctx.builder.append_block_param(merge_block, ctx.word);

    // On false, jump to the else block.
    ctx.builder.ins().brz(cond, else_block, &[]);
    // On true, "fall through"
    ctx.builder.ins().jump(then_block, &[]);

    // Compile the then block.
    ctx.builder.switch_to_block(then_block);
    ctx.builder.seal_block(then_block);

    let then_return = emit_expr(then, ctx)?;

    ctx.builder.ins().jump(merge_block, &[then_return]);

    // Compile the else block.
    ctx.builder.switch_to_block(else_block);
    ctx.builder.seal_block(else_block);

    let else_return = emit_expr(else_, ctx)?;

    ctx.builder.ins().jump(merge_block, &[else_return]);

    // Seal the merge block now that we've compiled all the ways to
    // reach it.
    ctx.builder.switch_to_block(merge_block);
    ctx.builder.seal_block(merge_block);

    let res = ctx.builder.block_params(merge_block)[0];

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_evaluation(exprs: &[Expr], expected: Expr) {
        assert_eq!(crate::compiler::roundtrip_exprs(exprs).unwrap(), expected)
    }

    #[test]
    fn if_return() {
        let ast = [Expr::List(vec![
            Expr::Symbol("if".to_string()),
            Expr::Bool(true),
            Expr::Integer(10),
            Expr::Integer(20),
        ])];
        let expected = Expr::Integer(10);
        test_evaluation(&ast, expected);

        let ast = [Expr::List(vec![
            Expr::Symbol("if".to_string()),
            Expr::Bool(false),
            Expr::Integer(10),
            Expr::Nil,
        ])];
        let expected = Expr::Nil;
        test_evaluation(&ast, expected);
    }

    #[test]
    fn if_let() {
        let ast = [Expr::List(vec![
            Expr::Symbol("if".to_string()),
            Expr::List(vec![
                Expr::Symbol("let".to_string()),
                Expr::Symbol("tel".to_string()),
                Expr::Bool(true),
            ]),
            Expr::Symbol("tel".to_string()),
            Expr::Integer(20),
        ])];
        let expected = Expr::Bool(true);
        test_evaluation(&ast, expected);
    }
}
