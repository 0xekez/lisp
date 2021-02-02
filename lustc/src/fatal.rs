use crate::{compiler, foreign, Expr};

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

pub(crate) fn emit_error(
    message: &Expr,
    exit_code: &Expr,
    ctx: &mut compiler::Context,
) -> Result<cranelift::prelude::Value, String> {
    foreign::emit_foreign_call("puts", &[message.clone()], ctx)?;
    foreign::emit_foreign_call("exit", &[exit_code.clone()], ctx)
}
