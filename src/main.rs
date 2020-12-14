use lustcompiler::compiler;
use lustcompiler::Expr;

fn main() {
    let ast = Expr::Integer(-1);

    let res = compiler::roundtrip_expr(ast);

    println!("=> {:?}", res);
}
