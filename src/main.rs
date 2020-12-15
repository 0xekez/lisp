use lustcompiler::compiler;
use lustcompiler::Expr;

fn main() {
    let ast = Expr::List(vec![
        Expr::Symbol("integer->char".to_string()),
        Expr::List(vec![
            Expr::Symbol("char->integer".to_string()),
            Expr::Char('❤'),
        ]),
    ]);

    let res = compiler::roundtrip_expr(ast);

    println!("=> {:?}", res);
}
