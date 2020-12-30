use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        println!("usage: lustc <file>");
        return;
    }

    let res = lustc::roundtrip_file(&args[1]);

    println!("=> {:?}", res);
}
