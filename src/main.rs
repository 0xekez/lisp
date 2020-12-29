fn main() {
    let input = "(add1 25)";

    let res = lustcompiler::roundtrip_string(&input);

    println!("=> {:?}", res);
}
