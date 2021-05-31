//! Garbage collection

use backtrace::Backtrace;

fn foo() {
    let bt = Backtrace::new();
    println!("{:?}", bt)
}

pub(crate) fn do_gc() {
    foo()
}
