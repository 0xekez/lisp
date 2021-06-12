//! Garbage collection

use backtrace::Backtrace;

// The number of bytes that have been allocated since the last gc run.
static mut ALLOC_AMOUNT: crate::Word = 0;

// This is the same threshold suggested by emacs lsp for increased
// emacs performance.
const GC_THRESHOLD: crate::Word = 100000000;

// Shim which will later do garbage collection. Has extremely crude
// heuristic for when to do allocation. Real garbage collection should
// update this.
pub extern "C" fn do_gc(amount: crate::Word) {
    unsafe {
        ALLOC_AMOUNT += amount;
    }
    // Trigger garbage collection if we're using over our gc threshold.
    // memory.
    if unsafe { ALLOC_AMOUNT > GC_THRESHOLD } {
        let bt = Backtrace::new_unresolved();
        println!("{:?}", bt);
        unsafe {
            ALLOC_AMOUNT = 0;
        }
    }
}
