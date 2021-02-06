use std::time::Instant;

static mut SHOW_TIMES: bool = false;

pub fn init(show_times: bool) {
    unsafe {
        SHOW_TIMES = show_times;
    }
}

// source: https://github.com/matklad/hashbench/blob/master/src/main.rs#L39-L47
pub fn timeit(label: &'static str) -> impl Drop {
    struct Timer(&'static str, Instant);
    impl Drop for Timer {
        fn drop(&mut self) {
            if unsafe { SHOW_TIMES } {
                println!("{:<33} {:.2?}", self.0, self.1.elapsed());
            }
        }
    }
    Timer(label, Instant::now())
}
