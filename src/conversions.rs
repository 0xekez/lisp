use crate::{Expr, UWord, Word};

pub(crate) static FIXNUM_SHIFT: Word = 2;
pub(crate) static FIXNUM_MASK: Word = 0b11;
pub(crate) static FIXNUM_TAG: Word = 0;

pub(crate) static CHAR_SHIFT: Word = 8;
pub(crate) static CHAR_MASK: Word = 0b11111111;
pub(crate) static CHAR_TAG: Word = 0b00001111;

pub(crate) static BOOL_SHIFT: Word = 7;
pub(crate) static BOOL_MASK: Word = 0b1111111;
pub(crate) static BOOL_TAG: Word = 0b0011111;

pub(crate) static NIL_VALUE: Word = 0b00101111;

/// Values on the heap use their last three bits (values 0..7) to
/// store their type tag. The tag mask extracts that tag value.
pub(crate) static HEAP_TAG_MASK: Word = 0b111;
/// In order to get the actual pointer that a value on the heap points
/// to we use the inverse of ptr mask which zeros out the heap tag.
pub(crate) static HEAP_PTR_MASK: Word = !HEAP_TAG_MASK;

pub(crate) static PAIR_TAG: Word = 0b001;

pub(crate) static CLOSURE_TAG: Word = 0b110;

pub fn word_is_char(what: Word) -> bool {
    what & CHAR_MASK == CHAR_TAG
}

pub fn word_is_int(what: Word) -> bool {
    what & FIXNUM_MASK == FIXNUM_TAG
}

pub fn word_is_bool(what: Word) -> bool {
    what & BOOL_MASK == BOOL_TAG
}

pub fn word_is_nil(what: Word) -> bool {
    what == NIL_VALUE
}

pub fn word_is_pair(what: Word) -> bool {
    what & HEAP_TAG_MASK == PAIR_TAG
}

pub fn word_is_object(what: Word) -> bool {
    word_is_pair(what)
}

pub fn word_is_immediate(what: Word) -> bool {
    word_is_int(what)
        || word_is_char(what)
        || word_is_bool(what)
        || word_is_nil(what)
        || word_is_pair(what)
}

pub fn word_get_object_address(what: Word) -> UWord {
    debug_assert!(word_is_object(what));
    (what & HEAP_PTR_MASK) as UWord
}

pub fn list_to_immediate(list: &[Expr]) -> Word {
    if let Some(e) = list.first() {
        let mut pair = Vec::with_capacity(2);
        pair.push(e.immediate_rep());
        pair.push(list_to_immediate(&list[1..]));
        let ptr_word = pair.as_mut_ptr() as Word;
        std::mem::forget(pair);
        ptr_word | PAIR_TAG
    } else {
        NIL_VALUE
    }
}

pub fn list_from_immediate(ptr_word: Word) -> Expr {
    debug_assert_eq!(ptr_word & HEAP_TAG_MASK, PAIR_TAG);
    let ptr = (ptr_word & HEAP_PTR_MASK) as *mut Word;
    let slice = unsafe { std::slice::from_raw_parts(ptr, 2) };
    let first = Expr::from_immediate(slice[0]);
    let rest = Expr::from_immediate(slice[1]);

    Expr::List(vec![first, rest])
}

impl Expr {
    pub fn is_immediate(&self) -> bool {
        true
    }

    pub fn immediate_rep(&self) -> Word {
        debug_assert!(self.is_immediate(), "expected immediate type");
        match self {
            Expr::Integer(i) => (i << FIXNUM_SHIFT) | FIXNUM_TAG,
            Expr::Char(c) => ((*c as Word) << CHAR_SHIFT) | CHAR_TAG,
            Expr::Bool(b) => ((*b as Word) << BOOL_SHIFT) | BOOL_TAG,
            Expr::Nil => NIL_VALUE,
            Expr::List(v) => list_to_immediate(v),
            Expr::Symbol(_) => todo!("symbol immediates unsupported"),
        }
    }

    pub fn from_immediate(what: Word) -> Expr {
        debug_assert!(word_is_immediate(what), "expected immediate type");
        match () {
            _ if word_is_pair(what) => list_from_immediate(what),
            _ if word_is_int(what) => Expr::Integer(what >> FIXNUM_SHIFT),
            _ if word_is_char(what) => {
                Expr::Char(unsafe { std::mem::transmute_copy(&(what >> CHAR_SHIFT)) })
            }
            _ if word_is_bool(what) => {
                Expr::Bool(unsafe { std::mem::transmute_copy(&(what >> BOOL_SHIFT)) })
            }
            _ if word_is_nil(what) => Expr::Nil,
            _ => Expr::Nil,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_roundtrip(expr: Expr) {
        assert_eq!(expr.clone(), crate::compiler::roundtrip_expr(expr).unwrap())
    }

    #[test]
    fn roundtrip_int() {
        for i in -100..100 {
            test_roundtrip(Expr::Integer(i));
        }
    }

    #[test]
    fn roundtrip_bool() {
        test_roundtrip(Expr::Bool(false));
        test_roundtrip(Expr::Bool(true));
    }

    #[test]
    fn roundtrip_char() {
        for c in '😀'..'😗' {
            test_roundtrip(Expr::Char(c));
        }

        for c in 'a'..'z' {
            test_roundtrip(Expr::Char(c));
        }
    }

    #[test]
    fn roundtrip_nil() {
        test_roundtrip(Expr::Nil);
    }

    #[test]
    fn rountrip_list() {
        let start = Expr::List(vec![Expr::Integer(1), Expr::Bool(false)]);
        let start_immediate = start.immediate_rep();
        let end = Expr::from_immediate(start_immediate);

        // Roundtripping a list results in a cons structure and not a
        // list like we started with due to the way that lists are
        // represented in Lust vs Rust.
        assert_eq!(
            end,
            Expr::List(vec![
                Expr::Integer(1),
                Expr::List(vec![Expr::Bool(false), Expr::Nil])
            ])
        )
    }
}
