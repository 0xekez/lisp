use crate::{Expr, Word};

static FIXNUM_SHIFT: Word = 2;
static FIXNUM_MASK: Word = 0x3;
static FIXNUM_TAG: Word = 0;

static CHAR_SHIFT: Word = 8;
static CHAR_MASK: Word = 0b11111111;
static CHAR_TAG: Word = 0b00001111;

static BOOL_SHIFT: Word = 7;
static BOOL_MASK: Word = 0b1111111;
static BOOL_TAG: Word = 0b0011111;

static NIL_VALUE: Word = 0b00101111;

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

pub fn word_is_immediate(what: Word) -> bool {
    word_is_int(what) || word_is_char(what) || word_is_bool(what) || word_is_nil(what)
}

impl Expr {
    pub fn is_immediate(&self) -> bool {
        true
    }

    pub fn immediate_rep(&self) -> Word {
        debug_assert!(self.is_immediate(), "expected immediate type");
        match self {
            Self::Integer(i) => (i << FIXNUM_SHIFT) | FIXNUM_TAG,
            Self::Char(c) => ((*c as Word) << CHAR_SHIFT) | CHAR_TAG,
            Self::Bool(b) => ((*b as Word) << BOOL_SHIFT) | BOOL_TAG,
            Self::Nil => NIL_VALUE,
        }
    }

    pub fn from_immediate(what: Word) -> Expr {
        debug_assert!(word_is_immediate(what), "expected immediate type");
        match () {
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
