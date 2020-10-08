use crate::location::Location;
use crate::tokenizer::{Token, Tokenizer};

// Welcome to the token buffer. This is possibly the strangest part of
// lust thus far, but there are some reasons for it to be this way so
// bear with me. This file implements a token buffer class which
// incrementally yields tokens to the parser. At any given time the
// token stream could end so the returned tokens are wrapped in an
// option.
//
// The token buffer exposes a peek_token method which gives the parser
// some look ahead and then needs a way to advance. Before the changes
// that I'm about to get to, the typical parse routine would look
// something like this and would be subtly broken:
//
// match buffer.peek_token() {
//   Some(t) => match t.ttype {
//     TokenType::Cparen => Foo(buffer.next_token())
//   }
//   None => "damn"
// }
//
// The problem with code like this is that if we were to implement a
// sufficiently general next_token() method it too would need to
// return an optional Token because it has no way of verifying that
// the caller has peeked and ensured that its return value
// exists. Doing that leads to really gross code in the parser where
// you peek for a token, and then have to check again that the
// next_token exists or do something dangerous and unwrap the return
// value. In order to get around that problem we do the following:
//
// When you peek for a token, you get an optional value that is None
// if there is no remaining tokens. If there is remaining tokens, you
// get the token you asked for and you get a CheckedTokenBuffer. A
// CheckedTokenBuffer is a struct that holds a pointer to the buffer
// that you called peek on and the next token in the buffer. If you'd
// like to get the token out of it, you can call advance on it. This
// yields the token that it holds and moves its parent token buffer
// forward it its token stream.
//
// This is much cleaner and safer. We can remove the next_token()
// method entirely and the type system ensures that we never read a
// bad token from the token buffer. It looks something like this:
//
// match buffer.peek_token() {
//   Some((t, checked)) => match t.ttype {
//     TokenType::Cparen => Foo(checked.advance())
//   }
//   None => "damn"
// }
//
// TLDR: when peek_token is successful it returns a handle that allows
// you to advance to the next token without verifying that there is
// one.

/// A simple buffer for tokens that allows for lookahead in the
/// parser.
#[derive(Debug)]
pub struct TokenBuffer<'a> {
    tokenizer: Tokenizer<'a>,
    peek: Option<Token>,
}

pub struct CheckedTokenBuffer<'a, 'b> {
    parent: &'a mut TokenBuffer<'b>,
    token: Token,
}

impl<'a> TokenBuffer<'a> {
    /// Creates a new token buffer at the beginning of input.
    pub fn new(input: &'a str) -> Self {
        let mut tokenizer = Tokenizer::new(input);
        let peek = tokenizer.next_token();
        Self { tokenizer, peek }
    }

    /// Look at the next token without retreiving it. If there is no
    /// next token this will return None.
    pub fn peek_token(&mut self) -> Option<(Token, CheckedTokenBuffer<'_, 'a>)> {
        // There is some nasy coppies happening in here that I don't
        // have the mental stamina to work out right now becuase we
        // borrow self as mutable below and as such can't easily not
        // make a copy.
        match self.peek.clone() {
            None => None,
            Some(t) => Some((
                t.clone(),
                CheckedTokenBuffer {
                    parent: self,
                    token: t.clone(),
                },
            )),
        }
    }

    pub fn loc(&self) -> Location {
        self.tokenizer.loc()
    }

    /// Retreive the next token and advance the token buffer. If there
    /// is no next token this will return None.
    fn next_token(&mut self) -> Option<Token> {
        let res = self.peek.clone();
        self.peek = self.tokenizer.next_token();
        res
    }
}

impl<'a, 'b> CheckedTokenBuffer<'a, 'b> {
    pub fn advance(self) -> Token {
        self.parent.next_token();
        self.token.clone()
    }
}
