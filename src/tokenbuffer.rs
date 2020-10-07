/// Handles feeding tokens into the parser.
use crate::tokenizer::{Token, Tokenizer};

/// A simple buffer for tokens that allows for lookahead in the
/// parser.
pub struct TokenBuffer<'a> {
    tokenizer: Tokenizer<'a>,
    peek: Option<Token>,
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
    pub fn peek_token(&self) -> &Option<Token> {
        &self.peek
    }

    /// Retreive the next token and advance the token buffer. If there
    /// is no next token this will return None.
    pub fn next_token(&mut self) -> Option<Token> {
        let res = self.peek.clone();
        self.peek = self.tokenizer.next_token();
        res
    }
}
