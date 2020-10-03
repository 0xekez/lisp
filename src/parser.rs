use crate::tokenizer::{Location, Token, TokenType};
use colored::*;

#[derive(Debug, Clone)]
pub struct Error {
    pub loc: Location,
    pub what: String,
}

#[derive(Debug)]
pub struct Parser<'a> {
    source: &'a str,
    // tokens: Vec<Token>,
    // loc: usize,
    // program: Program,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f32),
    String(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct Program {
    program: Vec<Expr>,
    errors: Vec<Error>,
}

impl Program {
    fn new() -> Self {
        Self {
            program: Vec::new(),
            errors: Vec::new(),
        }
    }
}

impl Error {
    fn new(what: &str, token: &Token) -> Self {
        Self {
            loc: token.loc.clone(),
            what: what.to_string(),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, // , tokens: Vec<Token>
    ) -> Self {
        Self {
            source: source,
            // tokens: tokens,
            // loc: 0,
            // program: Program::new(),
        }
    }

    // fn has_next(&self) -> bool {
    //     self.loc < self.tokens.len()
    // }
    // fn peek_token(&self) -> &Token {
    //     &self.tokens[self.loc]
    // }
    // fn peek_tok_type(&self) -> &TokenType {
    //     &self.peek_token().token
    // }
    // fn advance(&mut self) {
    //     self.loc += 1;
    // }

    // fn parse_list(&self) -> Result<Expr, Error> {}

    // pub fn parse_expr(&mut self) -> Result<Expr, Error> {
    //     if !self.has_next() {
    //         return Err(Error::new(
    //             "unexpected end of file",
    //             self.tokens[self.tokens.len() - 2],
    //         ));
    //     }
    //     match self.peek_token().token {
    //         TokenType::Oparen => Ok(self.parse_list()),
    //         TokenType::Cparen => Err(self.error_on_tok),
    //         TokenType::Number(f) => {
    //             self.advance();
    //             Ok(Expr::Number(f))
    //         }
    //     }
    // }

    // pub fn parse(&mut self) -> Program {
    //     let program = Program::new();
    //     while self.peek_token.token != TokenType::Eof {
    //         let res = self.parse_expr();
    //     }
    //     self.program.clone()
    // }

    // fn error_on_tok(&mut self, what: &str) {
    //     let token = self.peek_token();
    //     let err = Error {
    //         loc: token.loc.clone(),
    //         what: what.to_string(),
    //     };
    //     self.show_error(&err);
    //     self.program.errors.push(err);
    // }

    fn underline_error(error: &Error) {
        print!(" |  ");
        for _ in 0..error.loc.col_start {
            print!(" ");
        }
        print!("^");
        for _ in 0..(error.loc.col_end - error.loc.col_start - 1) {
            print!("-");
        }
        println!("");
    }

    fn show_error_message(error: &Error) {
        print!(" |  ");
        for _ in 0..error.loc.col_start {
            print!(" ");
        }
        print!("{}: ", "error".magenta());
        println!("{}", error.what);
    }

    // (+ 10.0.0 5)
    //    ^-----
    //    error: malformed expression
    pub fn show_error(&self, error: &Error) {
        println!("\n |  {}", self.source);
        Self::underline_error(error);
        Self::show_error_message(error);
        println!("");
    }
}
