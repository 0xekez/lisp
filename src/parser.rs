// Handles parsing of Lust expressions and emits some parse errors
// along the way.
use crate::tokenizer::{Location, Token, TokenBuffer, TokenType};
use colored::*;

#[derive(Debug)]
pub struct Error {
    pub loc: Location,
    pub what: String,
}

#[derive(Debug)]
pub struct ParseResult {
    pub expr: Option<Expr>,
    pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    source: &'a str,
    tokens: TokenBuffer<'a>,
}

#[derive(Debug)]
pub enum ExprVal {
    Number(f32),
    String(String),
    List(Vec<Expr>),
    Id(String),
}

#[derive(Debug)]
pub struct Expr {
    expr: ExprVal,
    loc: Location,
}

#[derive(Debug)]
pub struct Program {
    program: Vec<Expr>,
    errors: Vec<Error>,
}

impl Expr {
    pub fn new(start: &Token, end: &Token, expr: ExprVal) -> Self {
        Self {
            expr,
            loc: Location {
                col_start: start.loc.col_start,
                col_end: end.loc.col_end,
            },
        }
    }
}

impl ParseResult {
    pub fn new() -> Self {
        Self {
            expr: None,
            errors: vec![],
        }
    }
    pub fn from_expr(expr: Expr) -> Self {
        Self {
            expr: Some(expr),
            errors: vec![],
        }
    }
    pub fn from_err(error: Error) -> Self {
        Self {
            expr: None,
            errors: vec![error],
        }
    }
    pub fn merge_errors(&mut self, mut other: Self) {
        println!("merge errroes");
        self.errors.append(&mut other.errors);
    }
    pub fn merge_err(&mut self, err: Error) {
        self.errors.push(err);
    }
}

impl Error {
    pub fn on_tok(what: &str, token: &Token) -> Self {
        Self {
            loc: token.loc.clone(),
            what: what.to_string(),
        }
    }

    pub fn on_expr(what: &str, expr: &Expr) -> Self {
        Self {
            loc: expr.loc.clone(),
            what: what.to_string(),
        }
    }

    fn underline_error(&self) {
        print!(" |  ");
        for _ in 0..self.loc.col_start {
            print!(" ");
        }
        print!("^");
        for _ in 0..(self.loc.col_end - self.loc.col_start - 1) {
            print!("-");
        }
        println!("");
    }

    fn show_error_message(&self) {
        print!(" |  ");
        for _ in 0..self.loc.col_start {
            print!(" ");
        }
        print!("{}: ", "error".magenta());
        println!("{}", self.what);
    }

    // (+ 10.0.0 5)
    //    ^-----
    //    error: malformed expression
    pub fn show_error(&self, source: &str) {
        // FIXME: this assumes that source is terminated by a newline.
        print!(" |  {}", source);
        self.underline_error();
        self.show_error_message();
    }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            tokens: TokenBuffer::new(source),
        }
    }

    // Collect list of errors and list of expressions.
    pub fn parse_list(&mut self) -> ParseResult {
        let mut res = ParseResult::new();
        let oparen = self.tokens.next_token();
        let mut v = Vec::new();

        let predicate = self.parse_expr();
        match predicate.expr {
            Some(e) => match e.expr {
                ExprVal::Id(_) => (),
                _ => res.merge_err(Error::on_expr("invalid list predicate", &e)),
            },
            None => res.merge_errors(predicate),
        }

        let end_tok = loop {
            match self.tokens.peek_token().token {
                TokenType::Cparen => {
                    break self.tokens.next_token();
                }
                TokenType::Eof => {
                    res.merge_err(Error::on_tok("unbalanced parenthesis", &oparen));
                    break self.tokens.next_token();
                }
                _ => (),
            }
            let mut pr = self.parse_expr();
            if let Some(e) = pr.expr {
                v.push(e);
            }
            res.errors.append(&mut pr.errors);
        };
        res.expr = Some(Expr::new(&oparen, &end_tok, ExprVal::List(v)));
        res
    }

    pub fn parse_expr(&mut self) -> ParseResult {
        match self.tokens.peek_token().token.clone() {
            TokenType::Oparen => self.parse_list(),
            TokenType::Cparen => ParseResult::from_err(Error::on_tok(
                "unexpected closing paren",
                &self.tokens.next_token(),
            )),
            TokenType::Number(f) => ParseResult::from_expr(Expr {
                expr: ExprVal::Number(f),
                loc: self.tokens.next_token().loc,
            }),
            TokenType::Id(s) => ParseResult::from_expr(Expr {
                expr: ExprVal::Id(s),
                loc: self.tokens.next_token().loc,
            }),
            TokenType::String(s) => ParseResult::from_expr(Expr {
                expr: ExprVal::String(s),
                loc: self.tokens.next_token().loc,
            }),
            TokenType::Eof => ParseResult::from_err(Error::on_tok(
                "unexpected end of file",
                &self.tokens.next_token(),
            )),
            TokenType::Unrecognized(_) => ParseResult::from_err(Error::on_tok(
                "malformed expression",
                &self.tokens.next_token(),
            )),
        }
    }
}
