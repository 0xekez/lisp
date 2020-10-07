/// Handles parsing of Lust expressions and emits some parse errors
/// along the way.
use crate::errors::Error;
use crate::tokenizer::{Location, Token, TokenBuffer, TokenType};

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
    pub expr: ExprVal,
    pub loc: Location,
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
        self.errors.append(&mut other.errors);
    }
    pub fn merge_err(&mut self, err: Error) {
        self.errors.push(err);
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

        // If there is a predicate attempt to validate it. This is
        // garbage and should be moved to the type checker.
        // if self.tokens.peek_token().token != TokenType::Cparen {
        //     let predicate = self.parse_expr();
        //     match predicate.expr {
        //         Some(e) => match e.expr {
        //             ExprVal::Id(_) => (),
        //             _ => res.merge_err(Error::on_expr("invalid list predicate", &e)),
        //         },
        //         None => res.merge_errors(predicate),
        //     }
        // }

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
            TokenType::Unrecognized(s) => ParseResult::from_err(Error::on_tok(
                &Error::guess_intent(&s),
                &self.tokens.next_token(),
            )),
        }
    }
}
