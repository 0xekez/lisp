/// Handles parsing of Lust expressions and emits some parse errors
/// along the way.
use crate::errors::Error;
use crate::location::Location;
use crate::tokenbuffer::TokenBuffer;
use crate::tokenizer::{Token, TokenType};

/// Used internally by the parser to store information about the state
/// of the parse.
#[derive(Debug)]
pub struct ParseResult {
    /// Holds the expression being parsed if we succeded in parsing
    /// any of it.
    pub expr: Option<Expr>,
    /// A list of errors that occured during the current parse.
    pub errors: Vec<Error>,
}

/// The parser for Lust programs.
#[derive(Debug)]
pub struct Parser<'a> {
    /// The source string that we're parsing. We keep a reference to
    /// this arround for emitting errors.
    source: &'a str,
    /// A token buffer that will be associated with SOURCE.
    tokbuffer: TokenBuffer<'a>,
}

/// An expression's value.
#[derive(Debug, PartialEq, Clone)]
pub enum ExprVal {
    Number(f32),
    String(String),
    List(Vec<Expr>),
    Id(String),
}

/// An expression. Holds a value and a location.
#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    /// The vallue of the expression.
    pub val: ExprVal,
    /// Where in the source that this expression lives.
    pub loc: Location,
}

#[derive(Debug)]
pub struct Program {
    program: Vec<Expr>,
    errors: Vec<Error>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            program: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn merge_result(&mut self, mut res: ParseResult) {
        self.errors.append(&mut res.errors);
        if let Some(e) = res.expr {
            self.program.push(e);
        }
    }
}

impl Expr {
    pub fn new(start: &Token, end: &Token, expr: ExprVal) -> Self {
        Self {
            val: expr,
            loc: Location::union(&start.loc, &end.loc),
        }
    }
    pub fn at_loc(loc: Location, val: ExprVal) -> Self {
        Self { loc, val }
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
            tokbuffer: TokenBuffer::new(source),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut p = Program::new();
        while self.tokbuffer.has_next() {
            p.merge_result(self.parse_expr());
        }
        p
    }

    /// Parses a list from the tokenbuffer.
    fn parse_list(&mut self, oparen: Token) -> ParseResult {
        let mut res = ParseResult::new();
        let mut v = Vec::new();
        loop {
            match self.tokbuffer.peek_token() {
                Some((tok, buffer)) => match tok.ttype {
                    TokenType::Cparen => {
                        buffer.advance();
                        break;
                    }
                    _ => (),
                },
                None => {
                    res.merge_err(Error::on_tok("unbalanced parenthesis", &oparen));
                    break;
                }
            }
            let mut pr = self.parse_expr();
            if let Some(e) = pr.expr {
                v.push(e);
            }
            res.errors.append(&mut pr.errors);
        }

        let start = &oparen.loc;
        let end = match &v.first() {
            Some(ref e) => &e.loc,
            None => start,
        };
        res.expr = Some(Expr::at_loc(Location::union(start, end), ExprVal::List(v)));
        res
    }

    pub fn has_more(&self) -> bool {
        self.tokbuffer.has_next()
    }

    fn expand(&mut self, name: &str, startloc: Location) -> ParseResult {
        let id = ExprVal::Id(name.to_string());
        let mut bodyres = self.parse_expr();
        let call = match bodyres.expr {
            Some(e) => ExprVal::List(vec![
                Expr {
                    val: id,
                    loc: startloc.clone(),
                },
                e,
            ]),
            None => id,
        };
        let expr = Expr {
            val: call,
            loc: Location::union(&startloc, &self.tokbuffer.loc()),
        };
        let mut parseres = ParseResult::from_expr(expr);
        parseres.errors.append(&mut bodyres.errors);
        parseres
    }

    /// Parses an expression from the tokenbuffer.
    pub fn parse_expr(&mut self) -> ParseResult {
        match self.tokbuffer.peek_token() {
            Some((t, buffer)) => match t.ttype {
                TokenType::Oparen => {
                    let tok = buffer.advance();
                    self.parse_list(tok)
                }

                TokenType::Cparen => ParseResult::from_err(Error::on_tok(
                    "unexpected closing paren",
                    &buffer.advance(),
                )),

                TokenType::Number(f) => ParseResult::from_expr(Expr {
                    val: ExprVal::Number(f),
                    loc: buffer.advance().loc,
                }),

                TokenType::Id(s) => ParseResult::from_expr(Expr {
                    val: ExprVal::Id(s),
                    loc: buffer.advance().loc,
                }),

                TokenType::String(s) => ParseResult::from_expr(Expr {
                    val: ExprVal::String(s),
                    loc: buffer.advance().loc,
                }),

                TokenType::Quote => {
                    let loc = buffer.advance().loc;
                    self.expand("quote", loc)
                }
                TokenType::Negate => {
                    let loc = buffer.advance().loc;
                    self.expand("negate", loc)
                }
                TokenType::Quaziquote => {
                    let loc = buffer.advance().loc;
                    self.expand("quaziquote", loc)
                }
                TokenType::Comma => {
                    let loc = buffer.advance().loc;
                    self.expand("comma", loc)
                }
                TokenType::Unrecognized(s, _) => ParseResult::from_err(Error::on_tok(
                    &format!("malformed token: {}", s),
                    &buffer.advance(),
                )),
            },
            None => {
                let mut res = ParseResult::new();
                res.merge_err(Error::at_loc(
                    "unexpected end of input parsing expression",
                    &self.tokbuffer.loc(),
                ));
                res
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::TokenType;

    #[test]
    fn empty_list() {
        let src = "()";
        let mut parser = Parser::new(&src);
        let res = parser.parse_expr();
        if let Some(e) = res.expr {
            assert_eq!(ExprVal::List(vec![]), e.val);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn single_number() {
        let src = "1.5";
        let mut parser = Parser::new(&src);
        let res = parser.parse_expr();
        if let Some(e) = res.expr {
            assert_eq!(ExprVal::Number(1.5), e.val);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn small_list() {
        let src = "(1 hello \"hello\")";
        let mut parser = Parser::new(&src);
        let res = parser.parse_expr();
        if let Some(e) = res.expr {
            match e.val {
                ExprVal::List(v) => {
                    assert_eq!(v.len(), 3);
                    assert_eq!(v[0].val, ExprVal::Number(1.0));
                    assert_eq!(v[1].val, ExprVal::Id("hello".to_string()));
                    assert_eq!(v[2].val, ExprVal::String("hello".to_string()));
                }
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    fn small_bad_list() {
        let src = "(1 hello \"hello\"";
        let mut parser = Parser::new(&src);
        let res = parser.parse_expr();
        if let Some(e) = res.expr {
            match e.val {
                ExprVal::List(v) => {
                    assert_eq!(v.len(), 3);
                    assert_eq!(v[0].val, ExprVal::Number(1.0));
                    assert_eq!(v[1].val, ExprVal::Id("hello".to_string()));
                    assert_eq!(v[2].val, ExprVal::String("hello".to_string()));
                }
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }
        assert_eq!(res.errors.len(), 1);
        // Note this test is liable to break. See issue #2 which
        // tracks a better way to handle this.
        assert_eq!(res.errors[0].what, "unbalanced parenthesis".to_string());
    }
}
