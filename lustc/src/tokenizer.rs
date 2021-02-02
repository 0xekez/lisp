use crate::location::Location;
use crate::reader::{self, Reader};

/// A token type. When paired with a location makes a token.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// A number. Anything that matches the regex [0-9]+.[0-9]+.
    Number(i64),
    /// A string. Strings are made up of a sequence of non-newline
    /// characters that begin and end with '"'. The enclosed string
    /// does not contain the opening and closing quotes. The \n and \t
    /// escape sequences are supported.
    String(String),
    /// Opening parenthesis.
    Oparen,
    /// Closing parenthesis.
    Cparen,
    /// A quote '
    Quote,
    // A quaziquote `
    Quaziquote,
    // A comma
    Comma,
    // A - sign
    Negate,
    /// An identifier. This is any sequence of characters not matched
    /// by the above rules.
    Id(String),
    /// An unrecognized token. Typically used to represent a malformed
    /// number or string. The enclosed string is the unrecognized text
    /// and the TokenType is the type of token we were parsing when it
    /// failed.
    Unrecognized(String, Box<TokenType>),
}

/// A token that the tokenizer will emit.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    /// The type of token that we're dealing with.
    pub ttype: TokenType,
    /// The location of the token.
    pub loc: Location,
}

/// A tokenizer for a lust program.
#[derive(Debug)]
pub struct Tokenizer<'a> {
    reader: Reader<'a>,
}

impl<'a> Tokenizer<'a> {
    /// Makes a new tokenizer that starts at the beginning of the
    /// input string.
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            reader: Reader::new(input),
        }
    }

    /// Gets the current location of the tokenizer in the source
    /// string.
    pub(crate) fn loc(&self) -> Location {
        Location {
            start: self.reader.loc(),
            end: self.reader.loc(),
        }
    }

    /// Yields the next token from the tokenizer if there is one, None
    /// otherwise.
    pub(crate) fn next_token(&mut self) -> Option<Token> {
        self.reader.skip_whitespace();
        match self.reader.peek() {
            None => None,
            Some(c) => Some(match c {
                // Comments
                ';' => {
                    self.reader.skip_line();
                    return self.next_token();
                }
                '0'..='9' => self.tokenize_number(),
                '(' => self.eat_token_at_point(TokenType::Oparen),
                ')' => self.eat_token_at_point(TokenType::Cparen),
                '\'' => self.eat_token_at_point(TokenType::Quote),
                '`' => self.eat_token_at_point(TokenType::Quaziquote),
                ',' => self.eat_token_at_point(TokenType::Comma),
                '-' => match self.reader.peek_2() {
                    Some('0'..='9') => self.eat_token_at_point(TokenType::Negate),
                    _ => self.eat_token_at_point(TokenType::Id("-".to_string())),
                },
                '"' => self.tokenize_string(),
                _ => self.tokenize_id(),
            }),
        }
    }

    /// Eats the token at point returning a new token and moving the
    /// reader forward.
    fn eat_token_at_point(&mut self, ttype: TokenType) -> Token {
        let start = self.reader.loc();
        self.reader.next();
        Token {
            ttype,
            loc: Location {
                start,
                end: self.reader.loc(),
            },
        }
    }

    /// Tokenizes a number and returns a number token if sucessful.
    fn tokenize_number(&mut self) -> Token {
        let start = self.reader.loc();
        let mut res = String::new();
        // Advance until we hit something that would break a number
        loop {
            if let Some(c) = self.reader.peek() {
                if match c {
                    '0'..='9' => true,
                    '.' => false,
                    '(' | ')' => false,
                    c => !c.is_ascii_whitespace(),
                } {
                    res.push(*c);
                    // Don't care if this fails.
                    self.reader.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        match res.parse::<i64>() {
            Ok(f) => Token::new(start, self.reader.loc(), TokenType::Number(f)),
            Err(_) => Token::new(
                start,
                self.reader.loc(),
                TokenType::Unrecognized(res, Box::new(TokenType::Number(0))),
            ),
        }
    }

    /// Tokenizes and ID. This will never return unrecognized.
    fn tokenize_id(&mut self) -> Token {
        let start = self.reader.loc();
        let mut res = String::new();
        loop {
            match self.reader.peek() {
                Some(c) => {
                    if match c {
                        ')' | '(' => true,
                        c => c.is_ascii_whitespace(),
                    } {
                        break;
                    } else {
                        res.push(*c);
                        self.reader.next();
                    }
                }
                None => break,
            }
        }
        Token::new(start, self.reader.loc(), TokenType::Id(res))
    }

    /// Tokenizes a string. If the string has an invalid excape or
    /// does not close itself before a newline returns an unrecognized
    /// tokeh with the amount of the input that it consumed.
    fn tokenize_string(&mut self) -> Token {
        let start = self.reader.loc();
        let mut res = String::new();
        let mut valid = true;
        // Eat opening quote.
        self.reader.next();
        loop {
            match self.reader.next() {
                Some(c) => match c {
                    '\\' => match self.reader.next() {
                        Some(c) => match c {
                            'n' => res.push('\n'),
                            't' => res.push('\t'),
                            '"' => res.push('"'),
                            c => {
                                valid = false;
                                res.push_str(&format!("\\{}", c).to_string());
                            }
                        },
                        None => {
                            valid = false;
                            break;
                        }
                    },
                    '"' => break,
                    c => res.push(c),
                },
                None => {
                    valid = false;
                    break;
                }
            }
        }
        if valid {
            Token::new(start, self.reader.loc(), TokenType::String(res))
        } else {
            Token::new(
                start,
                self.reader.loc(),
                TokenType::Unrecognized(format!("\"{}\"", &res), Box::new(TokenType::String(res))),
            )
        }
    }
}

impl Token {
    /// Makes a new token from two reader locations.
    pub(crate) fn new(start: reader::Location, end: reader::Location, ttype: TokenType) -> Self {
        Self {
            loc: Location { start, end },
            ttype,
        }
    }

    /// Makes a new token from raw position values.
    pub fn from_raw(
        s_line: usize,
        s_col: usize,
        e_line: usize,
        e_col: usize,
        ttype: TokenType,
    ) -> Self {
        Self::new(
            reader::Location::from_raw(s_line, s_col),
            reader::Location::from_raw(e_line, e_col),
            ttype,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn call() {
        let call = "(+ 1 1)";
        let mut tokenizer = Tokenizer::new(&call);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            if let Some(token) = token {
                actual.push(token);
            } else {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::from_raw(0, 0, 0, 1, TokenType::Oparen),
                Token::from_raw(0, 1, 0, 2, TokenType::Id("+".to_string())),
                Token::from_raw(0, 3, 0, 4, TokenType::Number(1)),
                Token::from_raw(0, 5, 0, 6, TokenType::Number(1)),
                Token::from_raw(0, 6, 0, 7, TokenType::Cparen),
            ]
        );
    }

    #[test]
    fn strings() {
        let strings = "\"hello\" \"hello\\t\"";
        let mut tokenizer = Tokenizer::new(&strings);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            if let Some(token) = token {
                actual.push(token);
            } else {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::from_raw(0, 0, 0, 7, TokenType::String("hello".to_string())),
                Token::from_raw(0, 8, 0, 17, TokenType::String("hello\t".to_string())),
            ]
        );
    }

    #[test]
    fn newline() {
        let input = "(+ 2\n  2)";
        let mut tokenizer = Tokenizer::new(&input);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            if let Some(token) = token {
                actual.push(token);
            } else {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::from_raw(0, 0, 0, 1, TokenType::Oparen),
                Token::from_raw(0, 1, 0, 2, TokenType::Id("+".to_string())),
                Token::from_raw(0, 3, 0, 4, TokenType::Number(2)),
                Token::from_raw(1, 2, 1, 3, TokenType::Number(2)),
                Token::from_raw(1, 3, 1, 4, TokenType::Cparen),
            ]
        );
    }

    #[test]
    fn tab() {
        let input = "(+ 2\n\t2)";
        let mut tokenizer = Tokenizer::new(&input);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            if let Some(token) = token {
                actual.push(token);
            } else {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::from_raw(0, 0, 0, 1, TokenType::Oparen),
                Token::from_raw(0, 1, 0, 2, TokenType::Id("+".to_string())),
                Token::from_raw(0, 3, 0, 4, TokenType::Number(2)),
                Token::from_raw(1, 8, 1, 9, TokenType::Number(2)),
                Token::from_raw(1, 9, 1, 10, TokenType::Cparen),
            ]
        );
    }
}
