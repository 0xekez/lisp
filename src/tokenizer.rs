#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Number(f32),
    String(String),
    Eof,
    Oparen,
    Cparen,
    Id(String),
    Unrecognized(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub col_start: usize,
    pub col_end: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub loc: Location,
    pub token: TokenType,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a str,
    position: usize,
}

impl Token {
    pub fn new(col_start: usize, col_end: usize, token: TokenType) -> Self {
        Self {
            loc: Location { col_start, col_end },
            token,
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input,
            position: 0,
        }
    }

    fn has_next(&self) -> bool {
        self.position < self.input.len()
    }

    fn peek_char(&self) -> char {
        self.input[self.position..].chars().next().unwrap()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn make_tok_at_position(&self, token: TokenType) -> Token {
        Token::new(self.position - 1, self.position, token)
    }

    fn skip_whitespace(&mut self) {
        while self.has_next() && self.peek_char().is_ascii_whitespace() {
            self.advance();
        }
    }

    fn tokenize_number(&mut self) -> Token {
        let start_pos = self.position;
        self.advance();
        // Advance until we hit something that would break a
        // number. This means that we'll eat invalid numbers and then
        // fail to parse them and return unrecognized tokens.
        while self.has_next()
            && match self.peek_char() {
                '0'..='9' => true,
                '.' => true,
                '(' | ')' => false,
                c => !c.is_ascii_whitespace(),
            }
        {
            self.advance();
        }
        match self.input[start_pos..self.position].parse::<f32>() {
            Ok(f) => Token::new(start_pos, self.position, TokenType::Number(f)),
            Err(_) => Token::new(
                start_pos,
                self.position,
                TokenType::Unrecognized(self.input[start_pos..self.position].to_string()),
            ),
        }
    }

    fn tokenize_id(&mut self) -> Token {
        let start_pos = self.position;
        self.advance();
        while self.has_next()
            && match self.peek_char() {
                ')' | '(' => false,
                c => !c.is_ascii_whitespace(),
            }
        {
            self.advance();
        }
        Token::new(
            start_pos,
            self.position,
            TokenType::Id(self.input[start_pos..self.position].to_string()),
        )
    }

    // Same rules as chocopy. Put whatever you want inside and \t \n
    // are allowed.
    fn tokenize_string(&mut self) -> Token {
        let start_pos = self.position;
        let mut res = String::new();
        let mut valid = true;
        // Eat opening quote.
        self.advance();
        loop {
            if !self.has_next() {
                valid = false;
                break;
            }
            match self.peek_char() {
                '\\' => {
                    self.advance();
                    match self.peek_char() {
                        'n' => res.push('\n'),
                        't' => res.push('\t'),
                        '"' => res.push('"'),
                        _ => {
                            res.push_str("<unregonized>");
                            valid = false;
                        }
                    }
                    self.advance();
                }
                '"' => break,
                c => {
                    res.push(c);
                    self.advance();
                }
            }
        }
        // Eat closing paren.
        self.advance();
        if valid {
            Token::new(start_pos, self.position, TokenType::String(res))
        } else {
            Token::new(start_pos, self.position, TokenType::String(res))
        }
    }

    /*
    (defun single_char_token (char token)
      (interactive "schar: \nstoken: \n")
      (save-excursion
        (insert (format "'%s' => {self.advance(); self.make_tok_at_position(TokenType::%s)}" char token))))
    */
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if !self.has_next() {
            // Advance so that the EOF token has position one past the
            // end.
            self.advance();
            return self.make_tok_at_position(TokenType::Eof);
        }
        match self.peek_char() {
            '0'..='9' => self.tokenize_number(),
            '(' => {
                self.advance();
                self.make_tok_at_position(TokenType::Oparen)
            }
            ')' => {
                self.advance();
                self.make_tok_at_position(TokenType::Cparen)
            }
            '"' => self.tokenize_string(),
            _ => self.tokenize_id(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_call() {
        let call = "(+ 1 1.5)";
        let mut tokenizer = Tokenizer::new(&call);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            actual.push(token.clone());
            if (token.token == TokenType::Eof) {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::new(0, 1, TokenType::Oparen),
                Token::new(1, 2, TokenType::Id("+".to_string())),
                Token::new(3, 4, TokenType::Number(1.0)),
                Token::new(5, 8, TokenType::Number(1.5)),
                Token::new(8, 9, TokenType::Cparen),
                Token::new(9, 10, TokenType::Eof),
            ]
        );
    }

    #[test]
    fn numbers() {
        let numbers = "1.5 3.0.0 5";
        let mut tokenizer = Tokenizer::new(&numbers);
        let mut actual = Vec::new();
        loop {
            let token = tokenizer.next_token();
            actual.push(token.clone());
            if (token.token == TokenType::Eof) {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::new(0, 3, TokenType::Number(1.5)),
                Token::new(4, 9, TokenType::Unrecognized("3.0.0".to_string())),
                Token::new(10, 11, TokenType::Number(5.0)),
                Token::new(11, 12, TokenType::Eof),
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
            actual.push(token.clone());
            if (token.token == TokenType::Eof) {
                break;
            }
        }
        assert_eq!(
            actual,
            vec![
                Token::new(0, 7, TokenType::String("hello".to_string())),
                Token::new(8, 17, TokenType::String("hello\t".to_string())),
                Token::new(17, 18, TokenType::Eof),
            ]
        );
    }
}
