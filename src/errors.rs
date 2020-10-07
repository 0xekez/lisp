use crate::parser::Expr;
use crate::tokenizer::{Location, Token, TokenType};
use colored::*;

#[derive(Debug)]
pub struct Error {
    pub loc: Location,
    pub what: String,
    suggest: Option<Suggestion>,
}

#[derive(Debug)]
pub struct Suggestion {
    pub loc: Location,
    pub what: String,
}

pub trait Printable {
    fn get_loc(&self) -> &Location;
    fn get_message(&self) -> &String;
    fn print_name(&self);

    fn underline_loc(&self) {
        print!(" |  ");
        for _ in 0..self.get_loc().col_start {
            print!(" ");
        }
        print!("^");
        for _ in 0..(self.get_loc().col_end - self.get_loc().col_start - 1) {
            print!("-");
        }
        println!("");
    }

    fn show_message(&self) {
        print!(" |  ");
        for _ in 0..self.get_loc().col_start {
            print!(" ");
        }
        self.print_name();
        println!(": {}", self.get_message());
    }

    // (+ 10.0.0 5)
    //    ^-----
    //    error: malformed expression
    fn show(&self, source: &str) {
        // FIXME: this assumes that source is terminated by a newline.
        print!(" |  {}", source);
        self.underline_loc();
        self.show_message();
    }
}

impl Printable for Error {
    fn get_loc(&self) -> &Location {
        &self.loc
    }
    fn get_message(&self) -> &String {
        &self.what
    }
    fn print_name(&self) {
        print!("{}", "error".magenta());
    }
    fn show(&self, source: &str) {
        print!(" |  {}", source);
        self.underline_loc();
        self.show_message();
        if let Some(ref s) = self.suggest {
            s.show_message();
        }
    }
}

impl Printable for Suggestion {
    fn get_loc(&self) -> &Location {
        &self.loc
    }
    fn get_message(&self) -> &String {
        &self.what
    }
    fn print_name(&self) {
        print!("{}", "suggestion".bright_blue());
    }
}

impl Error {
    pub fn on_tok(what: &str, token: &Token) -> Self {
        Self {
            loc: token.loc.clone(),
            what: what.to_string(),
            suggest: Suggestion::on_tok(token),
        }
    }

    pub fn on_expr(what: &str, expr: &Expr) -> Self {
        Self {
            loc: expr.loc.clone(),
            what: what.to_string(),
            suggest: None,
        }
    }

    pub fn guess_intent(what: &String) -> String {
        if what.chars().all(|c| c.is_numeric() || c == '.') {
            "malformed number".to_string()
        } else if what.chars().any(|c| c == '"') {
            "malformed string".to_string()
        } else {
            "malformed expression".to_string()
        }
    }
}

impl Suggestion {
    fn string_suggestion(s: &String, loc: &Location) -> Option<Self> {
        let mut iter = s.chars();
        loop {
            if let Some(c) = iter.next() {
                if c == '\\' {
                    if let Some(e) = iter.next() {
                        break Some(Self {
                            what: format!(
                                "invalid excape character '\\{}'. \
                                 lust supports '\\t', '\\\\', and '\\n'",
                                e
                            ),
                            loc: loc.clone(),
                        });
                    } else {
                        break None;
                    }
                }
            } else {
                break None;
            }
        }
    }

    pub fn on_tok(token: &Token) -> Option<Self> {
        if let TokenType::Unrecognized(s) = &token.token {
            match &Error::guess_intent(s)[..] {
                "malformed string" => Self::string_suggestion(s, &token.loc),
                &_ => None,
            }
        } else {
            None
        }
    }
}
