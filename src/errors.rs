use crate::location::Location;
use crate::reader;
use crate::tokenizer::{Token, TokenType};
use colored::*;

#[derive(Debug)]
pub struct Error {
    pub loc: Location,
    pub what: String,
    suggestion: Option<Suggestion>,
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

    /// Shows the filename for the current printable.
    fn show_file(file: &str, line: usize, col: usize) {
        for _ in 0..col {
            print!(" ");
        }
        println!(
            "*--> {}",
            format!("{}:{}:{}", file, line, col,).bright_blue()
        );
    }

    /// Shows the message for the current printable. For example:
    /// `error: malformed string`
    fn show_message(&self, file: &str) {
        let mystart = self.get_loc().start;
        for _ in 0..mystart.col {
            print!(" ");
        }
        print!("|  ");
        self.print_name();
        println!(": {}", self.get_message());
        Self::show_file(file, mystart.line, mystart.col);
    }

    /// Writes an underline to the console in the range [start, end)
    fn underline_between(start: usize, end: usize) {
        for _ in 0..start {
            print!(" ");
        }
        print!("^");
        for _ in (start + 1)..end {
            print!("-")
        }
        println!();
    }

    /// Shows a multi line printable
    fn show_multi_line(&self, source: &str, file: &str) {
        let startloc = self.get_loc().start;
        let endloc = self.get_loc().end;
        let lines: Vec<_> = source.lines().collect();

        let startline = lines[startloc.line];
        println!("{}", startline);
        Self::underline_between(startloc.col, startline.len());
        self.show_message(file);

        // When split into lines a string like "hello\n" will only
        // return one line containing "hello", however the tokenizer
        // will determine that the end line of the error is the first
        // character on the second line. I'm not sure what a clean way
        // to get around this is to be honest because it seems like
        // this is a weird case where the errors ends on the -1th
        // character of the second line. For now this hack exists.
        if endloc.line < lines.len() {
            println!();
            let endline = lines[endloc.line];
            println!("{}", endline);
            // start underline at first non-whitespace character if
            // possible.
            let start = if let Some(i) = endline.find(|c: char| !c.is_ascii_whitespace()) {
                i
            } else {
                0
            };
            Self::underline_between(start, endloc.col);
            for _ in 0..start {
                print!(" ");
            }
            print!("|  ");
            println!("{}: error ends here", "note".bright_cyan());
            Self::show_file(file, self.get_loc().end.line, start);
        }
    }

    /// Prints a printable in the following format:
    /// (+ 10.0.0 5)
    ///    ^-----
    ///    error: malformed expression
    ///      --> std.lisp:1:41
    /// TODO: when we implement reading from files this needs to work.
    fn show(&self, source: &str, file: &str) {
        let startloc = self.get_loc().start;
        let endloc = self.get_loc().end;
        if startloc.line != endloc.line {
            self.show_multi_line(source, file);
            return;
        }
        let lines: Vec<_> = source.lines().collect();
        let line = lines[startloc.line];
        println!("{}", line);
        Self::underline_between(startloc.col, endloc.col);
        self.show_message(file);
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

    fn show(&self, source: &str, file: &str) {
        let startloc = self.get_loc().start;
        let endloc = self.get_loc().end;
        if startloc.line != endloc.line {
            self.show_multi_line(source, file);
            return;
        }
        let lines: Vec<_> = source.lines().collect();
        let line = lines[startloc.line];
        println!("{}", line);
        Self::underline_between(startloc.col, endloc.col);
        self.show_message(file);

        if let Some(s) = &self.suggestion {
            s.show(source, file);
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
            suggestion: Suggestion::on_tok(token),
        }
    }
    pub fn at_loc(what: &str, loc: &Location) -> Self {
        Self {
            loc: loc.clone(),
            what: what.to_string(),
            suggestion: None,
        }
    }
    pub fn from_raw(s_line: usize, s_col: usize, e_line: usize, e_col: usize, what: &str) -> Self {
        let tok = Token::from_raw(
            s_line,
            s_col,
            e_line,
            e_col,
            TokenType::Id("filler".to_string()),
        );
        Self::on_tok(what, &tok)
    }
}

impl Suggestion {
    fn string_suggestion(s: &String, loc: &Location) -> Option<Self> {
        let mut iter = s.chars().enumerate();
        loop {
            if let Some((start, c)) = iter.next() {
                if c == '\\' {
                    if let Some((end, e)) = iter.next() {
                        break Some(Self {
                            what: format!(
                                "invalid excape character '\\{}'. \
                                 lust supports '\\t', '\\\\', and '\\n'",
                                e
                            ),
                            loc: Location {
                                start: reader::Location {
                                    line: loc.start.line,
                                    col: loc.start.col + start,
                                },
                                end: reader::Location {
                                    line: loc.start.line,
                                    col: loc.start.col + end,
                                },
                            },
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
        if let TokenType::Unrecognized(s, b) = &token.ttype {
            if let TokenType::String(_) = **b {
                return Self::string_suggestion(s, &token.loc);
            }
        }
        None
    }
}
