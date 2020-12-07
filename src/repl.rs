use std::borrow::Cow::{self, Borrowed, Owned};

use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::validate::{self, MatchingBracketValidator, Validator};
use rustyline_derive::{Completer, Helper, Hinter};

#[derive(Helper, Completer, Hinter)]
pub struct REPLHelper {
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    pub colored_prompt: String,
}

// impl Completer for REPLHelper {
//     type Candidate = Pair;

//     fn complete(
//         &self,
//         line: &str,
//         pos: usize,
//         ctx: &Context<'_>,
//     ) -> Result<(usize, Vec<Pair>), ReadlineError> {
//         self.completer.complete(line, pos, ctx)
//     }
// }

impl Highlighter for REPLHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Validator for REPLHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}

impl REPLHelper {
    pub fn new() -> Self {
        Self {
            highlighter: MatchingBracketHighlighter::new(),
            colored_prompt: "".to_owned(),
            validator: MatchingBracketValidator::new(),
        }
    }
}
