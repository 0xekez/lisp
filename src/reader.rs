/// A small reader that reads non-whitespace tokens from an input
/// string and keeps track of its visual location in the string.
use std::iter;

/// A location in a string.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

/// A reader which yields characters from a SOURCE. The reader keeps
/// track of its location in LOC and never yields whitespace tokens.
#[derive(Debug)]
pub(crate) struct Reader<'a> {
    // Tracks the visible location of the reader. This means that tabs
    // do not count as a single character.
    pub loc: Location,
    source: &'a str,
    chars: iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> Reader<'a> {
    /// Makes a new reader at line zero and column zero in SOURCE.
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            loc: Location::new(),
            source,
            chars: source.chars().peekable(),
        }
        // Prime the reader.
        // s.skip_whitespace();
        // s
    }

    /// Updates the location of the reader based on C. If C is a tab
    /// character we increment the location following the unix
    /// convention (moving to the next multiple of eight).
    fn update_loc(&mut self, c: char) {
        match c {
            '\n' => {
                // If this newline is the last item in the input then
                // it we shouldn't move to a new line because we've
                // reached the end. Any location returned on that line
                // will cause an index out of bounds error because the
                // line doesn't actually have any content.
                self.loc.col = 0;
                self.loc.line += 1;
            }
            '\t' => {
                // We use the unix convention for tabs although
                // this could become troublesome. What this means
                // is tabs move us to the next multiple of eight.
                self.loc.col += if self.loc.col % 8 == 0 {
                    8
                } else {
                    self.loc.col % 8
                };
            }
            // No windows style line endings '\r' or '\r' '\n'
            // supported.
            _ => self.loc.col += 1,
        }
    }

    /// All operations that involve advancing a character should pass
    /// through this function so that location information can be
    /// properly updated.
    fn next_char(&mut self) -> Option<char> {
        // We assume that our past self has skipped whitespace so we
        // can skip the call here.
        let c = self.chars.next();
        if let Some(c) = c {
            self.update_loc(c);
        }
        c
    }

    /// Ignore whitespace characters. This should always be called
    /// after a new character is read out.
    pub(crate) fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.peek() {
                if c.is_ascii_whitespace() {
                    self.next_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    pub(crate) fn skip_line(&mut self) {
        loop {
            match self.next_char() {
                Some(c) => {
                    if c == '\n' {
                        break;
                    }
                }
                None => break,
            }
        }
    }

    /// Peek at the current character.
    pub(crate) fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Get the current character and advance the reader.
    pub(crate) fn next(&mut self) -> Option<char> {
        self.next_char()
    }

    pub(crate) fn peek_2(&self) -> Option<char> {
        let mut copy = self.chars.clone();
        copy.next();
        match copy.peek() {
            Some(s) => Some(s.clone()),
            None => None,
        }
    }

    /// Get the readers current location.
    pub(crate) fn loc(&self) -> Location {
        self.loc
    }
}

impl Location {
    /// Makes a new location at position (0, 0)
    pub(crate) fn new() -> Self {
        Self { line: 0, col: 0 }
    }

    /// Makes a new location at position (LINE, COL)
    pub fn from_raw(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}
