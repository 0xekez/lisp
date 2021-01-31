use crate::reader;

/// A location in source code. Stores in the form [start, end)
#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    /// The (line, column) index of the first character in the token.
    pub start: reader::Location,
    /// The (line, column) index of one past the last character in the
    /// token.
    pub end: reader::Location,
}

impl Location {
    pub fn union(start: &Self, end: &Self) -> Self {
        Self {
            start: start.start,
            end: end.end,
        }
    }
}
