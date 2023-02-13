pub use ident::Ident;
pub use literal::Literal;
pub use punct::{Punct, Spacing};
pub use span::Span;
use std::error;
use std::fmt;

mod ident;
mod literal;
mod punct;
mod span;

/// Describes how a sequence of token trees is delimited.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// The sequence is delimited by a parentheses `(...)`.
    Parenthesis,
    /// The sequence is delimited by a brace `{...}`.
    Brace,
    /// The sequence is delimited by a bracket `[...]`.
    Bracket,
    /// Invisible delimiter to preserve operator priority.
    None,
}

/// Error returned from `from_str` functions.
#[derive(Debug)]
pub struct LexError;

impl fmt::Display for LexError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl error::Error for LexError {}
