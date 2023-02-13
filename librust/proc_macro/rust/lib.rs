pub use ident::Ident;
pub use literal::Literal;
pub use punct::Punct;
pub use span::Span;
use std::error;
use std::fmt;

mod ident;
mod literal;
mod punct;
mod span;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    /// Invisible delimiter
    None,
}

#[derive(Debug)]
pub struct LexError;

impl fmt::Display for LexError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl error::Error for LexError {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Spacing {
    Alone,
    Joint,
}
