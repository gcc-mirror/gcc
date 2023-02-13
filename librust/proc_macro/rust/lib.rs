pub use ident::Ident;
pub use literal::Literal;
pub use punct::{Punct, Spacing};
pub use span::Span;
use std::error;
use std::{fmt, str::FromStr};

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

/// An abstract sequence of token trees.
///
/// This type provides interfaces for iterating over those token trees. This
/// is both the input and the output of `#[proc_macro]`,
/// `#[proc_macro_attribute]` and `#[proc_macro_derive]` definitions.
#[derive(Clone)]
pub struct TokenStream {
    // Internal implementation details
}

impl TokenStream {
    // TODO: Add experimental API functions for this type

    /// Creates an empty `TokenStream` containing no token trees.
    pub fn new() -> Self {
        todo!("Implement this function")
    }

    /// Checks if this `TokenStream` is empty.
    pub fn is_empty(&self) -> bool {
        todo!("Implement this function")
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(_src: &str) -> Result<Self, LexError> {
        todo!("Implement this function")
    }
}
