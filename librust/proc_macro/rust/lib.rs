pub use group::{Delimiter, Group};
pub use ident::Ident;
pub use literal::Literal;
pub use punct::{Punct, Spacing};
pub use span::Span;
use std::error;
use std::{fmt, str::FromStr};

mod group;
mod ident;
mod literal;
mod punct;
mod span;

/// A single token or a delimited sequence of token trees.
#[derive(Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

impl TokenTree {
    /// Get the [`Span`] for this TokenTree.
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// Set the span for this TokenTree.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, _span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Debug for TokenTree {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl From<Group> for TokenTree {
    fn from(_g: Group) -> Self {
        todo!("Implement this function")
    }
}

impl From<Ident> for TokenTree {
    fn from(_i: Ident) -> Self {
        todo!("Implement this function")
    }
}

impl From<Punct> for TokenTree {
    fn from(_p: Punct) -> Self {
        todo!("Implement this function")
    }
}

impl From<Literal> for TokenTree {
    fn from(_l: Literal) -> Self {
        todo!("Implement this function")
    }
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
