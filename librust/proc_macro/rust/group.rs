use std::fmt;
use Span;
use TokenStream;

/// Describes how a sequence of token trees is delimited.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// The sequence is delimited by a parenthesis `(...)`.
    Parenthesis,
    /// The sequence is delimited by a brace `{...}`.
    Brace,
    /// The sequence is delimited by a bracket `[...]`.
    Bracket,
    /// Invisible delimiter to preserve operator priority.
    None,
}

/// A delimited token stream.
#[derive(Clone)]
pub struct Group {
    // Internal implementation details
}

impl Group {
    /// Creates a new `Group`.
    ///
    /// # Arguments
    ///
    /// * `delimiter` - The delimiter surrounding the inner [`TokenStream`].
    /// * `stream` - The tokenstream for this `Group`.
    pub fn new(_delimiter: Delimiter, _stream: TokenStream) -> Self {
        todo!("Implement this function")
    }

    /// Get the delimiter of the `Group`.
    pub fn delimiter(&self) -> Delimiter {
        todo!("Implement this function")
    }

    /// Get the stream of the `Group`.
    ///
    /// # Note
    ///
    /// The returned stream does not include the delimiters of this group.
    pub fn stream(&self) -> TokenStream {
        todo!("Implement this function")
    }

    /// Get the span for the delimiters of this token stream, spanning the
    /// entire group.
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// Get the span pointing to the opening delimiter of this `Group`.
    pub fn span_open(&self) -> Span {
        todo!("Implement this function")
    }

    /// Get the span pointing to the closing delimiter of this `Group`.
    pub fn span_close(&self) -> Span {
        todo!("Implement this function")
    }

    /// Change the span for this `Group`'s delimiters, but not its internal
    /// tokens.
    ///
    /// # Note
    ///
    /// This method will **not** set the span of all the internal tokens spanned
    /// by this group, but rather it will only set the span of the delimiter
    /// tokens at the level of the `Group`.
    pub fn set_span(&mut self, _span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Display for Group {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Group {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}
