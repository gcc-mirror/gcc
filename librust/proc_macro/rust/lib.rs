use std::error;
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    /// Invisible delimiter
    None,
}

#[derive(Copy, Clone)]
pub struct Span {
    // Internal implementation details
}

impl Span {
    // TODO: Add experimental API functions for this type

    /// Creates a new span that resolves at the macro call location
    pub fn call_site() -> Self {
        todo!("Implement this function")
    }

    /// Creates a new span that resolved sometimes at macro call site, and
    /// sometimes at macro definition site
    pub fn mixed_site() -> Self {
        todo!("Implement this function")
    }

    /// Creates a new span with the same line/column informations but that
    /// resolve symbols as though it were at `other`
    pub fn resolved_at(&self, other: Span) -> Self {
        todo!("Implement this function")
    }

    /// Creates a new span with the same name resolution behavior as self, but
    /// with the line/column information of `other`.
    pub fn located_at(&self, other: Span) -> Self {
        todo!("Implement this function")
    }

    /// Return the source text behind a span.
    pub fn source_text(&self) -> Option<String> {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

#[derive(Clone)]
pub struct Ident {
    // Internal implementation details
}

impl Ident {
    /// Creates a new identifier from a string and a span
    pub fn new(string: &str, span: Span) -> Self {
        todo!("Implement this function")
    }

    /// Creates a raw new identifier from a string and a span
    pub fn new_raw(string: &str, span: Span) -> Self {
        todo!("Implement this function")
    }

    /// Return the span of the identifier
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// change the span of the identifier
    pub fn set_span(&mut self, span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Ident {
    /// display debug friendly version of the Identifier
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

#[derive(Debug)]
pub struct LexError;

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl error::Error for LexError {}
