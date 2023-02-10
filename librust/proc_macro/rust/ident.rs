use std::fmt;
use Span;

#[derive(Clone)]
pub struct Ident {
    // Internal implementation details
}

impl Ident {
    /// Creates a new identifier from a string and a span
    pub fn new(_string: &str, _span: Span) -> Self {
        todo!("Implement this function")
    }

    /// Creates a raw new identifier from a string and a span
    pub fn new_raw(_string: &str, _span: Span) -> Self {
        todo!("Implement this function")
    }

    /// Return the span of the identifier
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// change the span of the identifier
    pub fn set_span(&mut self, _span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Ident {
    /// display debug friendly version of the Identifier
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}
