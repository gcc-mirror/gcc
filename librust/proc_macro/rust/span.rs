use std::fmt;

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
    pub fn resolved_at(&self, _other: Span) -> Self {
        todo!("Implement this function")
    }

    /// Creates a new span with the same name resolution behavior as self, but
    /// with the line/column information of `other`.
    pub fn located_at(&self, _other: Span) -> Self {
        todo!("Implement this function")
    }

    /// Return the source text behind a span.
    pub fn source_text(&self) -> Option<String> {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}
