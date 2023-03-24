use bridge;
use std::fmt;

/// A region of source code along with macro expansion information.
#[derive(Copy, Clone)]
pub struct Span(pub(crate) bridge::span::Span);

impl Span {
    // TODO: Add experimental API functions for this type

    /// Creates a new span that resolves at the macro call location.
    pub fn call_site() -> Self {
        Span(bridge::span::Span::call_site())
    }

    /// Creates a new span that resolved sometimes at macro call site, and
    /// sometimes at macro definition site.
    pub fn mixed_site() -> Self {
        Span(bridge::span::Span::mixed_site())
    }

    /// Creates a new span with the same line/column informations but that
    /// resolve symbols as though it were at `other`.
    ///
    /// # Arguments
    ///
    /// * `other` - Other span to resolve at.
    pub fn resolved_at(&self, other: Span) -> Self {
        Span(self.0.resolved_at(other.0))
    }

    /// Creates a new span with the same name resolution behavior as self, but
    /// with the line/column information of `other`.
    ///
    /// # Arguments
    ///
    /// * `other` - Other span containing the line/column informations to use.
    pub fn located_at(&self, other: Span) -> Self {
        Span(self.0.located_at(other.0))
    }

    /// Return the source text behind a span.
    pub fn source_text(&self) -> Option<String> {
        self.0.source_text()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
