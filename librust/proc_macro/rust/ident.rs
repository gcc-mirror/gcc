use bridge;
use std::fmt;
use Span;

/// An identifier.
#[derive(Clone)]
pub struct Ident(pub(crate) bridge::ident::Ident);

impl Ident {
    /// Creates a new identifier.
    ///
    /// # Arguments
    ///
    /// * `string` - A valid identifier.
    /// * `span` - The span of the identifier.
    ///
    /// # Panics
    ///
    /// The `string` argument must be a valid identifier permitted by the
    /// language, otherwise the function will panic.
    pub fn new(string: &str, span: Span) -> Self {
        Ident(bridge::ident::Ident::new(string, span.0))
    }

    /// Creates a new raw identifier.
    ///
    /// # Arguments
    ///
    /// * `string` - A valid identifier.
    /// * `span` - The span of the identifier.
    ///
    /// # Panics
    ///
    /// The `string` argument must be a valid identifier permitted by the
    /// language. Furthermore, it should not be a keyword used in path
    /// segments, otherwise this function will panic.
    pub fn new_raw(string: &str, span: Span) -> Self {
        Ident(bridge::ident::Ident::new_raw(string, span.0))
    }

    /// Return the span of the identifier
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Change the span of the identifier.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0);
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
