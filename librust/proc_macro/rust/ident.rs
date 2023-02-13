use std::fmt;
use Span;

/// An identifier.
#[derive(Clone)]
pub struct Ident {
    // Internal implementation details
}

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
    pub fn new(_string: &str, _span: Span) -> Self {
        todo!("Implement this function")
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
    pub fn new_raw(_string: &str, _span: Span) -> Self {
        todo!("Implement this function")
    }

    /// Return the span of the identifier
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// Change the span of the identifier.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, _span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string.
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}
