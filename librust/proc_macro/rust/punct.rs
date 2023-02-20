use bridge;
use std::convert::TryInto;
use std::fmt;
use Span;

/// Describes the context of a [`Punct`] relatively to the next token.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Spacing {
    /// A [`Punct`] is not immediately followed by another `Punct`.
    Alone,
    /// A [`Punct`] is immediately followed by another `Punct` and can be
    /// combined into a multi-character operator.
    Joint,
}

/// Single punctuation character such as `+`, `-` or `#`.
///
/// Multi-character operators like `+=` are represented as two instances of
/// `Punct` with different forms of `Spacing` returned.
#[derive(Clone)]
pub struct Punct(pub(crate) bridge::punct::Punct);

impl Punct {
    /// Creates a new `Punct` from a given character and spacing.
    ///
    /// # Arguments
    ///
    /// * `ch` - The punctuation character.
    /// * `spacing` - The link between this character and the next one.
    ///
    /// # Panics
    ///
    /// This function will panic if the `ch` argument is not a valid
    /// punctuation character allowed by the language.
    pub fn new(ch: char, spacing: Spacing) -> Self {
        Punct(bridge::punct::Punct::new(ch, spacing))
    }

    /// Get the value for this punctuation character as `char`.
    pub fn as_char(&self) -> char {
        self.0
            .ch
            .try_into()
            .expect("Cannot convert from u32 to char")
    }

    /// Get the [`Spacing`] of this punctuation character, indicating whether
    /// the following character can be combined into a multi-character operator
    /// or not.
    pub fn spacing(&self) -> Spacing {
        self.0.spacing
    }

    /// Get the [`Span`] for this punctuation character.
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Set the span for this punctuation character.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0);
    }
}

impl fmt::Display for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq<char> for Punct {
    fn eq(&self, rhs: &char) -> bool {
        self.as_char() == *rhs
    }
}

impl PartialEq<Punct> for char {
    fn eq(&self, rhs: &Punct) -> bool {
        *self == rhs.as_char()
    }
}
