use std::fmt;
use Spacing;
use Span;

#[derive(Clone)]
pub struct Punct {
    // Internal implementation details.
}

impl Punct {
    /// Creates a new [Punct] from a given character and spacing.
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
    pub fn new(_ch: char, _spacing: Spacing) -> Self {
        todo!("Implement this function")
    }

    /// Get the value for this punctuation character as `char`.
    pub fn as_char(&self) -> char {
        todo!("Implement this function")
    }

    /// Get the [`Spacing`] of this punctuation character, indicating whether
    /// the following character can be combined into a multi-character operator
    /// or not.
    pub fn spacing(&self) -> Spacing {
        todo!("Implement this function")
    }

    /// Get the [`Span`] for this punctuation character.
    pub fn span(&self) -> Span {
        todo!("Implement this function")
    }

    /// Set the span for this punctuation character.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, _span: Span) {
        todo!("Implement this function")
    }
}

impl fmt::Display for Punct {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Debug for Punct {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl PartialEq<char> for Punct {
    fn eq(&self, _rhs: &char) -> bool {
        todo!("Implement this function")
    }
}

impl PartialEq<Punct> for char {
    fn eq(&self, _rhs: &Punct) -> bool {
        todo!("Implement this function")
    }
}
