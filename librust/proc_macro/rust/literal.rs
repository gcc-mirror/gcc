use std::fmt;
use std::str::FromStr;
use LexError;
use Span;

/// A type representing a literal value except `true` and `false`.
///
/// This could be one of the following:
/// * literal string (`"hello"`)
/// * byte string (`b"hello"`)
/// * character (`'a'`)
/// * byte character (`b'a'`)
/// * unsuffixed integer (`42`)
/// * suffixed integer (`42u8`)
/// * unsuffixed floating point number (`1.618`)
/// * suffixed floating point number (`1.618f32`)
///
/// # Note
///
/// Boolean literals like `true` and `false` are `Ident`s and do not belong
/// here.
#[derive(Clone)]
pub struct Literal {
    // Internal implementation details
}

impl Literal {
    // TODO: Add experimental API functions for this type

    pub fn u8_suffixed(_n: u8) -> Self {
        todo!("Implement this function")
    }

    pub fn u16_suffixed(_n: u16) -> Self {
        todo!("Implement this function")
    }

    pub fn u32_suffixed(_n: u32) -> Self {
        todo!("Implement this function")
    }

    pub fn u64_suffixed(_n: u64) -> Self {
        todo!("Implement this function")
    }

    pub fn u128_suffixed(_n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_suffixed(_n: usize) -> Self {
        todo!("Implement this function")
    }

    pub fn i8_suffixed(_n: i8) -> Self {
        todo!("Implement this function")
    }

    pub fn i16_suffixed(_n: i16) -> Self {
        todo!("Implement this function")
    }

    pub fn i32_suffixed(_n: i32) -> Self {
        todo!("Implement this function")
    }

    pub fn i64_suffixed(_n: i64) -> Self {
        todo!("Implement this function")
    }

    pub fn i128_suffixed(_n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_suffixed(_n: isize) -> Self {
        todo!("Implement this function")
    }

    // Unsuffixed

    pub fn u8_unsuffixed(_n: u8) -> Self {
        todo!("Implement this function")
    }

    pub fn u16_unsuffixed(_n: u16) -> Self {
        todo!("Implement this function")
    }

    pub fn u32_unsuffixed(_n: u32) -> Self {
        todo!("Implement this function")
    }

    pub fn u64_unsuffixed(_n: u64) -> Self {
        todo!("Implement this function")
    }

    pub fn u128_unsuffixed(_n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_unsuffixed(_n: usize) -> Self {
        todo!("Implement this function")
    }

    pub fn i8_unsuffixed(_n: i8) -> Self {
        todo!("Implement this function")
    }

    pub fn i16_unsuffixed(_n: i16) -> Self {
        todo!("Implement this function")
    }

    pub fn i32_unsuffixed(_n: i32) -> Self {
        todo!("Implement this function")
    }

    pub fn i64_unsuffixed(_n: i64) -> Self {
        todo!("Implement this function")
    }

    pub fn i128_unsuffixed(_n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_unsuffixed(_n: isize) -> Self {
        todo!("Implement this function")
    }

    pub fn f32_unsuffixed(_n: f32) -> Self {
        todo!("Implement this function")
    }

    pub fn f32_suffixed(_n: f32) -> Self {
        todo!("Implement this function")
    }

    pub fn f64_unsuffixed(_n: f64) -> Self {
        todo!("Implement this function")
    }

    pub fn f64_suffixed(_n: f64) -> Self {
        todo!("Implement this function")
    }

    pub fn string(_string: &str) -> Self {
        todo!("Implement this function")
    }

    pub fn character(_c: char) -> Self {
        todo!("Implement this function")
    }

    pub fn byte_string(_bytes: &[u8]) -> Self {
        todo!("Implement this function")
    }

    /// Get the [`Span`] for this literal.
    pub fn span(&self) -> Span {
        todo!("Get the span of a literal")
    }

    /// Set the span for this literal.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, _span: Span) {
        todo!("Set the span of a literal")
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl FromStr for Literal {
    type Err = LexError;

    fn from_str(_src: &str) -> Result<Self, LexError> {
        todo!("Implement this function")
    }
}
