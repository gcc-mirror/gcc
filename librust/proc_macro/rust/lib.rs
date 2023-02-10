use std::error;
use std::fmt;
use std::str::FromStr;

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

#[derive(Clone)]
pub struct Literal {
    // Internal implementation details
}

impl Literal {
    // TODO: Add experimental API functions for this type

    pub fn u8_suffixed(n: u8) -> Self {
        todo!("Implement this function")
    }

    pub fn u16_suffixed(n: u16) -> Self {
        todo!("Implement this function")
    }

    pub fn u32_suffixed(n: u32) -> Self {
        todo!("Implement this function")
    }

    pub fn u64_suffixed(n: u64) -> Self {
        todo!("Implement this function")
    }

    pub fn u128_suffixed(n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_suffixed(n: usize) -> Self {
        todo!("Implement this function")
    }

    pub fn i8_suffixed(n: i8) -> Self {
        todo!("Implement this function")
    }

    pub fn i16_suffixed(n: i16) -> Self {
        todo!("Implement this function")
    }

    pub fn i32_suffixed(n: i32) -> Self {
        todo!("Implement this function")
    }

    pub fn i64_suffixed(n: i64) -> Self {
        todo!("Implement this function")
    }

    pub fn i128_suffixed(n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_suffixed(n: isize) -> Self {
        todo!("Implement this function")
    }

    // Unsuffixed

    pub fn u8_unsuffixed(n: u8) -> Self {
        todo!("Implement this function")
    }

    pub fn u16_unsuffixed(n: u16) -> Self {
        todo!("Implement this function")
    }

    pub fn u32_unsuffixed(n: u32) -> Self {
        todo!("Implement this function")
    }

    pub fn u64_unsuffixed(n: u64) -> Self {
        todo!("Implement this function")
    }

    pub fn u128_unsuffixed(n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_unsuffixed(n: usize) -> Self {
        todo!("Implement this function")
    }

    pub fn i8_unsuffixed(n: i8) -> Self {
        todo!("Implement this function")
    }

    pub fn i16_unsuffixed(n: i16) -> Self {
        todo!("Implement this function")
    }

    pub fn i32_unsuffixed(n: i32) -> Self {
        todo!("Implement this function")
    }

    pub fn i64_unsuffixed(n: i64) -> Self {
        todo!("Implement this function")
    }

    pub fn i128_unsuffixed(n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_unsuffixed(n: isize) -> Self {
        todo!("Implement this function")
    }

    pub fn f32_unsuffixed(n: f32) -> Self {
        todo!("Implement this function")
    }

    pub fn f32_suffixed(n: f32) -> Self {
        todo!("Implement this function")
    }

    pub fn f64_unsuffixed(n: f64) -> Self {
        todo!("Implement this function")
    }

    pub fn f64_suffixed(n: f64) -> Self {
        todo!("Implement this function")
    }

    pub fn string(string: &str) -> Self {
        todo!("Implement this function")
    }

    pub fn character(c: char) -> Self {
        todo!("Implement this function")
    }

    pub fn byte_string(bytes: &[u8]) -> Self {
        todo!("Implement this function")
    }

    pub fn span(&self) -> Span {
        todo!("Get the span of a literal")
    }

    pub fn set_span(&mut self, span: Span) {
        todo!("Set the span of a literal")
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Implement this function")
    }
}

impl FromStr for Literal {
    type Err = LexError;

    fn from_str(src: &str) -> Result<Self, LexError> {
        todo!("Implement this function")
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Spacing {
    Alone,
    Joint,
}
