//! Bridge internal span representation and functions
//!
//! # Note
//!
//! All methods accessing source location in rust are unstable, hence this
//! implementation with an empty structure.

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Span {}

impl Span {
    pub fn call_site() -> Self {
        Span {}
    }

    pub fn mixed_site() -> Self {
        Span {}
    }

    pub fn resolved_at(&self, _other: Span) -> Self {
        Span {}
    }

    pub fn located_at(&self, _other: Span) -> Self {
        Span {}
    }

    pub fn source_text(&self) -> Option<String> {
        None
    }
}
