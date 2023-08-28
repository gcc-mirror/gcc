//! Bridge internal span representation and functions
//!
//! # Note
//!
//! All methods accessing source location in rust are unstable, hence this
//! implementation with an empty structure.

/// # Note: Gcc does not have a span interner, a span will not contain an index
#[derive(Copy, Clone, Debug, Default)]
#[repr(C)]
pub struct Span {
    location: u32,
}

impl Span {
    pub fn call_site() -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn mixed_site() -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn resolved_at(&self, _other: Span) -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn located_at(&self, _other: Span) -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn source_text(&self) -> Option<String> {
        // FIXME: implement this function properly
        None
    }
}
