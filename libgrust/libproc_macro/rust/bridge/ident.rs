use bridge::span::Span;
use std::convert::TryInto;
use std::ffi::c_uchar;
use std::fmt;

extern "C" {
    fn Ident__new(string: *const c_uchar, len: u64, span: Span) -> Ident;
    fn Ident__new_raw(string: *const c_uchar, len: u64, span: Span) -> Ident;
    fn Ident__drop(ident: *mut Ident);
    fn Ident__clone(ident: *const Ident) -> Ident;
}

#[repr(C)]
#[derive(Debug)]
pub struct Ident {
    pub(crate) is_raw: bool,
    pub(crate) val: *const c_uchar,
    len: u64,
    span: Span,
}

impl Ident {
    pub fn new(string: &str, span: Span) -> Self {
        unsafe { Ident__new(string.as_ptr(), string.len().try_into().unwrap(), span) }
    }

    pub fn new_raw(string: &str, span: Span) -> Self {
        unsafe { Ident__new_raw(string.as_ptr(), string.len().try_into().unwrap(), span) }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl Drop for Ident {
    fn drop(&mut self) {
        unsafe { Ident__drop(self as *mut Ident) }
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_raw {
            f.write_str("r#")?;
        }
        fmt::Display::fmt(
            unsafe {
                std::str::from_utf8(std::slice::from_raw_parts(
                    self.val,
                    self.len.try_into().unwrap(),
                ))
                .unwrap()
            },
            f,
        )
    }
}

impl Clone for Ident {
    fn clone(&self) -> Self {
        unsafe { Ident__clone(self as *const Ident) }
    }
}
