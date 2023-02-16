use bridge::span::Span;
use std::ffi::{c_char, CStr, CString};
use std::fmt;

extern "C" {
    fn Ident__new(string: *const c_char) -> Ident;
    fn Ident__new_raw(string: *const c_char) -> Ident;
    fn Ident__drop(ident: *const Ident);
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Ident {
    pub(crate) is_raw: bool,
    pub(crate) val: *const c_char,
}

impl Ident {
    pub fn new(string: &str, _span: Span) -> Self {
        let string = CString::new(string).expect("Cannot convert to CString");
        unsafe { Ident__new(string.as_ptr()) }
    }

    pub fn new_raw(string: &str, _span: Span) -> Self {
        let string = CString::new(string).expect("Cannot convert to CString");
        unsafe { Ident__new_raw(string.as_ptr()) }
    }

    pub fn span(&self) -> Span {
        Span {}
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}

impl Drop for Ident {
    fn drop(&mut self) {
        unsafe { Ident__drop(self as *const Ident) }
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
                CStr::from_ptr(self.val)
                    .to_str()
                    .expect("Cannot convert back to rust string")
            },
            f,
        )
    }
}
