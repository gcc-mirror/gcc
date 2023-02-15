use bridge::span::Span;
use std::ffi::CString;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Ident {
    pub(crate) is_raw: bool,
    pub(crate) val: CString,
}

impl Ident {
    pub fn new(string: &str, _span: Span) -> Self {
        Ident {
            is_raw: false,
            val: CString::new(string).expect("Cannot create CString from rust String"),
        }
    }

    pub fn new_raw(string: &str, _span: Span) -> Self {
        Ident {
            is_raw: true,
            val: CString::new(string).expect("Cannot create CString from rust String"),
        }
    }

    pub fn span(&self) -> Span {
        Span {}
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}
