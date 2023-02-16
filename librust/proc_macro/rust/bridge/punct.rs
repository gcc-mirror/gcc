use bridge::span::Span;
use std::convert::TryInto;
use std::ffi::c_uchar;
use Spacing;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Punct {
    pub(crate) ch: c_uchar,
    pub(crate) spacing: Spacing,
}

impl Punct {
    pub fn new(ch: char, spacing: Spacing) -> Self {
        Punct {
            ch: ch
                .try_into()
                .expect("Failed to convert rust char to c char"),
            spacing,
        }
    }

    pub fn span(&self) -> Span {
        Span {}
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}
