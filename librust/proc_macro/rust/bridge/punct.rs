use bridge::span::Span;
use Spacing;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Punct {
    pub(crate) ch: u32,
    pub(crate) spacing: Spacing,
}

impl Punct {
    pub fn new(ch: char, spacing: Spacing) -> Self {
        Punct {
            ch: ch.into(),
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
