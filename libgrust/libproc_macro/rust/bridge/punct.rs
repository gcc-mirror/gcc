use bridge::span::Span;
use std::convert::TryFrom;
use std::fmt;
use Spacing;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Punct {
    pub(crate) ch: u32,
    pub(crate) spacing: Spacing,
    span: Span,
}

impl Punct {
    pub fn new(ch: char, spacing: Spacing) -> Self {
        Punct {
            ch: ch.into(),
            spacing,
            span: Span::default(),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}

impl fmt::Display for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Spacing::Alone = self.spacing {
            f.write_str(" ")?;
        }
        char::try_from(self.ch).unwrap().fmt(f)
    }
}
