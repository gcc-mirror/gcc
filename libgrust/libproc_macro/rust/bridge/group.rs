use bridge::span::Span;
use bridge::token_stream::TokenStream;
use std::fmt;
use Delimiter;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Group {
    delimiter: Delimiter,
    stream: TokenStream,
    span: Span,
}

impl Group {
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
        Group {
            delimiter,
            stream,
            span: Span::default(),
        }
    }

    pub fn delimiter(&self) -> Delimiter {
        self.delimiter
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }

    pub fn stream(&self) -> TokenStream {
        self.stream.clone()
    }
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.delimiter {
            Delimiter::Parenthesis => f.write_str("(")?,
            Delimiter::Brace => f.write_str("{")?,
            Delimiter::Bracket => f.write_str("[")?,
            Delimiter::None => (),
        }

        self.stream.fmt(f)?;

        match self.delimiter {
            Delimiter::Parenthesis => f.write_str(")")?,
            Delimiter::Brace => f.write_str("}")?,
            Delimiter::Bracket => f.write_str("]")?,
            Delimiter::None => (),
        }

        Ok(())
    }
}
