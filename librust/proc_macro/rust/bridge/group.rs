use bridge;
use std::fmt;
use Delimiter;

#[repr(C)]
#[derive(Clone)]
pub struct Group {
    delimiter: Delimiter,
    stream: bridge::token_stream::TokenStream,
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
