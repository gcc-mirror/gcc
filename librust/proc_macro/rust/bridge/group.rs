use bridge;
use Delimiter;

#[repr(C)]
#[derive(Clone)]
pub struct Group {
    delimiter: Delimiter,
    stream: bridge::token_stream::TokenStream,
}
