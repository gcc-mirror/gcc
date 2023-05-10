pub mod ffistring;
pub mod group;
pub mod ident;
pub mod literal;
pub mod punct;
pub mod span;
pub mod token_stream;

extern "C" {
    fn bridge__is_available() -> bool;
}

pub fn is_available() -> bool {
    unsafe { bridge__is_available() }
}
