pub mod ident;
pub mod literal;
pub mod punct;
pub mod span;

extern "C" {
    fn bridge__is_available() -> bool;
}

pub fn is_available() -> bool {
    unsafe { bridge__is_available() }
}
