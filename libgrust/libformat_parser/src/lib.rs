//! FFI interface for `rustc_format_parser`

// what's the plan? Have a function return something that can be constructed into a vector?
// or an iterator?

use std::ffi::CStr;

// TODO: Use rustc's version here #3
use generic_format_parser::Piece;

// FIXME: Rename?
pub mod rust {
    use generic_format_parser::{ParseMode, Parser, Piece};

    pub fn collect_pieces(input: &str) -> Vec<Piece<'_>> {
        // let parser = Parser::new();
        let parser = Parser::new(input, None, None, true, ParseMode::Format);

        parser.into_iter().collect()
    }
}

#[repr(C)]
pub struct PieceSlice {
    base_ptr: *const Piece<'static /* FIXME: That's wrong */>,
    len: usize,
}

#[no_mangle]
pub extern "C" fn collect_pieces(input: *const libc::c_char) -> PieceSlice {
    // FIXME: Add comment
    let str = unsafe { CStr::from_ptr(input) };

    // FIXME: No unwrap
    let pieces = rust::collect_pieces(str.to_str().unwrap());

    PieceSlice {
        base_ptr: pieces.as_ptr(),
        len: pieces.len(),
    }
}
