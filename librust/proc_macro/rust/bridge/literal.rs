use bridge::span::Span;
use std::convert::TryInto;
use std::ffi::c_uchar;

extern "C" {
    fn Literal__drop(literal: *const Literal);
    fn Literal__string(str: *const c_uchar) -> Literal;
    fn Literal__byte_string(bytes: *const u8) -> Literal;
}

#[repr(C)]
#[derive(Clone)]
pub enum Unsigned {
    Unsigned8(u8),
    Unsigned16(u16),
    Unsigned32(u32),
    Unsigned64(u64),
    // FIXME: 128 bits ffi is not safe for now
    // https://github.com/rust-lang/rust/issues/54341
    //
    // Unsigned128(u128),
}

#[repr(C)]
#[derive(Clone)]
pub enum Signed {
    Signed8(i8),
    Signed16(i16),
    Signed32(i32),
    Signed64(i64),
    // FIXME: 128 bits ffi is not safe for now
    // https://github.com/rust-lang/rust/issues/54341
    //
    // Signed128(i128),
}

#[repr(C)]
#[derive(Clone)]
pub enum Literal {
    /// String literal internal representation
    ///
    /// # Note
    /// This variant is constructed through FFI
    #[allow(dead_code)]
    String {
        data: *const c_uchar,
        size: u64,
    },
    /// Bytestring literal internal representation
    ///
    /// # Note
    /// This variant is constructed through FFI
    #[allow(dead_code)]
    ByteString {
        data: *const u8,
        size: u64,
    },
    Char(u32),
    Unsigned(Unsigned, bool),
    Signed(Signed, bool),
    Usize(u64, bool),
    ISize(i64, bool),
    Float32(f32, bool),
    Float64(f64, bool),
}

impl Literal {
    pub fn u8_suffixed(n: u8) -> Self {
        Literal::Unsigned(Unsigned::Unsigned8(n), true)
    }

    pub fn u16_suffixed(n: u16) -> Self {
        Literal::Unsigned(Unsigned::Unsigned16(n), true)
    }

    pub fn u32_suffixed(n: u32) -> Self {
        Literal::Unsigned(Unsigned::Unsigned32(n), true)
    }

    pub fn u64_suffixed(n: u64) -> Self {
        Literal::Unsigned(Unsigned::Unsigned64(n), true)
    }

    pub fn u128_suffixed(_n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_suffixed(n: usize) -> Self {
        Literal::Usize(n.try_into().expect("Cannot convert usize to u64"), true)
    }

    pub fn i8_suffixed(n: i8) -> Self {
        Literal::Signed(Signed::Signed8(n), true)
    }

    pub fn i16_suffixed(n: i16) -> Self {
        Literal::Signed(Signed::Signed16(n), true)
    }

    pub fn i32_suffixed(n: i32) -> Self {
        Literal::Signed(Signed::Signed32(n), true)
    }

    pub fn i64_suffixed(n: i64) -> Self {
        Literal::Signed(Signed::Signed64(n), true)
    }

    pub fn i128_suffixed(_n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_suffixed(n: isize) -> Self {
        Literal::ISize(n.try_into().expect("Cannot convert isize to i64"), true)
    }

    // Unsuffixed

    pub fn u8_unsuffixed(n: u8) -> Self {
        Literal::Unsigned(Unsigned::Unsigned8(n), false)
    }

    pub fn u16_unsuffixed(n: u16) -> Self {
        Literal::Unsigned(Unsigned::Unsigned16(n), false)
    }

    pub fn u32_unsuffixed(n: u32) -> Self {
        Literal::Unsigned(Unsigned::Unsigned32(n), false)
    }

    pub fn u64_unsuffixed(n: u64) -> Self {
        Literal::Unsigned(Unsigned::Unsigned64(n), false)
    }

    pub fn u128_unsuffixed(_n: u128) -> Self {
        todo!("Implement this function")
    }

    pub fn usize_unsuffixed(n: usize) -> Self {
        Literal::Usize(n.try_into().expect("Cannot convert usize to u64"), false)
    }

    pub fn i8_unsuffixed(n: i8) -> Self {
        Literal::Signed(Signed::Signed8(n), false)
    }

    pub fn i16_unsuffixed(n: i16) -> Self {
        Literal::Signed(Signed::Signed16(n), false)
    }

    pub fn i32_unsuffixed(n: i32) -> Self {
        Literal::Signed(Signed::Signed32(n), false)
    }

    pub fn i64_unsuffixed(n: i64) -> Self {
        Literal::Signed(Signed::Signed64(n), false)
    }

    pub fn i128_unsuffixed(_n: i128) -> Self {
        todo!("Implement this function")
    }

    pub fn isize_unsuffixed(n: isize) -> Self {
        Literal::ISize(n.try_into().expect("Cannot convert isize to i64"), false)
    }

    pub fn f32_unsuffixed(n: f32) -> Self {
        Literal::Float32(n, false)
    }

    pub fn f32_suffixed(n: f32) -> Self {
        Literal::Float32(n, true)
    }

    pub fn f64_unsuffixed(n: f64) -> Self {
        Literal::Float64(n, false)
    }

    pub fn f64_suffixed(n: f64) -> Self {
        Literal::Float64(n, true)
    }

    pub fn string(string: &str) -> Self {
        unsafe { Literal__string(string.as_ptr()) }
    }

    pub fn character(c: char) -> Self {
        Literal::Char(c.into())
    }

    pub fn byte_string(bytes: &[u8]) -> Self {
        unsafe { Literal__byte_string(bytes.as_ptr()) }
    }

    pub fn span(&self) -> Span {
        Span {}
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}

impl Drop for Literal {
    fn drop(&mut self) {
        match self {
            Literal::String { .. } | Literal::ByteString { .. } => unsafe {
                Literal__drop(self as *const Literal)
            },
            _ => (),
        }
    }
}
