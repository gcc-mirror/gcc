use bridge::span::Span;
use std::convert::{TryFrom, TryInto};
use std::ffi::c_uchar;
use std::fmt;
use std::str::FromStr;
use LexError;

extern "C" {
    fn Literal__drop(literal: *const Literal);
    fn Literal__string(str: *const c_uchar, len: u64) -> Literal;
    fn Literal__byte_string(bytes: *const u8, len: u64) -> Literal;
    fn Literal__from_string(str: *const c_uchar, len: u64, lit: *mut Literal) -> bool;
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum Unsigned {
    Unsigned8(u8),
    Unsigned16(u16),
    Unsigned32(u32),
    Unsigned64(u64),
    // u128 is not ffi safe, hence this representation
    // https://github.com/rust-lang/rust/issues/54341
    Unsigned128(u64, u64),
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum Signed {
    Signed8(i8),
    Signed16(i16),
    Signed32(i32),
    Signed64(i64),
    // i128 is not ffi safe, hence this representation
    // https://github.com/rust-lang/rust/issues/54341
    Signed128(u64, u64),
}

#[repr(C)]
#[derive(Debug)]
pub enum Literal {
    /// String literal internal representation
    ///
    /// # Note
    /// This variant is constructed through FFI
    #[allow(dead_code)]
    String {
        data: *const c_uchar,
        len: u64,
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

    pub fn u128_suffixed(n: u128) -> Self {
        Literal::Unsigned(
            Unsigned::Unsigned128(
                (n >> 64).try_into().unwrap(),
                (n & 0xFFFFFFFFFFFFFFFF).try_into().unwrap(),
            ),
            true,
        )
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

    pub fn i128_suffixed(n: i128) -> Self {
        Literal::Signed(
            Signed::Signed128(
                (n >> 64).try_into().unwrap(),
                (n & 0xFFFFFFFFFFFFFFFF).try_into().unwrap(),
            ),
            true,
        )
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

    pub fn u128_unsuffixed(n: u128) -> Self {
        Literal::Unsigned(
            Unsigned::Unsigned128(
                (n >> 64).try_into().unwrap(),
                (n & 0xFFFFFFFFFFFFFFFF).try_into().unwrap(),
            ),
            false,
        )
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

    pub fn i128_unsuffixed(n: i128) -> Self {
        Literal::Signed(
            Signed::Signed128(
                (n >> 64).try_into().unwrap(),
                (n & 0xFFFFFFFFFFFFFFFF).try_into().unwrap(),
            ),
            false,
        )
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
        unsafe { Literal__string(string.as_ptr(), string.len().try_into().unwrap()) }
    }

    pub fn character(c: char) -> Self {
        Literal::Char(c.into())
    }

    pub fn byte_string(bytes: &[u8]) -> Self {
        unsafe { Literal__byte_string(bytes.as_ptr(), bytes.len().try_into().unwrap()) }
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String { data, len } => {
                let slice =
                    unsafe { std::slice::from_raw_parts(*data, (*len).try_into().unwrap()) };
                f.write_str("\"")?;
                f.write_str(std::str::from_utf8(slice).unwrap())?;
                f.write_str("\"")?;
            }
            Literal::ByteString { data, size } => {
                f.write_str("b\"")?;
                let slice =
                    unsafe { std::slice::from_raw_parts(*data, (*size).try_into().unwrap()) };
                for &byte in slice {
                    if byte != b'"' && (b' '..=b'z').contains(&byte) {
                        char::try_from(byte).unwrap().fmt(f)?;
                    } else {
                        write!(f, "\\x{:02x}", byte)?;
                    }
                }
                f.write_str("b\"")?;
            }
            Literal::Char(val) => {
                let ch: char = (*val).try_into().unwrap();
                match ch {
                    '\'' => f.write_str("'\\''")?,
                    '\0' => f.write_str("'\\0'")?,
                    '\n' => f.write_str("'\\n'")?,
                    ' '..='z' => write!(f, "'{}'", ch)?,
                    _ => write!(f, "'\\u{:x}'", val)?,
                }
            }
            Literal::Unsigned(val, suffixed) => match val {
                Unsigned::Unsigned8(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("u8")?;
                    }
                }
                Unsigned::Unsigned16(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("u16")?;
                    }
                }
                Unsigned::Unsigned32(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("u32")?;
                    }
                }
                Unsigned::Unsigned64(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("u64")?;
                    }
                }
                Unsigned::Unsigned128(h, l) => {
                    ((u128::from(*h) << 64) & u128::from(*l)).fmt(f)?;
                    if *suffixed {
                        f.write_str("u128")?;
                    }
                }
            },
            Literal::Signed(val, suffixed) => match val {
                Signed::Signed8(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("i8")?;
                    }
                }
                Signed::Signed16(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("i16")?;
                    }
                }
                Signed::Signed32(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("i32")?;
                    }
                }
                Signed::Signed64(val) => {
                    val.fmt(f)?;
                    if *suffixed {
                        f.write_str("i64")?;
                    }
                }
                Signed::Signed128(h, l) => {
                    ((i128::from(*h) << 64) & i128::from(*l)).fmt(f)?;
                    if *suffixed {
                        f.write_str("i128")?;
                    }
                }
            },
            Literal::Usize(val, suffixed) => {
                val.fmt(f)?;
                if *suffixed {
                    f.write_str("usize")?;
                }
            }
            Literal::ISize(val, suffixed) => {
                val.fmt(f)?;
                if *suffixed {
                    f.write_str("isize")?;
                }
            }
            Literal::Float32(val, suffixed) => {
                val.fmt(f)?;
                if *suffixed {
                    f.write_str("f32")?;
                }
            }
            Literal::Float64(val, suffixed) => {
                val.fmt(f)?;
                if *suffixed {
                    f.write_str("f64")?;
                }
            }
        }
        Ok(())
    }
}

impl FromStr for Literal {
    type Err = LexError;

    fn from_str(string: &str) -> Result<Self, LexError> {
        let mut lit = Literal::Char(0);
        // TODO: We might want to pass a LexError by reference to retrieve
        // error information
        if unsafe {
            Literal__from_string(
                string.as_ptr(),
                string.len().try_into().unwrap(),
                &mut lit as *mut Literal,
            )
        } {
            Err(LexError)
        } else {
            Ok(lit)
        }
    }
}

impl Clone for Literal {
    fn clone(&self) -> Self {
        match self {
            Literal::String { data, len } => unsafe { Literal__string(*data, *len) },
            Literal::ByteString { data, size } => unsafe { Literal__byte_string(*data, *size) },
            Literal::Char(val) => Literal::Char(*val),
            Literal::Unsigned(val, suffixed) => Literal::Unsigned(*val, *suffixed),
            Literal::Signed(val, suffixed) => Literal::Signed(*val, *suffixed),
            Literal::Usize(val, suffixed) => Literal::Usize(*val, *suffixed),
            Literal::ISize(val, suffixed) => Literal::ISize(*val, *suffixed),
            Literal::Float32(val, suffixed) => Literal::Float32(*val, *suffixed),
            Literal::Float64(val, suffixed) => Literal::Float64(*val, *suffixed),
        }
    }
}
