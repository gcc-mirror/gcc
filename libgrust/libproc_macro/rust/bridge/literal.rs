use bridge::{ffistring::FFIString, span::Span};
use std::convert::TryInto;
use std::ffi::c_uchar;
use std::fmt;
use std::str::FromStr;
use LexError;

extern "C" {
    fn Literal__from_string(str: *const c_uchar, len: u64, lit: *mut Literal) -> bool;
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum LitKind {
    Byte,
    Char,
    Integer,
    Float,
    Str,
    StrRaw(u8),
    ByteStr,
    ByteStrRaw(u8),
    Err,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Literal {
    kind: LitKind,
    text: FFIString,
    suffix: FFIString,
    // FIXME: Add span, cannot add whilst Span remain an empty type
}

macro_rules! suffixed_int_literals {
    ($($name: ident => $kind: ident,)*) => ($(
        pub fn $name(n : $kind) -> Literal {
            Literal {
                kind : LitKind::Integer,
                text: FFIString::from(&n.to_string()),
                suffix: FFIString::from(stringify!($kind))
            }
        }
    )*)
}

macro_rules! unsuffixed_int_literals {
    ($($name: ident => $kind: ident,)*) => ($(
        pub fn $name(n : $kind) -> Literal {
            Literal {
                kind : LitKind::Integer,
                text: FFIString::from(&n.to_string()),
                suffix: FFIString::from("")
            }
        }
    )*)
}

impl Literal {
    suffixed_int_literals! {
        u8_suffixed => u8,
        u16_suffixed => u16,
        u32_suffixed => u32,
        u64_suffixed => u64,
        u128_suffixed => u128,
        usize_suffixed => usize,
        i8_suffixed => i8,
        i16_suffixed => i16,
        i32_suffixed => i32,
        i64_suffixed => i64,
        i128_suffixed => i128,
        isize_suffixed => isize,
    }

    unsuffixed_int_literals! {
        u8_unsuffixed => u8,
        u16_unsuffixed => u16,
        u32_unsuffixed => u32,
        u64_unsuffixed => u64,
        u128_unsuffixed => u128,
        usize_unsuffixed => usize,
        i8_unsuffixed => i8,
        i16_unsuffixed => i16,
        i32_unsuffixed => i32,
        i64_unsuffixed => i64,
        i128_unsuffixed => i128,
        isize_unsuffixed => isize,
    }

    pub fn f32_unsuffixed(n: f32) -> Self {
        let mut repr = n.to_string();
        if !repr.contains('.') {
            repr.push_str(".0");
        }

        Literal {
            kind: LitKind::Float,
            text: FFIString::from(&repr),
            suffix: FFIString::from(""),
        }
    }

    pub fn f32_suffixed(n: f32) -> Self {
        Literal {
            kind: LitKind::Float,
            text: FFIString::from(&n.to_string()),
            suffix: FFIString::from("f32"),
        }
    }

    pub fn f64_unsuffixed(n: f64) -> Self {
        let mut repr = n.to_string();
        if !repr.contains('.') {
            repr.push_str(".0");
        }

        Literal {
            kind: LitKind::Float,
            text: FFIString::from(&repr),
            suffix: FFIString::from(""),
        }
    }

    pub fn f64_suffixed(n: f64) -> Self {
        Literal {
            kind: LitKind::Float,
            text: FFIString::from(&n.to_string()),
            suffix: FFIString::from("f64"),
        }
    }

    pub fn string(string: &str) -> Self {
        Literal {
            kind: LitKind::Str,
            text: FFIString::from(string),
            suffix: FFIString::from(""),
        }
    }

    pub fn character(c: char) -> Self {
        Literal {
            kind: LitKind::Char,
            text: FFIString::from(&c.to_string()),
            suffix: FFIString::from(""),
        }
    }

    pub fn byte_string(bytes: &[u8]) -> Self {
        Literal {
            kind: LitKind::ByteStr,
            text: FFIString::from(&bytes.escape_ascii().to_string()),
            suffix: FFIString::from(""),
        }
    }

    pub fn span(&self) -> Span {
        Span {}
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = &self.text.to_string();
        match self.kind {
            LitKind::Byte => {
                f.write_str("b'")?;
                f.write_str(text)?;
                f.write_str("'")?;
            }
            LitKind::Char => {
                f.write_str("'")?;
                f.write_str(text)?;
                f.write_str("'")?;
            }
            LitKind::Str => {
                f.write_str("\"")?;
                f.write_str(text)?;
                f.write_str("\"")?;
            }
            LitKind::StrRaw(n) => {
                f.write_str("r")?;
                for _ in 0..n {
                    f.write_str("#")?;
                }
                f.write_str("\"")?;
                f.write_str(text)?;
                f.write_str("\"")?;
            }
            LitKind::ByteStr => {
                f.write_str("b\"")?;
                f.write_str(text)?;
                f.write_str("\"")?;
            }
            LitKind::ByteStrRaw(n) => {
                f.write_str("br")?;
                for _ in 0..n {
                    f.write_str("#")?;
                }
                f.write_str("\"")?;
                f.write_str(text)?;
                f.write_str("\"")?;
            }
            _ => f.write_str(text)?,
        }

        f.write_str(&self.suffix.to_string())?;
        Ok(())
    }
}

impl FromStr for Literal {
    type Err = LexError;

    fn from_str(string: &str) -> Result<Self, LexError> {
        // Structure that will be filled in by the cpp
        let mut lit = Literal {
            kind: LitKind::Err,
            text: FFIString::from(""),
            suffix: FFIString::from(""),
        };
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
