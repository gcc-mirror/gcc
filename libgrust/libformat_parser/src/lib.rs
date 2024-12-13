//! FFI interface for `rustc_format_parser`

// what's the plan? Have a function return something that can be constructed into a vector?
// or an iterator?

use std::ffi::CStr;

trait StringLeakExt {
    fn leak<'a>(self) -> &'a mut str;
}

impl StringLeakExt for String {
    fn leak<'a>(self) -> &'a mut str {
        Box::leak(self.into_boxed_str())
    }
}

trait IntoFFI<T> {
    fn into_ffi(self) -> T;
}

impl<T> IntoFFI<*const T> for Option<T>
where
    T: Sized,
{
    fn into_ffi(self) -> *const T {
        match self.as_ref() {
            None => std::ptr::null(),
            Some(r) => r as *const T,
        }
    }
}

// FIXME: Make an ffi module in a separate file
// FIXME: Remember to leak the boxed type somehow
// FIXME: How to encode the Option type? As a pointer? Option<T> -> Option<&T> -> *const T could work maybe?
mod ffi {
    use super::IntoFFI;

    // FIXME: We need to ensure we deal with memory properly - whether it's owned by the C++ side or the Rust side
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    #[repr(C)]
    pub struct RustHamster {
        ptr: *const u8,
        len: usize,
    }

    impl<'a> From<&'a str> for RustHamster {
        fn from(s: &'a str) -> RustHamster {
            RustHamster {
                ptr: s.as_ptr(),
                len: s.len(),
            }
        }
    }

    // Note: copied from rustc_span
    /// Range inside of a `Span` used for diagnostics when we only have access to relative positions.
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    #[repr(C)]
    pub struct InnerSpan {
        pub start: usize,
        pub end: usize,
    }

    /// The location and before/after width of a character whose width has changed from its source code
    /// representation
    #[derive(Copy, Clone, PartialEq, Eq)]
    #[repr(C)]
    pub struct InnerWidthMapping {
        /// Index of the character in the source
        pub position: usize,
        /// The inner width in characters
        pub before: usize,
        /// The transformed width in characters
        pub after: usize,
    }

    // TODO: Not needed for now?
    // /// Whether the input string is a literal. If yes, it contains the inner width mappings.
    // #[derive(Clone, PartialEq, Eq)]
    // #[repr(C)]
    // enum InputStringKind {
    //     NotALiteral,
    //     Literal {
    //         width_mappings: Vec<InnerWidthMapping>,
    //     },
    // }

    // TODO: Not needed for now?
    // /// The type of format string that we are parsing.
    // #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    // #[repr(C)]
    // pub enum ParseMode {
    //     /// A normal format string as per `format_args!`.
    //     Format,
    //     /// An inline assembly template string for `asm!`.
    //     InlineAsm,
    // }

    #[derive(Copy, Clone)]
    #[repr(C)]
    struct InnerOffset(usize);

    /// A piece is a portion of the format string which represents the next part
    /// to emit. These are emitted as a stream by the `Parser` class.
    #[derive(Debug, Clone, PartialEq)]
    #[repr(C)]
    pub enum Piece<'a> {
        /// A literal string which should directly be emitted
        String(RustHamster),
        /// This describes that formatting should process the next argument (as
        /// specified inside) for emission.
        // do we need a pointer here? we're doing big cloning anyway
        NextArgument(Argument<'a>),
    }

    /// Representation of an argument specification.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub struct Argument<'a> {
        /// Where to find this argument
        pub position: Position<'a>,
        /// The span of the position indicator. Includes any whitespace in implicit
        /// positions (`{  }`).
        pub position_span: InnerSpan,
        /// How to format the argument
        pub format: FormatSpec<'a>,
    }

    /// Specification for the formatting of an argument in the format string.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub struct FormatSpec<'a> {
        /// Optionally specified character to fill alignment with.
        pub fill: Option<char>,
        /// Span of the optionally specified fill character.
        pub fill_span: *const InnerSpan,
        /// Optionally specified alignment.
        pub align: Alignment,
        /// The `+` or `-` flag.
        pub sign: *const Sign,
        /// The `#` flag.
        pub alternate: bool,
        /// The `0` flag.
        pub zero_pad: bool,
        /// The `x` or `X` flag. (Only for `Debug`.)
        pub debug_hex: *const DebugHex,
        /// The integer precision to use.
        pub precision: Count<'a>,
        /// The span of the precision formatting flag (for diagnostics).
        pub precision_span: *const InnerSpan,
        /// The string width requested for the resulting format.
        pub width: Count<'a>,
        /// The span of the width formatting flag (for diagnostics).
        pub width_span: *const InnerSpan,
        /// The descriptor string representing the name of the format desired for
        /// this argument, this can be empty or any number of characters, although
        /// it is required to be one word.
        pub ty: &'a str,
        /// The span of the descriptor string (for diagnostics).
        pub ty_span: *const InnerSpan,
    }

    /// Enum describing where an argument for a format can be located.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Position<'a> {
        /// The argument is implied to be located at an index
        ArgumentImplicitlyIs(usize),
        /// The argument is located at a specific index given in the format,
        ArgumentIs(usize),
        /// The argument has a name.
        ArgumentNamed(&'a str),
    }

    /// Enum of alignments which are supported.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Alignment {
        /// The value will be aligned to the left.
        AlignLeft,
        /// The value will be aligned to the right.
        AlignRight,
        /// The value will be aligned in the center.
        AlignCenter,
        /// The value will take on a default alignment.
        AlignUnknown,
    }

    /// Enum for the sign flags.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Sign {
        /// The `+` flag.
        Plus,
        /// The `-` flag.
        Minus,
    }

    /// Enum for the debug hex flags.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum DebugHex {
        /// The `x` flag in `{:x?}`.
        Lower,
        /// The `X` flag in `{:X?}`.
        Upper,
    }

    /// A count is used for the precision and width parameters of an integer, and
    /// can reference either an argument or a literal integer.
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Count<'a> {
        /// The count is specified explicitly.
        CountIs(usize),
        /// The count is specified by the argument with the given name.
        CountIsName(&'a str, InnerSpan),
        /// The count is specified by the argument at the given index.
        CountIsParam(usize),
        /// The count is specified by a star (like in `{:.*}`) that refers to the argument at the given index.
        CountIsStar(usize),
        /// The count is implied and cannot be explicitly specified.
        CountImplied,
    }

    impl<'a> From<generic_format_parser::Piece<'a>> for Piece<'a> {
        fn from(old: generic_format_parser::Piece<'a>) -> Self {
            match old {
                generic_format_parser::Piece::String(x) => Piece::String(x.into()),
                generic_format_parser::Piece::NextArgument(x) => {
                    // FIXME: This is problematic - if we do this, then we probably run into the issue that the Box
                    // is freed at the end of the call to collect_pieces. if we just .leak() it, then we have
                    // a memory leak... should we resend the info back to the Rust lib afterwards to free it?
                    // this is definitely the best way - store that pointer in the FFI piece and rebuild the box
                    // in a Rust destructor
                    let ptr = Box::leak(x);
                    let dst = Into::<Argument>::into(*ptr);

                    Piece::NextArgument(dst)
                }
            }
        }
    }

    impl<'a> From<generic_format_parser::Argument<'a>> for Argument<'a> {
        fn from(old: generic_format_parser::Argument<'a>) -> Self {
            Argument {
                position: old.position.into(),
                position_span: old.position_span.into(),
                format: old.format.into(),
            }
        }
    }

    impl<'a> From<generic_format_parser::Position<'a>> for Position<'a> {
        fn from(old: generic_format_parser::Position<'a>) -> Self {
            match old {
                generic_format_parser::Position::ArgumentImplicitlyIs(x) => {
                    Position::ArgumentImplicitlyIs(x.into())
                }
                generic_format_parser::Position::ArgumentIs(x) => Position::ArgumentIs(x.into()),
                generic_format_parser::Position::ArgumentNamed(x) => {
                    Position::ArgumentNamed(x.into())
                }
            }
        }
    }

    impl From<generic_format_parser::InnerSpan> for InnerSpan {
        fn from(old: generic_format_parser::InnerSpan) -> Self {
            InnerSpan {
                start: old.start,
                end: old.end,
            }
        }
    }

    impl<'a> From<generic_format_parser::FormatSpec<'a>> for FormatSpec<'a> {
        fn from(old: generic_format_parser::FormatSpec<'a>) -> Self {
            FormatSpec {
                fill: old.fill,
                fill_span: old.fill_span.map(Into::into).into_ffi(),
                align: old.align.into(),
                sign: old.sign.map(Into::into).into_ffi(),
                alternate: old.alternate,
                zero_pad: old.zero_pad,
                debug_hex: old.debug_hex.map(Into::into).into_ffi(),
                precision: old.precision.into(),
                precision_span: old.precision_span.map(Into::into).into_ffi(),
                width: old.width.into(),
                width_span: old.width_span.map(Into::into).into_ffi(),
                ty: old.ty,
                ty_span: old.ty_span.map(Into::into).into_ffi(),
            }
        }
    }

    impl From<generic_format_parser::DebugHex> for DebugHex {
        fn from(old: generic_format_parser::DebugHex) -> Self {
            match old {
                generic_format_parser::DebugHex::Lower => DebugHex::Lower,
                generic_format_parser::DebugHex::Upper => DebugHex::Upper,
            }
        }
    }

    impl<'a> From<generic_format_parser::Count<'a>> for Count<'a> {
        fn from(old: generic_format_parser::Count<'a>) -> Self {
            match old {
                generic_format_parser::Count::CountIs(x) => Count::CountIs(x),
                generic_format_parser::Count::CountIsName(x, y) => Count::CountIsName(x, y.into()),
                generic_format_parser::Count::CountIsParam(x) => Count::CountIsParam(x),
                generic_format_parser::Count::CountIsStar(x) => Count::CountIsStar(x),
                generic_format_parser::Count::CountImplied => Count::CountImplied,
            }
        }
    }

    impl From<generic_format_parser::Sign> for Sign {
        fn from(old: generic_format_parser::Sign) -> Self {
            match old {
                generic_format_parser::Sign::Plus => Sign::Plus,
                generic_format_parser::Sign::Minus => Sign::Minus,
            }
        }
    }

    impl From<generic_format_parser::Alignment> for Alignment {
        fn from(old: generic_format_parser::Alignment) -> Self {
            match old {
                generic_format_parser::Alignment::AlignLeft => Alignment::AlignLeft,
                generic_format_parser::Alignment::AlignRight => Alignment::AlignRight,
                generic_format_parser::Alignment::AlignCenter => Alignment::AlignCenter,
                generic_format_parser::Alignment::AlignUnknown => Alignment::AlignUnknown,
            }
        }
    }
}

// FIXME: Rename?
pub mod rust {
    use generic_format_parser::{ParseMode, Parser, Piece};

    pub fn collect_pieces(
        input: &str,
        style: Option<usize>,
        snippet: Option<String>,
        append_newline: bool,
    ) -> Vec<Piece<'_>> {
        let parser = Parser::new(input, style, snippet, append_newline, ParseMode::Format);

        parser.into_iter().collect()
    }
}

// TODO: Should we instead make an FFIVector struct?
#[repr(C)]
pub struct PieceSlice {
    base_ptr: *mut ffi::Piece<'static /* FIXME: That's wrong */>,
    len: usize,
    cap: usize,
}

#[repr(C)]
// FIXME: we should probably use FFIString here
pub struct RustString {
    ptr: *const u8,
    len: usize,
    cap: usize,
}

#[repr(C)]
pub struct FormatArgsHandle(PieceSlice, RustString);

#[no_mangle]
pub extern "C" fn collect_pieces(
    input: *const libc::c_char,
    append_newline: bool,
) -> FormatArgsHandle {
    // FIXME: Add comment
    let str = unsafe { CStr::from_ptr(input) };
    let str = str.to_str().unwrap().to_owned();

    // we are never going to free this string here (we leak it later on), so we can extend its lifetime
    // to send it across an FFI boundary.
    // FIXME: Is that correct?
    let s = &str;
    let s = unsafe { std::mem::transmute::<&'_ str, &'static str>(s) };

    // FIXME: No unwrap
    let pieces: Vec<ffi::Piece<'_>> = rust::collect_pieces(s, None, None, append_newline)
        .into_iter()
        .map(Into::into)
        .collect();

    let piece_slice = PieceSlice {
        len: pieces.len(),
        cap: pieces.capacity(),
        base_ptr: pieces.leak().as_mut_ptr(),
    };
    let rust_string = RustString {
        len: str.len(),
        cap: str.capacity(),
        ptr: str.leak().as_ptr(),
    };

    FormatArgsHandle(piece_slice, rust_string)
}

#[no_mangle]
pub unsafe extern "C" fn destroy_pieces(FormatArgsHandle(piece_slice, s): FormatArgsHandle) {
    let PieceSlice { base_ptr, len, cap } = piece_slice;
    drop(Vec::from_raw_parts(base_ptr, len, cap));

    let RustString { ptr, len, cap } = s;
    drop(String::from_raw_parts(ptr as *mut u8, len, cap));
}

#[no_mangle]
pub extern "C" fn clone_pieces(
    FormatArgsHandle(piece_slice, s): &FormatArgsHandle,
) -> FormatArgsHandle {
    let PieceSlice { base_ptr, len, cap } = *piece_slice;

    let v = unsafe { Vec::from_raw_parts(base_ptr, len, cap) };
    let cloned_v = v.clone();

    // FIXME: Add documentation
    v.leak();

    let piece_slice = PieceSlice {
        len: cloned_v.len(),
        cap: cloned_v.capacity(),
        base_ptr: cloned_v.leak().as_mut_ptr(),
    };

    let RustString { ptr, len, cap } = *s;
    let s = unsafe { String::from_raw_parts(ptr as *mut u8, len, cap) };
    let cloned_s = s.clone();

    // FIXME: Documentation
    s.leak();

    let rust_string = RustString {
        len: cloned_s.len(),
        cap: cloned_s.capacity(),
        ptr: cloned_s.leak().as_ptr(),
    };

    FormatArgsHandle(piece_slice, rust_string)
}
