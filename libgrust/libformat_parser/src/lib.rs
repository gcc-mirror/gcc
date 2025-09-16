//! FFI interface for `rustc_format_parser`

use std::alloc::Layout;

// what's the plan? Have a function return something that can be constructed into a vector?
// or an iterator?

trait IntoFFI<T> {
    fn into_ffi(self) -> T;
}

// FIXME: Make an ffi module in a separate file
// FIXME: Remember to leak the boxed type somehow
// FIXME: How to encode the Option type? As a pointer? Option<T> -> Option<&T> -> *const T could work maybe?
pub mod ffi {
    use super::IntoFFI;
    use std::marker::PhantomData;
    use std::mem::MaybeUninit;

    #[repr(C)]
    pub struct FFIVec<T> {
        data: *mut T,
        len: usize,
        cap: usize
    }

    impl<T> IntoFFI<FFIVec<T>> for Vec<T> {
        fn into_ffi(mut self) -> FFIVec<T> {
            let ret = FFIVec {
                data: self.as_mut_ptr(),
                len: self.len(),
                cap: self.capacity()
            };
            self.leak();
            ret
        }
    }

    impl<T> Drop for FFIVec<T> {
        fn drop(&mut self) {
            unsafe {
                Vec::from_raw_parts(self.data, self.len, self.cap);
            }
        }
    }

    impl<T> FFIVec<T> {
        fn with_vec_ref<R, F: for<'a> FnOnce(&'a Vec<T>) -> R>(
            &self, f: F
        ) -> R {
            let v = unsafe {
                Vec::from_raw_parts(self.data, self.len, self.cap)
            };
            let ret = f(&v);
            v.leak();
            ret
        }

        // currently unused
        // may be nice to have later, though
        #[allow(unused)]
        fn with_vec_mut_ref<R, F: for<'a> FnOnce(&'a mut Vec<T>) -> R>(
            &mut self, f: F
        ) -> R {
            let mut v = unsafe {
                Vec::from_raw_parts(self.data, self.len, self.cap)
            };
            let ret = f(&mut v);
            self.data = v.as_mut_ptr();
            self.len = v.len();
            self.cap = v.capacity();
            v.leak();
            ret
        }
    }

    impl<T> Clone for FFIVec<T>
    where
        T: Clone
    {
        fn clone(&self) -> FFIVec<T> {
            self.with_vec_ref(|v| v.clone().into_ffi())
        }
    }

    // https://github.com/rust-lang/rfcs/blob/master/text/2195-really-tagged-unions.md
    #[repr(u8)]
    #[derive(Copy, Clone, PartialEq, Eq)]
    pub enum FFIOpt<T> {
        Some(T),
        None
    }

    impl<T> IntoFFI<FFIOpt<T>> for Option<T> {
        fn into_ffi(self) -> FFIOpt<T> {
            match self {
                Some(v) => FFIOpt::Some(v),
                None => FFIOpt::None
            }
        }
    }

    // FIXME: We need to ensure we deal with memory properly - whether it's owned by the C++ side or the Rust side
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    #[repr(C)]
    pub struct RustHamster<'a> {
        ptr: *const u8,
        len: usize,
        phantom: PhantomData<&'a u8>
    }

    impl<'a> IntoFFI<RustHamster<'a>> for &'a str {
        fn into_ffi(self) -> RustHamster<'a> {
            RustHamster {
                ptr: self.as_ptr(),
                len: self.len(),
                phantom: PhantomData,
            }
        }
    }

    impl<'a> RustHamster<'a> {
        pub fn as_str(&self) -> &'a str {
            unsafe {
                let slice: &'a [u8] = std::slice::from_raw_parts(self.ptr, self.len);
                std::str::from_utf8_unchecked(slice)
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
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    #[repr(C)]
    pub enum ParseMode {
        /// A normal format string as per `format_args!`.
        Format = 0,
        /// An inline assembly template string for `asm!`.
        InlineAsm,
    }

    /// A piece is a portion of the format string which represents the next part
    /// to emit. These are emitted as a stream by the `Parser` class.
    #[derive(Clone)]
    #[repr(C)]
    pub enum Piece<'a> {
        /// A literal string which should directly be emitted
        String(RustHamster<'a>),
        /// This describes that formatting should process the next argument (as
        /// specified inside) for emission.
        // do we need a pointer here? we're doing big cloning anyway
        NextArgument(Argument<'a>),
    }

    /// Representation of an argument specification.
    #[derive(Clone)]
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
    #[derive(Clone)]
    #[repr(C)]
    pub struct FormatSpec<'a> {
        /// Optionally specified character to fill alignment with.
        pub fill: FFIOpt<char>,
        /// Span of the optionally specified fill character.
        pub fill_span: FFIOpt<InnerSpan>,
        /// Optionally specified alignment.
        pub align: Alignment,
        /// The `+` or `-` flag.
        pub sign: FFIOpt<Sign>,
        /// The `#` flag.
        pub alternate: bool,
        /// The `0` flag.
        pub zero_pad: bool,
        /// The `x` or `X` flag. (Only for `Debug`.)
        pub debug_hex: FFIOpt<DebugHex>,
        /// The integer precision to use.
        pub precision: Count<'a>,
        /// The span of the precision formatting flag (for diagnostics).
        pub precision_span: FFIOpt<InnerSpan>,
        /// The string width requested for the resulting format.
        pub width: Count<'a>,
        /// The span of the width formatting flag (for diagnostics).
        pub width_span: FFIOpt<InnerSpan>,
        /// The descriptor string representing the name of the format desired for
        /// this argument, this can be empty or any number of characters, although
        /// it is required to be one word.
        pub ty: RustHamster<'a>,
        /// The span of the descriptor string (for diagnostics).
        pub ty_span: FFIOpt<InnerSpan>,
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
        ArgumentNamed(RustHamster<'a>),
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
        CountIsName(RustHamster<'a>, InnerSpan),
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
                generic_format_parser::Piece::String(x) => Piece::String(x.into_ffi()),
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
                    Position::ArgumentNamed(x.into_ffi())
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
                fill: old.fill.into_ffi(),
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
                ty: old.ty.into_ffi(),
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
                generic_format_parser::Count::CountIsName(x, y) => Count::CountIsName(x.into_ffi(), y.into()),
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
    use crate::ffi::ParseMode;
    use generic_format_parser::{Parser, Piece};
    pub fn collect_pieces(
        input: &str,
        style: Option<usize>,
        snippet: Option<String>,
        append_newline: bool,
        parse_mode: ParseMode,
    ) -> Vec<Piece<'_>> {
        let converted_parse_mode = match parse_mode {
            ParseMode::Format => generic_format_parser::ParseMode::Format,
            ParseMode::InlineAsm => generic_format_parser::ParseMode::InlineAsm,
        };
        let parser = Parser::new(input, style, snippet, append_newline, converted_parse_mode);
        parser.into_iter().collect()
    }
}

// TODO: Should we instead make an FFIVector struct?
type PieceVec<'a> = ffi::FFIVec<ffi::Piece<'a>>;

#[no_mangle]
pub extern "C" fn collect_pieces<'a>(
    input: ffi::RustHamster<'a>,
    append_newline: bool,
    parse_mode: crate::ffi::ParseMode,
) -> PieceVec<'a> {
    // FIXME: No unwrap
    let pieces: Vec<ffi::Piece<'_>> =
        rust::collect_pieces(input.as_str(), None, None, append_newline, parse_mode)
            .into_iter()
            .map(Into::into)
            .collect();

    pieces.into_ffi()
}

#[no_mangle]
pub extern "C" fn clone_pieces<'a, 'b>(
    piece_vec: &'a PieceVec<'b>
) -> PieceVec<'b> {
    piece_vec.clone()
}

// we need Layout::repeat
// function signature is a bit different, so call it repeat_x
trait LayoutExt {
    fn repeat_x(&self, n: usize) -> Layout;
}

impl LayoutExt for Layout {
    fn repeat_x(&self, n: usize) -> Layout {
        let elem = self.pad_to_align();
        let total_size = elem.size().checked_mul(n).unwrap();
        Layout::from_size_align(total_size, elem.align()).unwrap()
    }
}

#[no_mangle]
pub unsafe extern "C" fn rust_ffi_alloc(
    count: usize, elem_size: usize, align: usize
) -> *mut u8 {
    unsafe {
        std::alloc::alloc(
            Layout::from_size_align_unchecked(elem_size, align)
                .repeat_x(count)
        )
    }
}

#[no_mangle]
pub unsafe extern "C" fn rust_ffi_dealloc(
    data: *mut u8, count: usize, elem_size: usize, align: usize
) {
    unsafe {
        std::alloc::dealloc(
            data,
            Layout::from_size_align_unchecked(elem_size, align)
                .repeat_x(count)
        )
    }
}
