// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU Proc Macro Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

use bridge;
use std::fmt;
use std::str::FromStr;
use LexError;
use Span;

/// A type representing a literal value except `true` and `false`.
///
/// This could be one of the following:
/// * literal string (`"hello"`)
/// * byte string (`b"hello"`)
/// * character (`'a'`)
/// * byte character (`b'a'`)
/// * unsuffixed integer (`42`)
/// * suffixed integer (`42u8`)
/// * unsuffixed floating point number (`1.618`)
/// * suffixed floating point number (`1.618f32`)
///
/// # Note
///
/// Boolean literals like `true` and `false` are `Ident`s and do not belong
/// here.
#[derive(Clone)]
pub struct Literal(pub(crate) bridge::literal::Literal);

impl Literal {
    // TODO: Add experimental API functions for this type
    // TODO: Generate those constructor with 1/2 macros instead

    pub fn u8_suffixed(n: u8) -> Self {
        Literal(bridge::literal::Literal::u8_suffixed(n))
    }

    pub fn u16_suffixed(n: u16) -> Self {
        Literal(bridge::literal::Literal::u16_suffixed(n))
    }

    pub fn u32_suffixed(n: u32) -> Self {
        Literal(bridge::literal::Literal::u32_suffixed(n))
    }

    pub fn u64_suffixed(n: u64) -> Self {
        Literal(bridge::literal::Literal::u64_suffixed(n))
    }

    pub fn u128_suffixed(n: u128) -> Self {
        Literal(bridge::literal::Literal::u128_suffixed(n))
    }

    pub fn usize_suffixed(n: usize) -> Self {
        Literal(bridge::literal::Literal::usize_suffixed(n))
    }

    pub fn i8_suffixed(n: i8) -> Self {
        Literal(bridge::literal::Literal::i8_suffixed(n))
    }

    pub fn i16_suffixed(n: i16) -> Self {
        Literal(bridge::literal::Literal::i16_suffixed(n))
    }

    pub fn i32_suffixed(n: i32) -> Self {
        Literal(bridge::literal::Literal::i32_suffixed(n))
    }

    pub fn i64_suffixed(n: i64) -> Self {
        Literal(bridge::literal::Literal::i64_suffixed(n))
    }

    pub fn i128_suffixed(n: i128) -> Self {
        Literal(bridge::literal::Literal::i128_suffixed(n))
    }

    pub fn isize_suffixed(n: isize) -> Self {
        Literal(bridge::literal::Literal::isize_suffixed(n))
    }

    // Unsuffixed

    pub fn u8_unsuffixed(n: u8) -> Self {
        Literal(bridge::literal::Literal::u8_unsuffixed(n))
    }

    pub fn u16_unsuffixed(n: u16) -> Self {
        Literal(bridge::literal::Literal::u16_unsuffixed(n))
    }

    pub fn u32_unsuffixed(n: u32) -> Self {
        Literal(bridge::literal::Literal::u32_unsuffixed(n))
    }

    pub fn u64_unsuffixed(n: u64) -> Self {
        Literal(bridge::literal::Literal::u64_unsuffixed(n))
    }

    pub fn u128_unsuffixed(n: u128) -> Self {
        Literal(bridge::literal::Literal::u128_unsuffixed(n))
    }

    pub fn usize_unsuffixed(n: usize) -> Self {
        Literal(bridge::literal::Literal::usize_unsuffixed(n))
    }

    pub fn i8_unsuffixed(n: i8) -> Self {
        Literal(bridge::literal::Literal::i8_unsuffixed(n))
    }

    pub fn i16_unsuffixed(n: i16) -> Self {
        Literal(bridge::literal::Literal::i16_unsuffixed(n))
    }

    pub fn i32_unsuffixed(n: i32) -> Self {
        Literal(bridge::literal::Literal::i32_unsuffixed(n))
    }

    pub fn i64_unsuffixed(n: i64) -> Self {
        Literal(bridge::literal::Literal::i64_unsuffixed(n))
    }

    pub fn i128_unsuffixed(n: i128) -> Self {
        Literal(bridge::literal::Literal::i128_unsuffixed(n))
    }

    pub fn isize_unsuffixed(n: isize) -> Self {
        Literal(bridge::literal::Literal::isize_unsuffixed(n))
    }

    pub fn f32_unsuffixed(n: f32) -> Self {
        Literal(bridge::literal::Literal::f32_unsuffixed(n))
    }

    pub fn f32_suffixed(n: f32) -> Self {
        Literal(bridge::literal::Literal::f32_suffixed(n))
    }

    pub fn f64_unsuffixed(n: f64) -> Self {
        Literal(bridge::literal::Literal::f64_unsuffixed(n))
    }

    pub fn f64_suffixed(n: f64) -> Self {
        Literal(bridge::literal::Literal::f64_suffixed(n))
    }

    pub fn string(string: &str) -> Self {
        Literal(bridge::literal::Literal::string(string))
    }

    pub fn character(c: char) -> Self {
        Literal(bridge::literal::Literal::character(c))
    }

    pub fn byte_string(bytes: &[u8]) -> Self {
        Literal(bridge::literal::Literal::byte_string(bytes))
    }

    /// Get the [`Span`] for this literal.
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Set the span for this literal.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0);
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for Literal {
    type Err = LexError;

    fn from_str(src: &str) -> Result<Self, LexError> {
        bridge::literal::Literal::from_str(src).map(Literal)
    }
}
