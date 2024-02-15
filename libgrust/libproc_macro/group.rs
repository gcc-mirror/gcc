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
use Span;
use TokenStream;

/// Describes how a sequence of token trees is delimited.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// The sequence is delimited by a parenthesis `(...)`.
    Parenthesis,
    /// The sequence is delimited by a brace `{...}`.
    Brace,
    /// The sequence is delimited by a bracket `[...]`.
    Bracket,
    /// Invisible delimiter to preserve operator priority.
    None,
}

/// A delimited token stream.
#[derive(Clone)]
pub struct Group(pub(crate) bridge::group::Group);

impl Group {
    /// Creates a new `Group`.
    ///
    /// # Arguments
    ///
    /// * `delimiter` - The delimiter surrounding the inner [`TokenStream`].
    /// * `stream` - The tokenstream for this `Group`.
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
        Group(bridge::group::Group::new(delimiter, stream.0))
    }

    /// Get the delimiter of the `Group`.
    pub fn delimiter(&self) -> Delimiter {
        self.0.delimiter()
    }

    /// Get the stream of the `Group`.
    ///
    /// # Note
    ///
    /// The returned stream does not include the delimiters of this group.
    pub fn stream(&self) -> TokenStream {
        TokenStream(self.0.stream())
    }

    /// Get the span for the delimiters of this token stream, spanning the
    /// entire group.
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Get the span pointing to the opening delimiter of this `Group`.
    pub fn span_open(&self) -> Span {
        Span(self.0.span())
    }

    /// Get the span pointing to the closing delimiter of this `Group`.
    pub fn span_close(&self) -> Span {
        Span(self.0.span())
    }

    /// Change the span for this `Group`'s delimiters, but not its internal
    /// tokens.
    ///
    /// # Note
    ///
    /// This method will **not** set the span of all the internal tokens spanned
    /// by this group, but rather it will only set the span of the delimiter
    /// tokens at the level of the `Group`.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0)
    }
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
