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

/// An identifier.
#[derive(Clone)]
pub struct Ident(pub(crate) bridge::ident::Ident);

impl Ident {
    /// Creates a new identifier.
    ///
    /// # Arguments
    ///
    /// * `string` - A valid identifier.
    /// * `span` - The span of the identifier.
    ///
    /// # Panics
    ///
    /// The `string` argument must be a valid identifier permitted by the
    /// language, otherwise the function will panic.
    pub fn new(string: &str, span: Span) -> Self {
        Ident(bridge::ident::Ident::new(string, span.0))
    }

    /// Creates a new raw identifier.
    ///
    /// # Arguments
    ///
    /// * `string` - A valid identifier.
    /// * `span` - The span of the identifier.
    ///
    /// # Panics
    ///
    /// The `string` argument must be a valid identifier permitted by the
    /// language. Furthermore, it should not be a keyword used in path
    /// segments, otherwise this function will panic.
    pub fn new_raw(string: &str, span: Span) -> Self {
        Ident(bridge::ident::Ident::new_raw(string, span.0))
    }

    /// Return the span of the identifier
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Change the span of the identifier.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0);
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
