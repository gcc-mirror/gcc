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
use std::convert::TryInto;
use std::fmt;
use Span;

/// Describes the context of a [`Punct`] relatively to the next token.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Spacing {
    /// A [`Punct`] is not immediately followed by another `Punct`.
    Alone,
    /// A [`Punct`] is immediately followed by another `Punct` and can be
    /// combined into a multi-character operator.
    Joint,
}

/// Single punctuation character such as `+`, `-` or `#`.
///
/// Multi-character operators like `+=` are represented as two instances of
/// `Punct` with different forms of `Spacing` returned.
#[derive(Clone)]
pub struct Punct(pub(crate) bridge::punct::Punct);

impl Punct {
    /// Creates a new `Punct` from a given character and spacing.
    ///
    /// # Arguments
    ///
    /// * `ch` - The punctuation character.
    /// * `spacing` - The link between this character and the next one.
    ///
    /// # Panics
    ///
    /// This function will panic if the `ch` argument is not a valid
    /// punctuation character allowed by the language.
    pub fn new(ch: char, spacing: Spacing) -> Self {
        Punct(bridge::punct::Punct::new(ch, spacing))
    }

    /// Get the value for this punctuation character as `char`.
    pub fn as_char(&self) -> char {
        self.0
            .ch
            .try_into()
            .expect("Cannot convert from u32 to char")
    }

    /// Get the [`Spacing`] of this punctuation character, indicating whether
    /// the following character can be combined into a multi-character operator
    /// or not.
    pub fn spacing(&self) -> Spacing {
        self.0.spacing
    }

    /// Get the [`Span`] for this punctuation character.
    pub fn span(&self) -> Span {
        Span(self.0.span())
    }

    /// Set the span for this punctuation character.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        self.0.set_span(span.0);
    }
}

impl fmt::Display for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq<char> for Punct {
    fn eq(&self, rhs: &char) -> bool {
        self.as_char() == *rhs
    }
}

impl PartialEq<Punct> for char {
    fn eq(&self, rhs: &Punct) -> bool {
        *self == rhs.as_char()
    }
}
