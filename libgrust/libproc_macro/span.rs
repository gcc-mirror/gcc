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

/// A region of source code along with macro expansion information.
#[derive(Copy, Clone)]
pub struct Span(pub(crate) bridge::span::Span);

impl Span {
    // TODO: Add experimental API functions for this type

    /// Creates a new span that resolves at the macro call location.
    pub fn call_site() -> Self {
        Span(bridge::span::Span::call_site())
    }

    /// Creates a new span that resolved sometimes at macro call site, and
    /// sometimes at macro definition site.
    pub fn mixed_site() -> Self {
        Span(bridge::span::Span::mixed_site())
    }

    /// Creates a new span with the same line/column informations but that
    /// resolve symbols as though it were at `other`.
    ///
    /// # Arguments
    ///
    /// * `other` - Other span to resolve at.
    pub fn resolved_at(&self, other: Span) -> Self {
        Span(self.0.resolved_at(other.0))
    }

    /// Creates a new span with the same name resolution behavior as self, but
    /// with the line/column information of `other`.
    ///
    /// # Arguments
    ///
    /// * `other` - Other span containing the line/column informations to use.
    pub fn located_at(&self, other: Span) -> Self {
        Span(self.0.located_at(other.0))
    }

    /// Return the source text behind a span.
    pub fn source_text(&self) -> Option<String> {
        self.0.source_text()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
