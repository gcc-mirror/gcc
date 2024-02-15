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

//! Bridge internal span representation and functions
//!
//! # Note
//!
//! All methods accessing source location in rust are unstable, hence this
//! implementation with an empty structure.

/// # Note: Gcc does not have a span interner, a span will not contain an index
#[derive(Copy, Clone, Debug, Default)]
#[repr(C)]
pub struct Span {
    location: u32,
}

impl Span {
    pub fn call_site() -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn mixed_site() -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn resolved_at(&self, _other: Span) -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn located_at(&self, _other: Span) -> Self {
        // FIXME: implement this function properly
        Span::default()
    }

    pub fn source_text(&self) -> Option<String> {
        // FIXME: implement this function properly
        None
    }
}
