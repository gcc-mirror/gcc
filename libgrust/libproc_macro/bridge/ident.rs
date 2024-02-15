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

use bridge::ffistring::FFIString;
use bridge::span::Span;
use std::fmt;

extern "C" {
    fn Ident__new(str: FFIString, span: Span) -> Ident;
    fn Ident__new_raw(str: FFIString, span: Span) -> Ident;
    fn Ident__drop(ident: *mut Ident);
    fn Ident__clone(ident: *const Ident) -> Ident;
}

#[repr(C)]
#[derive(Debug)]
pub struct Ident {
    pub(crate) is_raw: bool,
    value: FFIString,
    span: Span,
}

impl Ident {
    pub fn new(string: &str, span: Span) -> Self {
        unsafe { Ident__new(string.into(), span) }
    }

    pub fn new_raw(string: &str, span: Span) -> Self {
        unsafe { Ident__new_raw(string.into(), span) }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl Drop for Ident {
    fn drop(&mut self) {
        unsafe { Ident__drop(self as *mut Ident) }
    }
}

impl fmt::Display for Ident {
    /// Display as lossless converted string.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_raw {
            f.write_str("r#")?;
        }
        self.value.fmt(f)
    }
}

impl Clone for Ident {
    fn clone(&self) -> Self {
        unsafe { Ident__clone(self as *const Ident) }
    }
}
