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

use std::convert::TryInto;
use std::ffi::c_uchar;
use std::fmt;
use std::slice::from_raw_parts;
use std::str::from_utf8;

extern "C" {
    fn FFIString__new(data: *const c_uchar, len: u64) -> FFIString;
    fn FFIString__drop(string: *mut FFIString);
}

#[repr(C)]
#[derive(Debug)]
pub struct FFIString {
    data: *const c_uchar,
    len: u64,
}

impl<S> From<S> for FFIString
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        unsafe { FFIString__new(s.as_ref().as_ptr(), s.as_ref().len() as u64) }
    }
}

impl Clone for FFIString {
    fn clone(&self) -> Self {
        FFIString::from(&self.to_string())
    }
}

impl Drop for FFIString {
    fn drop(&mut self) {
        unsafe {
            FFIString__drop(self as *mut FFIString);
        }
    }
}

impl fmt::Display for FFIString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            from_utf8(unsafe {
                from_raw_parts(self.data, self.len.try_into().map_err(|_| fmt::Error)?)
            })
            .map_err(|_| fmt::Error)?,
        )
    }
}
