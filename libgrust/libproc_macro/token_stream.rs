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

//! Public implementation details for the `TokenStream` type, such as iterators.
use bridge;
use std::convert::TryInto;

use TokenStream;
use TokenTree;

/// An iterator over [`TokenStream`]'s [`TokenTree`]s.
#[derive(Clone)]
pub struct IntoIter {
    current: *const bridge::token_stream::TokenTree,
    end: *const bridge::token_stream::TokenTree,
}

impl Iterator for IntoIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        if self.current == self.end {
            None
        } else {
            let result = self.current;
            self.current = unsafe { self.current.add(1) };
            Some(unsafe { std::ptr::read(result) }.into())
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // TODO: I'm not a fan of those casts, once #![feature(ptr_sub_ptr)]
        // is implemented we may replace this line by the following:
        // self.end.sub_ptr(self.current)
        let remaining = self.end as usize - self.current as usize;
        (remaining, Some(remaining))
    }

    fn count(self) -> usize {
        self.end as usize - self.current as usize
    }
}

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        let capacity = self.0.size.try_into().unwrap();
        IntoIter {
            current: self.0.data,
            end: unsafe { self.0.data.add(capacity) },
        }
    }
}
