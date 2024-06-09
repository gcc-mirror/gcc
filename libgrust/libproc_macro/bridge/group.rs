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

use bridge::span::Span;
use bridge::token_stream::TokenStream;
use std::fmt;
use Delimiter;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Group {
    delimiter: Delimiter,
    stream: TokenStream,
    span: Span,
}

impl Group {
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
        Group {
            delimiter,
            stream,
            span: Span::default(),
        }
    }

    pub fn delimiter(&self) -> Delimiter {
        self.delimiter
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_span(&mut self, span: Span) {
        let _ = span;
    }

    pub fn stream(&self) -> TokenStream {
        self.stream.clone()
    }
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.delimiter {
            Delimiter::Parenthesis => f.write_str("(")?,
            Delimiter::Brace => f.write_str("{")?,
            Delimiter::Bracket => f.write_str("[")?,
            Delimiter::None => (),
        }

        self.stream.fmt(f)?;

        match self.delimiter {
            Delimiter::Parenthesis => f.write_str(")")?,
            Delimiter::Brace => f.write_str("}")?,
            Delimiter::Bracket => f.write_str("]")?,
            Delimiter::None => (),
        }

        Ok(())
    }
}
