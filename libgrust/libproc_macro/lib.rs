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

pub use group::{Delimiter, Group};
pub use ident::Ident;
pub use literal::Literal;
pub use punct::{Punct, Spacing};
pub use span::Span;
use std::error;
use std::{fmt, iter, str::FromStr};

mod bridge;
mod group;
mod ident;
mod literal;
mod punct;
mod span;
pub mod token_stream;

/// Determines whether proc_macro has been made accessible to the currently
/// running program.
///
/// # Note
///
/// This function provide a non panicking way to detect whether the API is
/// invoked from inside of a procedural macro.
pub fn is_available() -> bool {
    bridge::is_available()
}

/// A single token or a delimited sequence of token trees.
#[derive(Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

type InternalTokenTree = bridge::token_stream::TokenTree;

impl From<InternalTokenTree> for TokenTree {
    fn from(value: InternalTokenTree) -> Self {
        match value {
            InternalTokenTree::Group(g) => TokenTree::Group(Group(g)),
            InternalTokenTree::Ident(i) => TokenTree::Ident(Ident(i)),
            InternalTokenTree::Punct(p) => TokenTree::Punct(Punct(p)),
            InternalTokenTree::Literal(l) => TokenTree::Literal(Literal(l)),
        }
    }
}

impl TokenTree {
    /// Get the [`Span`] for this TokenTree.
    pub fn span(&self) -> Span {
        match self {
            TokenTree::Group(group) => group.span(),
            TokenTree::Ident(ident) => ident.span(),
            TokenTree::Punct(punct) => punct.span(),
            TokenTree::Literal(literal) => literal.span(),
        }
    }

    /// Set the span for this TokenTree.
    ///
    /// # Arguments
    ///
    /// * `span` - The new span value.
    pub fn set_span(&mut self, span: Span) {
        match self {
            TokenTree::Group(group) => group.set_span(span),
            TokenTree::Ident(ident) => ident.set_span(span),
            TokenTree::Punct(punct) => punct.set_span(span),
            TokenTree::Literal(literal) => literal.set_span(span),
        }
    }
}

impl fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Group(group) => group.fmt(f),
            TokenTree::Ident(ident) => ident.fmt(f),
            TokenTree::Punct(punct) => punct.fmt(f),
            TokenTree::Literal(literal) => literal.fmt(f),
        }
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Group(group) => group.fmt(f),
            TokenTree::Ident(ident) => ident.fmt(f),
            TokenTree::Punct(punct) => punct.fmt(f),
            TokenTree::Literal(literal) => literal.fmt(f),
        }
    }
}

impl From<Group> for TokenTree {
    fn from(g: Group) -> Self {
        TokenTree::Group(g)
    }
}

impl From<Ident> for TokenTree {
    fn from(i: Ident) -> Self {
        TokenTree::Ident(i)
    }
}

impl From<Punct> for TokenTree {
    fn from(p: Punct) -> Self {
        TokenTree::Punct(p)
    }
}

impl From<Literal> for TokenTree {
    fn from(l: Literal) -> Self {
        TokenTree::Literal(l)
    }
}

/// Error returned from `from_str` functions.
#[derive(Debug)]
pub struct LexError;

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("cannot parse string into token stream")
    }
}

impl error::Error for LexError {}

/// An abstract sequence of token trees.
///
/// This type provides interfaces for iterating over those token trees. This
/// is both the input and the output of `#[proc_macro]`,
/// `#[proc_macro_attribute]` and `#[proc_macro_derive]` definitions.
#[derive(Clone)]
pub struct TokenStream(bridge::token_stream::TokenStream);

impl TokenStream {
    // TODO: Add experimental API functions for this type

    /// Creates an empty `TokenStream` containing no token trees.
    pub fn new() -> Self {
        TokenStream(bridge::token_stream::TokenStream::new())
    }

    /// Checks if this `TokenStream` is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<Self, LexError> {
        bridge::token_stream::TokenStream::from_str(src).map(TokenStream)
    }
}

impl iter::FromIterator<TokenTree> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenTree>>(trees: I) -> Self {
        TokenStream(bridge::token_stream::TokenStream::from_tree_iterator(trees))
    }
}

impl iter::FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
        TokenStream(bridge::token_stream::TokenStream::from_iterator(streams))
    }
}

impl Extend<TokenTree> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, trees: I) {
        self.0.extend(trees);
    }
}

impl Extend<TokenStream> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
        self.0.extend(streams)
    }
}
