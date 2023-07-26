use bridge::{ffistring::FFIString, group::Group, ident::Ident, literal::Literal, punct::Punct};
use std::convert::TryInto;
use std::fmt;
use std::slice;
use std::str::FromStr;
use LexError;

type ExternalTokenTree = crate::TokenTree;
type ExternalTokenStream = crate::TokenStream;

extern "C" {
    fn TokenStream__new() -> TokenStream;
    fn TokenStream__with_capacity(capacity: u64) -> TokenStream;
    fn TokenStream__push(stream: *mut TokenStream, tree: TokenTree);
    fn TokenStream__from_string(str: FFIString, ts: *mut TokenStream) -> bool;
    fn TokenStream__clone(ts: *const TokenStream) -> TokenStream;
    fn TokenStream__drop(stream: *mut TokenStream);
}

#[repr(C)]
#[derive(Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
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

impl From<ExternalTokenTree> for TokenTree {
    fn from(value: ExternalTokenTree) -> Self {
        match value {
            ExternalTokenTree::Group(g) => TokenTree::Group(g.0),
            ExternalTokenTree::Ident(i) => TokenTree::Ident(i.0),
            ExternalTokenTree::Punct(p) => TokenTree::Punct(p.0),
            ExternalTokenTree::Literal(l) => TokenTree::Literal(l.0),
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct TokenStream {
    pub(crate) data: *const TokenTree,
    pub(crate) size: u64,
    capacity: u64,
}

impl TokenStream {
    pub fn new() -> Self {
        unsafe { TokenStream__new() }
    }

    fn with_capacity(capacity: u64) -> Self {
        unsafe { TokenStream__with_capacity(capacity) }
    }

    fn push(&mut self, tree: TokenTree) {
        unsafe { TokenStream__push(self as *mut TokenStream, tree) }
    }

    pub fn is_empty(&self) -> bool {
        0 == self.size
    }

    pub fn from_iterator<I>(it: I) -> Self
    where
        I: IntoIterator<Item = ExternalTokenStream>,
    {
        let it = it.into_iter();
        let mut result = TokenStream::with_capacity(it.size_hint().0.try_into().unwrap());
        for stream in it {
            for item in stream.into_iter() {
                result.push(item.into());
            }
        }
        result
    }

    pub fn from_tree_iterator<I>(it: I) -> Self
    where
        I: IntoIterator<Item = ExternalTokenTree>,
    {
        let it = it.into_iter();
        let mut result = TokenStream::with_capacity(it.size_hint().0.try_into().unwrap());
        for item in it {
            result.push(item.into());
        }
        result
    }
}

impl Extend<ExternalTokenTree> for TokenStream {
    fn extend<I: IntoIterator<Item = ExternalTokenTree>>(&mut self, trees: I) {
        for tt in trees {
            self.push(tt.into())
        }
    }
}

impl Extend<ExternalTokenStream> for TokenStream {
    fn extend<I: IntoIterator<Item = ExternalTokenStream>>(&mut self, streams: I) {
        for stream in streams {
            for tt in stream {
                self.push(tt.into());
            }
        }
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in unsafe { slice::from_raw_parts(self.data, self.size.try_into().unwrap()) } {
            i.fmt(f)?;
            match i {
                TokenTree::Punct(_) => (),
                _ => f.write_str(" ")?,
            }
        }
        Ok(())
    }
}

impl FromStr for TokenStream {
    type Err = LexError;
    fn from_str(string: &str) -> Result<Self, LexError> {
        let mut ts = TokenStream::new();
        if unsafe { TokenStream__from_string(string.into(), &mut ts as *mut TokenStream) } {
            Err(LexError)
        } else {
            Ok(ts)
        }
    }
}

impl Clone for TokenStream {
    fn clone(&self) -> Self {
        unsafe { TokenStream__clone(self as *const TokenStream) }
    }
}

impl Drop for TokenStream {
    fn drop(&mut self) {
        unsafe { TokenStream__drop(self as *mut TokenStream) }
    }
}
