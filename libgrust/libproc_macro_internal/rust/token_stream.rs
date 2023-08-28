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
