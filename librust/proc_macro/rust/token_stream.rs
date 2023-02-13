//! Public implementation details for the `TokenStream` type, such as iterators.
use TokenStream;
use TokenTree;

/// An iterator over [`TokenStream`]'s [`TokenTree`]s.
#[derive(Clone)]
pub struct IntoIter {
    // Internal implementation details
}

impl Iterator for IntoIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        todo!("Implement this function")
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        todo!("Implement this function")
    }

    fn count(self) -> usize {
        todo!("Implement this function")
    }
}

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        todo!("Implement this function")
    }
}
