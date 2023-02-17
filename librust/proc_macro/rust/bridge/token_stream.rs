use bridge::{group::Group, ident::Ident, literal::Literal, punct::Punct};

// TODO: There surely is a better way to achieve this. I don't like this
// "duplication" of the TokenTree enumeration. But I cannot use the public
// one since it's underlying types (public Group, Ident...) are not ffi safe.
// Flattening all those struct might be a solution.
#[repr(C)]
#[derive(Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

extern "C" {
    fn TokenStream__new() -> TokenStream;
}

#[repr(C)]
#[derive(Clone)]
pub struct TokenStream {
    pub(crate) data: *const TokenTree,
    pub(crate) size: u64,
}

impl TokenStream {
    pub fn new() -> Self {
        unsafe { TokenStream__new() }
    }

    pub fn is_empty(&self) -> bool {
        0 == self.size
    }
}
