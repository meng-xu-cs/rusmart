use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

fn fatal<S: AsRef<str>>(msg: S) -> TokenStream {
    [
        TokenTree::Ident(Ident::new("compile_error", Span::mixed_site())),
        TokenTree::Punct(Punct::new('!', Spacing::Alone)),
        TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            [TokenTree::Literal(Literal::string(msg.as_ref()))]
                .into_iter()
                .collect(),
        )),
    ]
    .into_iter()
    .collect()
}

/// Annotated over a Rust type to mark it as a type definition
#[proc_macro_attribute]
pub fn smt_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    // sanity check
    if !attr.is_empty() {
        return fatal("attribute does not take arguments");
    }

    // return the extended stream to continue compilation
    item
}
