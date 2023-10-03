use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use syn::{parse_macro_input, Item};

/// Return a compile error to the user marking the error
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
        TokenTree::Punct(Punct::new(';', Spacing::Alone)),
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

    // ensure the following stream is on a type
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Struct(_) | Item::Enum(_)) {
        return fatal("expect a struct or enum");
    }

    // return the original stream
    item
}

/// Annotated over a Rust function to mark it as an implementation
#[proc_macro_attribute]
pub fn smt_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    // sanity check
    if !attr.is_empty() {
        return fatal("attribute does not take arguments");
    }

    // ensure the following stream is on a function
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Fn(_)) {
        return fatal("expect a function");
    }

    // return the original stream
    item
}

/// Annotated over a Rust function to mark it as a specification
#[proc_macro_attribute]
pub fn smt_spec(attr: TokenStream, item: TokenStream) -> TokenStream {
    // extract the impl target
    let mut iter = attr.into_iter();
    match iter.next() {
        None => {
            return fatal("unable to find impl target");
        }
        Some(TokenTree::Ident(_)) => (),
        Some(_) => {
            return fatal("impl target is not an ident");
        }
    }
    if iter.next().is_some() {
        return fatal("extra tokens in the impl target");
    }

    // ensure the following stream is on a function
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Fn(_)) {
        return fatal("expect a function");
    }

    // return the original stream
    item
}
