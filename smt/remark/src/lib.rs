use proc_macro::token_stream::IntoIter;
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

/// Very limited set of tokens we care at this stage
enum Token {
    None,
    Comma,
    Ident,
}

impl Token {
    fn next(iter: &mut IntoIter) -> Option<Self> {
        let token = match iter.next() {
            None => Token::None,
            Some(TokenTree::Ident(_)) => Token::Ident,
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => Token::Comma,
            _ => return None,
        };
        Some(token)
    }

    /// Count number of identifiers in the stream
    fn count(stream: TokenStream) -> Option<usize> {
        let mut iter = stream.into_iter();
        let mut expect_ident = true;
        let mut count = 0;
        loop {
            match Self::next(&mut iter)? {
                Self::None => break,
                Self::Ident if expect_ident => {
                    count += 1;
                    expect_ident = false;
                }
                Self::Comma if !expect_ident => {
                    expect_ident = true;
                }
                _ => return None,
            }
        }
        Some(count)
    }
}

/// Annotated over a Rust function to mark it as a specification
#[proc_macro_attribute]
pub fn smt_spec(attr: TokenStream, item: TokenStream) -> TokenStream {
    // extract the impl targets
    if Token::count(attr).unwrap_or(0) == 0 {
        return fatal("invalid impl targets");
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
