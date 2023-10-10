use proc_macro::token_stream::IntoIter;
use proc_macro::{TokenStream, TokenTree};
use std::env;
use std::path::Path;

use syn::{parse_macro_input, Item, Result};

mod err;

mod parse_ctxt;
mod parse_expr;
mod parse_expr_intrinsic;
mod parse_expr_match;
mod parse_expr_method;
mod parse_func;
mod parse_path;
mod parse_type;

/// utility macro to short-cut the parsing
macro_rules! fatal {
    ($stream:ident, $message:literal) => {
        return proc_macro::TokenStream::from(
            syn::Error::new_spanned(proc_macro2::TokenStream::from($stream), $message)
                .into_compile_error(),
        )
    };
}

/// Annotated over a Rust type to mark it as a type definition
#[proc_macro_attribute]
pub fn smt_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    // sanity check
    if !attr.is_empty() {
        fatal!(attr, "attribute does not take arguments");
    }

    // ensure the following stream is on a type
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Struct(_) | Item::Enum(_)) {
        fatal!(item, "expect a struct or enum");
    }

    // return the original stream
    item
}

/// Annotated over a Rust function to mark it as an implementation
#[proc_macro_attribute]
pub fn smt_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    // sanity check
    if !attr.is_empty() {
        fatal!(attr, "attribute does not take arguments");
    }

    // ensure the following stream is on a function
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Fn(_)) {
        fatal!(item, "expect a function");
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
    if Token::count(attr.clone()).unwrap_or(0) == 0 {
        fatal!(attr, "invalid impl targets");
    }

    // ensure the following stream is on a function
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Fn(_)) {
        fatal!(item, "expect a function");
    }

    // return the original stream
    item
}

/// Internal entrypoint for ergonomic error propagation
fn derive_internal<P: AsRef<Path>>(input: P) -> Result<()> {
    let path_crate = input.as_ref();
    rusmart_utils::config::initialize();
    log::debug!("deriving for crate {}", path_crate.to_string_lossy());
    parse_ctxt::Context::new(path_crate)?
        .analyze_type()?
        .analyze_func_sig()?
        .analyze_func_body()?;
    log::debug!("derivation completed");
    Ok(())
}

/// Annotated over a module as the entrypoint to the derivation process
#[proc_macro_attribute]
pub fn smt_main(attr: TokenStream, item: TokenStream) -> TokenStream {
    // sanity check
    if !attr.is_empty() {
        fatal!(attr, "attribute does not take arguments");
    }

    // ensure the following stream is on a module
    let input = item.clone();
    let parsed = parse_macro_input!(input as Item);
    if !matches!(parsed, Item::Mod(_)) {
        fatal!(item, "expect a module");
    }

    // run the derivation
    match env::var_os("CARGO_MANIFEST_DIR") {
        None => fatal!(item, "CARGO_MANIFEST_DIR not found in environment"),
        Some(val) => match derive_internal(val) {
            Ok(()) => TokenStream::new(),
            Err(e) => TokenStream::from(e.into_compile_error()),
        },
    }
}
