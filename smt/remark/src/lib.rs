// module tree structure
mod attr;
mod err;
mod func;
mod generics;
mod ty;

use proc_macro::TokenStream as Syntax;
use proc_macro2::TokenStream;
use syn::{parse_macro_input, Item};

use crate::err::{fail_if_error, fail_on};

/// Annotation over a Rust type
/// This annotation can only be applied on a Rust type definition i.e., struct and enum
/// Attributes will not be accepted. i.e., #[smt_type(<...attrs...>)] will cause an error
/// This will basically generate the code for deriving Clone, Debug, Hash, Default, Copy for the struct/enum
/// and also implement the SMT trait for the struct/enum
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
pub fn smt_type(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(ty::derive_for_type(attr, item)) // no attributes allowed for smt_type
}

/// Annotation over a Rust function
/// This annotation can only be applied on a Rust function i.e., fn as top-level module item (in contrast to fn in an impl block)
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
pub fn smt_impl(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(func::derive_for_impl(attr, item))
}

/// Annotation over a Rust function
/// This annotation can only be applied on a Rust function i.e., fn as top-level module item (in contrast to fn in an impl block)
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
pub fn smt_spec(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(func::derive_for_spec(attr, item))
}

/// Annotation over a Rust const
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
pub fn smt_axiom(attr: Syntax, item: Syntax) -> Syntax {
    // check attributes
    let attr: TokenStream = TokenStream::from(attr);
    if !attr.is_empty() {
        fail_on!(attr, "unexpected"); // no attributes allowed for smt_axiom
    }

    // produce the output
    let output = item.clone();

    // ensure that the underlying item is a type
    let target: Item = parse_macro_input!(item as Item);
    if !matches!(target, Item::Fn(_)) { // only functions are allowed for smt_axiom
        fail_on!(target, "expect fn");
    }

    // do nothing with the axiom definition
    output
}
