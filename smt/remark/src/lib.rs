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
#[proc_macro_attribute]
pub fn smt_type(attr: Syntax, item: Syntax) -> Syntax {
    fail_if_error!(ty::derive_for_type(attr, item))
}

/// Annotation over a Rust function
#[proc_macro_attribute]
pub fn smt_impl(attr: Syntax, item: Syntax) -> Syntax {
    fail_if_error!(func::derive_for_impl(attr, item))
}

/// Annotation over a Rust function
#[proc_macro_attribute]
pub fn smt_spec(attr: Syntax, item: Syntax) -> Syntax {
    fail_if_error!(func::derive_for_spec(attr, item))
}

/// Annotation over a Rust const
#[proc_macro_attribute]
pub fn smt_axiom(attr: Syntax, item: Syntax) -> Syntax {
    // check attributes
    let attr = TokenStream::from(attr);
    if !attr.is_empty() {
        fail_on!(attr, "unexpected");
    }

    // produce the output
    let output = item.clone();

    // ensure that the underlying item is a type
    let target = parse_macro_input!(item as Item);
    if !matches!(target, Item::Fn(_)) {
        fail_on!(target, "expect fn");
    }

    // do nothing with the axiom definition
    output
}
