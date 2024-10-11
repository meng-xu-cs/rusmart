use proc_macro::TokenStream as Syntax;
use proc_macro2::TokenStream;
use syn::{parse_macro_input, Item};

use rusmart_smt_remark::{fail_if_error, fail_on};
use rusmart_smt_remark::{func, ty};
/// Annotation over a Rust type
/// This annotation can only be applied on a Rust type definition i.e., struct and enum
/// Attributes will not be accepted. i.e., #[smt_type(<...attrs...>)] will cause an error
/// This will basically generate the code for deriving Clone, Debug, Hash, Default, Copy for the struct/enum
/// and also implement the SMT trait for the struct/enum
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
#[inline] // The #[inline] attribute in Rust is a compiler directive that suggests to the compiler that it should try to "inline" the function where it is used. Inlining a function means replacing a function call with the body of that function. Imagine you have a small function:
// fn add(a: i32, b: i32) -> i32 {
//     a + b
// }
// When you call add(2, 3) in your code, normally, the CPU needs to perform a function call operation, which means it jumps to the function's memory location, executes it, and then returns the result. Inlining, instead, copies the body of add directly into the call site, like let result = 2 + 3 instead of let result=add(3,2). This can be more efficient because it avoids the overhead of a function call. However, the compiler is free to ignore this if it believes inlining isn’t optimal (for example, if the function is very large or inlining would increase binary size too much). It has #[inline(always)] and #[inline(never)] variants as well, which force the compiler to always inline or never inline the function, respectively. For the most part, it's a good idea to trust the compiler's judgment for inlining unless you have a specific reason to use it.
pub fn smt_type(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(ty::derive_for_type(attr, item)) // no attributes allowed for smt_type
}

/// Annotation over a Rust function
/// This annotation can only be applied on a Rust function i.e., fn as top-level module item (in contrast to fn in an impl block)
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
#[inline]
pub fn smt_impl(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(func::derive_for_impl(attr, item))
}

/// Annotation over a Rust function
/// This annotation can only be applied on a Rust function i.e., fn as top-level module item (in contrast to fn in an impl block)
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
#[inline]
pub fn smt_spec(attr: Syntax, item: Syntax) -> Syntax {
    let attr: TokenStream = TokenStream::from(attr);
    let item: TokenStream = TokenStream::from(item);
    fail_if_error!(func::derive_for_spec(attr, item))
}

/// Annotation over a Rust const
#[proc_macro_attribute]
#[cfg(not(tarpaulin_include))]
#[inline]
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
