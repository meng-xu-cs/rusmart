//! # Remark - The remark library crate for Rusmart.
//! 
//! The `rusmart_smt_remark` package contains one library crate. The module tree structure is as follows:
//! - attr, generics, err, func, ty
//! - func and ty are public modules and can be accessed from outside the crate.
//! - The modules are used in the `rusmart_smt_remark_derive` package.

/// module tree structure
mod attr;
mod generics;
mod err;
// `proc-macro` crate types currently cannot export any items other than functions tagged with `#[proc_macro]`, `#[proc_macro_derive]`, or `#[proc_macro_attribute]`.
// since this crate is no longer a `proc-macro` crate, we can export the func and ty modules.
pub mod func;
pub mod ty;