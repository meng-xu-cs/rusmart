//! Error handling utilities
//! 
//! This `err` module provides five <macros by example> to handle errors in a more concise way:
//! - `fail_on!` to return a compiler error
//! - `fail_if_error!` to return a compiler error if an error is found
//! - `bail_on!` to return an Error instance
//! - `bail_if_exists!` to return an Error instance if a token exists
//! - `bail_if_missing!` to return an Error instance if a token is missing
//! 
//! The fail_on macro is equivalent to fail_if_error(bail_on(...)).
//! The macros will be accessible by `use rusmart_smt_remark::*` in or outside the crate.

/// Shortcut to return a compiler error
/// It has two patterns:
/// - `fail_on!(item (value), "message")` to return a compiler error with a literal message
/// - `fail_on!(item (value), "message {}", arg)` to return a compiler error with a formatted message
/// - The proc_macro::TokenStream is the input and return type of the top level procedural macros.
/// We know that "proc-macro is used at API level and proc-macro2 everywhere else" so the fail_on! macro will be used as the return type of procedural macros, defined in the lib.rs file of the smt-remark-derive crate.
// The into_compile_error method converts a syn::Error to proc_macro2::TokenStream.
// The proc_macro::TokenStream::from is used to convert the proc_macro2::TokenStream to proc_macro::TokenStream.
// Error has got a method named into_compile_error() which generates a compilation error from the error object.
// Syn uses spans to represent the location (line and column number) of the expression in the source where it was initially located. This is used mainly for error reporting. All structs (AST elements) implement Spanned. The span can be attached to errors so that the compile renders them on the offending lines.
#[macro_export]
macro_rules! fail_on {
    ($item:expr, $msg:literal $(,)?) => {
        return proc_macro::TokenStream::from(
            syn::Error::new_spanned($item, $msg).into_compile_error()
        )
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        return proc_macro::TokenStream::from(
            syn::Error::new_spanned($item, format!($fmt, $($arg)*)).into_compile_error()
        )
    };
}

// A declarative macro cannot be exported using #[macro_export] in a proc-macro crate (currently).
// Writing "pub use fail_on;" will not export a declarative macro and we need to use #[macro_export].
// procedural macros can only be exported from proc-macro crates (not declarative macros).
// So fail_on! as a declarative macro cannot be exported using #[macro_export] to be used from outside the crate. In other words, fail_on! can only be used inside the err.rs module.
// However, to make it accessible in other modules in the crate, we can write pub(crate) use fail_on;.
/// This line was previously needed to make the macro accessible in other modules in the crate.
/// It is no longer needed because the crate is no longer a proc-macro crate.
/// pub(crate) use fail_on;

/// Special case on fail: when an error happens
/// It has two patterns: 
/// - `fail_if_error!(Ok(proc_macro2::TokenStream::from(value)))` to return the value wrapped as token stream if it is Ok
/// - `fail_if_error!(Err(error))` to return the error as a compiler error
/// In the second case the `error` is of type syn::Error and the into_compile_error method converts a syn::Error to proc_macro2::TokenStream.
/// The proc_macro::TokenStream::from is used to convert the proc_macro2::TokenStream to proc_macro::TokenStream.
#[macro_export]
macro_rules! fail_if_error {
    ($item:expr) => {
        match $item {
            Ok(__v) => return proc_macro::TokenStream::from(__v),
            Err(__e) => return proc_macro::TokenStream::from(__e.into_compile_error()),
        }
    };
}
// pub(crate) use fail_if_error; not needed anymore

/// Shortcut to return a compiler error
/// It has two patterns:
/// - `bail_on!(item (value), "message")` to return an Error instance with a literal message
/// - `bail_on!(item (value), "message {}", arg)` to return an Error instance with a formatted message
/// This macro is similar to fail_on! but it returns an Error instance instead of a compiler error.
/// This error instance can be passed on to be used to generate a compiler error later.
#[macro_export]
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => {
        return Err(syn::Error::new_spanned($item, $msg))
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err(syn::Error::new_spanned($item, format!($fmt, $($arg)*)))
    };
}
// pub(crate) use bail_on; not needed anymore

/// Special case on bail: does not expect a token to exist
/// This macro is used to check if a token exists and if it does, it returns an error.
/// It has one pattern:
/// - `bail_if_exists!(item)` to return an Error instance if the item exists (is Some)
/// If the item is None, it does nothing.
/// If the item is Some, it returns Err(syn::Error::new_spanned(item, "unexpected")).
#[macro_export]
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => crate::bail_on!(__v, "unexpected"),
        }
    };
}
// pub(crate) use bail_if_exists; not needed anymore

/// Special case on bail: expects a token to exist
/// This macro is used to check if a token is missing and if it is, it returns an error.
/// It has one pattern:
/// - `bail_if_missing!(item, parent, "note")` to return an Error instance if the item is missing
/// If the item is Some, it returns the value.
/// If the item is None, it returns Err(syn::Error::new_spanned(parent, format!("expect {}", note))).
#[macro_export]
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => crate::bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}
// pub(crate) use bail_if_missing; not needed anymore


