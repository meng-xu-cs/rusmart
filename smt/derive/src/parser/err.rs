//! Error Handling Utilities
//!
//! This `err` module provides six <macros by example> to handle errors in a more concise way:
//! - `bail_on!` to exit the parsing early with an error
//! - `bail_on_with_note!` to exit the parsing early with an error and a note
//! - `bail_if_exists!` to handle the case where a token should not exist
//! - `bail_if_missing!` to handle the case where a token should exist
//! - `bail_if_non_empty!` to handle the case where a token should be empty
//! - `bail_if_empty!` to handle the case where a token should have at least one element
//!

// Note that some of these macros have been defined in the err.rs file in the smt/remark crate.
// Given that the rusmart-smt-remark crate is a proc-macro crate, it is not possible to export the declarative macros defined in the err.rs file.

/// Exit the parsing early with an error
/// It has two patterns:
/// - `bail_on!(item, "message")` to return an Error instance with a literal message
/// - `bail_on!(item, "message {}", arg)` to return an Error instance with a formatted message
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => { // item is anything that evaluates to a value. When pattern matched, the item is not evaluated. The pattern matching in macros is purely syntactic and happens at compile time without executing any code.
        { // This extra block is optional but it is a good practice to use it to avoid polluting the namespace. In case of defining a function inside the macro for example, it is necessary to use this block if we intend to use the macro multiple times in the same scope. Because the function will be defined multiple times in the same scope, which is not allowed in Rust; leading to an error. Note that Rust declarative macros are only hygienic in terms of local variables, loop labels, and crates. They are not hygienic in terms of functions, types, modules, etc. (proc-macros are always unhygienic).
            let __x = $item; // __x is the value of the item //? force evaluation of the item
            let __s = syn::spanned::Spanned::span(&__x); // __s is the span of the item. A span represents a region of source code, and it is used to provide context for diagnostics and other information.
            return Err(syn::Error::new( // create a new syn::Error with the given span, which will be used to signal an error that occurs at that specific location in the source code.
                __s,
                format!("{}\n{}", $msg, quote::quote_spanned!(__s => #__x)), // #__x is the item that caused the error (interpolated into the quote! macro). quote_spanned! is used to generate a string representation of the item that caused the error.
            ))
        }
    };

    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        {
            let __x = $item; // force evaluation of the item
            let __s = syn::spanned::Spanned::span(&__x); // get the span of the item (location in the source code)
            let __m = format!($fmt, $($arg)*); // format the message with the given arguments
            return Err(syn::Error::new(
                __s,
                format!("{}\n{}", __m, quote::quote_spanned!(__s => #__x)), // generate the error message with the formatted message and the item that caused the error
            ))
        }
    };
}
pub(crate) use bail_on;

/// Exit the parsing early with an error and a note
/// It has two patterns:
/// - `bail_on_with_note!(loc, "note", item, "message")` to return an Error instance with a literal message for the item and a note for the loc
/// - `bail_on_with_note!(loc, "note", item, "message {}", arg)` to return an Error instance with a formatted message for the item and a note for the loc
/// example: bail_on_with_note!(prev.name(), "previously defined here", item.name(), "duplicated axiom name");
macro_rules! bail_on_with_note {
    ($loc:expr, $note:literal, $item:expr, $msg:literal $(,)?) => {
        return Err({

            let __x1 = $item; // force eval of the item
            let __s1 = syn::spanned::Spanned::span(&__x1);
            let __n1 = format!("{}\n{}", $msg, quote::quote_spanned!(__s1 => #__x1));

            let __x2 = $loc; // force eval of the loc
            let __s2 = syn::spanned::Spanned::span(&__x2);
            let __n2 = format!("{}\n{}", $note, quote::quote_spanned!(__s2 => #__x2));

            let mut __e = syn::Error::new(__s1, __n1);
            __e.combine(syn::Error::new(__s2, __n2));
            __e
        })
    };
    ($loc:expr, $note:literal, $item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err({
            let __x1 = $item;
            let __s1 = syn::spanned::Spanned::span(&__x1);
            let __m1 = format!($fmt, $($arg)*);
            let __n1 = format!("{}\n{}", __m1, quote::quote_spanned!(__s1 => #__x1));

            let __x2 = $loc;
            let __s2 = syn::spanned::Spanned::span(&__x2);
            let __n2 = format!("{}\n{}", $note, quote::quote_spanned!(__s2 => #__x2));

            let mut __e = syn::Error::new(__s1, __n1);
            __e.combine(syn::Error::new(__s2, __n2));
            __e
        })
    };
}
pub(crate) use bail_on_with_note;

/// Special case on bail: does not expect a token to exist
/// This macro is used to check if a token exists and if it does, it returns an error.
/// It has one pattern:
/// - `bail_if_exists!(item)` to return an Error instance if the item exists (is Some)
/// If the item is None, it does nothing.
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => $crate::parser::err::bail_on!(__v, "unexpected"),
        }
    };
}
pub(crate) use bail_if_exists;

/// Special case on bail: expects a token to exist
/// This macro is used to check if a token is missing and if it is, it returns an error.
/// It has one pattern:
/// - `bail_if_missing!(item, parent, "note")` to return an Error instance if the item is missing
/// If the item is Some, it returns the value.
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => $crate::parser::err::bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}
pub(crate) use bail_if_missing;

/// Special case on bail: expect a token to be empty
/// This macro is used to check if a token is empty and if it is not, it returns an error.
/// It has one pattern:
/// - `bail_if_non_empty!(item)` to return an Error instance if the item is not empty
/// is_empty() is a method that checks if the length of the item is zero. The item can be a string, a vector, or any other type that implements the is_empty() method.
/// If the item is not empty, it returns Err(syn::Error::new_spanned(span_of_item, "unexpected \n {item}")).
macro_rules! bail_if_non_empty {
    ($item:expr) => {{
        let __v = $item;
        if !__v.is_empty() {
            $crate::parser::err::bail_on!(__v, "unexpected");
        }
    }};
}
pub(crate) use bail_if_non_empty;

/// Special case on bail: expect a token to be non-empty
/// This macro is used to check if a token is non-empty and if it is empty, it returns an error.
/// It has one pattern:
/// - `bail_if_empty!(item, parent, "note")` to return an Error instance if the item is empty
/// If the item is empty, it returns Err(syn::Error::new_spanned(span_of_parent, "expect {note} \n {parent}")).
/// If the item is not empty, it does nothing.
macro_rules! bail_if_empty {
    ($item:expr, $parent:expr, $note:literal) => {{
        if $item.is_empty() {
            $crate::parser::err::bail_on!($parent, "expect {}", $note);
        }
    }};
}
pub(crate) use bail_if_empty;
