/// Shortcut to return a compiler error
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
pub(crate) use fail_on;

/// Special case on fail: when an error happens
macro_rules! fail_if_error {
    ($item:expr) => {
        match $item {
            Ok(__v) => __v,
            Err(__e) => return proc_macro::TokenStream::from(__e.into_compile_error()),
        }
    };
}
pub(crate) use fail_if_error;

/// Shortcut to return a compiler error
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => {
        return Err(syn::Error::new_spanned($item, $msg))
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err(syn::Error::new_spanned($item, format!($fmt, $($arg)*)))
    };
}
pub(crate) use bail_on;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => crate::err::bail_on!(__v, "unexpected"),
        }
    };
}
pub(crate) use bail_if_exists;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => crate::err::bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}
pub(crate) use bail_if_missing;
