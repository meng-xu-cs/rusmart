/// Exit the parsing early with an error
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => {
        {
            let __x = $item;
            let __s = syn::spanned::Spanned::span(&__x);
            return Err(syn::Error::new(
                __s,
                format!("{}\n{}", $msg, quote::quote_spanned!(__s => #__x)),
            ))
        }
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        {
            let __x = $item;
            let __s = syn::spanned::Spanned::span(&__x);
            let __m = format!($fmt, $($arg)*);
            return Err(syn::Error::new(
                __s,
                format!("{}\n{}", __m, quote::quote_spanned!(__s => #__x)),
            ))
        }
    };
}
pub(crate) use bail_on;

/// Exit the parsing early with an error and a note
macro_rules! bail_on_with_note {
    ($loc:expr, $note:literal, $item:expr, $msg:literal $(,)?) => {
        return Err({
            let __x1 = $item;
            let __s1 = syn::spanned::Spanned::span($item);
            let __n1 = format!("{}\n{}", $msg, quote::quote_spanned!(__s1 => #__x1));

            let __x2 = $loc;
            let __s2 = syn::spanned::Spanned::span($loc);
            let __n2 = format!("{}\n{}", $note, quote::quote_spanned!(__s2 => #__x2));

            let mut __e = syn::Error::new(__s1, __n1);
            __e.combine(syn::Error::new(__s2, __n2));
            __e
        })
    };
    ($loc:expr, $note:literal, $item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err({
            let __x1 = $item;
            let __s1 = syn::spanned::Spanned::span($item);
            let __m1 = format!($fmt, $($arg)*);
            let __n1 = format!("{}\n{}", __m1, quote::quote_spanned!(__s1 => #__x1));

            let __x2 = $loc;
            let __s2 = syn::spanned::Spanned::span($loc);
            let __n2 = format!("{}\n{}", $note, quote::quote_spanned!(__s2 => #__x2));

            let mut __e = syn::Error::new(__s1, __n1);
            __e.combine(syn::Error::new(__s2, __n2));
            __e
        })
    };
}
pub(crate) use bail_on_with_note;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => $crate::parser::err::bail_on!(__v, "unexpected"),
        }
    };
}
pub(crate) use bail_if_exists;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_non_empty {
    ($item:expr) => {{
        let __v = $item;
        if !__v.is_empty() {
            $crate::parser::err::bail_on!(__v, "unexpected");
        }
    }};
}
pub(crate) use bail_if_non_empty;

/// Special case on bail: expect a token to exist
macro_rules! bail_if_empty {
    ($item:expr, $note:literal) => {{
        let __v = $item;
        if __v.is_empty() {
            $crate::parser::err::bail_on!(__v, "expect {}", $note);
        }
    }};
}
pub(crate) use bail_if_empty;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => $crate::parser::err::bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}
pub(crate) use bail_if_missing;
