#[cfg(test)]
/// Utility rules of making unit tests
macro_rules! unit_test {
    ($name:ident, $stream:tt) => {
        #[test]
        fn $name() {
            let code = quote::quote! $stream;
            crate::parser::ctxt::Context::derive_from_stream(code).unwrap();
        }
    };
    ($name:ident, $stream:tt, $msg:expr) => {
        #[test]
        fn $name() {
            let code = quote::quote! $stream;
            match crate::parser::ctxt::Context::derive_from_stream(code) {
                Ok(_) => panic!("expect failure"),
                Err(e) => {
                    let err = e.to_string();
                    let exp = $msg;
                    if !err.contains(exp) {
                        panic!("\n==== expect ====\n{}\n==== actual ====\n{}", exp, err)
                    }
                }
            };
        }
    };
}

#[cfg(test)]
pub(crate) use unit_test;
