use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn smt_type(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[proc_macro_attribute]
pub fn smt_impl(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[proc_macro_attribute]
pub fn smt_spec(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
