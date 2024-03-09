use proc_macro::TokenStream as Syntax;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, Fields, Item, Result, Variant};

use crate::err::{bail_if_exists, bail_on};
use crate::generics::TypeParamGroup;

/// Derive for type annotations
pub fn derive_for_type(attr: Syntax, item: Syntax) -> Result<Syntax> {
    // check attributes
    let attr = TokenStream::from(attr);
    if !attr.is_empty() {
        bail_on!(attr, "unexpected");
    }

    // instrument necessary attributes
    let mut target = syn::parse::<Item>(item)?;
    let impl_default = match &mut target {
        Item::Struct(item_struct) => {
            // derivation of `Default`
            item_struct.attrs.push(parse_quote!(
                #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
            ));
            None
        }
        Item::Enum(item_enum) => {
            // no derivation of `Default`
            item_enum.attrs.push(parse_quote!(
                #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
            ));

            // construct manual `Default` implementation
            match item_enum.variants.first() {
                None => bail_on!(item_enum, "expect at least one variant"),
                Some(variant) => {
                    let Variant {
                        attrs: _,
                        ident,
                        fields,
                        discriminant,
                    } = variant;

                    bail_if_exists!(discriminant.as_ref().map(|(_, e)| e));
                    let default_impl = match fields {
                        Fields::Unit => quote! {
                            fn default() -> Self { Self::#ident }
                        },
                        Fields::Named(fields_named) => {
                            let init: Vec<_> = fields_named
                                .named
                                .iter()
                                .map(|f| {
                                    let field_name = f.ident.as_ref().expect("named field");
                                    let field_type = &f.ty;
                                    quote!(#field_name: #field_type::default())
                                })
                                .collect();
                            quote! {
                                fn default() -> Self { Self::#ident { #(#init),* } }
                            }
                        }
                        Fields::Unnamed(fields_unnamed) => {
                            let init: Vec<_> = fields_unnamed
                                .unnamed
                                .iter()
                                .map(|f| {
                                    let field_type = &f.ty;
                                    quote!(#field_type::default())
                                })
                                .collect();
                            quote! {
                                fn default() -> Self { Self::#ident(#(#init),*) }
                            }
                        }
                    };
                    Some(default_impl)
                }
            }
        }
        t => bail_on!(t, "expect type"),
    };

    // exact type name and generics
    let (ident, generics) = match &target {
        Item::Struct(item_struct) => (&item_struct.ident, &item_struct.generics),
        Item::Enum(item_enum) => (&item_enum.ident, &item_enum.generics),
        _ => unreachable!(),
    };

    // check validity of generics
    let ty_params = TypeParamGroup::parse_generics(generics)?;

    // construct the extended stream
    let tokenized_impl_generics = ty_params.to_syntax_def();
    let tokenized_type_ty_args = ty_params.to_syntax_use();

    let combined = match impl_default {
        None => quote! {
            #target
            impl #tokenized_impl_generics SMT for #ident #tokenized_type_ty_args {}
        },
        Some(default) => quote! {
            #target
            impl #tokenized_impl_generics Default for #ident #tokenized_type_ty_args {
                #default
            }
            impl #tokenized_impl_generics SMT for #ident #tokenized_type_ty_args {}
        },
    };
    Ok(Syntax::from(combined))
}
