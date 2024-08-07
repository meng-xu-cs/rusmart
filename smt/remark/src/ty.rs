use proc_macro::TokenStream as Syntax;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_quote, Field, FieldMutability, Fields, Item, ItemEnum, ItemStruct, Result, Variant,
};

use crate::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::generics::TypeParamGroup;

/// Derive for structs
fn derive_for_struct(item: &mut ItemStruct) -> Result<TokenStream> {
    let ItemStruct {
        attrs,
        vis: _,
        struct_token: _,
        ident: struct_name,
        generics,
        fields,
        semi_token: _,
    } = item;

    // derive attributes
    // NOTE: `Default` is derived for structs
    attrs.push(parse_quote!(
        #[derive(Debug, Clone, Copy, Default, Hash)]
    ));

    // derive implementation for
    // - `SMT::_cmp`
    let cmp_fn = match fields {
        Fields::Unit => bail_on!(item, "unexpected unit fields in struct"),
        Fields::Unnamed(defs) => {
            let mut cmp_exprs = vec![];
            for (i, field) in defs.unnamed.iter().enumerate() {
                let Field {
                    attrs: _,
                    vis: _,
                    mutability,
                    ident,
                    colon_token: _,
                    ty: _,
                } = field;

                // sanity checks
                if !matches!(mutability, FieldMutability::None) {
                    bail_on!(field, "unexpected field mutability declaration");
                }
                bail_if_exists!(ident);

                // derive the `SMT::_cmp` expressions
                let cmp_expr = quote! {
                    match self.#i._cmp(rhs.#i) {
                        std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
                        std::cmp::Ordering::Equal => (),
                        std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
                    };
                };

                // save for outer scopes
                cmp_exprs.push(cmp_expr);
            }

            // derive the `SMT::_cmp` function
            let cmp_fn = quote! {
                fn _cmp(self, rhs: Self) -> std::cmp::Ordering {
                    #(#cmp_exprs)*
                    std::cmp::Ordering::Equal
                }
            };
            cmp_fn
        }
        Fields::Named(defs) => {
            let mut cmp_exprs = vec![];
            for field in defs.named.iter() {
                let Field {
                    attrs: _,
                    vis: _,
                    mutability,
                    ident,
                    colon_token: _,
                    ty: _,
                } = field;

                // sanity checks
                if !matches!(mutability, FieldMutability::None) {
                    bail_on!(field, "unexpected field mutability declaration");
                }
                let name = bail_if_missing!(ident, field, "name");

                // derive the cmp operator
                let cmp_expr = quote! {
                    match self.#name._cmp(rhs.#name) {
                        std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
                        std::cmp::Ordering::Equal => (),
                        std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
                    };
                };

                // save for outer scopes
                cmp_exprs.push(cmp_expr);
            }

            let cmp_fn = quote! {
                fn _cmp(self, rhs: Self) -> std::cmp::Ordering {
                    #(#cmp_exprs)*
                    std::cmp::Ordering::Equal
                }
            };
            cmp_fn
        }
    };

    // check validity of generics
    let ty_params = TypeParamGroup::parse_generics(generics)?;

    // construct the extended stream
    let tokenized_impl_generics = ty_params.to_syntax_def();
    let tokenized_type_ty_insts = ty_params.to_syntax_use();

    let extended = quote! {
        impl #tokenized_impl_generics SMT for #struct_name #tokenized_type_ty_insts {
            #cmp_fn
        }
    };
    Ok(extended)
}

/// Derive for enums
fn derive_for_enum(item: &mut ItemEnum) -> Result<TokenStream> {
    let ItemEnum {
        attrs,
        vis: _,
        enum_token: _,
        ident: enum_name,
        generics,
        brace_token: _,
        variants,
    } = item;

    // derive attributes
    // NOTE: `Default` is NOT derived for enums
    attrs.push(parse_quote!(
        #[derive(Debug, Clone, Copy, Hash)]
    ));

    // derive implementation for
    // - `SMT::_cmp`

    // holds the match arms for the `SMT::_cmp` operator
    let mut cmp_arms = vec![];
    for (i, variant) in variants.iter().enumerate() {
        let Variant {
            attrs: _,
            ident: variant_name,
            fields,
            discriminant,
        } = variant;

        // sanity checks
        bail_if_exists!(discriminant.as_ref().map(|(_, e)| e));

        // match arms for comparing different variants
        let lhs_arm_skip = match fields {
            Fields::Unit => quote! {#enum_name::#variant_name},
            Fields::Unnamed(..) => quote! {#enum_name::#variant_name(..)},
            Fields::Named(..) => quote! {#enum_name::#variant_name{ .. }},
        };
        for (j, rhs_variant) in variants.iter().enumerate() {
            // handled separately
            if i == j {
                continue;
            }

            let rhs_variant_name = &rhs_variant.ident;
            let rhs_arm_skip = match rhs_variant.fields {
                Fields::Unit => quote! {#enum_name::#rhs_variant_name},
                Fields::Unnamed(..) => quote! {#enum_name::#rhs_variant_name(..)},
                Fields::Named(..) => quote! {#enum_name::#rhs_variant_name{ .. }},
            };

            let match_arm = if i < j {
                quote! {
                    (#lhs_arm_skip, #rhs_arm_skip) => return std::cmp::Ordering::Less,
                }
            } else {
                quote! {
                    (#lhs_arm_skip, #rhs_arm_skip) => return std::cmp::Ordering::Greater,
                }
            };
            cmp_arms.push(match_arm);
        }

        // match arm for comparing on the same variant
        let match_arm = match fields {
            Fields::Unit => {
                quote! {
                    (#enum_name::#variant_name, #enum_name::#variant_name) => (),
                }
            }
            Fields::Unnamed(defs) => {
                let mut cmp_lhs_vars = vec![];
                let mut cmp_rhs_vars = vec![];
                let mut cmp_exprs = vec![];

                for (f, field) in defs.unnamed.iter().enumerate() {
                    let Field {
                        attrs: _,
                        vis: _,
                        mutability,
                        ident,
                        colon_token: _,
                        ty: _,
                    } = field;

                    // sanity checks
                    if !matches!(mutability, FieldMutability::None) {
                        bail_on!(field, "unexpected field mutability declaration");
                    }
                    bail_if_exists!(ident);

                    // construct the expressions
                    let cmp_lhs_var = format_ident!("l{}", f);
                    let cmp_rhs_var = format_ident!("r{}", f);
                    let cmp_expr = quote! {
                        match #cmp_lhs_var._cmp(#cmp_rhs_var) {
                            std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
                            std::cmp::Ordering::Equal => (),
                            std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
                        };
                    };

                    cmp_lhs_vars.push(cmp_lhs_var);
                    cmp_rhs_vars.push(cmp_rhs_var);
                    cmp_exprs.push(cmp_expr);
                }

                quote! {
                    (
                        #enum_name::#variant_name(#(#cmp_lhs_vars),*),
                        #enum_name::#variant_name(#(#cmp_rhs_vars),*)
                    ) => {
                        #(#cmp_exprs)*
                    }
                }
            }
            Fields::Named(defs) => {
                let mut cmp_lhs_decls = vec![];
                let mut cmp_rhs_decls = vec![];
                let mut cmp_exprs = vec![];

                for field in defs.named.iter() {
                    let Field {
                        attrs: _,
                        vis: _,
                        mutability,
                        ident,
                        colon_token: _,
                        ty: _,
                    } = field;

                    // sanity checks
                    if !matches!(mutability, FieldMutability::None) {
                        bail_on!(field, "unexpected field mutability declaration");
                    }
                    let field_name = bail_if_missing!(ident, field, "name");

                    // construct the expressions
                    let cmp_lhs_var = format_ident!("l_{}", field_name);
                    let cmp_rhs_var = format_ident!("r_{}", field_name);
                    let cmp_expr = quote! {
                        match #cmp_lhs_var._cmp(#cmp_rhs_var) {
                            std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
                            std::cmp::Ordering::Equal => (),
                            std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
                        };
                    };

                    cmp_lhs_decls.push(quote! {#field_name: #cmp_lhs_var});
                    cmp_rhs_decls.push(quote! {#field_name: #cmp_rhs_var});
                    cmp_exprs.push(cmp_expr);
                }

                quote! {
                    (
                        #enum_name::#variant_name{ #(#cmp_lhs_decls),* },
                        #enum_name::#variant_name{ #(#cmp_rhs_decls),* }
                    ) => {
                        #(#cmp_exprs)*
                    }
                }
            }
        };
        cmp_arms.push(match_arm);
    }

    // derive the `SMT::_cmp` function
    let cmp_fn = quote! {
        fn _cmp(self, rhs: Self) -> std::cmp::Ordering {
            match (self, rhs) {
                #(#cmp_arms)*
            }
            std::cmp::Ordering::Equal
        }
    };

    // derive implementation for
    // - `Default::default`
    let variant = bail_if_missing!(variants.first(), item, "variants");
    let Variant {
        attrs: _,
        ident: variant_ident,
        fields,
        discriminant: _,
    } = variant;

    let default_fn = match fields {
        Fields::Unit => quote! {
            fn default() -> Self { Self::#variant_ident }
        },
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
                fn default() -> Self { Self::#variant_ident(#(#init),*) }
            }
        }
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
                fn default() -> Self { Self::#variant_ident { #(#init),* } }
            }
        }
    };

    // check validity of generics
    let ty_params = TypeParamGroup::parse_generics(generics)?;

    // construct the extended stream
    let tokenized_impl_generics = ty_params.to_syntax_def();
    let tokenized_type_ty_insts = ty_params.to_syntax_use();

    let extended = quote! {
        impl #tokenized_impl_generics Default for #enum_name #tokenized_type_ty_insts {
            #default_fn
        }

        impl #tokenized_impl_generics SMT for #enum_name #tokenized_type_ty_insts {
            #cmp_fn
        }
    };
    Ok(extended)
}

/// Derive for type annotations
pub fn derive_for_type(attr: Syntax, item: Syntax) -> Result<Syntax> {
    // check attributes
    let attr = TokenStream::from(attr);
    if !attr.is_empty() {
        bail_on!(attr, "unexpected");
    }

    // instrument necessary attributes and trait implementations
    let mut original = syn::parse::<Item>(item)?;
    let extended = match &mut original {
        Item::Struct(item_struct) => derive_for_struct(item_struct)?,
        Item::Enum(item_enum) => derive_for_enum(item_enum)?,
        t => bail_on!(t, "expect a type definition (i.e., struct or enum)"),
    };
    let combined = quote! {
        #original
        #extended
    };
    Ok(Syntax::from(combined))
}
