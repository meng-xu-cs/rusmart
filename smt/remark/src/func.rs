use proc_macro::TokenStream as Syntax;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{FnArg, ItemFn, Path, PathSegment, PatType, Result, Signature, Type, TypePath};

use crate::attr::{MetaValue, parse_dict};
use crate::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::generics::TypeParamGroup;

/// Derive for function annotations
fn check_and_derive(target: &ItemFn, method: Option<&Ident>) -> Result<Option<TokenStream>> {
    // unpack things
    let ItemFn {
        attrs: _,
        vis,
        sig,
        block: _,
    } = target;

    let Signature {
        constness,
        asyncness,
        unsafety,
        abi,
        fn_token: _,
        ident: func_name,
        generics,
        paren_token: _,
        inputs,
        variadic,
        output,
    } = sig;

    // should not appear
    bail_if_exists!(constness);
    bail_if_exists!(asyncness);
    bail_if_exists!(unsafety);
    bail_if_exists!(abi);
    bail_if_exists!(variadic);

    // extract the function generics
    let ty_params = TypeParamGroup::parse_generics(generics)?;

    // probe on the first parameter
    let param0 = match (inputs.first(), method) {
        (None, None) => return Ok(None),
        (None, Some(_)) => bail_on!(sig, "at least one parameter"),
        (Some(p), _) => p,
    };

    // extract the self type
    let (self_ty_name, self_ty_args) = match param0 {
        FnArg::Receiver(_) => bail_on!(param0, "expect type declaration"),
        FnArg::Typed(PatType {
            attrs: _,
            pat: _,
            colon_token: _,
            ty,
        }) => {
            // collect type arguments involved
            let self_ty_args = ty_params.collect_type_arguments(ty)?;

            // extract the name for the `self` type
            let self_ty_name = match method {
                None => {
                    // end of derivation
                    return Ok(None);
                }
                Some(_) => {
                    match ty.as_ref() {
                        // extract type that should be marked as self
                        Type::Path(TypePath {
                            qself: _,
                            path:
                                Path {
                                    leading_colon: _,
                                    segments,
                                },
                        }) => {
                            let mut iter = segments.iter();
                            let segment = bail_if_missing!(iter.next(), param0, "type name");
                            bail_if_exists!(iter.next());

                            let PathSegment {
                                ident,
                                arguments: _,
                            } = segment;
                            if self_ty_args.contains(ident) {
                                bail_on!(ident, "cannot derive a method for a type argument");
                            }

                            // done with the parsing
                            ident.clone()
                        }
                        _ => unreachable!(),
                    }
                }
            };
            (self_ty_name, self_ty_args)
        }
    };

    // derive the method generics
    let method_ty_params = ty_params.diff(&self_ty_args);

    // derive the generics tokens
    let tokenized_impl_generics = self_ty_args.to_syntax_def();
    let tokenized_method_generics = method_ty_params.to_syntax_def();

    let tokenized_self_ty_args = self_ty_args.to_syntax_use();
    let tokenized_func_ty_args = ty_params.to_syntax_invoke();

    // derive the parameter tokens
    let mut iter = inputs.iter();
    iter.next().unwrap();

    let mut method_params = vec![quote!(self)];
    let mut func_args = vec![quote!(self)];
    for arg in iter.by_ref() {
        match arg {
            FnArg::Receiver(_) => bail_on!(arg, "unexpect receiver"),
            FnArg::Typed(param) => {
                let pat = param.pat.as_ref();
                method_params.push(quote!(#param));
                func_args.push(quote!(#pat));
            }
        }
    }
    let tokenized_method_params = quote!(#(#method_params),*);
    let tokenized_func_args = quote!(#(#func_args),*);

    // construct the expanded stream
    let method = method.unwrap();
    let extended = quote! {
        impl #tokenized_impl_generics #self_ty_name #tokenized_self_ty_args {
            #vis fn #method #tokenized_method_generics (#tokenized_method_params) #output {
                #func_name #tokenized_func_ty_args(#tokenized_func_args)
            }
        }
    };
    Ok(Some(extended))
}

enum FnKind {
    Impl,
    Spec,
}

/// Derive for function annotations
fn derive_for_func(attr: Syntax, item: Syntax, kind: FnKind) -> Result<Syntax> {
    // check attributes
    let attr = TokenStream::from(attr);
    let mut dict = parse_dict(&attr)?;

    // ensure that the underlying item is a function
    let target = syn::parse::<ItemFn>(item.clone())?;

    // derive the method, if requested
    let derived = match dict.remove("method") {
        None => check_and_derive(&target, None)?,
        Some(MetaValue::One(ident)) => check_and_derive(&target, Some(&ident))?,
        Some(MetaValue::Set(_)) => bail_on!(attr, "invalid method attribute"),
    };

    // check for other attributes
    match kind {
        FnKind::Impl => dict.remove("specs"),
        FnKind::Spec => dict.remove("impls"),
    };
    if !dict.is_empty() {
        bail_on!(attr, "unknown attributes");
    }

    // consolidate the extended stream
    let output = match derived {
        None => item,
        Some(extended) => {
            let mut output = TokenStream::from(item);
            output.extend(extended);
            Syntax::from(output)
        }
    };
    Ok(output)
}

/// Derive for impl annotations
pub fn derive_for_impl(attr: Syntax, item: Syntax) -> Result<Syntax> {
    derive_for_func(attr, item, FnKind::Impl)
}

/// Derive for spec annotations
pub fn derive_for_spec(attr: Syntax, item: Syntax) -> Result<Syntax> {
    derive_for_func(attr, item, FnKind::Spec)
}
