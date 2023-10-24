use proc_macro::TokenStream as Syntax;
use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, FnArg, GenericArgument, GenericParam,
    Generics, Item, ItemFn, PatType, Path, PathArguments, PathSegment, Result, Signature,
    TraitBound, TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath,
};

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

/// Special case on fail: when an error happens
macro_rules! fail_if_error {
    ($item:expr) => {
        match $item {
            Ok(__v) => __v,
            Err(__e) => return proc_macro::TokenStream::from(__e.into_compile_error()),
        }
    };
}

/// Shortcut to return a compiler error
macro_rules! bail_on {
    ($item:expr, $msg:literal $(,)?) => {
        return Err(syn::Error::new_spanned($item, $msg))
    };
    ($item:expr, $fmt:expr, $($arg:tt)*) => {
        return Err(syn::Error::new_spanned($item, format!($fmt, $($arg)*)))
    };
}

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_exists {
    ($item:expr) => {
        match $item {
            None => (),
            Some(__v) => bail_on!(__v, "unexpected"),
        }
    };
}
pub(crate) use bail_if_exists;

/// Special case on bail: does not expect a token to exist
macro_rules! bail_if_missing {
    ($item:expr, $par:expr, $note:literal) => {
        match $item {
            None => bail_on!($par, "expect {}", $note),
            Some(__v) => __v,
        }
    };
}

/// Annotation value
enum MetaValue {
    One(Ident),
    Set(BTreeSet<Ident>),
}

/// Parse a key-value mapping from a token stream
fn parse_dict(stream: &TokenStream) -> Result<BTreeMap<String, MetaValue>> {
    let mut store = BTreeMap::new();

    let mut iter = stream.clone().into_iter();
    let mut cursor = iter.next();

    while cursor.is_some() {
        // extract key
        let token = bail_if_missing!(cursor.as_ref(), stream, "key");
        let key = match token {
            TokenTree::Ident(ident) => ident.to_string(),
            _ => bail_on!(token, "not a key"),
        };
        if store.contains_key(&key) {
            bail_on!(token, "duplicated key");
        }

        // equal sign
        let token = bail_if_missing!(iter.next(), stream, "=");
        match &token {
            TokenTree::Punct(punct) if punct.as_char() == '=' => (),
            _ => bail_on!(token, "expect ="),
        }

        // extract value
        let token = bail_if_missing!(iter.next(), stream, "val");
        let val = match token {
            TokenTree::Ident(ident) => MetaValue::One(ident),
            TokenTree::Group(group) if matches!(group.delimiter(), Delimiter::Bracket) => {
                let mut set = BTreeSet::new();

                let sub = group.stream();
                let mut sub_iter = sub.into_iter();
                let mut sub_cursor = sub_iter.next();
                while sub_cursor.is_some() {
                    // extract the item
                    let token = bail_if_missing!(sub_cursor.as_ref(), group, "item");
                    let item = match token {
                        TokenTree::Ident(ident) => ident.clone(),
                        _ => bail_on!(token, "not an item"),
                    };
                    if !set.insert(item.clone()) {
                        bail_on!(group, "duplicated item");
                    }

                    // advance the cursor
                    sub_cursor = sub_iter.next();
                    if matches!(sub_cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',')
                    {
                        sub_cursor = sub_iter.next();
                    }
                }

                MetaValue::Set(set)
            }
            _ => bail_on!(token, "expect value"),
        };

        // add to the key-value store
        store.insert(key, val);

        // check for more tokens
        cursor = iter.next();
        if matches!(cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',') {
            cursor = iter.next();
        }
    }

    Ok(store)
}

/// Annotation over a Rust type
#[proc_macro_attribute]
pub fn smt_type(attr: Syntax, item: Syntax) -> Syntax {
    // check attributes
    let attr = TokenStream::from(attr);
    if !attr.is_empty() {
        fail_on!(attr, "unexpected");
    }

    // produce the output
    let output = item.clone();

    // ensure that the underlying item is a type
    let target = parse_macro_input!(item as Item);
    if !matches!(target, Item::Struct(_) | Item::Enum(_)) {
        fail_on!(target, "expect type");
    }

    // do nothing with the type declaration
    output
}

/// Derive for function annotations
fn derive_for_func(attr: Syntax, item: Syntax) -> Result<Syntax> {
    // check attributes
    let attr = TokenStream::from(attr);
    let dict = parse_dict(&attr)?;

    // ensure that the underlying item is a function
    let input = item.clone();
    let target = syn::parse::<ItemFn>(input)?;

    // check whether we need to derive any method
    let method = match dict.get("method") {
        None => return Ok(item.clone()),
        Some(MetaValue::One(ident)) => ident,
        Some(MetaValue::Set(_)) => bail_on!(attr, "invalid method attribute"),
    };

    // unpack things
    let ItemFn {
        attrs: _,
        vis,
        sig,
        block: _,
    } = &target;

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

    // extract the self type
    let param0 = bail_if_missing!(inputs.first(), sig, "at least one parameter");
    let (self_ty_name, self_ty_params) = match param0 {
        FnArg::Receiver(_) => bail_on!(param0, "expect type declaration"),
        FnArg::Typed(PatType {
            attrs: _,
            pat: _,
            colon_token: _,
            ty,
        }) => match ty.as_ref() {
            // extract type
            Type::Path(TypePath {
                qself,
                path:
                    Path {
                        leading_colon,
                        segments,
                    },
            }) => {
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                bail_if_exists!(leading_colon);
                let mut iter = segments.iter();
                let segment = bail_if_missing!(iter.next(), param0, "type name");
                bail_if_exists!(iter.next());

                let PathSegment { ident, arguments } = segment;

                // extract type arguments
                let ty_params = match arguments {
                    PathArguments::None => vec![],
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token,
                        lt_token: _,
                        args,
                        gt_token: _,
                    }) => {
                        bail_if_exists!(colon2_token);

                        let mut names = vec![];
                        for arg in args {
                            match arg {
                                GenericArgument::Type(Type::Path(TypePath {
                                    qself,
                                    path:
                                        Path {
                                            leading_colon,
                                            segments,
                                        },
                                })) => {
                                    bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                                    bail_if_exists!(leading_colon);
                                    let mut iter = segments.iter();
                                    let segment = bail_if_missing!(
                                        iter.next(),
                                        arg,
                                        "type parameter name as type argument"
                                    );
                                    bail_if_exists!(iter.next());

                                    // extract type parameter
                                    let PathSegment { ident, arguments } = segment;
                                    if !matches!(arguments, PathArguments::None) {
                                        bail_on!(
                                            arguments,
                                            "expect type parameter as type argument"
                                        )
                                    }
                                    names.push(ident);
                                }
                                _ => bail_on!(arg, "expect type argument"),
                            }
                        }
                        names
                    }
                    PathArguments::Parenthesized(args) => bail_on!(args, "invalid type arguments"),
                };

                // done with the parsing
                (ident, ty_params)
            }
            _ => bail_on!(param0, "expect type path"),
        },
    };

    // extract the function generics
    let Generics {
        lt_token,
        params,
        gt_token,
        where_clause,
    } = generics;

    if params.is_empty() {
        bail_if_exists!(lt_token);
        bail_if_exists!(gt_token);
    } else {
        bail_if_missing!(lt_token, generics, "<");
        bail_if_missing!(gt_token, generics, ">");
    }
    bail_if_exists!(where_clause);

    let mut func_generics = vec![];
    for param in params {
        match param {
            GenericParam::Type(TypeParam {
                attrs: _,
                ident,
                colon_token,
                bounds,
                eq_token,
                default,
            }) => {
                bail_if_missing!(colon_token, param, ":");
                bail_if_exists!(eq_token);
                bail_if_exists!(default);

                // check that the SMT trait is enforced
                let mut iter = bounds.iter();
                let bound = bail_if_missing!(iter.next(), param, "trait");
                match bound {
                    TypeParamBound::Trait(TraitBound {
                        paren_token,
                        modifier,
                        lifetimes,
                        path:
                            Path {
                                leading_colon,
                                segments,
                            },
                    }) => {
                        if paren_token.is_some() {
                            bail_on!(bound, "invalid bound");
                        }
                        if !matches!(modifier, TraitBoundModifier::None) {
                            bail_on!(modifier, "invalid modifier");
                        }
                        bail_if_exists!(lifetimes);
                        bail_if_exists!(leading_colon);

                        let mut iter = segments.iter();
                        let segment = bail_if_missing!(iter.next(), bound, "trait name");
                        bail_if_exists!(iter.next());

                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected")
                        }
                        if ident.to_string().as_str() != "SMT" {
                            bail_on!(ident, "expect SMT trait");
                        }
                    }
                    _ => bail_on!(bound, "expect trait bound"),
                }
                bail_if_exists!(iter.next());

                // save the type parameter name
                func_generics.push(ident);
            }
            _ => bail_on!(param, "expect type parameter"),
        }
    }

    // cross-match the generics
    for name in self_ty_params.iter() {
        if !func_generics.contains(name) {
            bail_on!(generics, "incomplete generics");
        }
    }
    let method_ty_params: Vec<_> = func_generics
        .iter()
        .filter(|n| !self_ty_params.contains(*n))
        .cloned()
        .collect();

    // derive the generics tokens
    let generics_to_decl = |params: &[&Ident]| {
        if params.is_empty() {
            TokenStream::new()
        } else {
            let content = params.iter().map(|&n| quote!(#n: SMT));
            quote!(<#(#content),*>)
        }
    };
    let tokenized_impl_generics = generics_to_decl(&self_ty_params);
    let tokenized_method_generics = generics_to_decl(&method_ty_params);

    let generics_to_args = |params: &[&Ident]| {
        if params.is_empty() {
            TokenStream::new()
        } else {
            let content = params.iter().map(|&n| quote!(#n));
            quote!(<#(#content),*>)
        }
    };
    let tokenized_self_ty_args = generics_to_args(&self_ty_params);
    let tokenized_func_ty_args = generics_to_args(&func_generics);

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
    let extended = quote! {
        impl #tokenized_impl_generics #self_ty_name #tokenized_self_ty_args {
            #vis fn #method #tokenized_method_generics (#tokenized_method_params) #output {
                #func_name #tokenized_func_ty_args(#tokenized_func_args)
            }
        }
    };

    // combine the original declaration with the extended block
    let mut output = TokenStream::from(item);
    output.extend(extended);
    Ok(Syntax::from(output))
}

/// Annotation over a Rust function
#[proc_macro_attribute]
pub fn smt_impl(attr: Syntax, item: Syntax) -> Syntax {
    fail_if_error!(derive_for_func(attr, item))
}

/// Annotation over a Rust function
#[proc_macro_attribute]
pub fn smt_spec(attr: Syntax, item: Syntax) -> Syntax {
    fail_if_error!(derive_for_func(attr, item))
}
