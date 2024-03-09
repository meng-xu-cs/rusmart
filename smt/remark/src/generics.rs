use std::collections::BTreeSet;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    AngleBracketedGenericArguments, GenericArgument, GenericParam, Generics, Path, PathArguments,
    PathSegment, Result, TraitBound, TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath,
};

use crate::err::{bail_if_exists, bail_if_missing, bail_on};

/// Type parameters
pub struct TypeParamGroup {
    params: Vec<Ident>,
}

impl TypeParamGroup {
    /// Parse the generics
    pub fn parse_generics(generics: &Generics) -> Result<Self> {
        let Generics {
            lt_token,
            params,
            gt_token,
            where_clause,
        } = generics;

        // sanity check
        if params.is_empty() {
            bail_if_exists!(lt_token);
            bail_if_exists!(gt_token);
        } else {
            bail_if_missing!(lt_token, generics, "<");
            bail_if_missing!(gt_token, generics, ">");
        }
        bail_if_exists!(where_clause);

        // collect type parameters
        let mut ty_params_set = BTreeSet::new();
        let mut ty_params_vec = vec![];
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

                    // save the type parameter name after duplication check
                    if !ty_params_set.insert(ident.clone()) {
                        bail_on!(ident, "duplicated declaration");
                    }
                    ty_params_vec.push(ident.clone());
                }
                _ => bail_on!(param, "expect type parameter"),
            }
        }

        // return the type parameter pack
        Ok(Self {
            params: ty_params_vec,
        })
    }

    /// Check if the group contains a specific type parameter
    pub fn contains(&self, name: &Ident) -> bool {
        self.params.contains(name)
    }

    /// Collect type arguments based on the current type parameter
    pub fn collect_type_arguments(&self, ty: &Type) -> Result<Self> {
        let mut ty_args_set = BTreeSet::new();
        let mut ty_args_vec = vec![];
        collect_type_arguments_recursive(ty, self, &mut ty_args_set, &mut ty_args_vec)?;
        Ok(Self {
            params: ty_args_vec,
        })
    }

    /// Calculate the difference of the type parameter
    pub fn diff(&self, other: &Self) -> Self {
        let filtered: Vec<_> = self
            .params
            .iter()
            .filter(|n| !other.contains(*n))
            .cloned()
            .collect();
        Self { params: filtered }
    }

    /// Convert into def syntax
    pub fn to_syntax_def(&self) -> TokenStream {
        if self.params.is_empty() {
            TokenStream::new()
        } else {
            let content = self.params.iter().map(|n| quote!(#n: SMT));
            quote!(<#(#content),*>)
        }
    }

    /// Convert into use syntax
    pub fn to_syntax_use(&self) -> TokenStream {
        if self.params.is_empty() {
            TokenStream::new()
        } else {
            let content = self.params.iter().map(|n| quote!(#n));
            quote!(<#(#content),*>)
        }
    }
}

/// Collect type arguments recursively
fn collect_type_arguments_recursive(
    ty: &Type,
    ty_params: &TypeParamGroup,
    ty_args: &mut BTreeSet<Ident>,
    ty_args_ordered: &mut Vec<Ident>,
) -> Result<()> {
    // extract the path segment
    let segment = match ty {
        Type::Path(TypePath {
            qself,
            path: Path {
                leading_colon,
                segments,
            },
        }) => {
            bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
            bail_if_exists!(leading_colon);

            let mut iter = segments.iter();
            let segment = bail_if_missing!(iter.next(), ty, "type name");
            bail_if_exists!(iter.next());
            segment
        }
        _ => bail_on!(ty, "expect type path"),
    };
    let PathSegment { ident, arguments } = segment;

    // analyze the segments
    match arguments {
        PathArguments::None => {
            if !ty_params.contains(ident) {
                // just a type not a type argument
                return Ok(());
            }
            if ty_args.insert(ident.clone()) {
                ty_args_ordered.push(ident.clone());
            }
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token,
            lt_token: _,
            args,
            gt_token: _,
        }) => {
            bail_if_exists!(colon2_token);
            if ty_params.contains(ident) {
                bail_on!(arguments, "type parameter should not have arguments");
            }

            for arg in args {
                match arg {
                    // extract type arguments recursively
                    GenericArgument::Type(sub_ty) => {
                        collect_type_arguments_recursive(
                            sub_ty,
                            ty_params,
                            ty_args,
                            ty_args_ordered,
                        )?;
                    }
                    _ => bail_on!(arg, "expect type argument"),
                }
            }
        }
        PathArguments::Parenthesized(args) => bail_on!(args, "invalid type arguments"),
    };
    Ok(())
}
