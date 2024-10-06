//! This module provides a utility for parsing and manipulating generic type parameters.
//!
//! The `TypeParamGroup` struct is used to collect and manage type parameters in generic definitions.
//! It provides methods for parsing generics, checking for type parameter existence, collecting type arguments,
//! calculating differences between groups, and converting type parameters into syntax suitable for definition and usage.
//!

use proc_macro2::{Ident, TokenStream};
use std::collections::BTreeSet;
// Import the `quote` macro to generate tokens
use quote::quote;
// Import various syntax-related types from the `syn` crate, used for parsing Rust code
use crate::{bail_if_exists, bail_if_missing, bail_on};
use syn::{
    AngleBracketedGenericArguments, GenericArgument, GenericParam, Generics, Path, PathArguments,
    PathSegment, Result, TraitBound, TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath,
};

/// Represents a group of type parameters in generic definitions.
///
/// This struct is used to collect and manipulate type parameters
/// when parsing and generating code for procedural macros.
#[derive(Debug)]
pub struct TypeParamGroup {
    /// A vector of identifiers for the type parameters.
    params: Vec<Ident>,
}

impl TypeParamGroup {
    /// Parses the generics from a `syn::Generics` and returns a `TypeParamGroup`.
    ///
    /// This function checks for correctness in the generics syntax, ensuring that:
    /// - Angle brackets are correctly used.
    /// - Each type parameter has a `: SMT` trait bound.
    /// - There are no duplicate type parameters.
    ///
    /// # Arguments
    ///
    /// * `generics` - A reference to the `Generics` to parse.
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing a `TypeParamGroup` if successful, or a `syn::Error`.
    pub fn parse_generics(generics: &Generics) -> Result<Self> {
        // Destructure the Generics for easier access to its components
        let Generics {
            lt_token,
            params,
            gt_token,
            where_clause,
        } = generics;

        // Sanity check: Ensure that the angle brackets are used correctly
        if params.is_empty() {
            // If there are no parameters, there should be no angle brackets
            // one cannot be only present, both need to be present for the syntax to pass the rust compiler.
            // so theoretically bail_if_exists!(gt_token); is unreachable invocation?
            bail_if_exists!(lt_token);
            bail_if_exists!(gt_token);
        } else {
            // If there are parameters, angle brackets must exist
            bail_if_missing!(lt_token, generics, "<"); //unreachable code as the rust compiler catches it beforehand
            bail_if_missing!(gt_token, generics, ">"); //unreachable code as the rust compiler catches it beforehand. you cannot have fn temp T:SMT (x:T) {....}
        }

        // The where clause is not expected in this context; bail if it exists
        bail_if_exists!(where_clause);

        // Collect type parameters
        let mut ty_params_set = BTreeSet::new(); // To ensure uniqueness
        let mut ty_params_vec = vec![]; // To maintain order

        for param in params {
            match param {
                // We expect Generic type parameters only
                GenericParam::Type(TypeParam {
                    attrs: _,
                    ident,
                    colon_token,
                    bounds,
                    eq_token,
                    default,
                }) => {
                    // Colon token must be present (e.g., `T: Trait`) so basically the bound is expected
                    bail_if_missing!(colon_token, param, ":");

                    // Equal token and default should not be present
                    // use of default is actually being phased out in Rust and it will lead to hard errors in the future
                    bail_if_exists!(eq_token);
                    bail_if_exists!(default); // unreachable invocation because it cannot exist if the = doesnt exist according to rust compiler

                    // The rest check that the `SMT` trait is enforced as a bound
                    let mut iter = bounds.iter();
                    // return Err(syn::Error::new_spanned(param, "expect trait"));
                    // bound will be an error if there is no trait bound
                    let bound = bail_if_missing!(iter.next(), param, "trait");

                    bail_if_exists!(iter.next()); // No extra bounds expected
                                                  //* so only one bound is expected and it should be the SMT trait

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
                                // Parentheses are not expected in the trait bound for example T: (SMT)
                                bail_on!(bound, "invalid bound");
                            }
                            if !matches!(modifier, TraitBoundModifier::None) {
                                // Modifier should be none (e.g., no `?` or `for<>`)
                                bail_on!(modifier, "invalid modifier");
                            }
                            bail_if_exists!(lifetimes); // Lifetimes are not expected for example for<'a> Foo<&'a T> Higher ranked trait bounds are not expected
                            // HRTB are the same as lifetimes but they mean that the trait is generic over all lifetimes 'a but if we wrote Foo<&'a T> it would mean that the trait is generic over a specific lifetime 'a
                            bail_if_exists!(leading_colon); // Leading colon is not expected for example T: ::std

                            // Check the path segments
                            let mut iter = segments.iter();
                            let segment = bail_if_missing!(iter.next(), bound, "trait name"); // this will never lead to an error.
                            bail_if_exists!(iter.next()); // Only one segment is expected and the trait name is expected to be SMT

                            let PathSegment { ident, arguments } = segment;
                            if !matches!(arguments, PathArguments::None) {
                                // Type arguments are not expected
                                bail_on!(arguments, "unexpected");
                            }
                            if ident.to_string().as_str() != "SMT" {
                                // The trait should be `SMT`
                                bail_on!(ident, "expect SMT trait");
                            }
                        }
                        // if the bound is not a trait bound return an error
                        _ => bail_on!(bound, "expect trait bound"),
                    }

                    // Save the type parameter name after duplication check
                    if !ty_params_set.insert(ident.clone()) {
                        bail_on!(ident, "duplicated declaration");
                    }
                    ty_params_vec.push(ident.clone());
                }
                _ => bail_on!(param, "expect generic type parameter"),
            }
        }

        // Return the type parameter group
        Ok(Self {
            params: ty_params_vec,
        })
    }

    /// Checks if the group contains a specific type parameter.
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier of the type parameter to check.
    ///
    /// # Returns
    ///
    /// Returns `true` if the type parameter is in the group, `false` otherwise.
    pub fn contains(&self, name: &Ident) -> bool {
        self.params.contains(name)
    }

    /// Collects type arguments from a given type based on the current type parameters.
    ///
    /// This function recursively inspects the given type and collects any type parameters
    /// that are used as type arguments. It helps in tracking which type parameters are
    /// actually used in a type definition.
    ///
    /// # Arguments
    ///
    /// * `ty` - The type from which to collect type arguments.
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing a `TypeParamGroup` of collected type arguments.
    pub fn collect_type_arguments(&self, ty: &Type) -> Result<Self> {
        let mut ty_args_set = BTreeSet::new();
        let mut ty_args_vec = vec![];
        collect_type_arguments_recursive(ty, self, &mut ty_args_set, &mut ty_args_vec)?;
        Ok(Self {
            params: ty_args_vec,
        })
    }

    /// Calculates the difference of type parameters between this group and another.
    ///
    /// This function returns a new `TypeParamGroup` containing the type parameters
    /// that are present in `self` but not in `other`.
    ///
    /// # Arguments
    ///
    /// * `other` - The other `TypeParamGroup` to compare against.
    ///
    /// # Returns
    ///
    /// Returns a new `TypeParamGroup` containing the difference.
    pub fn diff(&self, other: &Self) -> Self {
        let filtered: Vec<Ident> = self
            .params
            .iter()
            .filter(|n| !other.contains(n))
            .cloned()
            .collect();
        Self { params: filtered }
    }

    /// Converts the type parameters into a syntax suitable for definition (e.g., `<T: SMT>`).
    ///
    /// # Returns
    ///
    /// Returns a `TokenStream` representing the type parameter definitions.
    pub fn to_syntax_def(&self) -> TokenStream {
        if self.params.is_empty() {
            TokenStream::new()
        } else {
            let content = self.params.iter().map(|n| quote!(#n: SMT));
            quote!(<#(#content),*>) //interpolates the content into the token stream: <T: SMT, U: SMT>
        }
    }

    /// Converts the type parameters into a syntax suitable for use (e.g., `<T>`).
    ///
    /// # Returns
    ///
    /// Returns a `TokenStream` representing the type parameter usages.
    pub fn to_syntax_use(&self) -> TokenStream {
        if self.params.is_empty() {
            TokenStream::new()
        } else {
            let content = self.params.iter().map(|n| quote!(#n));
            quote!(<#(#content),*>) //interpolates the content into the token stream: <T, U>
        }
    }

    /// Converts the type parameters into a syntax suitable for function invocation (e.g., `::<T>`).
    ///
    /// # Returns
    ///
    /// Returns a `TokenStream` representing the type parameter invocations.
    pub fn to_syntax_invoke(&self) -> TokenStream {
        if self.params.is_empty() {
            TokenStream::new()
        } else {
            let content = self.params.iter().map(|n| quote!(#n));
            quote!(::<#(#content),*>) //interpolates the content into the token stream: ::<T, U>
        }
    }
}

/// Recursively collects type arguments from a given type.
///
/// This helper function is used to traverse a type and collect any type parameters
/// that are used as type arguments. It updates the provided sets and vectors with
/// the collected identifiers.
///
/// # Arguments
///
/// * `ty` - The type from which to collect arguments.
/// * `ty_params` - The current `TypeParamGroup` of type parameters.
/// * `ty_args` - A set to store unique type argument identifiers.
/// * `ty_args_ordered` - A vector to maintain the order of type arguments.
///
/// # Returns
///
/// Returns `Result<()>` indicating success or a `syn::Error`.
fn collect_type_arguments_recursive(
    ty: &Type,
    ty_params: &TypeParamGroup,
    ty_args: &mut BTreeSet<Ident>,
    ty_args_ordered: &mut Vec<Ident>,
) -> Result<()> {
    // Extract the path segment from the type
    let segment = match ty {
        Type::Path(TypePath {
            qself,
            path: Path {
                leading_colon,
                segments,
            },
        }) => {
            // Qualified Self types and leading colons are not expected
            bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
            bail_if_exists!(leading_colon);

            // We expect exactly one segment in the path
            let mut iter = segments.iter();
            let segment = bail_if_missing!(iter.next(), ty, "type name");
            bail_if_exists!(iter.next()); // should only have one
            segment
        }
        _ => bail_on!(ty, "expect type path"), // can be invoked with tuple (A,B)
    };
    let PathSegment { ident, arguments } = segment;

    // Analyze the segments
    match arguments {
        PathArguments::None => {
            if !ty_params.contains(ident) {
                // here the group is checked if it contains the ident
                // Just a type, not a type argument; do nothing
                return Ok(());
            }
            if ty_args.insert(ident.clone()) {
                // Insert into ordered vector if it's a new argument
                ty_args_ordered.push(ident.clone());
            }
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token,
            lt_token: _,
            args,
            gt_token: _,
        }) => {
            bail_if_exists!(colon2_token); // Double colon is not expected
            if ty_params.contains(ident) {
                // Type parameters should not have arguments
                bail_on!(arguments, "type parameter should not have arguments");
            }

            for arg in args {
                match arg {
                    // Extract type arguments recursively
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

// ----------------------------------------------------------------------------------------------- //

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Generics, Type};
    use syn::punctuated::Punctuated;

    #[test]
    fn test_parse_generics_params_empty_bail() {
        // this invokes bail_if_exists!(lt_token); inside is param empty.
        let generics: Generics = parse_quote! {<>};
        let type_param_group: Result<TypeParamGroup> = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    fn test_parse_generics_params_empty_ok() {
        // this one creates TypeParamGroup {params:vec![]}
        let generics: Generics = parse_quote! {};
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_ok_and(|x| { x.params.is_empty() }));
    }

    #[test]
    fn test_parse_generics_bail_where() {
        // this invokes bail_if_exists!(where_clause);
        let item_fn: syn::ItemFn = parse_quote! {
            fn foo<T: Default>(x: T) -> T
            where T: std::fmt::Debug
            {
                x
            }
        };
        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    // bail_if_missing!(colon_token, param, ":"); is invoked
    fn test_parse_generics_colon_missing() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T>(x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "expect :");
    }

    #[test]
    // bail_if_exists!(eq_token); is invoked
    fn test_parse_generics_eq_token() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T : SMT = String >(x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    // let bound = bail_if_missing!(iter.next(), param, "trait"); will be invoked
    fn test_parse_generics_no_trait() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T : >(x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "expect trait");
    }

    #[test]
    // bail_if_exists!(iter.next()); // No extra bounds expected is invoked
    //* so only one bound is expected and it should be the SMT trait
    fn test_parse_generics_one_trait() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: SMT + a> (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    // bail_on!(bound, "invalid bound");
    fn test_parse_generics_invalid_bound() {
        let item_fn: syn::ItemTrait = parse_quote! {
            trait Example<T : (?Sized)> {
                type T;
            }
        };

        let generics: Generics = item_fn.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "invalid bound");
    }

    #[test]
    // bail_on!(modifier, "invalid modifier"); is invoked
    fn test_parse_generics_invalid_modifier() {
        let item_fn: syn::ItemTrait = parse_quote! {
            trait Example<T : ?Sized> {
                type T;
            }
        };

        let generics: Generics = item_fn.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(
            type_param_group.err().unwrap().to_string(),
            "invalid modifier"
        );
    }

    // bail_if_exists!(lifetimes); // Lifetimes are not expected
    // for<'a> Foo<&'a T>
    #[test]
    fn test_parse_generics_lifetimes() {
        let generics: Generics = parse_quote! {
            <T: for<'a> Foo<&'a T>>
        };

        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    // bail_if_exists!(leading_colon); // Leading colon is not expected is invoked
    fn test_parse_generics_leading_colon() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: ::std>(x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    #[test]
    //bail_if_exists!(iter.next()); // Only one segment is expected and the trait name is expected to be SMT
    fn test_parse_generics_one_segment() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: std::SMT > (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    // if !matches!(arguments, PathArguments::None) {
    //     bail_on!(arguments, "unexpected");
    // }
    // the above is invoked
    #[test]
    fn test_parse_generics_path_args_not_none() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: SMT<String> > (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(type_param_group.err().unwrap().to_string(), "unexpected");
    }

    // if ident.to_string().as_str() != "SMT" {
    //     bail_on!(ident, "expect SMT trait");
    // }
    // the above is invoked
    #[test]
    fn test_parse_generics_smt() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: SMTT > (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(
            type_param_group.err().unwrap().to_string(),
            "expect SMT trait"
        );
    }

    //  _ => bail_on!(bound, "expect trait bound"), is invoked
    #[test]
    fn test_parse_generics_expect_trait_bound() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: 'a> (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(
            type_param_group.err().unwrap().to_string(),
            "expect trait bound"
        );
    }

    // if !ty_params_set.insert(ident.clone()) {
    //     bail_on!(ident, "duplicated declaration");
    // }
    // the above is invoked
    #[test]
    fn parse_generics_duplicated_declaration() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: SMT, T: SMT> (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(
            type_param_group.err().unwrap().to_string(),
            "duplicated declaration"
        );
    }

    //_ => bail_on!(param, "expect generic type parameter"), is invoked
    #[test]
    fn test_parse_generics_expect_generic_type_parameter() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<'a> (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_err());
        assert_eq!(
            type_param_group.err().unwrap().to_string(),
            "expect generic type parameter"
        );
    }

    // no bail
    #[test]
    fn test_parse_generics() {
        let item_fn: syn::ItemFn = parse_quote! {
            fn temp<T: SMT> (x:T) -> String {
                x.to_string()
            }
        };

        let generics: Generics = item_fn.sig.generics;
        let type_param_group = TypeParamGroup::parse_generics(&generics);

        assert!(type_param_group.is_ok());
        assert_eq!(type_param_group.unwrap().params.len(), 1);
    }

    #[test]
    fn test_contains() {
        let type_param_group = TypeParamGroup {
            params: vec![parse_quote! {U}, parse_quote! {T}],
        };

        let ident: Ident = parse_quote! {U};
        let res = type_param_group.contains(&ident);
        assert!(res);
    }

    #[test]
    fn test_diff() {
        let type_param_group_one = TypeParamGroup {
            params: vec![parse_quote!(U), parse_quote!(T)],
        };
        let type_param_group_two = TypeParamGroup {
            params: vec![parse_quote!(U)],
        };

        let res = type_param_group_one.diff(&type_param_group_two);

        assert_eq!(res.params.len(), 1);
        assert!(res.contains(&parse_quote!(T)));
        assert!(!res.contains(&parse_quote!(U)));
    }

    #[test]
    fn test_to_syntax_def_one() {
        let group = TypeParamGroup { params: vec![] };

        let token_stream = group.to_syntax_def();
        assert!(token_stream.is_empty());
    }

    #[test]
    fn test_to_syntax_def_two() {
        // Test converting to syntax definition
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let tokens = group.to_syntax_def();
        let expected: TokenStream = quote!(<T: SMT, U: SMT>);
        assert_eq!(tokens.to_string(), expected.to_string());
    }

    #[test]
    fn test_to_syntax_use_one() {
        // Test converting to syntax usage
        let group = TypeParamGroup { params: vec![] };
        let tokens = group.to_syntax_use();
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_to_syntax_use_two() {
        // Test converting to syntax usage
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let tokens = group.to_syntax_use();
        let expected: TokenStream = quote!(<T, U>);
        assert_eq!(tokens.to_string(), expected.to_string());
    }

    #[test]
    fn test_to_syntax_invoke_one() {
        // Test converting to syntax invocation
        let group = TypeParamGroup { params: vec![] };
        let tokens = group.to_syntax_invoke();
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_to_syntax_invoke_two() {
        // Test converting to syntax invocation
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let tokens = group.to_syntax_invoke();
        let expected: TokenStream = quote!(::<T, U>);
        assert_eq!(tokens.to_string(), expected.to_string());
    }

    #[test]
    fn test_collect_type_arguments() {
        // Test collecting type arguments from a type
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(Option<T>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_ok());
        let collected = result.unwrap();
        assert_eq!(collected.params.len(), 1);

        // get params
        let params = collected.params;
        assert_eq!(params[0].to_string(), "T");

        let ty: Type = parse_quote!(Result<T, U>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_ok());
        let collected = result.unwrap();
        assert_eq!(collected.params.len(), 2);

        // get params
        let params = collected.params;
        assert_eq!(params[0].to_string(), "T");
        assert_eq!(params[1].to_string(), "U");
    }

    // bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
    #[test]
    fn test_collect_type_arguments_qself() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(<T as Trait>::AssociatedType);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "unexpected");
    }

    // bail_if_exists!(leading_colon);
    #[test]
    fn test_collect_type_arguments_leading_colon() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(::std::String);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "unexpected");
    }

    // _ => bail_on!(ty, "expect type path"), is invoked
    #[test]
    fn test_collect_type_arguments_expect_type_path() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!((A, B));
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "expect type path");
    }

    // PathArguments::None => {
    //     if !ty_params.contains(ident) {
    //         return Ok(());
    //     }
    #[test]
    fn test_collect_type_arguments_normal_type() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(String); // params does not contain String
        let result = group.collect_type_arguments(&ty);

        assert!(result.is_ok());
        let collected = result.unwrap();
        assert_eq!(collected.params.len(), 0);
    }

    // PathArguments::None
    #[test]
    fn test_collect_type_arguments_normal_type_two() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(T); // params does contain T
        let result = group.collect_type_arguments(&ty);

        assert!(result.is_ok());
        let collected = result.unwrap();
        assert_eq!(collected.params.len(), 1);
        assert_eq!(collected.params[0].to_string(), "T");
    }

    // bail_if_exists!(colon2_token); // Double colon is not expected
    #[test]
    fn test_collect_type_arguments_colon2_token() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(Option::<T>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "unexpected");
    }

    // if ty_params.contains(ident) {
    //     // Type parameters should not have arguments
    //     bail_on!(arguments, "type parameter should not have arguments");
    // }
    #[test]
    fn test_collect_type_arguments_type_parameter() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(Option), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(Option<T>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "type parameter should not have arguments"
        );
    }

    //  _ => bail_on!(arg, "expect type argument"),
    #[test]
    fn test_collect_type_arguments_expect_type_argument() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(Option<32>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "expect type argument");
    }

    // bail_if_exists!(iter.next()); // should only have one
    // segment
    #[test]
    fn test_collect_type_arguments_more_segments() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        let ty: Type = parse_quote!(std::Option<T, U>);
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "unexpected");
    }

    // PathArguments::Parenthesized(args) => bail_on!(args, "invalid type arguments"),
    #[test]
    fn test_collect_type_arguments_parenthesized() {
        let group = TypeParamGroup {
            params: vec![parse_quote!(T), parse_quote!(U)],
        };
        // let ty: Type = parse_quote!(myString(T)); //? this does not get parsed as a type 
        let ty: Type = Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: None,
                // punctuated path segments myString(T)
                segments: Punctuated::from_iter(vec![PathSegment {
                    ident: parse_quote!(myString),
                    arguments: PathArguments::Parenthesized(parse_quote!((T))),
                }]),
            }
        });
        let result = group.collect_type_arguments(&ty);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap().to_string(), "invalid type arguments");
    }
}
