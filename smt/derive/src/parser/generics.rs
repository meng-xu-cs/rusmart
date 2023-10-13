use std::collections::BTreeMap;

use quote::quote_spanned;
use syn::{
    GenericParam, Generics as GenericsDecl, ItemEnum, ItemFn, ItemStruct, Result, TraitBound,
    TraitBoundModifier, TypeParam, TypeParamBound,
};

use crate::parser::ctxt::{MarkedImpl, MarkedSpec, MarkedType};
use crate::parser::err::{
    bail_if_empty, bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on,
};
use crate::parser::name::{ReservedIdent, TypeParamName};
use crate::parser::util::PathUtil;

/// Reserved trait
#[allow(clippy::upper_case_acronyms)]
pub enum SysTrait {
    SMT,
}

impl ReservedIdent for SysTrait {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident {
            "SMT" => Self::SMT,
            _ => return None,
        };
        Some(matched)
    }
}

impl SysTrait {
    /// Ensure that a generics declaration satisfies the trait
    fn validate_type_param_decl(param: &TypeParam) -> Result<TypeParamName> {
        let TypeParam {
            attrs: _,
            ident,
            colon_token,
            bounds,
            eq_token,
            default,
        } = param;

        bail_if_exists!(eq_token);
        bail_if_exists!(default);

        // ensure that the trait bound includes and only includes SMT
        bail_if_missing!(colon_token, param, "trait bound");

        let mut iter = bounds.iter();
        let bound = bail_if_missing!(iter.next(), bounds, "trait bound");
        match bound {
            TypeParamBound::Trait(trait_bound) => {
                let TraitBound {
                    paren_token,
                    modifier,
                    lifetimes,
                    path,
                } = trait_bound;

                bail_if_exists!(paren_token.as_ref().map(|e| quote_spanned!(e.span=>)));
                bail_if_exists!(lifetimes);
                if !matches!(modifier, TraitBoundModifier::None) {
                    bail_on!(modifier, "unexpected");
                }

                let trait_name: SysTrait = PathUtil::expect_ident_reserved(path)?;
                if !matches!(trait_name, SysTrait::SMT) {
                    bail_on!(path, "unexpected trait")
                }
            }
            _ => bail_on!(bound, "invalid bound"),
        }
        bail_if_exists!(iter.next());

        // all checks passed
        ident.try_into()
    }
}

/// Declaration of generics
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Generics {
    params: BTreeMap<TypeParamName, usize>,
}

impl Generics {
    /// Create a new generics for intrinsics
    pub fn intrinsic(params: Vec<TypeParamName>) -> Self {
        Self {
            params: params
                .into_iter()
                .enumerate()
                .map(|(n, i)| (i, n))
                .collect(),
        }
    }

    /// Convert from generics
    pub fn from_generics(generics: &GenericsDecl) -> Result<Self> {
        let GenericsDecl {
            lt_token,
            params,
            where_clause,
            gt_token,
        } = generics;

        let params = match lt_token {
            None => {
                bail_if_exists!(gt_token);
                bail_if_exists!(where_clause);
                bail_if_non_empty!(params);
                BTreeMap::new()
            }
            Some(_) => {
                bail_if_missing!(gt_token, generics, ">");
                bail_if_exists!(where_clause);
                bail_if_empty!(params, "type parameter");

                let mut declared = BTreeMap::new();
                for (i, item) in params.iter().enumerate() {
                    match item {
                        GenericParam::Type(ty_param) => {
                            let name = SysTrait::validate_type_param_decl(ty_param)?;
                            match declared.insert(name, i) {
                                None => (),
                                Some(_) => bail_on!(ty_param, "name conflict"),
                            }
                        }
                        _ => bail_on!(item, "type parameters only"),
                    }
                }
                declared
            }
        };

        Ok(Self { params })
    }

    /// Convert from a marked type
    pub fn from_marked_type(item: &MarkedType) -> Result<Self> {
        let generics = match item {
            MarkedType::Enum(ItemEnum {
                attrs: _,
                vis: _,
                enum_token: _,
                ident: _,
                generics,
                brace_token: _,
                variants: _,
            }) => generics,
            MarkedType::Struct(ItemStruct {
                attrs: _,
                vis: _,
                struct_token: _,
                ident: _,
                generics,
                fields: _,
                semi_token: _,
            }) => generics,
        };
        Self::from_generics(generics)
    }

    /// Get the length of the arena
    pub fn len(&self) -> usize {
        self.params.len()
    }

    /// Retrieve a parameter
    pub fn get(&self, name: &TypeParamName) -> Option<usize> {
        self.params.get(name).copied()
    }

    /// Shape the parameters in its declaration order
    pub fn vec(&self) -> Vec<TypeParamName> {
        let rev: BTreeMap<_, _> = self.params.iter().map(|(k, v)| (*v, k.clone())).collect();
        rev.into_values().collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(with_smt_trait, {
        #[smt_type]
        struct S<T: SMT>(T);
    });

    unit_test!(multi_params, {
        #[smt_type]
        enum E<K: SMT, V: SMT> {
            UseK(K),
            UseV(V),
        }
    });

    unit_test!(
        no_trait,
        {
            #[smt_type]
            struct S<T>(T);
        },
        "expect trait bound"
    );

    unit_test!(
        other_trait,
        {
            #[smt_type]
            struct S<T: NotSMT>(T);
        },
        "not an intrinsic trait"
    );

    unit_test!(
        one_param_no_trait,
        {
            #[smt_type]
            enum E<K: SMT, V> {
                UseK(K),
                UseV(V),
            }
        },
        "expect trait bound"
    );

    unit_test!(
        name_conflicts,
        {
            #[smt_type]
            enum E<K: SMT, K: SMT> {
                UseK(K),
            }
        },
        "name conflict"
    );
}
