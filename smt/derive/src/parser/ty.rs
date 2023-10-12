use quote::quote_spanned;
use syn::{
    GenericParam, Generics, Ident, ItemEnum, ItemStruct, Result, TraitBound, TraitBoundModifier,
    TypeParam, TypeParamBound,
};

use crate::parser::ctxt::MarkedType;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::name::{ReservedIdent, TypeParamName};
use crate::parser::path::PathUtil;

/// Reserved trait
pub enum SysTrait {
    SMT,
}

impl ReservedIdent for SysTrait {
    /// Get a trait from an ident
    fn from_ident(ident: &Ident) -> Result<Self> {
        let matched = match ident.to_string().as_str() {
            "SMT" => Self::SMT,
            _ => bail_on!(ident, "not an intrinsic trait"),
        };
        Ok(matched)
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
        bail_if_missing!(colon_token, param, "no trait bound");

        let mut iter = bounds.iter();
        let bound = bail_if_missing!(iter.next(), bounds, "[invariant] no trait bound");
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

/// An arena for declaration of generics
pub struct GenericsDecl {
    /// List of declared type parameters, checked for uniqueness
    params: Vec<TypeParamName>,
}

impl GenericsDecl {
    /// Convert from generics
    fn from_generics(generics: &Generics) -> Result<Self> {
        let Generics {
            lt_token,
            params,
            where_clause,
            gt_token,
        } = generics;

        let params = match lt_token {
            None => {
                bail_if_exists!(gt_token);
                bail_if_exists!(where_clause);
                if !params.is_empty() {
                    bail_on!(params, "[invariant] unexpected")
                }
                vec![]
            }
            Some(_) => {
                bail_if_missing!(gt_token, generics, "[invariant] unmatched");
                bail_if_exists!(where_clause);
                if params.is_empty() {
                    bail_on!(generics, "expects one or more parameters");
                }

                let mut declared = vec![];
                for item in params {
                    match item {
                        GenericParam::Type(ty_param) => {
                            let name = SysTrait::validate_type_param_decl(ty_param)?;
                            if declared.contains(&name) {
                                bail_on!(ty_param, "name conflicts");
                            }
                            declared.push(name);
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
}
