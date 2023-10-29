use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use quote::quote_spanned;
use syn::{
    GenericParam, Generics as GenericsDecl, ItemEnum, ItemStruct, PathArguments, Result,
    TraitBound, TraitBoundModifier, TypeParam, TypeParamBound,
};

use rusmart_utils::display::format_seq;

use crate::parser::ctxt::MarkedType;
use crate::parser::err::{
    bail_if_empty, bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on,
};
use crate::parser::expr::CtxtForExpr;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::{ReservedIdent, TypeParamName, UsrTypeName};
use crate::parser::ty::TypeTag;
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
    pub params: Vec<TypeParamName>,
}

impl Generics {
    /// Create a new generics for intrinsics
    pub fn intrinsic(params: Vec<TypeParamName>) -> Self {
        for name in &params {
            if params.iter().filter(|n| *n == name).count() != 1 {
                panic!("duplicated type parameter name");
            }
        }
        Self { params }
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
                vec![]
            }
            Some(_) => {
                bail_if_missing!(gt_token, generics, ">");
                bail_if_exists!(where_clause);
                bail_if_empty!(params, "type parameter");

                let mut declared = vec![];
                for item in params {
                    match item {
                        GenericParam::Type(ty_param) => {
                            let name = SysTrait::validate_type_param_decl(ty_param)?;
                            if declared.contains(&name) {
                                bail_on!(ty_param, "name conflict");
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

    /// Filter another set of type parameters
    pub fn filter(&self, names: &BTreeSet<TypeParamName>) -> Self {
        let filtered = self
            .params
            .iter()
            .filter(|n| !names.contains(*n))
            .cloned()
            .collect();
        Self { params: filtered }
    }
}

impl Display for Generics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.params.is_empty() {
            f.write_str("")
        } else {
            format_seq(",", "<", ">", &self.params).fmt(f)
        }
    }
}

/// Partial instantiation of generics
pub struct GenericsInstPartial {
    args: BTreeMap<TypeParamName, (usize, Option<TypeTag>)>,
}

impl GenericsInstPartial {
    /// Create an instantiation by setting
    pub fn new_without_args(generics: &Generics) -> Self {
        let ty_args = generics
            .params
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), (i, None)))
            .collect();
        GenericsInstPartial { args: ty_args }
    }

    /// Create an instantiation with generics and type argument (optionally parsed)
    pub fn try_with_args(generics: &Generics, args: &[TypeTag]) -> Option<Self> {
        if generics.params.len() != args.len() {
            return None;
        }
        let ty_args = generics
            .params
            .iter()
            .zip(args)
            .enumerate()
            .map(|(i, (n, t))| (n.clone(), (i, Some(t.clone()))))
            .collect();
        Some(GenericsInstPartial { args: ty_args })
    }

    /// A utility function to parse type arguments
    pub fn from_args<T: CtxtForExpr>(
        ctxt: &T,
        generics: &Generics,
        arguments: &PathArguments,
    ) -> Result<Self> {
        let ty_params = &generics.params;
        let ty_args = match arguments {
            PathArguments::None => ty_params
                .iter()
                .enumerate()
                .map(|(i, n)| (n.clone(), (i, None)))
                .collect(),
            PathArguments::AngleBracketed(pack) => {
                let ty_args = TypeTag::from_args(ctxt, pack)?;
                if ty_args.len() != ty_params.len() {
                    bail_on!(pack, "type argument number mismatch");
                }
                ty_params
                    .iter()
                    .zip(ty_args)
                    .enumerate()
                    .map(|(i, (n, t))| (n.clone(), (i, Some(t))))
                    .collect()
            }
            PathArguments::Parenthesized(_) => bail_on!(arguments, "invalid arguments"),
        };
        Ok(GenericsInstPartial { args: ty_args })
    }

    /// Complete the instantiation with the help of a type unifier
    pub fn complete(self, unifier: &mut TypeUnifier) -> GenericsInstFull {
        let args = self
            .args
            .into_iter()
            .map(|(k, (i, inst))| {
                let completed = match inst {
                    None => TypeRef::Var(unifier.mk_var()),
                    Some(tag) => (&tag).into(),
                };
                (k, (i, completed))
            })
            .collect();
        GenericsInstFull { args }
    }
}

/// Complete instantiation of generics
pub struct GenericsInstFull {
    args: BTreeMap<TypeParamName, (usize, TypeRef)>,
}

impl GenericsInstFull {
    /// Shape the arguments in its declaration order, filling missed ones with fresh type variables
    pub fn vec(&self) -> Vec<TypeRef> {
        let rev: BTreeMap<_, _> = self
            .args
            .iter()
            .map(|(_, (i, t))| (*i, t.clone()))
            .collect();
        rev.into_values().collect()
    }

    /// Make a type ref combined with type name
    pub fn make_ty(&self, name: UsrTypeName) -> TypeRef {
        TypeRef::User(name, self.vec())
    }

    /// Merge two generics into one
    pub fn merge(&self, other: &Self) -> Option<Self> {
        let args: BTreeMap<_, _> = self
            .args
            .iter()
            .chain(&other.args)
            .map(|(name, (idx, ty))| (name.clone(), (*idx + self.args.len(), ty.clone())))
            .collect();

        // should not have conflicting type parameter names
        if args.len() == self.args.len() + other.args.len() {
            Some(Self { args })
        } else {
            None
        }
    }

    /// Instantiate a type tag by applying type parameter substitution
    pub fn instantiate(&self, tag: &TypeTag) -> Option<TypeRef> {
        let updated = match tag {
            TypeTag::Boolean => TypeRef::Boolean,
            TypeTag::Integer => TypeRef::Integer,
            TypeTag::Rational => TypeRef::Rational,
            TypeTag::Text => TypeRef::Text,
            TypeTag::Cloak(sub) => TypeRef::Cloak(self.instantiate(sub)?.into()),
            TypeTag::Seq(sub) => TypeRef::Seq(self.instantiate(sub)?.into()),
            TypeTag::Set(sub) => TypeRef::Set(self.instantiate(sub)?.into()),
            TypeTag::Map(key, val) => {
                TypeRef::Map(self.instantiate(key)?.into(), self.instantiate(val)?.into())
            }
            TypeTag::Error => TypeRef::Error,
            TypeTag::User(name, args) => TypeRef::User(
                name.clone(),
                args.iter()
                    .map(|t| self.instantiate(t))
                    .collect::<Option<_>>()?,
            ),
            TypeTag::Pack(elems) => TypeRef::Pack(
                elems
                    .iter()
                    .map(|t| self.instantiate(t))
                    .collect::<Option<_>>()?,
            ),
            TypeTag::Parameter(name) => self.args.get(name).map(|(_, t)| t)?.clone(),
        };
        Some(updated)
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
