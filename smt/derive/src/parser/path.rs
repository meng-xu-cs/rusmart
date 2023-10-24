use std::collections::BTreeMap;

use syn::{Path, PathArguments, PathSegment, Result};

use crate::parser::adt::ADTBranch;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::func::{CastFuncName, FuncName, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName};
use crate::parser::ty::{SysTypeName, TypeBody, TypeName, TypeTag};

/// Instantiation of generics
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct GenericsInstantiated {
    args: BTreeMap<TypeParamName, (usize, Option<TypeTag>)>,
}

impl GenericsInstantiated {
    /// Create an instantiation with generics and type argument (optionally parsed)
    pub fn new(generics: &Generics, ty_args_opt: Option<&[TypeTag]>) -> Option<Self> {
        let ty_params = &generics.params;
        let ty_args = match ty_args_opt {
            None => ty_params
                .iter()
                .enumerate()
                .map(|(i, n)| (n.clone(), (i, None)))
                .collect(),
            Some(tags) => {
                if tags.len() != ty_params.len() {
                    return None;
                }
                ty_params
                    .iter()
                    .zip(tags)
                    .enumerate()
                    .map(|(i, (n, t))| (n.clone(), (i, Some(t.clone()))))
                    .collect()
            }
        };
        Some(GenericsInstantiated { args: ty_args })
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
        Ok(GenericsInstantiated { args: ty_args })
    }

    /// Retrieve an argument
    pub fn get(&self, name: &TypeParamName) -> Option<Option<&TypeTag>> {
        self.args.get(name).map(|(_, t)| t.as_ref())
    }

    /// Shape the arguments in its declaration order, filling missed ones with fresh type variables
    pub fn vec(&self, unifier: &mut TypeUnifier) -> Vec<TypeRef> {
        let rev: BTreeMap<_, _> = self
            .args
            .iter()
            .map(|(_, (i, t))| {
                let ty_ref = match t.as_ref() {
                    None => TypeRef::Var(unifier.mk_var()),
                    Some(tag) => tag.into(),
                };
                (*i, ty_ref)
            })
            .collect();
        rev.into_values().collect()
    }
}

/// An identifier to a tuple with optional type arguments
pub struct TuplePath {
    pub ty_name: UsrTypeName,
    pub ty_args: GenericsInstantiated,
}

impl TuplePath {
    /// Extract a reference to a tuple from a path
    pub fn from_path<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);
        let mut iter = segments.iter().rev();

        // type
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let ty_name = ident.try_into()?;
        let generics = match ctxt.get_type_def(&ty_name) {
            None => bail_on!(ident, "no such type"),
            Some(def) => {
                if !matches!(def.body, TypeBody::Tuple(_)) {
                    bail_on!(ident, "not a tuple type");
                }
                &def.head
            }
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { ty_name, ty_args })
    }

    /// Build a type reference out of this path
    pub fn as_ty_ref(&self, unifier: &mut TypeUnifier) -> TypeRef {
        TypeRef::User(self.ty_name.clone(), self.ty_args.vec(unifier))
    }
}

/// An identifier to an ADT variant with optional type arguments
pub struct ADTPath {
    pub ty_name: UsrTypeName,
    pub ty_args: GenericsInstantiated,
    pub variant: String,
}

impl ADTPath {
    /// Extract a reference to an ADT branch from a path
    pub fn from_path<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);
        let mut iter = segments.iter();

        // type
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let ty_name = ident.try_into()?;
        let (generics, variants) = match ctxt.get_type_def(&ty_name) {
            None => bail_on!(ident, "no such type"),
            Some(def) => match &def.body {
                TypeBody::Enum(details) => (&def.head, &details.variants),
                _ => bail_on!(ident, "not an enum type"),
            },
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, generics, arguments)?;

        // variant
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "branch");
        let variant = ident.to_string();
        if !variants.contains_key(&variant) {
            bail_on!(ident, "no such variant");
        }
        if !matches!(arguments, PathArguments::None) {
            bail_on!(arguments, "unexpected arguments");
        }

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self {
            ty_name,
            ty_args,
            variant,
        })
    }

    /// Getter to the branch
    pub fn branch(&self) -> ADTBranch {
        ADTBranch::new(self.ty_name.clone(), self.variant.clone())
    }

    /// Build a type reference out of this path
    pub fn as_ty_ref(&self, unifier: &mut TypeUnifier) -> TypeRef {
        TypeRef::User(self.ty_name.clone(), self.ty_args.vec(unifier))
    }
}

/// An identifier to a function without a qualified type
pub struct FuncPath {
    pub fn_name: UsrFuncName,
    pub ty_args: GenericsInstantiated,
}

impl FuncPath {
    /// Extract a reference to a function from a path
    pub fn from_path<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);
        let mut iter = segments.iter();

        // func
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let fn_name = ident.try_into()?;
        let generics = match ctxt.get_func_sig(&fn_name) {
            None => bail_on!(ident, "no such function"),
            Some(sig) => &sig.generics,
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { fn_name, ty_args })
    }
}

/// An identifier for a function with type variables
pub enum QualifiedPath {
    /// `Boolean::from(<literal>)`
    CastFromBool,
    /// `Integer::from(<literal>)`
    CastFromInt,
    /// `Rational::from(<literal>)`
    CastFromFloat,
    /// `Text::from(<literal>)`
    CastFromStr,
    /// `<sys-type>::[type-inst]::<sys-func>(<args>)`
    SysFuncOnSysType(SysTypeName, GenericsInstantiated, SysFuncName),
    /// `<usr-type>::[type-inst]::<sys-func>(<args>)`
    SysFuncOnUsrType(UsrTypeName, GenericsInstantiated, SysFuncName),
    /// `<type-param>::<sys-func>(<args>)`
    SysFuncOnParamType(TypeParamName, SysFuncName),
    /// `<sys-type>::[type-inst]::<usr-func>::[type-inst](<args>)`
    UsrFuncOnSysType(
        SysTypeName,
        GenericsInstantiated,
        UsrFuncName,
        GenericsInstantiated,
    ),
    /// `<usr-type>::[type-inst]::<usr-func>::[type-inst](<args>)`
    UsrFuncOnUsrType(
        UsrTypeName,
        GenericsInstantiated,
        UsrFuncName,
        GenericsInstantiated,
    ),
}

impl QualifiedPath {
    /// Extract a reference to a qualified call from a path
    pub fn from_path<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);
        let mut iter = segments.iter();

        // type
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let ty_name = TypeName::try_from(ctxt.generics(), ident)?;
        let ty_generics = match &ty_name {
            TypeName::Sys(name) => name.generics(),
            TypeName::Usr(name) => match ctxt.get_type_def(name) {
                None => bail_on!(ident, "no such type"),
                Some(def) => def.head.clone(),
            },
            TypeName::Param(_) => Generics::intrinsic(vec![]),
        };
        let inst_for_ty = GenericsInstantiated::from_args(ctxt, &ty_generics, arguments)?;

        // func
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let fn_name = FuncName::try_from(ident)?;
        let converted = match &fn_name {
            FuncName::Cast(name) => match name {
                CastFuncName::Into => bail_on!(ident, "not allowed"),
                CastFuncName::From => {
                    if !matches!(arguments, PathArguments::None) {
                        bail_on!(arguments, "unexpected");
                    }
                    match ty_name {
                        TypeName::Sys(SysTypeName::Boolean) => Self::CastFromBool,
                        TypeName::Sys(SysTypeName::Integer) => Self::CastFromInt,
                        TypeName::Sys(SysTypeName::Rational) => Self::CastFromFloat,
                        TypeName::Sys(SysTypeName::Text) => Self::CastFromStr,
                        _ => bail_on!(ident, "not a literal type"),
                    }
                }
            },
            FuncName::Sys(name) => {
                // none of the smt-native functions take type arguments
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected");
                }
                match ty_name {
                    TypeName::Sys(sys_name) => Self::SysFuncOnSysType(sys_name, inst_for_ty, *name),
                    TypeName::Usr(usr_name) => Self::SysFuncOnUsrType(usr_name, inst_for_ty, *name),
                    TypeName::Param(param) => Self::SysFuncOnParamType(param, *name),
                }
            }
            FuncName::Usr(name) => match ty_name {
                TypeName::Param(_) => {
                    bail_on!(ident, "user-defined function on type parameter")
                }
                TypeName::Sys(ty_name) => match ctxt.lookup_usr_func_on_sys_type(&ty_name, name) {
                    None => bail_on!(ident, "no such intrinsic function"),
                    Some(fty) => {
                        let inst_for_fn =
                            GenericsInstantiated::from_args(ctxt, &fty.generics, arguments)?;
                        Self::UsrFuncOnSysType(ty_name, inst_for_ty, name.clone(), inst_for_fn)
                    }
                },
                TypeName::Usr(ty_name) => match ctxt.lookup_usr_func_on_usr_type(&ty_name, name) {
                    None => bail_on!(ident, "no such function"),
                    Some(fty) => {
                        let inst_for_fn =
                            GenericsInstantiated::from_args(ctxt, &fty.generics, arguments)?;
                        Self::UsrFuncOnUsrType(ty_name, inst_for_ty, name.clone(), inst_for_fn)
                    }
                },
            },
        };

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(converted)
    }
}
