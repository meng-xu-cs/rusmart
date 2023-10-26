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
    /// Retrieve an argument
    pub fn get(&self, name: &TypeParamName) -> Option<&TypeRef> {
        self.args.get(name).map(|(_, t)| t)
    }

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
}

/// An identifier to a tuple with optional type arguments
pub struct TuplePath {
    ty_name: UsrTypeName,
    ty_args: GenericsInstPartial,
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
        let ty_args = GenericsInstPartial::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { ty_name, ty_args })
    }

    /// Turn the path into a name and full instantiation
    pub fn complete(self, unifier: &mut TypeUnifier) -> (UsrTypeName, GenericsInstFull) {
        let Self { ty_name, ty_args } = self;
        (ty_name, ty_args.complete(unifier))
    }
}

/// An identifier to a record with optional type arguments
pub struct RecordPath {
    ty_name: UsrTypeName,
    ty_args: GenericsInstPartial,
}

impl RecordPath {
    /// Extract a reference to a record from a path
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
                if !matches!(def.body, TypeBody::Record(_)) {
                    bail_on!(ident, "not a record type");
                }
                &def.head
            }
        };
        let ty_args = GenericsInstPartial::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { ty_name, ty_args })
    }

    /// Turn the path into a name and full instantiation
    pub fn complete(self, unifier: &mut TypeUnifier) -> (UsrTypeName, GenericsInstFull) {
        let Self { ty_name, ty_args } = self;
        (ty_name, ty_args.complete(unifier))
    }
}

/// An identifier to an ADT variant with optional type arguments
pub struct ADTPath {
    ty_name: UsrTypeName,
    ty_args: GenericsInstPartial,
    variant: String,
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
        let ty_args = GenericsInstPartial::from_args(ctxt, generics, arguments)?;

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

    /// Turn the path into a branch and full instantiation
    pub fn complete(self, unifier: &mut TypeUnifier) -> (ADTBranch, GenericsInstFull) {
        let Self {
            ty_name,
            ty_args,
            variant,
        } = self;
        (ADTBranch { ty_name, variant }, ty_args.complete(unifier))
    }
}

/// An identifier to a function without a qualified type
pub struct FuncPath {
    fn_name: UsrFuncName,
    ty_args: GenericsInstPartial,
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
        let generics = match ctxt.lookup_unqualified(&fn_name) {
            None => bail_on!(ident, "no such function"),
            Some(fty) => &fty.generics,
        };
        let ty_args = GenericsInstPartial::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { fn_name, ty_args })
    }

    /// Turn the path into a name and full instantiation
    pub fn complete(self, unifier: &mut TypeUnifier) -> (UsrFuncName, GenericsInstFull) {
        let Self { fn_name, ty_args } = self;
        (fn_name, ty_args.complete(unifier))
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
    SysFuncOnSysType(SysTypeName, GenericsInstPartial, SysFuncName),
    /// `<usr-type>::[type-inst]::<sys-func>(<args>)`
    SysFuncOnUsrType(UsrTypeName, GenericsInstPartial, SysFuncName),
    /// `<type-param>::<sys-func>(<args>)`
    SysFuncOnParamType(TypeParamName, SysFuncName),
    /// `<sys-type>::[type-inst]::<usr-func>::[type-inst](<args>)`
    UsrFuncOnSysType(
        SysTypeName,
        GenericsInstPartial,
        UsrFuncName,
        GenericsInstPartial,
    ),
    /// `<usr-type>::[type-inst]::<usr-func>::[type-inst](<args>)`
    UsrFuncOnUsrType(
        UsrTypeName,
        GenericsInstPartial,
        UsrFuncName,
        GenericsInstPartial,
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
        let inst_for_ty = GenericsInstPartial::from_args(ctxt, &ty_generics, arguments)?;

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
                            GenericsInstPartial::from_args(ctxt, &fty.generics, arguments)?;
                        Self::UsrFuncOnSysType(ty_name, inst_for_ty, name.clone(), inst_for_fn)
                    }
                },
                TypeName::Usr(ty_name) => match ctxt.lookup_usr_func_on_usr_type(&ty_name, name) {
                    None => bail_on!(ident, "no such function"),
                    Some(fty) => {
                        let inst_for_fn =
                            GenericsInstPartial::from_args(ctxt, &fty.generics, arguments)?;
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
