use syn::{Expr as Exp, ExprPath, Path, PathArguments, PathSegment, Result};

use crate::parser::adt::ADTBranch;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::func::{CastFuncName, FuncName, SysFuncName};
use crate::parser::generics::{Generics, GenericsInstFull, GenericsInstPartial};
use crate::parser::infer::TypeUnifier;
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{SysTypeName, TypeBody, TypeName};

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
            FuncName::Reserved(_) => bail_on!(ident, "reserved function"),
        };

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(converted)
    }
}

/// Marks what a path serving as a standalone expr can be
pub enum ExprPathAsTarget {
    /// a local variable
    Var(VarName),
    /// a unit variant in an enum
    EnumUnit(ADTPath),
}

impl ExprPathAsTarget {
    /// Parse from an expression
    pub fn parse<T: CtxtForExpr>(ctxt: &T, expr_path: &ExprPath) -> Result<Self> {
        let ExprPath {
            attrs: _,
            qself,
            path,
        } = expr_path;
        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));

        // case by number of path segments
        let parsed = match path.segments.len() {
            1 => Self::Var(path.try_into()?),
            2 => Self::EnumUnit(ADTPath::from_path(ctxt, path)?),
            _ => bail_on!(path, "unrecognized path"),
        };
        Ok(parsed)
    }
}

/// Marks what a path serving as a callee expr can be
pub enum ExprPathAsCallee {
    /// `<type-name>::[ty-args]::<func-name>::[ty-args](...)`
    FuncWithType(QualifiedPath),
    /// `<usr-func-name>::[ty-args](...)`
    FuncNoPrefix(FuncPath),
    /// `<adt>::[ty-args]::<variant>(...)`
    CtorEnum(ADTPath),
    /// `<tuple>::[ty-args](...)`
    CtorTuple(TuplePath),
}

impl ExprPathAsCallee {
    /// Parse from an expression
    pub fn parse<T: CtxtForExpr>(ctxt: &T, expr: &Exp) -> Result<Self> {
        let path = match expr {
            Exp::Path(p) => {
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = p;
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                path
            }
            _ => bail_on!(expr, "invalid callee"),
        };

        // case by number of path segments
        let parsed = match path.segments.len() {
            1 => {
                match (
                    TuplePath::from_path(ctxt, path),
                    FuncPath::from_path(ctxt, path),
                ) {
                    (Ok(_), Ok(_)) => bail_on!(path, "ambiguous callee"),
                    (Ok(tuple), Err(_)) => Self::CtorTuple(tuple),
                    (Err(_), Ok(func)) => Self::FuncNoPrefix(func),
                    (Err(e1), Err(e2)) => {
                        bail_on!(path, "unable to resolve callee\n{}\n{}", e1, e2);
                    }
                }
            }
            2 => {
                match (
                    ADTPath::from_path(ctxt, path),
                    QualifiedPath::from_path(ctxt, path),
                ) {
                    (Ok(_), Ok(_)) => bail_on!(path, "ambiguous callee"),
                    (Ok(adt), Err(_)) => Self::CtorEnum(adt),
                    (Err(_), Ok(qualified)) => Self::FuncWithType(qualified),
                    (Err(e1), Err(e2)) => {
                        bail_on!(path, "unable to resolve callee\n{}\n{}", e1, e2);
                    }
                }
            }
            _ => bail_on!(path, "unrecognized callee"),
        };
        Ok(parsed)
    }
}

/// Marks what a path serving as a record expr can be
pub enum ExprPathAsRecord {
    /// `<adt>::[ty-args]::<variant> { ... }`
    CtorEnum(ADTPath),
    /// `<record>::[ty-args] { ... }`
    CtorRecord(RecordPath),
}

impl ExprPathAsRecord {
    /// Parse from an expression
    pub fn parse<T: CtxtForExpr>(ctxt: &T, path: &Path) -> Result<Self> {
        // case by number of path segments
        let parsed = match path.segments.len() {
            1 => Self::CtorRecord(RecordPath::from_path(ctxt, path)?),
            2 => Self::CtorEnum(ADTPath::from_path(ctxt, path)?),
            _ => bail_on!(path, "unrecognized record"),
        };
        Ok(parsed)
    }
}
