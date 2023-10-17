use std::collections::BTreeMap;

use syn::{Path, PathArguments, PathSegment, Result};

use crate::parser::adt::ADTBranch;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::func::{FuncName, SysFuncName};
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
        let ty_params = generics.vec();
        let ty_args = match ty_args_opt {
            None => ty_params
                .into_iter()
                .enumerate()
                .map(|(i, n)| (n, (i, None)))
                .collect(),
            Some(tags) => {
                if tags.len() != ty_params.len() {
                    return None;
                }
                ty_params
                    .into_iter()
                    .zip(tags)
                    .enumerate()
                    .map(|(i, (n, t))| (n, (i, Some(t.clone()))))
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
        let ty_params = generics.vec();
        let ty_args = match arguments {
            PathArguments::None => ty_params
                .into_iter()
                .enumerate()
                .map(|(i, n)| (n, (i, None)))
                .collect(),
            PathArguments::AngleBracketed(pack) => {
                let ty_args = TypeTag::from_args(ctxt, pack)?;
                if ty_args.len() != ty_params.len() {
                    bail_on!(pack, "type argument number mismatch");
                }
                ty_params
                    .into_iter()
                    .zip(ty_args)
                    .enumerate()
                    .map(|(i, (n, t))| (n, (i, Some(t))))
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
#[derive(Clone)]
pub struct TuplePath {
    ty_name: UsrTypeName,
    ty_args: GenericsInstantiated,
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
                if !matches!(def.body(), TypeBody::Tuple(_)) {
                    bail_on!(ident, "not a tuple type");
                }
                def.head()
            }
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { ty_name, ty_args })
    }

    /// Getter to the branch
    pub fn ty_name(&self) -> &UsrTypeName {
        &self.ty_name
    }

    /// Getter to the type arguments
    pub fn ty_args(&self) -> &GenericsInstantiated {
        &self.ty_args
    }

    /// Build a type reference out of this path
    pub fn as_ty_ref(&self, unifier: &mut TypeUnifier) -> TypeRef {
        TypeRef::User(self.ty_name.clone(), self.ty_args.vec(unifier))
    }
}

/// An identifier to an ADT variant with optional type arguments
#[derive(Clone)]
pub struct ADTPath {
    ty_name: UsrTypeName,
    ty_args: GenericsInstantiated,
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
            Some(def) => match def.body() {
                TypeBody::Enum(details) => (def.head(), details.variants()),
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

    /// Getter to the type arguments
    pub fn ty_args(&self) -> &GenericsInstantiated {
        &self.ty_args
    }

    /// Build a type reference out of this path
    pub fn as_ty_ref(&self, unifier: &mut TypeUnifier) -> TypeRef {
        TypeRef::User(self.ty_name.clone(), self.ty_args.vec(unifier))
    }
}

/// An identifier to a function with optional type arguments
#[derive(Clone)]
pub struct FuncPath {
    fn_name: UsrFuncName,
    ty_args: GenericsInstantiated,
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
        let generics = match ctxt.get_func_sig(&fn_name, ctxt.kind()) {
            None => bail_on!(ident, "no such function"),
            Some(sig) => sig.generics(),
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self { fn_name, ty_args })
    }

    /// Getter to the function name
    pub fn fn_name(&self) -> &UsrFuncName {
        &self.fn_name
    }

    /// Getter to the type arguments
    pub fn ty_args(&self) -> &GenericsInstantiated {
        &self.ty_args
    }
}

/// An identifier for a function with type variables
#[derive(Clone)]
pub struct QualifiedPath {
    ty_name: TypeName,
    fn_name: FuncName,
    ty_args: GenericsInstantiated,
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
                Some(def) => def.head().clone(),
            },
            TypeName::Param(_) => Generics::intrinsic(vec![]),
        };
        if !matches!(arguments, PathArguments::None) {
            bail_on!(arguments, "unexpected type arguments");
        }

        // func
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type name");
        let fn_name = FuncName::try_from(ident)?;
        let fn_generics = match &fn_name {
            FuncName::Sys(name) => match name {
                SysFuncName::Into => bail_on!(ident, "not allowed"),
                SysFuncName::From => {
                    if !matches!(
                        ty_name,
                        TypeName::Sys(SysTypeName::Boolean)
                            | TypeName::Sys(SysTypeName::Integer)
                            | TypeName::Sys(SysTypeName::Rational)
                            | TypeName::Sys(SysTypeName::Text)
                    ) {
                        bail_on!(ident, "not a literal type");
                    }
                    // does not take any type arguments
                    Generics::intrinsic(vec![])
                }
                SysFuncName::Eq | SysFuncName::Ne => ty_generics,
            },
            FuncName::Usr(name) => match ctxt.get_func_sig(name, ctxt.kind()) {
                None => bail_on!(ident, "no such function"),
                Some(sig) => sig.generics().clone(),
            },
        };
        let ty_args = GenericsInstantiated::from_args(ctxt, &fn_generics, arguments)?;

        // ensure that there are no more tokens
        bail_if_exists!(iter.next());
        Ok(Self {
            ty_name,
            fn_name,
            ty_args,
        })
    }

    /// Getter to the type name
    pub fn ty_name(&self) -> &TypeName {
        &self.ty_name
    }

    /// Getter to the func name
    pub fn fn_name(&self) -> &FuncName {
        &self.fn_name
    }

    /// Getter to the type arguments
    pub fn ty_args(&self) -> &GenericsInstantiated {
        &self.ty_args
    }
}
