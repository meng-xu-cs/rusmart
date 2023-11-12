use std::collections::BTreeMap;

use syn::{Pat, PatTuple, PatType, Result};

use crate::parser::err::bail_on;
use crate::parser::expr::{CtxtForExpr, VarDecl};
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::VarName;
use crate::parser::ty::TypeTag;

/// Variable declaration
pub struct LetDecl {
    pub vars: BTreeMap<VarName, TypeRef>,
    pub decl: VarDecl,
}

impl LetDecl {
    /// Parse a pattern for declaration only
    fn parse_decl(
        vars: &mut BTreeMap<VarName, TypeRef>,
        unifier: &mut TypeUnifier,
        pat: &Pat,
        ety: Option<&TypeTag>,
    ) -> Result<VarDecl> {
        let decl = match pat {
            Pat::Ident(ident) => {
                let name: VarName = pat.try_into()?;
                let ty = match ety {
                    None => TypeRef::Var(unifier.mk_var()),
                    Some(t) => {
                        if matches!(t, TypeTag::Pack(_)) {
                            bail_on!(ident, "expect a non-pack type");
                        }
                        t.into()
                    }
                };
                if vars.insert(name.clone(), ty.clone()).is_some() {
                    bail_on!(ident, "duplicated name");
                }
                VarDecl::One(name, ty)
            }
            Pat::Tuple(PatTuple {
                attrs: _,
                paren_token: _,
                elems,
            }) => {
                let mut decls = vec![];
                match ety {
                    None => {
                        for item in elems {
                            let sub = Self::parse_decl(vars, unifier, item, None)?;
                            decls.push(sub);
                        }
                    }
                    Some(TypeTag::Pack(pack)) => {
                        if pack.len() != elems.len() {
                            bail_on!(elems, "pack size mismatch");
                        }
                        for (e, t) in elems.iter().zip(pack) {
                            let sub = Self::parse_decl(vars, unifier, e, Some(t))?;
                            decls.push(sub);
                        }
                    }
                    _ => bail_on!(elems, "expect a pack type"),
                }
                VarDecl::Pack(decls)
            }
            _ => bail_on!(pat, "unrecognized pattern"),
        };
        Ok(decl)
    }

    /// Parse a pattern for declarations and potentially the type
    pub fn parse<T: CtxtForExpr>(ctxt: &T, unifier: &mut TypeUnifier, pat: &Pat) -> Result<Self> {
        // separate types and names
        let (pat, ty_opt) = match pat {
            Pat::Type(PatType {
                attrs: _,
                pat,
                colon_token: _,
                ty,
            }) => (pat.as_ref(), Some(TypeTag::from_type(ctxt, ty.as_ref())?)),
            _ => (pat, None),
        };

        // parse them recursively
        let mut vars = BTreeMap::new();
        let decl = Self::parse_decl(&mut vars, unifier, pat, ty_opt.as_ref())?;
        Ok(Self { vars, decl })
    }
}
