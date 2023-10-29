use std::collections::BTreeMap;

use proc_macro2::TokenTree;
use syn::{
    parse2, Expr, ExprClosure, ExprMacro, Macro, MacroDelimiter, Pat, PatType, Result, ReturnType,
};

use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::name::{ReservedIdent, VarName};
use crate::parser::ty::TypeTag;
use crate::parser::util::{PatUtil, PathUtil};

/// Reserved macro name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum SysMacroName {
    Exists,
    Forall,
    Choose,
}

impl ReservedIdent for SysMacroName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "exists" => Self::Exists,
            "forall" => Self::Forall,
            "choose" => Self::Choose,
            _ => return None,
        };
        Some(matched)
    }
}

/// Details of a quantified operation
pub enum Quantifier {
    Typed {
        name: SysMacroName,
        vars: BTreeMap<VarName, TypeTag>,
        body: Box<Expr>,
    },
    Iterated {
        name: SysMacroName,
        vars: BTreeMap<VarName, Expr>,
        body: Expr,
    },
}

impl Quantifier {
    /// Parse a token stream
    pub fn parse<T: CtxtForExpr>(ctxt: &T, expr: &ExprMacro) -> Result<Self> {
        let ExprMacro {
            attrs: _,
            mac:
                Macro {
                    path,
                    bang_token: _,
                    delimiter,
                    tokens,
                },
        } = expr;
        if !matches!(delimiter, MacroDelimiter::Paren(_)) {
            bail_on!(expr, "expect macro invocation with parenthesis");
        }

        // basics
        let name = PathUtil::expect_ident_reserved(path)?;

        // check the macro style
        let tester = tokens.clone();
        let parsed = match tester.into_iter().next() {
            None => bail_on!(expr, "expect content"),
            Some(TokenTree::Punct(punct)) if punct.as_char() == '|' => {
                // closure style
                let stream = tokens.clone();

                // parse into a closure
                let closure = parse2::<ExprClosure>(stream)?;
                let ExprClosure {
                    attrs: _,
                    lifetimes,
                    constness,
                    movability,
                    asyncness,
                    capture,
                    or1_token: _,
                    inputs,
                    or2_token: _,
                    output,
                    body,
                } = closure;
                bail_if_exists!(lifetimes);
                bail_if_exists!(constness);
                bail_if_exists!(movability);
                bail_if_exists!(asyncness);
                bail_if_exists!(capture);

                // parameters
                let mut param_decls = BTreeMap::new();
                for param in inputs {
                    match param {
                        Pat::Type(typed) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = &typed;

                            let name = PatUtil::expect_name(pat)?;
                            if param_decls.contains_key(&name) {
                                bail_on!(pat, "conflicting quantifier variable name");
                            }
                            let ty = TypeTag::from_type(ctxt, ty)?;
                            param_decls.insert(name, ty);
                        }
                        _ => bail_on!(param, "invalid quantifier variable declaration"),
                    }
                }

                // expect no return type
                match output {
                    ReturnType::Default => (),
                    ReturnType::Type(_, rty) => bail_on!(rty, "unexpected return type"),
                };

                // done with parsing
                Self::Typed {
                    name,
                    vars: param_decls,
                    body,
                }
            }
            Some(_) => {
                // foreach style
                todo!()
            }
        };
        Ok(parsed)
    }
}
