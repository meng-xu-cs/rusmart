use proc_macro2::TokenTree;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    parse2, Expr, ExprClosure, ExprMacro, Ident, Macro, MacroDelimiter, Pat, PatType, Result,
    ReturnType, Token,
};

use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::name::{ReservedIdent, VarName};
use crate::parser::ty::TypeTag;

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

/// AST for a variable declaration in an iterative quantifiers
struct IterVar {
    ident: Ident,
    #[allow(dead_code)]
    in_token: Token![in],
    collection: Expr,
}

impl Parse for IterVar {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            in_token: input.parse()?,
            collection: input.parse()?,
        })
    }
}

/// AST for an iterative quantifiers
struct IterQuant {
    vars: Punctuated<IterVar, Token![,]>,
    #[allow(dead_code)]
    imply_token: Token![=>],
    body: Expr,
}

impl Parse for IterQuant {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            vars: input.parse_terminated(IterVar::parse, Token![,])?,
            imply_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

/// Details of a quantified operation
pub enum Quantifier {
    Typed {
        name: SysMacroName,
        vars: Vec<(VarName, TypeTag)>,
        body: Expr,
    },
    Iterated {
        name: SysMacroName,
        vars: Vec<(VarName, Expr)>,
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
        let name = SysMacroName::parse_path(path)?;

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
                let mut param_decls = vec![];
                for param in inputs {
                    match param {
                        Pat::Type(typed) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = &typed;

                            let name: VarName = pat.as_ref().try_into()?;
                            if param_decls.iter().any(|(n, _)| n == &name) {
                                bail_on!(pat, "conflicting quantifier variable name");
                            }
                            let ty = TypeTag::from_type(ctxt, ty)?;
                            param_decls.push((name, ty));
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
                    body: body.as_ref().clone(),
                }
            }
            Some(_) => {
                // foreach style
                let stream = tokens.clone();

                // parse into a custom syntax
                let syntax = parse2::<IterQuant>(stream)?;
                let IterQuant {
                    vars,
                    imply_token: _,
                    body,
                } = syntax;

                // variables
                let mut var_decls = vec![];
                for var in vars {
                    let IterVar {
                        ident,
                        in_token: _,
                        collection,
                    } = var;

                    let name: VarName = (&ident).try_into()?;
                    if var_decls.iter().any(|(n, _)| n == &name) {
                        bail_on!(&ident, "conflicting quantifier variable name");
                    }
                    var_decls.push((name, collection));
                }

                // done with parsing
                Self::Iterated {
                    name,
                    vars: var_decls,
                    body,
                }
            }
        };
        Ok(parsed)
    }
}
