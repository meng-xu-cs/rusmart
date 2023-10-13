use std::collections::BTreeMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Block, Expr as Exp, ExprBlock, Local, LocalInit, Pat, PatType, Result, Stmt};

use crate::parser::ctxt::ContextWithSig;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::func::FuncSig;
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::name::{UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, TypeTag};
use crate::parser::util::{ExprUtil, PatUtil};

/// Operations
enum Op {
    /// `<var>`
    Var(VarName),
}

/// Instructions (operations with type)
struct Inst {
    op: Box<Op>,
    ty: TypeRef,
}

/// Expressions
enum Expr {
    /// a single instruction
    Unit(Inst),
    /// `{ let <v1> = ...; let <v2> = ...; ...; <op>(<v1>, <v2>, ...) }`
    Block {
        lets: Vec<(VarName, Expr)>,
        body: Inst,
    },
}

impl Expr {
    /// Retrieve the type of an expression
    pub fn ty(&self) -> &TypeRef {
        let i = match self {
            Self::Unit(inst) => inst,
            Self::Block { lets: _, body } => body,
        };
        &i.ty
    }
}

/// Marks whether this expression is for impl or spec
#[derive(Copy, Clone)]
pub enum Kind {
    /// actual implementation
    Impl,
    /// formal specification
    Spec,
}

/// Root expression parser (for function body)
pub struct ExprParserRoot<'ctx> {
    /// context provider
    ctxt: &'ctx ContextWithSig,
    /// the expression is in spec or impl
    kind: Kind,
    /// function generics
    generics: Generics,
    /// function parameters
    params: BTreeMap<VarName, TypeRef>,
    /// function return type
    ret_ty: TypeRef,
    /// type unifier
    typing: TypeUnifier,
}

/// Parser stacking mechanism
enum ExprParserParent<'exp, 'p, 'ctx: 'p> {
    Root(&'p mut ExprParserRoot<'ctx>),
    Derived(&'p mut ExprParserDerived<'exp, 'p, 'ctx>),
}

/// Derived expression parser (for one and only one expression)
struct ExprParserDerived<'exp, 'p, 'ctx: 'p> {
    /// parent of the parser
    parent: ExprParserParent<'exp, 'p, 'ctx>,
    /// expression to be analyzed
    target: &'exp Exp,
    /// expected type
    exp_ty: TypeRef,
    /// variables in scope and their types
    vars: BTreeMap<VarName, TypeRef>,
    /// new let-bindings created, if any
    bindings: Vec<(VarName, Expr)>,
}

impl<'ctx> ExprParserRoot<'ctx> {
    /// Creating a new context for parsing the function body expression
    pub fn new(ctxt: &'ctx ContextWithSig, kind: Kind, sig: &FuncSig) -> Self {
        Self {
            ctxt,
            kind,
            generics: sig.generics().clone(),
            params: sig
                .param_map()
                .into_iter()
                .map(|(k, v)| (k, (&v).into()))
                .collect(),
            ret_ty: sig.ret_ty().into(),
            typing: TypeUnifier::new(),
        }
    }

    /// Parse the body of the function
    pub fn parse(mut self, stmts: &[Stmt]) -> Result<()> {
        // create an artificial expression
        let quoted_stmts: TokenStream = stmts.iter().map(|i| quote!(i)).collect();
        let target: Exp = syn::parse_quote!( { #quoted_stmts } );

        // derive a parser
        let exp_ty = self.ret_ty.clone();
        let parser = ExprParserDerived {
            parent: ExprParserParent::Root(&mut self),
            target: &target,
            exp_ty,
            vars: BTreeMap::new(),
            bindings: vec![],
        };
        parser.run()?;

        // check type completeness
        todo!()
    }
}

impl CtxtForType for ExprParserRoot<'_> {
    fn generics(&self) -> &Generics {
        &self.generics
    }

    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.ctxt.get_type_generics(name)
    }
}

impl<'exp, 'p, 'ctx: 'p> ExprParserDerived<'exp, 'p, 'ctx> {
    /// Retrieve the context
    fn root(&self) -> &'p ExprParserRoot {
        match &self.parent {
            ExprParserParent::Root(root) => root,
            ExprParserParent::Derived(derived) => derived.root(),
        }
    }

    /// Fork a child parser and convert a child expression
    fn parse(&'p mut self, target: &'exp Exp, exp_ty: TypeRef) -> Result<Expr> {
        Self {
            parent: ExprParserParent::Derived(self),
            target,
            exp_ty,
            vars: BTreeMap::new(),
            bindings: vec![],
        }
        .run()
    }

    /// Look up the type of a variable
    fn lookup_var(&self, name: &VarName) -> Option<&TypeRef> {
        match self.vars.get(name) {
            None => match &self.parent {
                ExprParserParent::Root(root) => root.params.get(name),
                ExprParserParent::Derived(derived) => derived.lookup_var(name),
            },
            Some(ty) => Some(ty),
        }
    }

    /// Create a fresh type parameter
    fn fresh_type_param(&mut self) -> TypeRef {
        match &mut self.parent {
            ExprParserParent::Root(root) => TypeRef::Var(root.typing.mk_var()),
            ExprParserParent::Derived(derived) => derived.fresh_type_param(),
        }
    }

    /// Consume a stream of statements
    fn convert_stmts(mut self, stmts: &'exp [Stmt]) -> Result<Expr> {
        let mut expr_found = None;
        let mut iter = stmts.iter();
        for stmt in iter.by_ref() {
            // parse the statement
            match stmt {
                Stmt::Local(binding) => {
                    // this is a let-binding
                    let Local {
                        attrs: _,
                        let_token: _,
                        pat,
                        init,
                        semi_token: _,
                    } = binding;

                    // find the name and type (optionally)
                    let (name, vty) = match pat {
                        Pat::Ident(_) => (PatUtil::expect_name(pat)?, None),
                        Pat::Type(pty) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = pty;
                            (
                                PatUtil::expect_name(pat)?,
                                Some(TypeTag::from_type(self.root(), ty.as_ref())?),
                            )
                        }
                        _ => bail_on!(pat, "unrecognized binding"),
                    };
                    if self.lookup_var(&name).is_some() {
                        bail_on!(pat, "name conflict");
                    }

                    // extract the body
                    let body = bail_if_missing!(init, binding, "expect initializer");
                    let LocalInit {
                        eq_token: _,
                        expr,
                        diverge,
                    } = body;
                    bail_if_exists!(diverge.as_ref().map(|(_, div)| div));

                    // parse the body with a new parser
                    let body_expr = match vty.as_ref() {
                        None => {
                            // assign a new type parameter to this variable
                            let exp_ty = self.fresh_type_param();
                            self.parse(expr, exp_ty)?
                        }
                        Some(t) => self.parse(expr, t.into())?,
                    };

                    // done, continue to next statement
                    self.vars.insert(name.clone(), body_expr.ty().clone());
                    self.bindings.push((name, body_expr));
                }
                Stmt::Expr(expr, semi_token) => {
                    // expecting a unit expression
                    bail_if_exists!(semi_token);

                    // convert the expression and mark that we have found it in the statements
                    expr_found = Some(self.convert_expr(expr)?);

                    // end of loop
                    break;
                }
                Stmt::Item(_) | Stmt::Macro(_) => bail_on!(stmt, "unexpected item"),
            }
        }

        // bail if there are more statements
        match iter.next() {
            None => (),
            Some(stmt) => {
                bail_on!(stmt, "unexpected statement after the main expression");
            }
        }

        // check that we have an expression
        match expr_found {
            None => bail_on!(
                stmts.last().expect("at least one statement"),
                "unable to find the main expression"
            ),
            Some(e) => Ok(e),
        }
    }

    /// Parse an expression
    fn convert_expr(self, target: &'exp Exp) -> Result<Expr> {
        // case on expression type
        let inst = match target {
            Exp::Path(expr_path) => {
                let name = ExprUtil::expect_name_from_path(expr_path)?;

                // look up the type
                let ty = match self.lookup_var(&name) {
                    None => bail_on!(expr_path, "unknown local variable"),
                    Some(t) => t.clone(),
                };

                Inst {
                    op: Op::Var(name).into(),
                    ty,
                }
            }
            Exp::Block(expr_block) => {
                let ExprBlock {
                    attrs: _,
                    label,
                    block:
                        Block {
                            brace_token: _,
                            stmts,
                        },
                } = expr_block;

                bail_if_exists!(label);
                return self.convert_stmts(stmts);
            }
            _ => todo!(),
        };

        // decide to go with an instruction or a block
        let converted = if self.bindings.is_empty() {
            Expr::Unit(inst)
        } else {
            Expr::Block {
                lets: self.bindings,
                body: inst,
            }
        };
        Ok(converted)
    }

    /// Consume the parser by parsing the expression assigned
    pub fn run(self) -> Result<Expr> {
        let target = self.target;
        self.convert_expr(target)
    }
}
