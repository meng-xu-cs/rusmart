use std::collections::BTreeMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Block, Expr as Exp, ExprBlock, Local, LocalInit, Pat, PatType, Result, Stmt};

use crate::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parse_func::FuncSig;
use crate::parse_infer::{TypeUnifier, TypeVar};
use crate::parse_path::{FuncName, TypeName, VarName};
use crate::parse_type::{CtxtForType, TypeDef, TypeTag};

/// A context suitable for expression analysis with type inference
pub trait CtxtForExpr: CtxtForType {
    /// Retrieve the type definition
    fn get_type(&self, name: &TypeName) -> Option<&TypeDef>;

    /// Retrieve the function signature for impl
    fn get_impl_sig(&self, name: &FuncName) -> Option<&FuncSig>;

    /// Retrieve the function signature for spec
    fn get_spec_sig(&self, name: &FuncName) -> Option<&FuncSig>;
}

/// Phi node, guarded by condition
struct PhiNode<'ty> {
    cond: Expr<'ty>,
    body: Expr<'ty>,
}

/// Operations
enum Op<'ty> {
    /// `<var>`
    Var(VarName),
    /// `match (v1, v2, ...) { (a1, a2, ...) => <body1> } ...`
    /// TODO
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi {
        nodes: Vec<PhiNode<'ty>>,
        default: Expr<'ty>,
    },
    /// `forall(|<v>: <t>| {<expr>})`
    Forall {
        vars: BTreeMap<VarName, TypeTag>,
        body: Expr<'ty>,
    },
    /// `exists(|<v>: <t>| {<expr>})`
    Exists {
        vars: BTreeMap<VarName, TypeTag>,
        body: Expr<'ty>,
    },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    /// TODO
    /// `<function>(<a1>, <a2>, ...)`
    Procedure {
        name: FuncName,
        args: Vec<Expr<'ty>>,
    },
}

/// Instructions (operations with type)
struct Inst<'ty> {
    op: Box<Op<'ty>>,
    ty: TypeVar<'ty>,
}

/// Expressions
enum Expr<'ty> {
    /// a single instruction
    Unit(Inst<'ty>),
    /// `{ let <v1> = ...; let <v2> = ...; ...; <op>(<v1>, <v2>, ...) }`
    Block {
        lets: Vec<(VarName, Expr<'ty>)>,
        body: Inst<'ty>,
    },
}

impl<'ty> Expr<'ty> {
    /// Retrieve the type of an expression
    pub fn ty(&self) -> &TypeVar<'ty> {
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
pub struct ExprParserRoot<'ty, 'ctx, T: CtxtForExpr> {
    /// context provider
    ctxt: &'ctx T,
    /// the expression is in spec or impl
    kind: Kind,
    /// function parameters
    params: BTreeMap<VarName, TypeVar<'static>>,
    /// function return type
    ret_ty: TypeVar<'static>,
    /// type unifier
    typing: &'ty TypeUnifier,
}

/// Derived expression parser (for one and only one expression)
enum ExprParserParent<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> {
    Root(&'p ExprParserRoot<'ty, 'ctx, T>),
    Derived(&'p ExprParserDerived<'exp, 'p, 'ty, 'ctx, T>),
}

/// Derived expression parser (for one and only one expression)
struct ExprParserDerived<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> {
    /// parent of the parser
    parent: ExprParserParent<'exp, 'p, 'ty, 'ctx, T>,
    /// expression to be analyzed
    target: &'exp Exp,
    /// expected type
    exp_ty: TypeVar<'ty>,
    /// variables in scope and their types
    vars: BTreeMap<VarName, TypeVar<'ty>>,
    /// new let-bindings created, if any
    bindings: Vec<(VarName, Expr<'ty>)>,
}

impl<'ty, 'ctx, T: CtxtForExpr> ExprParserRoot<'ty, 'ctx, T> {
    /// Creating a new context for parsing the function body expression
    pub fn new(ctxt: &'ctx T, typing: &'ty mut TypeUnifier, kind: Kind, sig: &FuncSig) -> Self {
        Self {
            ctxt,
            kind,
            params: sig
                .param_map()
                .into_iter()
                .map(|(k, v)| (k, (&v).into()))
                .collect(),
            ret_ty: sig.ret_ty().into(),
            typing,
        }
    }

    /// Parse the body of the function
    pub fn parse(self, stmts: &[Stmt]) -> Result<()> {
        // create an artificial expression
        let quoted_stmts: TokenStream = stmts.iter().map(|i| quote!(i)).collect();
        let target: Exp = syn::parse_quote!( { #quoted_stmts } );

        // derive a parser
        let parser = ExprParserDerived {
            parent: ExprParserParent::Root(&self),
            target: &target,
            exp_ty: self.ret_ty.clone(),
            vars: BTreeMap::new(),
            bindings: vec![],
        };
        parser.run()?;

        // check type completeness
        todo!()
    }
}

impl<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> ExprParserDerived<'exp, 'p, 'ty, 'ctx, T> {
    /// Retrieve the context
    fn ctxt(&self) -> &'ctx T {
        match self.parent {
            ExprParserParent::Root(root) => root.ctxt,
            ExprParserParent::Derived(derived) => derived.ctxt(),
        }
    }

    /// Fork a child parser and convert a child expression
    fn parse(&'p self, target: &'exp Exp, exp_ty: TypeVar<'ty>) -> Result<Expr<'ty>> {
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
    fn lookup_var(&self, name: &VarName) -> Option<&TypeVar<'ty>> {
        match self.vars.get(name) {
            None => match self.parent {
                ExprParserParent::Root(root) => root.params.get(name),
                ExprParserParent::Derived(derived) => derived.lookup_var(name),
            },
            Some(ty) => Some(ty),
        }
    }

    /// Create a fresh type parameter
    fn fresh_type_param(&self) -> TypeVar<'ty> {
        match self.parent {
            ExprParserParent::Root(root) => root.typing.mk_param(),
            ExprParserParent::Derived(derived) => derived.fresh_type_param(),
        }
    }

    /// Consume a stream of statements
    fn convert_stmts(mut self, stmts: &'exp [Stmt]) -> Result<Expr<'ty>> {
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
                        Pat::Ident(_) => (VarName::from_pat(pat)?, None),
                        Pat::Type(pty) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = pty;
                            (
                                VarName::from_pat(pat)?,
                                Some(TypeTag::from_type(self.ctxt(), ty.as_ref())?),
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
    fn convert_expr(self, target: &Exp) -> Result<Expr<'ty>> {
        // case on expression type
        let inst = match target {
            Exp::Path(expr_path) => {
                let name = VarName::from_expr_path(expr_path)?;

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
    pub fn run(self) -> Result<Expr<'ty>> {
        let target = self.target;
        self.convert_expr(target)
    }
}
