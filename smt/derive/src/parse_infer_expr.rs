use std::collections::BTreeMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Expr as Exp, Result, Stmt};

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
    params: BTreeMap<VarName, TypeTag>,
    /// function return type
    ret_ty: TypeTag,
    /// type unifier
    typing: &'ty mut TypeUnifier,
}

/// Derived expression parser (for one and only one expression)
enum ExprParserParent<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> {
    Root(&'p ExprParserRoot<'ty, 'ctx, T>),
    Derived(&'p ExprParserDerived<'exp, 'p, 'ty, 'ctx, T>),
}

/// Derived expression parser (for one and only one expression)
pub struct ExprParserDerived<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> {
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
            params: sig.param_map(),
            ret_ty: sig.ret_ty().clone(),
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
            exp_ty: (&self.ret_ty).into(),
            vars: BTreeMap::new(),
            bindings: vec![],
        };
        parser.run()?;

        // check type completeness
        todo!()
    }
}

impl<'exp, 'p, 'ty: 'p, 'ctx: 'p, T: CtxtForExpr> ExprParserDerived<'exp, 'p, 'ty, 'ctx, T> {
    /// parse the expression assigned
    fn run(self) -> Result<Expr<'ty>> {
        todo!()
    }
}
