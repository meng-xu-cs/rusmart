use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::TokenStream;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parse2, Arm, Block, Expr as Exp, ExprBlock, ExprCall, ExprClosure, ExprIf, ExprMacro,
    ExprMatch, ExprMethodCall, ExprTuple, ExprUnary, Local, LocalInit, Macro, MacroDelimiter, Pat,
    PatTuple, PatType, Result, ReturnType, Stmt, UnOp,
};

use crate::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parse_expr_intrinsic::Intrinsic;
use crate::parse_expr_match::{MatchAnalyzer, MatchCombo};
use crate::parse_func::FuncSig;
use crate::parse_path::{
    CallTarget, CallTargetGuess, FuncName, QuantifierMacro, TypeName, VarName,
};
use crate::parse_type::{CtxtForType, TypeDef, TypeTag};

/// A context suitable for expression analysis
pub trait CtxtForExpr: CtxtForType {
    /// Retrieve the type definition
    fn get_type(&self, name: &TypeName) -> Option<&TypeDef>;

    /// Retrieve the function signature for impl
    fn get_impl_sig(&self, name: &FuncName) -> Option<&FuncSig>;

    /// Retrieve the function signature for spec
    fn get_spec_sig(&self, name: &FuncName) -> Option<&FuncSig>;
}

/// Phi node, guarded by condition
#[derive(Clone)]
pub struct PhiNode {
    cond: Expr,
    body: Expr,
}

/// Operations
#[derive(Clone)]
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `match (v1, v2, ...) { (a1, a2, ...) => <body1> } ...`
    Match {
        heads: Vec<Expr>,
        combo: Vec<MatchCombo>,
    },
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi { nodes: Vec<PhiNode>, default: Expr },
    /// `forall(|<v>: <t>| {<expr>})`
    Forall {
        vars: BTreeMap<VarName, TypeTag>,
        body: Expr,
    },
    /// `exists(|<v>: <t>| {<expr>})`
    Exists {
        vars: BTreeMap<VarName, TypeTag>,
        body: Expr,
    },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
    /// `<function>(<a1>, <a2>, ...)`
    Procedure { name: FuncName, args: Vec<Expr> },
}

/// Instructions (operations with type)
#[derive(Clone)]
pub struct Inst {
    op: Box<Op>,
    ty: TypeTag,
}

/// Expressions
#[derive(Clone)]
pub enum Expr {
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
    pub fn ty(&self) -> &TypeTag {
        let i = match self {
            Self::Unit(inst) => inst,
            Self::Block { lets: _, body } => body,
        };
        &i.ty
    }

    /// Extract one from impl body
    pub fn from_impl<T: CtxtForExpr>(ctxt: &T, sig: &FuncSig, stmt: &[Stmt]) -> Result<Self> {
        ExprParseCtxt::new(ctxt, Kind::Impl, sig).convert_stmts(stmt)
    }

    /// Extract one from spec body
    pub fn from_spec<T: CtxtForExpr>(ctxt: &T, sig: &FuncSig, stmt: &[Stmt]) -> Result<Self> {
        ExprParseCtxt::new(ctxt, Kind::Spec, sig).convert_stmts(stmt)
    }
}

/// Marks whether this expression is for impl or spec
#[derive(Copy, Clone)]
enum Kind {
    /// actual implementation
    Impl,
    /// formal specification
    Spec,
}

/// A parser for one and only one expression
pub struct ExprParseCtxt<'a, T: CtxtForExpr> {
    /// context provider
    ctxt: &'a T,
    /// the expression is in spec or impl
    kind: Kind,
    /// the expected type of this expression
    expected: Option<TypeTag>,
    /// variables in scope and their types
    vars: BTreeMap<VarName, TypeTag>,
    /// new let-bindings created, if any
    bindings: Vec<(VarName, Expr)>,
}

impl<'a, T: CtxtForExpr> ExprParseCtxt<'a, T> {
    /// Creating a new context for parsing the function body expression
    fn new(ctxt: &'a T, kind: Kind, sig: &FuncSig) -> Self {
        Self {
            ctxt,
            kind,
            vars: sig.param_map(),
            expected: Some(sig.ret_ty().clone()),
            bindings: vec![],
        }
    }

    /// Duplicate a context for parsing sub-expressions
    pub fn dup(&self, ety: Option<TypeTag>) -> Self {
        Self {
            ctxt: self.ctxt,
            kind: self.kind,
            vars: self.vars.clone(),
            expected: ety,
            bindings: vec![],
        }
    }

    /// Getter to the expected type
    pub fn expected_type(&self) -> Option<&TypeTag> {
        self.expected.as_ref()
    }

    /// Get the callee signature
    fn get_func_sig(&self, name: &FuncName) -> Option<&FuncSig> {
        match self.kind {
            Kind::Impl => self.ctxt.get_impl_sig(name),
            Kind::Spec => self
                .ctxt
                .get_spec_sig(name)
                .or_else(|| self.ctxt.get_impl_sig(name)),
        }
    }

    /// Consume the stream of statements
    fn convert_stmts(mut self, stmts: &[Stmt]) -> Result<Expr> {
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
                                Some(TypeTag::from_type(self.ctxt, ty.as_ref())?),
                            )
                        }
                        _ => bail_on!(pat, "unrecognized binding"),
                    };
                    if self.vars.contains_key(&name) {
                        bail_on!(pat, "name conflict");
                    }

                    // parse the body
                    let body = bail_if_missing!(init, binding, "expect initializer");
                    let LocalInit {
                        eq_token: _,
                        expr,
                        diverge,
                    } = body;
                    bail_if_exists!(diverge.as_ref().map(|(_, div)| div));

                    let parser = self.dup(vty);
                    let body_expr = parser.convert_expr(expr)?;

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

    /// Convert an expression
    pub fn convert_expr(self, expr: &Exp) -> Result<Expr> {
        // case on expression type
        let inst = match expr {
            Exp::Path(expr_path) => {
                let name = VarName::from_expr_path(expr_path)?;

                // look up the type
                let ty = match self.vars.get(&name) {
                    None => bail_on!(expr_path, "unknown local variable"),
                    Some(t) => t.clone(),
                };

                // construct and return the expression
                let inst = Inst {
                    op: Op::Var(name).into(),
                    ty,
                };
                return Ok(Expr::Unit(inst));
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
            Exp::If(expr_if) => {
                let mut phi_nodes = vec![];
                let mut cursor = expr_if;
                let default_expr = loop {
                    let ExprIf {
                        attrs: _,
                        if_token: _,
                        cond,
                        then_branch,
                        else_branch,
                    } = cursor;

                    // convert the condition
                    let new_cond = match cond.as_ref() {
                        Exp::Unary(expr_unary) => {
                            let ExprUnary { attrs: _, op, expr } = expr_unary;
                            if !matches!(op, UnOp::Deref(_)) {
                                bail_on!(cond, "not a Boolean deref");
                            }
                            self.dup(Some(TypeTag::Boolean)).convert_expr(expr)?
                        }
                        _ => bail_on!(cond, "not a Boolean deref"),
                    };

                    // convert the then block
                    let new_then = self
                        .dup(self.expected.clone())
                        .convert_stmts(&then_branch.stmts)?;

                    // now we have a node
                    phi_nodes.push(PhiNode {
                        cond: new_cond,
                        body: new_then,
                    });

                    // convert the else block
                    let else_expr = bail_if_missing!(
                        else_branch.as_ref().map(|(_, e)| e.as_ref()),
                        expr_if,
                        "expect else branch"
                    );

                    match else_expr {
                        Exp::If(elseif_expr) => {
                            cursor = elseif_expr;
                        }
                        _ => {
                            break self.dup(self.expected.clone()).convert_expr(else_expr)?;
                        }
                    }
                };

                // check consistency of body type
                let ref_ty = default_expr.ty().clone();
                for item in &phi_nodes {
                    if item.body.ty() != &ref_ty {
                        bail_on!(expr_if, "phi node type mismatch");
                    }
                }

                // construct the operator
                let op = Op::Phi {
                    nodes: phi_nodes,
                    default: default_expr,
                };
                Inst {
                    op: op.into(),
                    ty: ref_ty,
                }
            }
            Exp::Match(expr_match) => {
                let ExprMatch {
                    attrs: _,
                    match_token: _,
                    expr: base,
                    brace_token: _,
                    arms,
                } = expr_match;

                // collects heads of the match case
                let (is_tuple, head_exprs) = match base.as_ref() {
                    Exp::Tuple(expr_tuple) => {
                        let ExprTuple {
                            attrs: _,
                            paren_token: _,
                            elems,
                        } = expr_tuple;
                        (true, elems.iter().collect::<Vec<_>>())
                    }
                    _ => (false, vec![base.as_ref()]),
                };

                let mut heads = vec![];
                let mut heads_options = vec![];
                for elem in head_exprs {
                    let head = self.dup(None).convert_expr(elem)?;
                    let (adt_name, adt_variants): (_, BTreeSet<_>) = match head.ty() {
                        TypeTag::User(name) => match self.ctxt.get_type(name) {
                            None => bail_on!(elem, "no such type"),
                            Some(def) => match def {
                                TypeDef::Algebraic(adt) => {
                                    (name.clone(), adt.variants().keys().cloned().collect())
                                }
                                _ => bail_on!(elem, "not an ADT on match head"),
                            },
                        },
                        _ => bail_on!(elem, "not a user-defined type"),
                    };
                    heads.push(head);
                    heads_options.push((adt_name, adt_variants));
                }

                // go over the arms
                let mut analyzer = MatchAnalyzer::new(self.ctxt);
                for arm in arms {
                    let Arm {
                        attrs: _,
                        pat,
                        guard,
                        fat_arrow_token: _,
                        body,
                        comma: _,
                    } = arm;
                    bail_if_exists!(guard.as_ref().map(|(if_token, _)| if_token));

                    // find the bindings
                    let elem_pats = if is_tuple {
                        match pat {
                            Pat::Tuple(pat_tuple) => {
                                let PatTuple {
                                    attrs: _,
                                    paren_token: _,
                                    elems,
                                } = pat_tuple;
                                elems.iter().collect()
                            }
                            _ => bail_on!(pat, "expect tuple"),
                        }
                    } else {
                        vec![pat]
                    };

                    let mut atoms = vec![];
                    let mut bindings = BTreeMap::new();
                    for elem in elem_pats {
                        let (atom, partial) = analyzer.analyze_pat_match_head(elem)?;
                        for (var_name, var_ty) in partial {
                            if bindings.insert(var_name, var_ty).is_some() {
                                bail_on!(elem, "naming conflict");
                            }
                        }
                        atoms.push(atom);
                    }

                    // use the new bindings to analyze the body
                    let mut new_ctxt = self.dup(self.expected.clone());
                    for (var_name, var_ty) in bindings {
                        if new_ctxt.vars.insert(var_name, var_ty).is_some() {
                            bail_on!(pat, "naming conflict");
                        }
                    }
                    let body_expr = new_ctxt.convert_expr(body)?;

                    // save the match arm
                    analyzer.add_arm(atoms, body_expr);
                }

                // return the expression
                let (combo, ty) = analyzer.into_organized(expr_match, &heads_options)?;
                let op = Op::Match { heads, combo };
                Inst { op: op.into(), ty }
            }
            Exp::Call(expr_call) => {
                // extract the callee
                let ExprCall {
                    attrs: _,
                    func,
                    paren_token: _,
                    args,
                } = expr_call;
                let callee = CallTarget::from_expr(func)?;

                // decide on user or intrinsic function
                let (op, ty) = match callee.as_guess() {
                    CallTargetGuess::Usr(name) => {
                        // user-defined function
                        let callee_sig = match self.get_func_sig(name) {
                            None => bail_on!(func, "no such function"),
                            Some(sig) => sig,
                        };

                        // check return type
                        let rty = callee_sig.ret_ty();
                        match self.expected_type() {
                            None => (),
                            Some(ety) => {
                                if ety != rty {
                                    bail_on!(func, "return type mismatch");
                                }
                            }
                        }

                        // parse arguments
                        let params = callee_sig.param_vec();
                        if args.len() != params.len() {
                            bail_on!(args, "argument number mismatch");
                        }

                        let mut new_args = vec![];
                        for (arg_expr, (_, arg_ty)) in args.iter().zip(params.iter()) {
                            let converted =
                                self.dup(Some(arg_ty.clone())).convert_expr(arg_expr)?;
                            new_args.push(converted);
                        }
                        (
                            Op::Procedure {
                                name: name.clone(),
                                args: new_args,
                            },
                            rty.clone(),
                        )
                    }
                    CallTargetGuess::Sys(class, method) => {
                        let (intrinsic, ty) =
                            Intrinsic::convert_and_infer_type(&self, class, method, func, args)?;
                        (Op::Intrinsic(intrinsic), ty)
                    }
                };

                // pack into expression
                Inst { op: op.into(), ty }
            }
            Exp::MethodCall(expr_method) => {
                let ExprMethodCall {
                    attrs: _,
                    receiver,
                    dot_token: _,
                    method,
                    turbofish,
                    paren_token: _,
                    args,
                } = expr_method;
                bail_if_exists!(turbofish);

                // first check whether this is literal conversion
                if method.to_string().as_str() == "into" {
                    if !args.is_empty() {
                        bail_on!(args, "unexpected arguments");
                    }
                    let (intrinsic, ty) =
                        Intrinsic::expect_literal_into(receiver, self.expected_type())?;
                    Inst {
                        op: Op::Intrinsic(intrinsic).into(),
                        ty,
                    }
                } else {
                    let name = method.try_into()?;
                    self.expect_expr_method_call(receiver, &name, args, expr_method)?
                }
            }
            // smt-specific expressions
            Exp::Macro(expr_macro) => {
                let ExprMacro {
                    attrs: _,
                    mac:
                        Macro {
                            path,
                            bang_token: _,
                            delimiter,
                            tokens,
                        },
                } = expr_macro;
                if !matches!(delimiter, MacroDelimiter::Paren(_)) {
                    bail_on!(expr_macro, "expect macro invocation with parenthesis");
                }

                let quant = QuantifierMacro::from_path(path)?;
                let (vars, body) = self.expect_expr_quant_def(tokens)?;

                // pack into expression
                let op = match quant {
                    QuantifierMacro::Exists => Op::Exists { vars, body },
                    QuantifierMacro::Forall => Op::Forall { vars, body },
                };
                Inst {
                    op: op.into(),
                    ty: TypeTag::Boolean,
                }
            }
            _ => bail_on!(expr, "invalid expression"),
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

    /// Convert an expression to a quantifier body
    fn expect_expr_quant_def(
        &self,
        raw: &TokenStream,
    ) -> Result<(BTreeMap<VarName, TypeTag>, Expr)> {
        let stream = raw.clone();
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
        } = &closure;
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
                    } = typed;

                    let name = VarName::from_pat(pat)?;
                    if self.vars.contains_key(&name) || param_decls.contains_key(&name) {
                        bail_on!(pat, "conflicting quantifier variable name");
                    }

                    let ty = TypeTag::from_type(self.ctxt, ty)?;
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

        // analyze the body
        let mut parser = self.dup(Some(TypeTag::Boolean));
        parser
            .vars
            .extend(param_decls.iter().map(|(k, v)| (k.clone(), v.clone())));
        let body_expr = parser.convert_expr(body)?;

        // return the parsed result
        Ok((param_decls, body_expr))
    }
}
