use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::TokenStream;
use syn::{
    parse2, Arm, Block, Expr as Exp, ExprBlock, ExprCall, ExprClosure, ExprIf, ExprMacro,
    ExprMatch, ExprMethodCall, ExprPath, ExprTuple, ExprUnary, Local, LocalInit, Macro,
    MacroDelimiter, Pat, PatTuple, PatType, Path, PathArguments, PathSegment, Result, ReturnType,
    Stmt, UnOp,
};

use crate::parser::adt::{ADTBranch, ADTPath, MatchAnalyzer, MatchOrganizer};
use crate::parser::ctxt::ContextWithSig;
use crate::parser::dsl::SysMacroName;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on};
use crate::parser::func::{FuncName, FuncSig, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{TypeParamName, UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{
    CtxtForType, EnumVariant, SysTypeName, TypeBody, TypeDef, TypeName, TypeTag,
};
use crate::parser::util::{ExprPathAsTarget, PatUtil, PathUtil};

/// A context suitable for expr analysis
pub trait CtxtForExpr: CtxtForType {
    /// Kind of the current context
    fn kind(&self) -> Kind;

    /// Retrieve the definition of a user-defined type
    fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef>;

    /// Retrieve the signature of a user-defined function
    fn get_func_sig(&self, name: &UsrFuncName, kind: Kind) -> Option<&FuncSig>;

    /// Retrieve all variants of an enum
    fn get_adt_variants(&self, name: &UsrTypeName) -> Option<&BTreeMap<String, EnumVariant>> {
        let def = self.get_type_def(name)?;
        match def.body() {
            TypeBody::Enum(adt) => Some(adt.variants()),
            _ => None,
        }
    }

    /// Retrieve the definition of an enum variant
    fn get_adt_variant_details(
        &self,
        path: &ADTPath,
    ) -> Option<(BTreeMap<TypeParamName, TypeRef>, &EnumVariant)> {
        let def = self.get_type_def(path.branch().ty_name())?;
        let variant = match def.body() {
            TypeBody::Enum(adt) => adt.variants().get(path.branch().variant())?,
            _ => return None,
        };

        let generics = def.head();
        if generics.len() != path.ty_args().len() {
            return None;
        }

        let ty_inst = generics
            .vec()
            .into_iter()
            .zip(path.ty_args().iter().cloned())
            .collect();
        Some((ty_inst, variant))
    }
}

/// Bindings through unpacking during match
#[derive(Clone)]
pub enum Unpack {
    Unit,
    Tuple(BTreeMap<usize, VarName>),
    Record(BTreeMap<String, VarName>),
}

/// Marks how a variable of an ADT type is matched
#[derive(Clone)]
pub struct MatchVariant {
    branch: ADTBranch,
    unpack: Unpack,
}

impl MatchVariant {
    /// Create a new variant
    pub fn new(branch: ADTBranch, unpack: Unpack) -> Self {
        Self { branch, unpack }
    }
}

/// Marks how all variables in the match head are matched
#[derive(Clone)]
pub struct MatchCombo {
    variants: Vec<MatchVariant>,
    body: Expr,
}

impl MatchCombo {
    /// Create a new variant
    pub fn new(variants: Vec<MatchVariant>, body: Expr) -> Self {
        Self { variants, body }
    }
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
    /// `<adt>::<branch>`
    EnumUnit(ADTBranch),
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
    Procedure { name: UsrFuncName, args: Vec<Expr> },
}

/// Instructions (operations with type)
#[derive(Clone)]
pub struct Inst {
    op: Box<Op>,
    ty: TypeRef,
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
    pub fn ty(&self) -> &TypeRef {
        let i = match self {
            Self::Unit(inst) => inst,
            Self::Block { lets: _, body } => body,
        };
        &i.ty
    }

    /// Set the type
    pub fn set_ty(&mut self, ty: TypeRef) {
        match self {
            Self::Unit(inst) => inst.ty = ty,
            Self::Block { lets: _, body } => body.ty = ty,
        }
    }

    /// Visitor with pre/post traversal function
    pub fn visit<PRE, POST>(&mut self, mut pre: PRE, mut post: POST) -> anyhow::Result<()>
    where
        PRE: FnMut(&mut Self) -> anyhow::Result<()> + Copy,
        POST: FnMut(&mut Self) -> anyhow::Result<()> + Copy,
    {
        // pre-order invocation
        pre(self)?;

        // descend into children
        let inst = match self {
            Expr::Unit(inst) => inst,
            Expr::Block { lets, body } => {
                for (_, sub) in lets {
                    sub.visit(pre, post)?;
                }
                body
            }
        };

        match inst.op.as_mut() {
            Op::Var(_) => (),
            Op::EnumUnit { .. } => (),
            Op::Match { heads, combo } => {
                for head in heads {
                    head.visit(pre, post)?;
                }
                for item in combo {
                    item.body.visit(pre, post)?;
                }
            }
            Op::Phi { nodes, default } => {
                for node in nodes {
                    node.cond.visit(pre, post)?;
                    node.body.visit(pre, post)?;
                }
                default.visit(pre, post)?;
            }
            Op::Forall { vars: _, body } | Op::Exists { vars: _, body } => {
                body.visit(pre, post)?;
            }
            Op::Intrinsic(intrinsic) => match intrinsic {
                // boolean
                Intrinsic::BoolVal(_) => (),
                Intrinsic::BoolNot(e1) => e1.visit(pre, post)?,
                Intrinsic::BoolAnd(e1, e2)
                | Intrinsic::BoolOr(e1, e2)
                | Intrinsic::BoolXor(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // integer
                Intrinsic::IntVal(_) => (),
                Intrinsic::IntLt(e1, e2)
                | Intrinsic::IntLe(e1, e2)
                | Intrinsic::IntGe(e1, e2)
                | Intrinsic::IntGt(e1, e2)
                | Intrinsic::IntAdd(e1, e2)
                | Intrinsic::IntSub(e1, e2)
                | Intrinsic::IntMul(e1, e2)
                | Intrinsic::IntDiv(e1, e2)
                | Intrinsic::IntRem(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // rational
                Intrinsic::NumVal(_) => (),
                Intrinsic::NumLt(e1, e2)
                | Intrinsic::NumLe(e1, e2)
                | Intrinsic::NumGe(e1, e2)
                | Intrinsic::NumGt(e1, e2)
                | Intrinsic::NumAdd(e1, e2)
                | Intrinsic::NumSub(e1, e2)
                | Intrinsic::NumMul(e1, e2)
                | Intrinsic::NumDiv(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // string
                Intrinsic::StrVal(_) => (),
                Intrinsic::StrLt(e1, e2) | Intrinsic::StrLe(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // cloak
                Intrinsic::BoxReveal(e) | Intrinsic::BoxShield(e) => {
                    e.visit(pre, post)?;
                }
                // seq
                Intrinsic::SeqEmpty => (),
                Intrinsic::SeqLength(e) => e.visit(pre, post)?,
                Intrinsic::SeqAppend(e1, e2)
                | Intrinsic::SeqAt(e1, e2)
                | Intrinsic::SeqIncludes(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // set
                Intrinsic::SetEmpty => (),
                Intrinsic::SetLength(e) => e.visit(pre, post)?,
                Intrinsic::SetInsert(e1, e2) | Intrinsic::SetContains(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // map
                Intrinsic::MapEmpty => (),
                Intrinsic::MapLength(e) => e.visit(pre, post)?,
                Intrinsic::MapGet(e1, e2) | Intrinsic::MapContainsKey(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                Intrinsic::MapPut(e1, e2, e3) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                    e3.visit(pre, post)?;
                }
                // error
                Intrinsic::ErrFresh => (),
                Intrinsic::ErrMerge(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
                // smt
                Intrinsic::SmtEq(e1, e2) | Intrinsic::SmtNe(e1, e2) => {
                    e1.visit(pre, post)?;
                    e2.visit(pre, post)?;
                }
            },
            Op::Procedure { name: _, args } => {
                for arg in args {
                    arg.visit(pre, post)?;
                }
            }
        }

        // post-order invocation
        post(self)?;

        // done
        Ok(())
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

/// Root expression parser for function body
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
}

/// Derived expression parser (for one and only one expression)
struct ExprParserCursor<'r, 'ctx: 'r> {
    /// parent of the parser
    root: &'r ExprParserRoot<'ctx>,
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
        }
    }

    /// Parse the body of the function
    pub fn parse(self, stmts: &[Stmt]) -> Result<Expr> {
        let mut unifier = TypeUnifier::new();

        // derive a parser
        let exp_ty = self.ret_ty.clone();
        let parser = ExprParserCursor {
            root: &self,
            exp_ty,
            vars: self.params.clone(),
            bindings: vec![],
        };
        let mut parsed = parser.convert_stmts(&mut unifier, stmts)?;

        // check type completeness of the expression
        let result = parsed.visit(
            |expr| {
                if let TypeRef::Var(v) = expr.ty() {
                    let assigned = unifier.retrieve_type_assigned(v)?;
                    expr.set_ty(assigned)
                }
                Ok(())
            },
            |_| Ok(()),
        );
        match result {
            Ok(()) => (),
            Err(e) => bail_on!(stmts.last().expect("at least one statement"), "{}", e),
        }

        // things look good
        Ok(parsed)
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

impl CtxtForExpr for ExprParserRoot<'_> {
    fn kind(&self) -> Kind {
        self.kind
    }

    fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef> {
        self.ctxt.get_type_def(name)
    }

    fn get_func_sig(&self, name: &UsrFuncName, kind: Kind) -> Option<&FuncSig> {
        self.ctxt.get_func_sig(name, kind)
    }
}

impl<'r, 'ctx: 'r> ExprParserCursor<'r, 'ctx> {
    /// Fork a child parser
    fn fork(&self, exp_ty: TypeRef) -> Self {
        Self {
            root: self.root,
            exp_ty,
            vars: self.vars.clone(),
            bindings: vec![],
        }
    }

    /// Consume a stream of statements
    fn convert_stmts(mut self, unifier: &mut TypeUnifier, stmts: &[Stmt]) -> Result<Expr> {
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
                                Some(TypeTag::from_type(self.root, ty.as_ref())?),
                            )
                        }
                        _ => bail_on!(pat, "unrecognized binding"),
                    };
                    if self.vars.get(&name).is_some() {
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
                    let exp_ty = match vty.as_ref() {
                        None => {
                            // assign a new type var to this variable (i.e., to be inferred)
                            TypeRef::Var(unifier.mk_var())
                        }
                        Some(t) => t.into(),
                    };
                    let body_expr = self.fork(exp_ty).convert_expr(unifier, expr)?;

                    // done, continue to next statement
                    self.vars.insert(name.clone(), body_expr.ty().clone());
                    self.bindings.push((name, body_expr));
                }
                Stmt::Expr(expr, semi_token) => {
                    // expecting a unit expression
                    bail_if_exists!(semi_token);

                    // mark that we have found the main expression in the statements
                    expr_found = Some(expr);

                    // end of loop
                    break;
                }
                Stmt::Item(_) | Stmt::Macro(_) => bail_on!(stmt, "unexpected item"),
            }
        }

        // bail if there are more statements
        bail_if_exists!(iter.next());

        // check that we have an expression and convert it if so
        match expr_found {
            None => bail_on!(
                stmts.last().expect("at least one statement"),
                "unable to find the main expression"
            ),
            Some(expr) => self.convert_expr(unifier, expr),
        }
    }

    /// Parse an expression
    fn convert_expr(self, unifier: &mut TypeUnifier, target: &Exp) -> Result<Expr> {
        // case on expression type
        let inst = match target {
            Exp::Path(expr_path) => {
                match ExprPathAsTarget::parse(self.root, unifier, expr_path)? {
                    ExprPathAsTarget::Var(name) => {
                        let ty = match self.vars.get(&name) {
                            None => bail_on!(expr_path, "unknown local variable"),
                            Some(t) => t.clone(),
                        };

                        // unity the type and then build the instruction
                        let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                            Ok(unified) => unified,
                            Err(e) => bail_on!(target, "{}", e),
                        };
                        Inst {
                            op: Op::Var(name).into(),
                            ty: ty_unified,
                        }
                    }
                    ExprPathAsTarget::EnumUnit(adt) => {
                        let (_, variant) = match self.root.get_adt_variant_details(&adt) {
                            None => bail_on!(expr_path, "unknown type"),
                            Some(details) => details,
                        };
                        if !matches!(variant, EnumVariant::Unit) {
                            bail_on!(expr_path, "not a unit variant");
                        }

                        // unify the type and then build the instruction
                        let ty_unified = match unifier.unify(&adt.as_type_ref(), &self.exp_ty) {
                            Ok(unified) => unified,
                            Err(e) => bail_on!(target, "{}", e),
                        };
                        Inst {
                            op: Op::EnumUnit(adt.branch().clone()).into(),
                            ty: ty_unified,
                        }
                    }
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
                return self.convert_stmts(unifier, stmts);
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
                            self.fork(TypeRef::Boolean).convert_expr(unifier, expr)?
                        }
                        _ => bail_on!(cond, "not a Boolean deref"),
                    };

                    // convert the then block
                    let new_then = self
                        .fork(self.exp_ty.clone())
                        .convert_stmts(unifier, &then_branch.stmts)?;

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
                            break self
                                .fork(self.exp_ty.clone())
                                .convert_expr(unifier, else_expr)?;
                        }
                    }
                };

                // retrieve the unified type after parsing all phi nodes
                let unified_ty = unifier.instantiate_if_possible(&self.exp_ty);

                // construct the operator
                let op = Op::Phi {
                    nodes: phi_nodes,
                    default: default_expr,
                };
                Inst {
                    op: op.into(),
                    ty: unified_ty,
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
                for elem in head_exprs {
                    // parser the headers by tentatively marking their types as variables
                    let converted = self
                        .fork(TypeRef::Var(unifier.mk_var()))
                        .convert_expr(unifier, elem)?;
                    heads.push((converted, elem));
                }

                // go over the arms
                let mut organizer = MatchOrganizer::new();
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
                    if elem_pats.len() != heads.len() {
                        bail_on!(pat, "pattern number mismatch");
                    }

                    // analyze each atom
                    let mut atoms = vec![];
                    let mut bindings = BTreeMap::new();
                    for (elem, (head, _)) in elem_pats.iter().zip(heads.iter()) {
                        let (atom, partial) = MatchAnalyzer::analyze_pat_match_head(
                            self.root,
                            unifier,
                            head.ty(),
                            elem,
                        )?;
                        for (var_name, var_ty) in partial {
                            if bindings.insert(var_name, var_ty).is_some() {
                                bail_on!(elem, "naming conflict");
                            }
                        }
                        atoms.push(atom);
                    }

                    // use the new bindings to analyze the body
                    let mut new_ctxt = self.fork(self.exp_ty.clone());
                    for (var_name, var_ty) in bindings {
                        if new_ctxt.vars.insert(var_name, var_ty).is_some() {
                            bail_on!(pat, "naming conflict");
                        }
                    }
                    let body_expr = new_ctxt.convert_expr(unifier, body)?;

                    // save the match arm
                    organizer.add_arm(atoms, body_expr);
                }

                // list options of the head
                let mut heads_exprs = vec![];
                let mut heads_options = vec![];
                for (converted, original) in heads {
                    // retrieve all variants of the ADT
                    let (adt_name, adt_variants): (_, BTreeSet<_>) =
                        match unifier.instantiate_if_possible(converted.ty()) {
                            TypeRef::User(name, _) => match self.root.get_adt_variants(&name) {
                                None => bail_on!(original, "no scuch enum type"),
                                Some(variants) => (name, variants.keys().cloned().collect()),
                            },
                            _ => bail_on!(original, "not a user-defined type"),
                        };
                    heads_exprs.push(converted);
                    heads_options.push((adt_name, adt_variants));
                }

                // organize the entire match expression with exhaustive expansion
                let combo = organizer.into_organized(expr_match, &heads_options)?;

                // retrieve the unified type after parsing all match arms
                let unified_ty = unifier.instantiate_if_possible(&self.exp_ty);

                // construct the operator
                let op = Op::Match {
                    heads: heads_exprs,
                    combo,
                };
                Inst {
                    op: op.into(),
                    ty: unified_ty,
                }
            }
            Exp::Call(expr_call) => {
                let ExprCall {
                    attrs: _,
                    func,
                    paren_token: _,
                    args,
                } = expr_call;

                // extract the callee
                let mut iter = match func.as_ref() {
                    Exp::Path(p) => {
                        let ExprPath {
                            attrs: _,
                            qself,
                            path:
                                Path {
                                    leading_colon,
                                    segments,
                                },
                        } = p;
                        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                        bail_if_exists!(leading_colon);
                        segments.iter().rev()
                    }
                    _ => bail_on!(func, "invalid callee"),
                };

                // extract method and class
                let segment_method = bail_if_missing!(iter.next(), func, "callee");
                let class = match iter.next() {
                    None => None,
                    Some(segment) => {
                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected type arguments");
                        }
                        Some((TypeName::try_from(self.root.generics(), ident)?, segment))
                    }
                };
                bail_if_exists!(iter.next());

                // collect type arguments, if any
                let PathSegment { ident, arguments } = segment_method;

                // case on function names
                match FuncName::try_from(ident)? {
                    FuncName::Sys(name) => {
                        // qualifier required
                        let (qualifier, qsegment) = match class {
                            None => bail_on!(ident, "expect qualifier"),
                            Some(q) => q,
                        };

                        // none of the smt-system functions take path arguments
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected type arguments");
                        }

                        match name {
                            SysFuncName::From => {
                                // handle literal conversion
                                let (intrinsic, ty) = match qualifier {
                                    TypeName::Param(_) | TypeName::Usr(_) => {
                                        bail_on!(qsegment, "expect an intrinsic qualifier");
                                    }
                                    TypeName::Sys(tname) => match tname {
                                        SysTypeName::Boolean => (
                                            Intrinsic::BoolVal(Intrinsic::unpack_lit_bool(args)?),
                                            TypeRef::Boolean,
                                        ),
                                        SysTypeName::Integer => (
                                            Intrinsic::IntVal(Intrinsic::unpack_lit_int(args)?),
                                            TypeRef::Integer,
                                        ),
                                        SysTypeName::Rational => (
                                            Intrinsic::NumVal(Intrinsic::unpack_lit_float(args)?),
                                            TypeRef::Rational,
                                        ),
                                        SysTypeName::Text => (
                                            Intrinsic::StrVal(Intrinsic::unpack_lit_str(args)?),
                                            TypeRef::Text,
                                        ),
                                        _ => {
                                            bail_on!(qsegment, "expect a literal-enabled qualifier")
                                        }
                                    },
                                };

                                // unity the type and then build the instruction
                                let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                                    Ok(unified) => unified,
                                    Err(e) => bail_on!(target, "{}", e),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: ty_unified,
                                }
                            }
                            SysFuncName::Into => bail_on!(expr_call, "unexpected"),
                            SysFuncName::Eq | SysFuncName::Ne => {
                                // collect arguments
                                let mut iter = args.iter();
                                let lhs = bail_if_missing!(iter.next(), args, "expect 2 arguments");
                                let rhs = bail_if_missing!(iter.next(), args, "expect 2 arguments");
                                bail_if_exists!(iter.next());

                                // process it
                                let (parsed_lhs, parsed_rhs) =
                                    self.handle_equality_inequality(unifier, lhs, rhs, target)?;

                                // check qualifier consistency
                                let consistent = match (&qualifier, parsed_lhs.ty()) {
                                    (TypeName::Sys(SysTypeName::Boolean), TypeRef::Boolean)
                                    | (TypeName::Sys(SysTypeName::Integer), TypeRef::Integer)
                                    | (TypeName::Sys(SysTypeName::Rational), TypeRef::Rational)
                                    | (TypeName::Sys(SysTypeName::Text), TypeRef::Text)
                                    | (TypeName::Sys(SysTypeName::Cloak), TypeRef::Cloak(_))
                                    | (TypeName::Sys(SysTypeName::Seq), TypeRef::Seq(_))
                                    | (TypeName::Sys(SysTypeName::Set), TypeRef::Set(_))
                                    | (TypeName::Sys(SysTypeName::Map), TypeRef::Map(_, _))
                                    | (TypeName::Sys(SysTypeName::Error), TypeRef::Error) => true,
                                    (TypeName::Usr(name1), TypeRef::User(name2, _)) => {
                                        name1 == name2
                                    }
                                    (TypeName::Param(name1), TypeRef::Parameter(name2)) => {
                                        name1 == name2
                                    }
                                    _ => false,
                                };
                                if !consistent {
                                    bail_on!(
                                        expr_call,
                                        "qualifier and argument type does not match"
                                    );
                                }

                                // construct the operator
                                let intrinsic = match name {
                                    SysFuncName::Eq => Intrinsic::SmtEq(parsed_lhs, parsed_rhs),
                                    SysFuncName::Ne => Intrinsic::SmtNe(parsed_lhs, parsed_rhs),
                                    _ => panic!("invariant violation"),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: TypeRef::Boolean,
                                }
                            }
                        }
                    }
                    FuncName::Usr(name) => {
                        // qualifier should never be a type parameter or an intrinsic type
                        let qualifier_opt = match class {
                            None => None,
                            Some((tname, qsegment)) => {
                                if matches!(tname, TypeName::Param(_)) {
                                    bail_on!(qsegment, "type parameter cannot be a qualifier");
                                }
                                Some(tname)
                            }
                        };

                        // collect type arguments
                        let ty_args_opt = match arguments {
                            PathArguments::None => None,
                            PathArguments::AngleBracketed(ty_args) => {
                                Some(TypeTag::from_generics(self.root, ty_args)?)
                            }
                            PathArguments::Parenthesized(_) => {
                                bail_on!(arguments, "invalid type arguments")
                            }
                        };

                        // parse the arguments by tentatively marking their types as variables
                        let mut parsed_args = vec![];
                        for arg in args {
                            let parsed = self
                                .fork(TypeRef::Var(unifier.mk_var()))
                                .convert_expr(unifier, arg)?;
                            parsed_args.push(parsed);
                        }

                        // choose the function from function database
                        let inferred = self.root.ctxt.infer.get_inference(
                            unifier,
                            &name,
                            qualifier_opt.as_ref(),
                            ty_args_opt.as_deref(),
                            parsed_args,
                            &self.exp_ty,
                        );
                        match inferred {
                            Ok((op, unified_ret)) => Inst {
                                op: op.into(),
                                ty: unified_ret,
                            },
                            Err(e) => bail_on!(expr_call, "{}", e),
                        }
                    }
                }
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

                match FuncName::try_from(method)? {
                    FuncName::Sys(name) => {
                        // none of the smt-system functions take path arguments
                        bail_if_exists!(turbofish);

                        match name {
                            SysFuncName::Into => {
                                bail_if_non_empty!(args);
                                let (intrinsic, ty) = Intrinsic::parse_literal_into(receiver)?;

                                // unity the type and then build the instruction
                                let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                                    Ok(unified) => unified,
                                    Err(e) => bail_on!(target, "{}", e),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: ty_unified,
                                }
                            }
                            SysFuncName::From => bail_on!(expr_method, "unexpected"),
                            SysFuncName::Eq | SysFuncName::Ne => {
                                // collect arguments
                                let mut iter = args.iter();
                                let rhs = bail_if_missing!(iter.next(), args, "expect 1 argument");
                                bail_if_exists!(iter.next());

                                // process it
                                let (parsed_lhs, parsed_rhs) = self
                                    .handle_equality_inequality(unifier, receiver, rhs, target)?;

                                // construct the operator
                                let intrinsic = match name {
                                    SysFuncName::Eq => Intrinsic::SmtEq(parsed_lhs, parsed_rhs),
                                    SysFuncName::Ne => Intrinsic::SmtNe(parsed_lhs, parsed_rhs),
                                    _ => panic!("invariant violation"),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: TypeRef::Boolean,
                                }
                            }
                        }
                    }
                    FuncName::Usr(name) => {
                        // collect type arguments, if any
                        let ty_args_opt = match turbofish {
                            None => None,
                            Some(ty_args) => Some(TypeTag::from_generics(self.root, ty_args)?),
                        };

                        // parse the arguments by tentatively marking their types as variables
                        let mut parsed_args = vec![];
                        let parsed_receiver = self
                            .fork(TypeRef::Var(unifier.mk_var()))
                            .convert_expr(unifier, receiver)?;
                        parsed_args.push(parsed_receiver);

                        for arg in args {
                            let parsed = self
                                .fork(TypeRef::Var(unifier.mk_var()))
                                .convert_expr(unifier, arg)?;
                            parsed_args.push(parsed);
                        }

                        // choose the function from function database
                        let inferred = self.root.ctxt.infer.get_inference(
                            unifier,
                            &name,
                            None,
                            ty_args_opt.as_deref(),
                            parsed_args,
                            &self.exp_ty,
                        );
                        match inferred {
                            Ok((op, unified_ret)) => Inst {
                                op: op.into(),
                                ty: unified_ret,
                            },
                            Err(e) => bail_on!(expr_method, "{}", e),
                        }
                    }
                }
            }
            // SMT-specific macro (i.e., DSL)
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

                let quant = PathUtil::expect_ident_reserved(path)?;
                let (vars, body) = self.parse_dsl_quantifier(unifier, tokens)?;

                // pack into expression
                let op = match quant {
                    SysMacroName::Exists => Op::Exists { vars, body },
                    SysMacroName::Forall => Op::Forall { vars, body },
                };
                Inst {
                    op: op.into(),
                    ty: TypeRef::Boolean,
                }
            }
            _ => bail_on!(target, "invalid expression"),
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

    /// Utility function on parsing equality and inequalities
    fn handle_equality_inequality(
        &self,
        unifier: &mut TypeUnifier,
        lhs: &Exp,
        rhs: &Exp,
        host: &Exp,
    ) -> Result<(Expr, Expr)> {
        // parse the arguments tentatively by marking their types as variables
        let mut parsed_lhs = self
            .fork(TypeRef::Var(unifier.mk_var()))
            .convert_expr(unifier, lhs)?;
        let mut parsed_rhs = self
            .fork(TypeRef::Var(unifier.mk_var()))
            .convert_expr(unifier, rhs)?;

        // unify the types
        match unifier.unify(&TypeRef::Boolean, &self.exp_ty) {
            Ok(t) => t,
            Err(e) => bail_on!(host, "{}", e),
        };
        let arg_ty = match unifier.unify(parsed_lhs.ty(), parsed_rhs.ty()) {
            Ok(t) => t,
            Err(e) => bail_on!(host, "{}", e),
        };
        parsed_lhs.set_ty(arg_ty.clone());
        parsed_rhs.set_ty(arg_ty);

        // return the expressions
        Ok((parsed_lhs, parsed_rhs))
    }

    /// Convert a DSL macro to a quantifier body
    fn parse_dsl_quantifier(
        &self,
        unifier: &mut TypeUnifier,
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

                    let name = PatUtil::expect_name(pat)?;
                    if self.vars.contains_key(&name) || param_decls.contains_key(&name) {
                        bail_on!(pat, "conflicting quantifier variable name");
                    }
                    let ty = TypeTag::from_type(self.root, ty)?;
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
        let mut parser = self.fork(TypeRef::Boolean);
        parser
            .vars
            .extend(param_decls.iter().map(|(k, v)| (k.clone(), v.into())));
        let body_expr = parser.convert_expr(unifier, body)?;

        // return the parsed result
        Ok((param_decls, body_expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(forward_arg_plain, {
        #[smt_impl]
        fn foo(x: Boolean) -> Boolean {
            x
        }
    });

    unit_test!(forward_arg_ty_param, {
        #[smt_impl]
        fn foo<T: SMT>(x: T) -> T {
            x
        }
    });

    unit_test!(let_bind_arg_plain, {
        #[smt_impl]
        fn foo<T: SMT>(x: Boolean) -> Boolean {
            let y = x;
            y
        }
    });

    unit_test!(let_bind_arg_ty_param, {
        #[smt_impl]
        fn foo<T: SMT>(x: T) -> T {
            let y = x;
            y
        }
    });

    unit_test!(bool_basics, {
        #[smt_impl]
        fn foo(x: Boolean, y: Boolean) -> Boolean {
            x.not().and(false.into()).or(true.into()).xor(y).eq(x.ne(y))
        }
    });

    unit_test!(type_param_equal, {
        #[smt_impl]
        fn foo<T: SMT>(x: T, y: T) -> Boolean {
            x.eq(y).ne(T::ne(x, y))
        }
    });

    unit_test!(call_another, {
        #[smt_impl]
        fn foo<T: SMT>(x: T, y: T) -> Boolean {
            x.eq(y).ne(T::ne(x, y))
        }

        #[smt_impl]
        fn bar() -> Boolean {
            foo(true.into(), false.into())
        }
    });

    unit_test!(enum_ctor, {
        #[smt_type]
        enum ADT {
            Unit,
            Bool(Boolean),
            Items { a: Integer, b: Text },
        }

        #[smt_impl]
        fn foo(x: ADT) -> Boolean {
            x.eq(ADT::Unit)
                .xor(x.ne(ADT::Bool(false.into())))
                .xor(x.ne(ADT::Items {
                    a: 0.into(),
                    b: "abc",
                }))
        }
    });
}
