use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::TokenStream;
use syn::{
    parse2, Arm, Block, Expr as Exp, ExprBlock, ExprCall, ExprClosure, ExprField, ExprIf,
    ExprMacro, ExprMatch, ExprMethodCall, ExprStruct, ExprTuple, ExprUnary, FieldValue, Local,
    LocalInit, Macro, MacroDelimiter, Member, Pat, PatTuple, PatType, Result, ReturnType, Stmt,
    UnOp,
};

use crate::parser::adt::{ADTBranch, MatchAnalyzer, MatchOrganizer};
use crate::parser::apply::{Kind, TypeFn};
use crate::parser::ctxt::ContextWithSig;
use crate::parser::dsl::SysMacroName;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on};
use crate::parser::func::{CastFuncName, FuncName, FuncSig, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{bail_on_ts_err, ti_unify, TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{UsrFuncName, UsrTypeName, VarName};
use crate::parser::path::{ADTPath, QualifiedPath};
use crate::parser::ty::{CtxtForType, EnumVariant, SysTypeName, TypeBody, TypeDef, TypeTag};
use crate::parser::util::{
    ExprPathAsCallee, ExprPathAsRecord, ExprPathAsTarget, PatUtil, PathUtil,
};

/// A context suitable for expr analysis
pub trait CtxtForExpr: CtxtForType {
    /// Retrieve the definition of a user-defined type
    fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef>;

    /// Retrieve the definition of an enum variant
    fn get_adt_variant_details(&self, path: &ADTPath) -> Option<&EnumVariant> {
        let def = self.get_type_def(&path.ty_name)?;
        let variant = match &def.body {
            TypeBody::Enum(adt) => adt.variants.get(&path.variant)?,
            _ => return None,
        };
        Some(variant)
    }

    /// Retrieve the signature of a user-defined function
    fn lookup_unqualified(&self, name: &UsrFuncName) -> Option<&TypeFn>;

    /// Retrieve the function type for a user function on a system type (a.k.a., an intrinsic)
    fn lookup_usr_func_on_sys_type(
        &self,
        ty_name: &SysTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn>;

    /// Retrieve the function type for a user function on a user type (entirely user-defined)
    fn lookup_usr_func_on_usr_type(
        &self,
        ty_name: &UsrTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn>;
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
    pub branch: ADTBranch,
    pub unpack: Unpack,
}

/// Marks how all variables in the match head are matched
#[derive(Clone)]
pub struct MatchCombo {
    pub variants: Vec<MatchVariant>,
    pub body: Expr,
}

/// Phi node, guarded by condition
#[derive(Clone)]
pub struct PhiNode {
    pub cond: Expr,
    pub body: Expr,
}

/// Operations
#[derive(Clone)]
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `<tuple>(a1, a2, ...)`
    Tuple { name: UsrTypeName, slots: Vec<Expr> },
    /// `<record>{ f1: a1, f2: a2, ... }`
    Record {
        name: UsrTypeName,
        fields: BTreeMap<String, Expr>,
    },
    /// `<adt>::<branch>`
    EnumUnit(ADTBranch),
    /// `<adt>::<branch>(a1, a2, ...)`
    EnumTuple { branch: ADTBranch, slots: Vec<Expr> },
    /// `<adt>::<branch>(a1, a2, ...)`
    EnumRecord {
        branch: ADTBranch,
        fields: BTreeMap<String, Expr>,
    },
    /// `<base>.<index>`
    AccessSlot { base: Expr, slot: usize },
    /// `<base>.<field>`
    AccessField { base: Expr, field: String },
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
    pub op: Box<Op>,
    pub ty: TypeRef,
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
            Op::Tuple { name: _, slots } => {
                for slot in slots {
                    slot.visit(pre, post)?;
                }
            }
            Op::Record { name: _, fields } => {
                for field in fields.values_mut() {
                    field.visit(pre, post)?;
                }
            }
            Op::EnumUnit(_) => (),
            Op::EnumTuple { branch: _, slots } => {
                for slot in slots {
                    slot.visit(pre, post)?;
                }
            }
            Op::EnumRecord { branch: _, fields } => {
                for field in fields.values_mut() {
                    field.visit(pre, post)?;
                }
            }
            Op::AccessSlot { base, slot: _ } | Op::AccessField { base, field: _ } => {
                base.visit(pre, post)?;
            }
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
            generics: sig.generics.clone(),
            params: sig
                .param_map()
                .into_iter()
                .map(|(k, v)| (k, (&v).into()))
                .collect(),
            ret_ty: (&sig.ret_ty).into(),
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
                let refreshed = unifier.refresh_type(expr.ty());
                if !refreshed.validate() {
                    anyhow::bail!("incomplete type");
                }
                expr.set_ty(refreshed);
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
    fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef> {
        self.ctxt.get_type_def(name)
    }

    fn lookup_unqualified(&self, name: &UsrFuncName) -> Option<&TypeFn> {
        self.ctxt.fn_db.lookup_unqualified(self.kind, name)
    }

    fn lookup_usr_func_on_sys_type(
        &self,
        ty_name: &SysTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.ctxt
            .fn_db
            .lookup_usr_func_on_sys_type(self.kind, ty_name, fn_name)
    }

    fn lookup_usr_func_on_usr_type(
        &self,
        ty_name: &UsrTypeName,
        fn_name: &UsrFuncName,
    ) -> Option<&TypeFn> {
        self.ctxt
            .fn_db
            .lookup_usr_func_on_usr_type(self.kind, ty_name, fn_name)
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
        let op = match target {
            Exp::Path(expr_path) => {
                match ExprPathAsTarget::parse(self.root, expr_path)? {
                    ExprPathAsTarget::Var(name) => {
                        let ty = match self.vars.get(&name) {
                            None => bail_on!(expr_path, "unknown local variable"),
                            Some(t) => t.clone(),
                        };

                        // unity the type and then build the opcode
                        ti_unify!(unifier, &ty, &self.exp_ty, target);
                        Op::Var(name)
                    }
                    ExprPathAsTarget::EnumUnit(adt) => {
                        let variant = match self.root.get_adt_variant_details(&adt) {
                            None => bail_on!(expr_path, "[invariant] no such type or variant"),
                            Some(details) => details,
                        };
                        if !matches!(variant, EnumVariant::Unit) {
                            bail_on!(expr_path, "not a unit variant");
                        }

                        // unify the type and then build the instruction
                        let ty_ref = adt.as_ty_ref(unifier);
                        ti_unify!(unifier, &ty_ref, &self.exp_ty, target);
                        Op::EnumUnit(adt.into_branch())
                    }
                }
            }
            Exp::Struct(expr_struct) => {
                let ExprStruct {
                    attrs: _,
                    qself,
                    path,
                    brace_token: _,
                    fields: _,
                    dot2_token,
                    rest,
                } = expr_struct;
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                bail_if_exists!(dot2_token);
                bail_if_exists!(rest);

                match ExprPathAsRecord::parse(self.root, path)? {
                    ExprPathAsRecord::CtorRecord(record) => {
                        let fields = match self.root.get_type_def(&record.ty_name) {
                            None => bail_on!(expr_struct, "[invariant] no such type"),
                            Some(def) => match &def.body {
                                TypeBody::Record(record_def) => record_def
                                    .fields
                                    .iter()
                                    .map(|(k, t)| (k.clone(), t.into()))
                                    .collect(),
                                _ => bail_on!(expr_struct, "[invariant] not a record type"),
                            },
                        };
                        let ret_ty = record.as_ty_ref(unifier);

                        // parse the arguments
                        let field_vals =
                            self.parse_record_fields(unifier, &fields, &ret_ty, expr_struct)?;

                        // build the opcode
                        Op::Record {
                            name: record.ty_name,
                            fields: field_vals,
                        }
                    }
                    ExprPathAsRecord::CtorEnum(adt) => {
                        let variant = match self.root.get_adt_variant_details(&adt) {
                            None => bail_on!(expr_struct, "[invariant] no such type or variant"),
                            Some(details) => details,
                        };
                        let fields = match variant {
                            EnumVariant::Record(record_def) => record_def
                                .fields
                                .iter()
                                .map(|(k, t)| (k.clone(), t.into()))
                                .collect(),
                            _ => bail_on!(expr_struct, "not a record variant"),
                        };
                        let ret_ty = adt.as_ty_ref(unifier);

                        // parse the arguments
                        let field_vals =
                            self.parse_record_fields(unifier, &fields, &ret_ty, expr_struct)?;

                        // build the opcode
                        Op::EnumRecord {
                            branch: adt.into_branch(),
                            fields: field_vals,
                        }
                    }
                }
            }
            Exp::Field(expr_field) => {
                let ExprField {
                    attrs: _,
                    base,
                    dot_token: _,
                    member,
                } = expr_field;

                // parse the receiver without assuming its type
                let parsed_base = self
                    .fork(TypeRef::Var(unifier.mk_var()))
                    .convert_expr(unifier, base)?;
                let base_ty = match unifier.refresh_type(parsed_base.ty()) {
                    TypeRef::Var(_) => bail_on!(base, "type annotation needed"),
                    TypeRef::User(ty_name, ty_args) => match self.root.get_type_def(&ty_name) {
                        None => bail_on!(base, "no such type"),
                        Some(def) => {
                            if def.head.params.len() != ty_args.len() {
                                bail_on!(base, "type parameter number mismatch");
                            }
                            &def.body
                        }
                    },
                    _ => bail_on!(base, "invalid type"),
                };

                // derive the return type
                let (op, tag) = match (base_ty, member) {
                    (TypeBody::Tuple(def_tuple), Member::Unnamed(index)) => {
                        let slot = index.index as usize;
                        match def_tuple.slots.get(slot) {
                            None => bail_on!(index, "no such slot"),
                            Some(t) => (
                                Op::AccessSlot {
                                    base: parsed_base,
                                    slot,
                                },
                                t,
                            ),
                        }
                    }
                    (TypeBody::Record(def_record), Member::Named(ident)) => {
                        let field = ident.to_string();
                        match def_record.fields.get(&field) {
                            None => bail_on!(ident, "no such field"),
                            Some(t) => (
                                Op::AccessField {
                                    base: parsed_base,
                                    field,
                                },
                                t,
                            ),
                        }
                    }
                    _ => bail_on!(member, "slot/field mismatch"),
                };

                // unify the return type
                ti_unify!(unifier, &tag.into(), &self.exp_ty, expr_field);

                // return the constructed opcode
                op
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

                // construct the operator
                Op::Phi {
                    nodes: phi_nodes,
                    default: default_expr,
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
                        match unifier.refresh_type(converted.ty()) {
                            TypeRef::User(name, _) => {
                                let adt = match self.root.get_type_def(&name) {
                                    None => bail_on!(original, "no such type"),
                                    Some(def) => match &def.body {
                                        TypeBody::Enum(adt) => adt,
                                        _ => bail_on!(original, "not an enum type"),
                                    },
                                };
                                let variants = adt.variants.keys().cloned().collect();
                                (name, variants)
                            }
                            _ => bail_on!(original, "not a user-defined type"),
                        };
                    heads_exprs.push(converted);
                    heads_options.push((adt_name, adt_variants));
                }

                // organize the entire match expression with exhaustive expansion
                let combo = organizer.into_organized(expr_match, &heads_options)?;

                // construct the operator
                Op::Match {
                    heads: heads_exprs,
                    combo,
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
                match ExprPathAsCallee::parse(self.root, func)? {
                    ExprPathAsCallee::FuncNoPrefix(path) => {
                        // substitute types in function parameters
                        let fty = match self.root.lookup_unqualified(&path.fn_name) {
                            None => bail_on!(func, "[invariant] no such function"),
                            Some(fty) => fty,
                        };
                        let (params, ret_ty) =
                            bail_on_ts_err!(unifier.instantiate_func_ty(fty, &path.ty_args), func);

                        // parse arguments
                        let parsed_args =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::Procedure {
                            name: path.fn_name.clone(),
                            args: parsed_args,
                        }
                    }
                    ExprPathAsCallee::FuncWithType(path) => match path {
                        // casts
                        QualifiedPath::CastFromBool => {
                            let intrinsic = Intrinsic::BoolVal(Intrinsic::unpack_lit_bool(args)?);
                            ti_unify!(unifier, &TypeRef::Boolean, &self.exp_ty, target);
                            Op::Intrinsic(intrinsic)
                        }
                        QualifiedPath::CastFromInt => {
                            let intrinsic = Intrinsic::IntVal(Intrinsic::unpack_lit_int(args)?);
                            ti_unify!(unifier, &TypeRef::Integer, &self.exp_ty, target);
                            Op::Intrinsic(intrinsic)
                        }
                        QualifiedPath::CastFromFloat => {
                            let intrinsic = Intrinsic::NumVal(Intrinsic::unpack_lit_float(args)?);
                            ti_unify!(unifier, &TypeRef::Rational, &self.exp_ty, target);
                            Op::Intrinsic(intrinsic)
                        }
                        QualifiedPath::CastFromStr => {
                            let intrinsic = Intrinsic::StrVal(Intrinsic::unpack_lit_str(args)?);
                            ti_unify!(unifier, &TypeRef::Text, &self.exp_ty, target);
                            Op::Intrinsic(intrinsic)
                        }
                        // system function
                        QualifiedPath::SysFuncOnSysType(ty_name, ty_inst, fn_name) => {
                            // derive the operand type
                            let operand_ty = bail_on_ts_err!(
                                TypeRef::substitute_params(
                                    unifier,
                                    &ty_name.as_type_tag(),
                                    &ty_inst
                                ),
                                func
                            );

                            // parse arguments
                            self.parse_sys_func_args(unifier, &fn_name, &operand_ty, expr_call)?
                        }
                        QualifiedPath::SysFuncOnUsrType(ty_name, ty_inst, fn_name) => {
                            // derive the operand type
                            let operand_tag = match self.root.get_type_generics(&ty_name) {
                                None => bail_on!(func, "[invariant] no such type"),
                                Some(ty_generics) => TypeTag::User(
                                    ty_name,
                                    ty_generics
                                        .params
                                        .iter()
                                        .map(|n| TypeTag::Parameter(n.clone()))
                                        .collect(),
                                ),
                            };
                            let operand_ty = bail_on_ts_err!(
                                TypeRef::substitute_params(unifier, &operand_tag, &ty_inst),
                                func
                            );

                            // parse arguments
                            self.parse_sys_func_args(unifier, &fn_name, &operand_ty, expr_call)?
                        }
                        QualifiedPath::SysFuncOnParamType(ty_name, fn_name) => {
                            // derive the operand type
                            let operand_ty = TypeRef::Parameter(ty_name);

                            // parse arguments
                            self.parse_sys_func_args(unifier, &fn_name, &operand_ty, expr_call)?
                        }
                        // user-defined function on a system type (i.e., intrinsic function)
                        QualifiedPath::UsrFuncOnSysType(ty_name, mut ty_inst, fn_name, fn_inst) => {
                            // substitute types in function parameters
                            let fty =
                                match self.root.lookup_usr_func_on_sys_type(&ty_name, &fn_name) {
                                    None => bail_on!(func, "[invariant] no such function"),
                                    Some(fty) => fty,
                                };

                            if !ty_inst.append(fn_inst) {
                                bail_on!(func, "conflicting type parameter name");
                            }
                            let (params, ret_ty) =
                                bail_on_ts_err!(unifier.instantiate_func_ty(fty, &ty_inst), func);

                            // parse the arguments
                            let parsed_args =
                                self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                            // build the opcode
                            let intrinsic = match Intrinsic::new(&ty_name, &fn_name, parsed_args) {
                                Ok(parsed) => parsed,
                                Err(e) => bail_on!(target, "{}", e),
                            };
                            Op::Intrinsic(intrinsic)
                        }
                        // user-defined function on a user-defined type
                        QualifiedPath::UsrFuncOnUsrType(ty_name, mut ty_inst, fn_name, fn_inst) => {
                            // substitute types in function parameters
                            let fty =
                                match self.root.lookup_usr_func_on_usr_type(&ty_name, &fn_name) {
                                    None => bail_on!(func, "[invariant] no such function"),
                                    Some(fty) => fty,
                                };

                            if !ty_inst.append(fn_inst) {
                                bail_on!(func, "conflicting type parameter name");
                            }
                            let (params, ret_ty) =
                                bail_on_ts_err!(unifier.instantiate_func_ty(fty, &ty_inst), func);

                            // parse the arguments
                            let parsed_args =
                                self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                            // build the opcode
                            Op::Procedure {
                                name: fn_name.clone(),
                                args: parsed_args,
                            }
                        }
                    },
                    ExprPathAsCallee::CtorTuple(tuple) => {
                        let params: Vec<_> = match self.root.get_type_def(&tuple.ty_name) {
                            None => bail_on!(expr_call, "[invariant] no such type"),
                            Some(def) => match &def.body {
                                TypeBody::Tuple(tuple_def) => {
                                    tuple_def.slots.iter().map(|t| t.into()).collect()
                                }
                                _ => bail_on!(expr_call, "[invariant] not a tuple type"),
                            },
                        };
                        let ret_ty = tuple.as_ty_ref(unifier);

                        // parse the arguments
                        let parsed_slots =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::Tuple {
                            name: tuple.ty_name,
                            slots: parsed_slots,
                        }
                    }
                    ExprPathAsCallee::CtorEnum(adt) => {
                        let variant = match self.root.get_adt_variant_details(&adt) {
                            None => bail_on!(expr_call, "[invariant] no such type or variant"),
                            Some(details) => details,
                        };
                        let params: Vec<_> = match variant {
                            EnumVariant::Tuple(tuple_def) => {
                                tuple_def.slots.iter().map(|t| t.into()).collect()
                            }
                            _ => bail_on!(expr_call, "not a tuple variant"),
                        };
                        let ret_ty = adt.as_ty_ref(unifier);

                        // parse the arguments
                        let parsed_args =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::EnumTuple {
                            branch: adt.into_branch(),
                            slots: parsed_args,
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
                    FuncName::Cast(name) => {
                        // none of the cast functions take path arguments
                        bail_if_exists!(turbofish);

                        match name {
                            CastFuncName::Into => {
                                bail_if_non_empty!(args);
                                let (intrinsic, ty) = Intrinsic::parse_literal_into(receiver)?;
                                ti_unify!(unifier, &ty, &self.exp_ty, target);
                                Op::Intrinsic(intrinsic)
                            }
                            CastFuncName::From => bail_on!(expr_method, "unexpected"),
                        }
                    }
                    FuncName::Sys(name) => {
                        // none of the system functions take path arguments
                        bail_if_exists!(turbofish);

                        // parse the arguments by tentatively marking their types as variables
                        let operand_ty = TypeRef::Var(unifier.mk_var());
                        let parsed_lhs = self
                            .fork(operand_ty.clone())
                            .convert_expr(unifier, receiver)?;

                        let mut iter = args.iter();
                        let rhs = bail_if_missing!(iter.next(), args, "expect 1 argument");
                        bail_if_exists!(iter.next());
                        let parsed_rhs =
                            self.fork(operand_ty.clone()).convert_expr(unifier, rhs)?;

                        // unity the return type
                        ti_unify!(unifier, &TypeRef::Boolean, &self.exp_ty, target);

                        // build the opcode
                        match name {
                            SysFuncName::Eq => {
                                Op::Intrinsic(Intrinsic::SmtEq(parsed_lhs, parsed_rhs))
                            }
                            SysFuncName::Ne => {
                                Op::Intrinsic(Intrinsic::SmtNe(parsed_lhs, parsed_rhs))
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
                        let inferred = self.root.ctxt.fn_db.query_with_inference(
                            unifier,
                            self.root.kind,
                            &name,
                            ty_args_opt.as_deref(),
                            parsed_args,
                            &self.exp_ty,
                        );
                        match inferred {
                            Ok(op) => op,
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

                // unity the return type
                ti_unify!(unifier, &TypeRef::Boolean, &self.exp_ty, target);

                // pack into opcode
                match quant {
                    SysMacroName::Exists => Op::Exists { vars, body },
                    SysMacroName::Forall => Op::Forall { vars, body },
                }
            }
            _ => bail_on!(target, "invalid expression"),
        };

        // build the instruction
        let inst = Inst {
            op: op.into(),
            ty: unifier.refresh_type(&self.exp_ty),
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

    /// Parse arguments of a function call
    fn parse_call_arguments(
        &self,
        unifier: &mut TypeUnifier,
        params: &[TypeRef],
        ret_ty: &TypeRef,
        expr_call: &ExprCall,
    ) -> Result<Vec<Expr>> {
        let args = &expr_call.args;

        // parse the arguments
        if params.len() != args.len() {
            bail_on!(args, "argument number mismatch");
        }
        let mut parsed_args = vec![];
        for (param, arg) in params.iter().zip(args) {
            let parsed = self.fork(param.clone()).convert_expr(unifier, arg)?;
            parsed_args.push(parsed);
        }

        // unity the return type
        ti_unify!(unifier, ret_ty, &self.exp_ty, expr_call);

        // done
        Ok(parsed_args)
    }

    /// Parse fields of a record constructor
    fn parse_record_fields(
        &self,
        unifier: &mut TypeUnifier,
        fields: &BTreeMap<String, TypeRef>,
        ret_ty: &TypeRef,
        expr_struct: &ExprStruct,
    ) -> Result<BTreeMap<String, Expr>> {
        let args = &expr_struct.fields;

        // parse the arguments
        if fields.len() != args.len() {
            bail_on!(args, "argument number mismatch");
        }

        let mut parsed_args = BTreeMap::new();
        for arg in args {
            let FieldValue {
                attrs: _,
                member,
                colon_token: _,
                expr: expr_field,
            } = arg;
            let field_name = match member {
                Member::Named(ident) => ident.to_string(),
                Member::Unnamed(_) => bail_on!(member, "unnamed field member"),
            };

            let parsed_field = match fields.get(&field_name) {
                None => bail_on!(member, "no such field"),
                Some(t) => self.fork(t.clone()).convert_expr(unifier, expr_field)?,
            };
            match parsed_args.insert(field_name, parsed_field) {
                None => (),
                Some(_) => bail_on!(member, "duplicated field"),
            }
        }

        // unity the return type
        ti_unify!(unifier, ret_ty, &self.exp_ty, expr_struct);

        // done
        Ok(parsed_args)
    }

    /// Parse arguments of a function call
    fn parse_sys_func_args(
        &self,
        unifier: &mut TypeUnifier,
        fn_name: &SysFuncName,
        operand: &TypeRef,
        expr_call: &ExprCall,
    ) -> Result<Op> {
        // parse arguments
        let mut parsed_args = self
            .parse_call_arguments(
                unifier,
                &[operand.clone(), operand.clone()],
                &TypeRef::Boolean,
                expr_call,
            )?
            .into_iter();

        let parsed_lhs = parsed_args.next().unwrap();
        let parsed_rhs = parsed_args.next().unwrap();

        // build the opcode
        let op = match fn_name {
            SysFuncName::Eq => Op::Intrinsic(Intrinsic::SmtEq(parsed_lhs, parsed_rhs)),
            SysFuncName::Ne => Op::Intrinsic(Intrinsic::SmtNe(parsed_lhs, parsed_rhs)),
        };
        Ok(op)
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
                    b: "abc".into(),
                }))
        }
    });
}
