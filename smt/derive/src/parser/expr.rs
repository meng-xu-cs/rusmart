use std::collections::{BTreeMap, BTreeSet};

use syn::{
    Arm, Block, Expr as Exp, ExprBlock, ExprCall, ExprField, ExprIf, ExprMatch, ExprMethodCall,
    ExprStruct, ExprTuple, ExprUnary, FieldValue, Local, LocalInit, Member, Pat, PatTuple, Result,
    Stmt, UnOp,
};

use crate::parser::adt::{ADTBranch, MatchAnalyzer, MatchOrganizer};
use crate::parser::apply::{Kind, TypeFn};
use crate::parser::ctxt::ContextWithSig;
use crate::parser::dsl::{Quantifier, SysMacroName};
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on};
use crate::parser::func::{CastFuncName, FuncName, FuncSig, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{ti_unify, TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{UsrFuncName, UsrTypeName, VarName};
use crate::parser::pat::LetDecl;
use crate::parser::path::{ExprPathAsCallee, ExprPathAsRecord, ExprPathAsTarget, QualifiedPath};
use crate::parser::ty::{CtxtForType, EnumVariant, SysTypeName, TypeBody, TypeDef, TypeTag};

/// A context suitable for expr analysis
pub trait CtxtForExpr: CtxtForType {
    /// Retrieve the kind of the current parsing context
    fn kind(&self) -> Kind;

    /// Retrieve the definition of a user-defined type
    fn get_type_def(&self, name: &UsrTypeName) -> Option<&TypeDef>;

    /// Retrieve the definition of an enum variant
    fn get_adt_variant_details(&self, branch: &ADTBranch) -> Option<&EnumVariant> {
        let def = self.get_type_def(&branch.ty_name)?;
        let variant = match &def.body {
            TypeBody::Enum(adt) => adt.variants.get(&branch.variant)?,
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

/// Marks a declaration of variable(s)
#[derive(Clone)]
pub enum VarDecl {
    One(VarName),
    Pack(Vec<VarDecl>),
}

/// Let bindings
#[derive(Clone)]
pub struct LetBinding {
    pub decl: VarDecl,
    pub bind: Expr,
}

/// Operations
#[derive(Clone)]
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `<tuple>(a1, a2, ...)`
    Tuple {
        name: UsrTypeName,
        inst: Vec<TypeRef>,
        slots: Vec<Expr>,
    },
    /// `<record>{ f1: a1, f2: a2, ... }`
    Record {
        name: UsrTypeName,
        inst: Vec<TypeRef>,
        fields: BTreeMap<String, Expr>,
    },
    /// `<adt>::<branch>`
    EnumUnit {
        branch: ADTBranch,
        inst: Vec<TypeRef>,
    },
    /// `<adt>::<branch>(a1, a2, ...)`
    EnumTuple {
        branch: ADTBranch,
        inst: Vec<TypeRef>,
        slots: Vec<Expr>,
    },
    /// `<adt>::<branch>(a1, a2, ...)`
    EnumRecord {
        branch: ADTBranch,
        inst: Vec<TypeRef>,
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
    /// `forall!(|<v>: <t>| {<expr>})`
    Forall {
        vars: Vec<(VarName, TypeTag)>,
        body: Expr,
    },
    /// `exists!(|<v>: <t>| {<expr>})`
    Exists {
        vars: Vec<(VarName, TypeTag)>,
        body: Expr,
    },
    /// `choose!(|<v>: <t>| {<expr>})`
    Choose {
        vars: Vec<(VarName, TypeTag)>,
        body: Expr,
    },
    /// `forall!(<v> in <c> ... => <expr>)`
    IterForall {
        vars: Vec<(VarName, Expr)>,
        body: Expr,
    },
    /// `exists!(<v> in <c> ... => <expr>)`
    IterExists {
        vars: Vec<(VarName, Expr)>,
        body: Expr,
    },
    /// `choose!(<v> in <c> ... => <expr>)`
    IterChoose {
        vars: Vec<(VarName, Expr)>,
        body: Expr,
    },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
    /// `<function>(<a1>, <a2>, ...)`
    Procedure {
        name: UsrFuncName,
        inst: Vec<TypeRef>,
        args: Vec<Expr>,
    },
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
    Block { lets: Vec<LetBinding>, body: Inst },
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

    /// Visitor with ty/pre/post traversal function
    pub fn visit<TY, PRE, POST>(
        &mut self,
        mut ty: TY,
        mut pre: PRE,
        mut post: POST,
    ) -> anyhow::Result<()>
    where
        TY: FnMut(&mut TypeRef) -> anyhow::Result<()> + Copy,
        PRE: FnMut(&mut Self) -> anyhow::Result<()> + Copy,
        POST: FnMut(&mut Self) -> anyhow::Result<()> + Copy,
    {
        // pre-order invocation
        pre(self)?;

        // descend into children
        let inst = match self {
            Expr::Unit(inst) => inst,
            Expr::Block { lets, body } => {
                for binding in lets {
                    binding.bind.visit(ty, pre, post)?;
                }
                body
            }
        };

        // run on type
        ty(&mut inst.ty)?;

        // run on opcode
        match inst.op.as_mut() {
            Op::Var(_) => (),
            Op::Tuple {
                name: _,
                inst,
                slots,
            } => {
                for targ in inst {
                    ty(targ)?;
                }
                for slot in slots {
                    slot.visit(ty, pre, post)?;
                }
            }
            Op::Record {
                name: _,
                inst,
                fields,
            } => {
                for targ in inst {
                    ty(targ)?;
                }
                for field in fields.values_mut() {
                    field.visit(ty, pre, post)?;
                }
            }
            Op::EnumUnit { branch: _, inst } => {
                for targ in inst {
                    ty(targ)?;
                }
            }
            Op::EnumTuple {
                branch: _,
                inst,
                slots,
            } => {
                for targ in inst {
                    ty(targ)?;
                }
                for slot in slots {
                    slot.visit(ty, pre, post)?;
                }
            }
            Op::EnumRecord {
                branch: _,
                inst,
                fields,
            } => {
                for targ in inst {
                    ty(targ)?;
                }
                for field in fields.values_mut() {
                    field.visit(ty, pre, post)?;
                }
            }
            Op::AccessSlot { base, slot: _ } | Op::AccessField { base, field: _ } => {
                base.visit(ty, pre, post)?;
            }
            Op::Match { heads, combo } => {
                for head in heads {
                    head.visit(ty, pre, post)?;
                }
                for item in combo {
                    item.body.visit(ty, pre, post)?;
                }
            }
            Op::Phi { nodes, default } => {
                for node in nodes {
                    node.cond.visit(ty, pre, post)?;
                    node.body.visit(ty, pre, post)?;
                }
                default.visit(ty, pre, post)?;
            }
            Op::Forall { vars: _, body }
            | Op::Exists { vars: _, body }
            | Op::Choose { vars: _, body } => {
                body.visit(ty, pre, post)?;
            }
            Op::IterForall { vars, body }
            | Op::IterExists { vars, body }
            | Op::IterChoose { vars, body } => {
                for (_, bind) in vars {
                    bind.visit(ty, pre, post)?;
                }
                body.visit(ty, pre, post)?;
            }
            Op::Intrinsic(intrinsic) => match intrinsic {
                // boolean
                Intrinsic::BoolVal(_) => (),
                Intrinsic::BoolNot { val } => val.visit(ty, pre, post)?,
                Intrinsic::BoolAnd { lhs, rhs }
                | Intrinsic::BoolOr { lhs, rhs }
                | Intrinsic::BoolXor { lhs, rhs } => {
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
                // integer
                Intrinsic::IntVal(_) => (),
                Intrinsic::IntLt { lhs, rhs }
                | Intrinsic::IntLe { lhs, rhs }
                | Intrinsic::IntGe { lhs, rhs }
                | Intrinsic::IntGt { lhs, rhs }
                | Intrinsic::IntAdd { lhs, rhs }
                | Intrinsic::IntSub { lhs, rhs }
                | Intrinsic::IntMul { lhs, rhs }
                | Intrinsic::IntDiv { lhs, rhs }
                | Intrinsic::IntRem { lhs, rhs } => {
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
                // rational
                Intrinsic::NumVal(_) => (),
                Intrinsic::NumLt { lhs, rhs }
                | Intrinsic::NumLe { lhs, rhs }
                | Intrinsic::NumGe { lhs, rhs }
                | Intrinsic::NumGt { lhs, rhs }
                | Intrinsic::NumAdd { lhs, rhs }
                | Intrinsic::NumSub { lhs, rhs }
                | Intrinsic::NumMul { lhs, rhs }
                | Intrinsic::NumDiv { lhs, rhs } => {
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
                // string
                Intrinsic::StrVal(_) => (),
                Intrinsic::StrLt { lhs, rhs } | Intrinsic::StrLe { lhs, rhs } => {
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
                // cloak
                Intrinsic::BoxReveal { t, val } | Intrinsic::BoxShield { t, val } => {
                    ty(t)?;
                    val.visit(ty, pre, post)?;
                }
                // seq
                Intrinsic::SeqEmpty { t } => ty(t)?,
                Intrinsic::SeqLength { t, seq } => {
                    ty(t)?;
                    seq.visit(ty, pre, post)?;
                }
                Intrinsic::SeqAppend { t, seq, item } | Intrinsic::SeqIncludes { t, seq, item } => {
                    ty(t)?;
                    seq.visit(ty, pre, post)?;
                    item.visit(ty, pre, post)?;
                }
                Intrinsic::SeqAt { t, seq, idx } => {
                    ty(t)?;
                    seq.visit(ty, pre, post)?;
                    idx.visit(ty, pre, post)?;
                }
                // set
                Intrinsic::SetEmpty { t } => ty(t)?,
                Intrinsic::SetLength { t, set } => {
                    ty(t)?;
                    set.visit(ty, pre, post)?;
                }
                Intrinsic::SetInsert { t, set, item } | Intrinsic::SetContains { t, set, item } => {
                    ty(t)?;
                    set.visit(ty, pre, post)?;
                    item.visit(ty, pre, post)?;
                }
                // map
                Intrinsic::MapEmpty { k, v } => {
                    ty(k)?;
                    ty(v)?;
                }
                Intrinsic::MapLength { k, v, map } => {
                    ty(k)?;
                    ty(v)?;
                    map.visit(ty, pre, post)?
                }
                Intrinsic::MapGet { k, v, map, key }
                | Intrinsic::MapContainsKey { k, v, map, key } => {
                    ty(k)?;
                    ty(v)?;
                    map.visit(ty, pre, post)?;
                    key.visit(ty, pre, post)?;
                }
                Intrinsic::MapPut {
                    k,
                    v,
                    map,
                    key,
                    val,
                } => {
                    ty(k)?;
                    ty(v)?;
                    map.visit(ty, pre, post)?;
                    key.visit(ty, pre, post)?;
                    val.visit(ty, pre, post)?;
                }
                // error
                Intrinsic::ErrFresh => (),
                Intrinsic::ErrMerge { lhs, rhs } => {
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
                // smt
                Intrinsic::SmtEq { t, lhs, rhs } | Intrinsic::SmtNe { t, lhs, rhs } => {
                    ty(t)?;
                    lhs.visit(ty, pre, post)?;
                    rhs.visit(ty, pre, post)?;
                }
            },
            Op::Procedure {
                name: _,
                inst,
                args,
            } => {
                for targ in inst {
                    ty(targ)?;
                }
                for arg in args {
                    arg.visit(ty, pre, post)?;
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
    bindings: Vec<LetBinding>,
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
            |ty| {
                let refreshed = unifier.refresh_type(ty);
                if !refreshed.validate() {
                    anyhow::bail!("incomplete type");
                }
                *ty = refreshed;
                Ok(())
            },
            |_| Ok(()),
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

                    // extract names and optionally, the type
                    let LetDecl { vars, decl, ty } = LetDecl::parse(self.root, unifier, pat)?;

                    // extract the body
                    let body = bail_if_missing!(init, binding, "expect initializer");
                    let LocalInit {
                        eq_token: _,
                        expr,
                        diverge,
                    } = body;
                    bail_if_exists!(diverge.as_ref().map(|(_, div)| div));

                    // parse the body with a new parser
                    let body_expr = self.fork(ty).convert_expr(unifier, expr)?;

                    // done, continue to next statement
                    for (name, ty) in vars {
                        if self.vars.insert(name, ty).is_some() {
                            bail_on!(pat, "conflicting variable names");
                        }
                    }
                    self.bindings.push(LetBinding {
                        decl,
                        bind: body_expr,
                    });
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
                        let (branch, inst) = adt.complete(unifier);
                        let variant = match self.root.get_adt_variant_details(&branch) {
                            None => bail_on!(expr_path, "[invariant] no such type or variant"),
                            Some(details) => details,
                        };
                        if !matches!(variant, EnumVariant::Unit) {
                            bail_on!(expr_path, "not a unit variant");
                        }

                        // unify the return type
                        let ret_ty = inst.make_ty(branch.ty_name.clone());
                        ti_unify!(unifier, &ret_ty, &self.exp_ty, target);

                        // build the opcode
                        Op::EnumUnit {
                            branch,
                            inst: inst.vec(),
                        }
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
                        let (ty_name, inst) = record.complete(unifier);
                        let fields = match self.root.get_type_def(&ty_name) {
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
                        let ret_ty = inst.make_ty(ty_name.clone());

                        // parse the arguments
                        let field_vals =
                            self.parse_record_fields(unifier, &fields, &ret_ty, expr_struct)?;

                        // build the opcode
                        Op::Record {
                            name: ty_name,
                            inst: inst.vec(),
                            fields: field_vals,
                        }
                    }
                    ExprPathAsRecord::CtorEnum(adt) => {
                        let (branch, inst) = adt.complete(unifier);
                        let variant = match self.root.get_adt_variant_details(&branch) {
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
                        let ret_ty = inst.make_ty(branch.ty_name.clone());

                        // parse the arguments
                        let field_vals =
                            self.parse_record_fields(unifier, &fields, &ret_ty, expr_struct)?;

                        // build the opcode
                        Op::EnumRecord {
                            branch,
                            inst: inst.vec(),
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
                        let (fn_name, inst) = path.complete(unifier);

                        // substitute types in function parameters
                        let fty = match self.root.lookup_unqualified(&fn_name) {
                            None => bail_on!(func, "[invariant] no such function"),
                            Some(fty) => fty,
                        };
                        let (params, ret_ty) = match fty.instantiate(&inst) {
                            None => bail_on!(func, "no such type parameter"),
                            Some(instantiated) => instantiated,
                        };

                        // parse arguments
                        let parsed_args =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::Procedure {
                            name: fn_name,
                            inst: inst.vec(),
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
                            let ty_inst = ty_inst.complete(unifier);
                            let operand_ty = match ty_inst.instantiate(&ty_name.as_type_tag()) {
                                None => bail_on!(func, "no such type parameter"),
                                Some(t) => t,
                            };

                            // parse arguments
                            self.parse_sys_func_args(unifier, &fn_name, operand_ty, expr_call)?
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
                            let ty_inst = ty_inst.complete(unifier);
                            let operand_ty = match ty_inst.instantiate(&operand_tag) {
                                None => bail_on!(func, "no such type parameter"),
                                Some(t) => t,
                            };

                            // parse arguments
                            self.parse_sys_func_args(unifier, &fn_name, operand_ty, expr_call)?
                        }
                        QualifiedPath::SysFuncOnParamType(ty_name, fn_name) => self
                            .parse_sys_func_args(
                                unifier,
                                &fn_name,
                                TypeRef::Parameter(ty_name),
                                expr_call,
                            )?,
                        // user-defined function on a system type (i.e., intrinsic function)
                        QualifiedPath::UsrFuncOnSysType(ty_name, ty_inst, fn_name, fn_inst) => {
                            // derive type param substitutions
                            let fty =
                                match self.root.lookup_usr_func_on_sys_type(&ty_name, &fn_name) {
                                    None => bail_on!(func, "[invariant] no such function"),
                                    Some(fty) => fty,
                                };

                            // for simplicity, require type generics be the first set of type parameters
                            // TODO: relax this requirement
                            let inst =
                                match ty_inst.complete(unifier).merge(&fn_inst.complete(unifier)) {
                                    None => bail_on!(func, "conflicting type parameter name"),
                                    Some(inst) => inst,
                                };

                            let (params, ret_ty) = match fty.instantiate(&inst) {
                                None => bail_on!(func, "no such type parameter"),
                                Some(instantiated) => instantiated,
                            };

                            // parse the arguments
                            let parsed_args =
                                self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                            // build the opcode
                            let intrinsic =
                                match Intrinsic::new(&ty_name, &fn_name, inst.vec(), parsed_args) {
                                    Ok(parsed) => parsed,
                                    Err(e) => bail_on!(target, "{}", e),
                                };
                            Op::Intrinsic(intrinsic)
                        }
                        // user-defined function on a user-defined type
                        QualifiedPath::UsrFuncOnUsrType(ty_name, ty_inst, fn_name, fn_inst) => {
                            // derive type param substitutions
                            let fty =
                                match self.root.lookup_usr_func_on_usr_type(&ty_name, &fn_name) {
                                    None => bail_on!(func, "[invariant] no such function"),
                                    Some(fty) => fty,
                                };

                            // for simplicity, require type generics be the first set of type parameters
                            // TODO: relax this requirement
                            let inst =
                                match ty_inst.complete(unifier).merge(&fn_inst.complete(unifier)) {
                                    None => bail_on!(func, "conflicting type parameter name"),
                                    Some(inst) => inst,
                                };

                            let (params, ret_ty) = match fty.instantiate(&inst) {
                                None => bail_on!(func, "no such type parameter"),
                                Some(instantiated) => instantiated,
                            };

                            // parse the arguments
                            let parsed_args =
                                self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                            // build the opcode
                            Op::Procedure {
                                name: fn_name.clone(),
                                inst: inst.vec(),
                                args: parsed_args,
                            }
                        }
                    },
                    ExprPathAsCallee::CtorTuple(tuple) => {
                        let (ty_name, inst) = tuple.complete(unifier);
                        let params: Vec<_> = match self.root.get_type_def(&ty_name) {
                            None => bail_on!(expr_call, "[invariant] no such type"),
                            Some(def) => match &def.body {
                                TypeBody::Tuple(tuple_def) => {
                                    tuple_def.slots.iter().map(|t| t.into()).collect()
                                }
                                _ => bail_on!(expr_call, "[invariant] not a tuple type"),
                            },
                        };
                        let ret_ty = inst.make_ty(ty_name.clone());

                        // parse the arguments
                        let parsed_slots =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::Tuple {
                            name: ty_name,
                            inst: inst.vec(),
                            slots: parsed_slots,
                        }
                    }
                    ExprPathAsCallee::CtorEnum(adt) => {
                        let (branch, inst) = adt.complete(unifier);
                        let variant = match self.root.get_adt_variant_details(&branch) {
                            None => bail_on!(expr_call, "[invariant] no such type or variant"),
                            Some(details) => details,
                        };
                        let params: Vec<_> = match variant {
                            EnumVariant::Tuple(tuple_def) => {
                                tuple_def.slots.iter().map(|t| t.into()).collect()
                            }
                            _ => bail_on!(expr_call, "not a tuple variant"),
                        };
                        let ret_ty = inst.make_ty(branch.ty_name.clone());

                        // parse the arguments
                        let parsed_slots =
                            self.parse_call_arguments(unifier, &params, &ret_ty, expr_call)?;

                        // build the opcode
                        Op::EnumTuple {
                            branch,
                            inst: inst.vec(),
                            slots: parsed_slots,
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
                        let intrinsic = match name {
                            SysFuncName::Eq => Intrinsic::SmtEq {
                                t: unifier.refresh_type(&operand_ty),
                                lhs: parsed_lhs,
                                rhs: parsed_rhs,
                            },
                            SysFuncName::Ne => Intrinsic::SmtNe {
                                t: unifier.refresh_type(&operand_ty),
                                lhs: parsed_lhs,
                                rhs: parsed_rhs,
                            },
                        };

                        Op::Intrinsic(intrinsic)
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
                            self.root,
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
                let quant = Quantifier::parse(self.root, expr_macro)?;
                match quant {
                    Quantifier::Typed { name, vars, body } => {
                        // parse body
                        let mut new_ctxt = self.fork(TypeRef::Boolean);
                        for (var, ty) in &vars {
                            if new_ctxt.vars.insert(var.clone(), ty.into()).is_some() {
                                bail_on!(expr_macro, "duplicated variable binding");
                            }
                        }
                        let quant_body = new_ctxt.convert_expr(unifier, &body)?;

                        // decide return type and opcode
                        let (rty, op) = match name {
                            SysMacroName::Forall => (
                                TypeRef::Boolean,
                                Op::Forall {
                                    vars,
                                    body: quant_body,
                                },
                            ),
                            SysMacroName::Exists => (
                                TypeRef::Boolean,
                                Op::Exists {
                                    vars,
                                    body: quant_body,
                                },
                            ),
                            SysMacroName::Choose => {
                                let rty = if vars.len() == 1 {
                                    let (_, ty) = vars.first().unwrap();
                                    ty.into()
                                } else {
                                    TypeRef::Pack(vars.iter().map(|(_, t)| t.into()).collect())
                                };
                                (
                                    rty,
                                    Op::Choose {
                                        vars,
                                        body: quant_body,
                                    },
                                )
                            }
                        };

                        // unify the return type
                        ti_unify!(unifier, &rty, &self.exp_ty, target);

                        // done
                        op
                    }
                    Quantifier::Iterated { name, vars, body } => {
                        // context for body parsing
                        let mut new_ctxt = self.fork(TypeRef::Boolean);
                        let mut var_tys = vec![];
                        let mut var_exprs = vec![];

                        // parse collection expressions
                        for (var, collection) in vars {
                            let sub_ctxt = self.fork(TypeRef::Var(unifier.mk_var()));
                            let sub_expr = sub_ctxt.convert_expr(unifier, &collection)?;
                            let var_ty = match sub_expr.ty() {
                                TypeRef::Seq(_) => TypeRef::Integer,
                                TypeRef::Set(sub) => sub.as_ref().clone(),
                                TypeRef::Map(key, _) => key.as_ref().clone(),
                                TypeRef::Var(_) => {
                                    bail_on!(&collection, "unable to infer collection type")
                                }
                                _ => bail_on!(&collection, "not a collection type"),
                            };

                            // add the variable declaration to the context of body parsing
                            if new_ctxt.vars.insert(var.clone(), var_ty.clone()).is_some() {
                                bail_on!(expr_macro, "duplicated variable binding");
                            }

                            // save the variable expression and its type (for choose operator)
                            var_tys.push(var_ty);
                            var_exprs.push((var, sub_expr));
                        }

                        // parse the constraint
                        let quant_body = new_ctxt.convert_expr(unifier, &body)?;

                        // decide return type and opcode
                        let (rty, op) = match name {
                            SysMacroName::Forall => (
                                TypeRef::Boolean,
                                Op::IterForall {
                                    vars: var_exprs,
                                    body: quant_body,
                                },
                            ),
                            SysMacroName::Exists => (
                                TypeRef::Boolean,
                                Op::IterExists {
                                    vars: var_exprs,
                                    body: quant_body,
                                },
                            ),
                            SysMacroName::Choose => {
                                let rty = if var_tys.len() == 1 {
                                    var_tys.into_iter().next().unwrap()
                                } else {
                                    TypeRef::Pack(var_tys)
                                };
                                (
                                    rty,
                                    Op::IterChoose {
                                        vars: var_exprs,
                                        body: quant_body,
                                    },
                                )
                            }
                        };

                        // unify the return type
                        ti_unify!(unifier, &rty, &self.exp_ty, target);

                        // done
                        op
                    }
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
        operand: TypeRef,
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
            SysFuncName::Eq => Op::Intrinsic(Intrinsic::SmtEq {
                t: operand,
                lhs: parsed_lhs,
                rhs: parsed_rhs,
            }),
            SysFuncName::Ne => Op::Intrinsic(Intrinsic::SmtNe {
                t: operand,
                lhs: parsed_lhs,
                rhs: parsed_rhs,
            }),
        };
        Ok(op)
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
