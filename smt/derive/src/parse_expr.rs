use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use syn::{
    Arm, Block, Expr as Exp, ExprBlock, ExprCall, ExprLit, ExprMatch, ExprPath, ExprTuple,
    FieldPat, Lit, Local, LocalInit, Member, Pat, PatOr, PatStruct, PatTuple, PatTupleStruct,
    PatType, Path, PathArguments, PathSegment, Result, Stmt,
};

use crate::parse_ctxt::{bail_if_exists, bail_if_missing, bail_on, FuncName, TypeName, VarName};
use crate::parse_func::FuncSig;
use crate::parse_type::{ADTVariant, CtxtForType, TypeDef, TypeTag};

/// A context suitable for expression analysis
pub trait CtxtForExpr: CtxtForType {
    /// Retrieve the type definition
    fn get_type(&self, name: &TypeName) -> Option<&TypeDef>;

    /// Retrieve the function signature for impl
    fn get_impl_sig(&self, name: &FuncName) -> Option<&FuncSig>;

    /// Retrieve the function signature for spec
    fn get_spec_sig(&self, name: &FuncName) -> Option<&FuncSig>;
}

/// Intrinsic procedure
#[derive(Clone)]
pub enum Intrinsic {
    /// `Boolean::new`
    BoolVal(bool),
    /// `Boolean::not`
    Not(Expr),
    /// `Boolean::and`
    And(Expr, Expr),
    /// `Boolean::or`
    Or(Expr, Expr),
    /// `Boolean::xor`
    Xor(Expr, Expr),
    /// `Integer::new`
    IntVal(i128),
    /// `Integer::eq`
    IntEq(Expr, Expr),
    /// `Integer::ne`
    IntNe(Expr, Expr),
    /// `Integer::lt`
    IntLt(Expr, Expr),
    /// `Integer::le`
    IntLe(Expr, Expr),
    /// `Integer::ge`
    IntGe(Expr, Expr),
    /// `Integer::gt`
    IntGt(Expr, Expr),
    /// `Integer::add`
    IntAdd(Expr, Expr),
    /// `Integer::sub`
    IntSub(Expr, Expr),
    /// `Integer::mul`
    IntMul(Expr, Expr),
    /// `Integer::div`
    IntDiv(Expr, Expr),
    /// `Integer::rem`
    IntRem(Expr, Expr),
    /// `Rational::new`
    NumVal(i128),
    /// `Rational::eq`
    NumEq(Expr, Expr),
    /// `Rational::ne`
    NumNe(Expr, Expr),
    /// `Rational::lt`
    NumLt(Expr, Expr),
    /// `Rational::le`
    NumLe(Expr, Expr),
    /// `Rational::ge`
    NumGe(Expr, Expr),
    /// `Rational::gt`
    NumGt(Expr, Expr),
    /// `Rational::add`
    NumAdd(Expr, Expr),
    /// `Rational::sub`
    NumSub(Expr, Expr),
    /// `Rational::mul`
    NumMul(Expr, Expr),
    /// `Rational::div`
    NumDiv(Expr, Expr),
    /// `Text::new`
    StrVal(String),
    /// `Text::eq`
    StrEq(Expr, Expr),
    /// `Text::ne`
    StrNe(Expr, Expr),
    /// `Text::lt`
    StrLt(Expr, Expr),
    /// `Text::le`
    StrLe(Expr, Expr),
    /// `Seq::empty`
    SeqEmpty,
    /// `Seq::append`
    SeqAppend(Expr, Expr),
    /// `Seq::length`
    SeqLength(Expr),
    /// `Seq::contains`
    SeqContains(Expr, Expr),
    /// `Seq::at_unchecked`
    SeqAt(Expr, Expr),
    /// `Set::empty`
    SetEmpty,
    /// `Set::insert`
    SetInsert(Expr, Expr),
    /// `Set::length`
    SetLength(Expr),
    /// `Set::contains`
    SetContains(Expr, Expr),
    /// `Map::empty`
    MapEmpty,
    /// `Map::put_unchecked`
    MapPut(Expr, Expr, Expr),
    /// `Map::get_unchecked`
    MapGet(Expr, Expr),
    /// `Map::length`
    MapLength(Expr),
    /// `Map::contains_key`
    MapContainsKey(Expr, Expr),
    /// `Error::fresh`
    ErrFresh,
    /// `Error::merge`
    ErrMerge(Expr, Expr),
}

/// Phi node, guarded by condition
#[derive(Clone)]
pub struct PhiNode {
    cond: Expr,
    body: Expr,
}

/// Bindings through unpacking
#[derive(Clone)]
pub enum Unpack {
    Unit,
    Tuple(BTreeMap<usize, VarName>),
    Record(BTreeMap<String, VarName>),
}

/// ADT variant identifier
#[derive(Ord, PartialOrd, Eq, PartialEq)]
struct ADTBranch {
    adt: TypeName,
    branch: String,
}

/// Match atom for a specific variable in head
enum MatchAtom {
    Default,
    Binding(BTreeMap<ADTBranch, Unpack>),
}

/// A full match arm for all variables in head
struct MatchArm {
    atoms: Vec<MatchAtom>,
    body: Expr,
}

/// Marks how a variable of an ADT type is matched
#[derive(Clone)]
pub struct MatchVariant {
    adt: TypeName,
    branch: String,
    unpack: Unpack,
}

/// Marks how all variables in head are matched
#[derive(Clone)]
pub struct MatchCombo {
    variants: Vec<MatchVariant>,
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

/// A helper for expression parsing
struct ExprParseCtxt<'a, T: CtxtForExpr> {
    ctxt: &'a T,
    kind: Kind,
    vars: BTreeMap<VarName, TypeTag>,
    expected: Option<TypeTag>,
    bindings: Vec<(VarName, Expr)>,
    new_vars: BTreeMap<VarName, TypeTag>,
}

impl<'a, T: CtxtForExpr> ExprParseCtxt<'a, T> {
    /// For creating a new impl context
    fn new(ctxt: &'a T, kind: Kind, sig: &FuncSig) -> Self {
        Self {
            ctxt,
            kind,
            vars: sig.param_map(),
            expected: Some(sig.ret_ty().clone()),
            bindings: vec![],
            new_vars: BTreeMap::new(),
        }
    }

    /// Duplicate a context for parsing sub-expressions
    fn dup(&self, ety: Option<TypeTag>) -> Self {
        let mut vars = self.vars.clone();
        for (name, ty) in self.new_vars.iter() {
            vars.insert(name.clone(), ty.clone());
        }
        Self {
            ctxt: self.ctxt,
            kind: self.kind,
            vars,
            expected: ety,
            bindings: vec![],
            new_vars: BTreeMap::new(),
        }
    }

    /// Get the type of a local variable
    fn get_var_type(&self, name: &VarName) -> Option<&TypeTag> {
        self.vars.get(name).or_else(|| self.new_vars.get(name))
    }

    /// Get the callee signature
    fn get_func_sig(&self, name: &FuncName, path: &Path) -> Result<&FuncSig> {
        let candidate = match self.kind {
            Kind::Impl => self.ctxt.get_impl_sig(name),
            Kind::Spec => self
                .ctxt
                .get_spec_sig(name)
                .or_else(|| self.ctxt.get_impl_sig(name)),
        };
        match candidate {
            None => bail_on!(path, "unknown function"),
            Some(sig) => Ok(sig),
        }
    }

    /// Retrieve a type variant by path
    fn get_adt_variant_by_path(&self, path: &Path) -> Result<(ADTBranch, &ADTVariant)> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        // extract names
        let mut iter = segments.iter().rev();
        let variant_name = match iter.next() {
            None => bail_on!(path, "invalid path to a type variant"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected path arguments");
                }
                ident.to_string()
            }
        };
        let type_name = match iter.next() {
            None => bail_on!(path, "invalid path to a type variant"),
            Some(segment) => {
                let PathSegment { ident, arguments } = segment;
                if !matches!(arguments, PathArguments::None) {
                    bail_on!(arguments, "unexpected path arguments");
                }
                ident.try_into()?
            }
        };

        // look-up
        let type_def = match self.ctxt.get_type(&type_name) {
            None => bail_on!(path, "no such type"),
            Some(def) => def,
        };

        let variant_def = match type_def {
            TypeDef::Algebraic(adt) => match adt.variants().get(&variant_name) {
                None => bail_on!(path, "no such variant"),
                Some(variant) => variant,
            },
            _ => bail_on!(path, "not an ADT"),
        };

        // done
        let branch_id = ADTBranch {
            adt: type_name,
            branch: variant_name,
        };
        Ok((branch_id, variant_def))
    }

    /// Analyze a pattern for: match arm -> head -> case -> binding
    fn analyze_pat_match_binding(&self, pat: &Pat) -> Result<Option<VarName>> {
        let binding = match pat {
            Pat::Wild(_) => None,
            _ => Some(VarName::from_pat(pat)?),
        };
        Ok(binding)
    }

    /// Analyze a pattern for: match arm -> head -> case
    fn analyze_pat_match_case(
        &self,
        pat: &Pat,
    ) -> Result<(ADTBranch, Unpack, BTreeMap<VarName, TypeTag>)> {
        let mut bindings = BTreeMap::new();

        let (branch, unpack) = match pat {
            Pat::Path(pat_path) => {
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = pat_path;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    ADTVariant::Unit => (),
                    _ => bail_on!(pat, "unexpected pattern"),
                }
                (branch, Unpack::Unit)
            }
            Pat::TupleStruct(pat_tuple) => {
                let PatTupleStruct {
                    attrs: _,
                    qself,
                    path,
                    paren_token: _,
                    elems,
                } = pat_tuple;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    ADTVariant::Tuple(def_tuple) => {
                        let slots = def_tuple.slots();
                        if elems.len() != slots.len() {
                            bail_on!(elems, "number of slots mismatch");
                        }

                        let mut unpack = BTreeMap::new();
                        for (i, (elem, slot)) in elems.iter().zip(slots.iter()).enumerate() {
                            match self.analyze_pat_match_binding(elem)? {
                                None => (),
                                Some(var) => {
                                    match bindings.insert(var.clone(), slot.clone()) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(elem, "duplicated name");
                                        }
                                    }
                                    unpack.insert(i, var);
                                }
                            }
                        }
                        (branch, Unpack::Tuple(unpack))
                    }
                    _ => bail_on!(pat, "unexpected pattern"),
                }
            }
            Pat::Struct(pat_struct) => {
                let PatStruct {
                    attrs: _,
                    qself,
                    path,
                    brace_token: _,
                    fields,
                    rest,
                } = pat_struct;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));
                bail_if_exists!(rest);

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    ADTVariant::Record(def_record) => {
                        let records = def_record.fields();
                        if fields.len() != records.len() {
                            bail_on!(fields, "number of fields mismatch");
                        }

                        let mut unpack = BTreeMap::new();
                        for field in fields {
                            let FieldPat {
                                attrs: _,
                                member,
                                colon_token: _,
                                pat,
                            } = field;
                            let field_name = match member {
                                Member::Named(name) => name.to_string(),
                                Member::Unnamed(_) => bail_on!(member, "unnamed field"),
                            };

                            let field_type = match records.get(&field_name) {
                                None => bail_on!(member, "no such field"),
                                Some(t) => t,
                            };

                            match self.analyze_pat_match_binding(pat)? {
                                None => (),
                                Some(var) => {
                                    match bindings.insert(var.clone(), field_type.clone()) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(pat, "duplicated name");
                                        }
                                    }
                                    unpack.insert(field_name, var);
                                }
                            }
                        }
                        (branch, Unpack::Record(unpack))
                    }
                    _ => bail_on!(pat, "unexpected pattern"),
                }
            }
            _ => bail_on!(pat, "invalid case pattern"),
        };

        Ok((branch, unpack, bindings))
    }

    /// Analyze a pattern for: match arm -> head
    fn analyze_pat_match_head(&self, pat: &Pat) -> Result<(MatchAtom, BTreeMap<VarName, TypeTag>)> {
        let (atom, bindings) = match pat {
            Pat::Wild(_) => (MatchAtom::Default, BTreeMap::new()),
            Pat::Or(pat_or) => {
                let PatOr {
                    attrs: _,
                    leading_vert,
                    cases,
                } = pat_or;
                bail_if_exists!(leading_vert);

                let mut iter = cases.iter();
                let mut variants = BTreeMap::new();
                let (branch, unpack, ref_bindings) = match iter.next() {
                    None => bail_on!(cases, "no case patterns"),
                    Some(pat_case) => self.analyze_pat_match_case(pat_case)?,
                };
                if variants.insert(branch, unpack).is_some() {
                    bail_on!(cases, "duplicated adt variant");
                }

                for case in iter.by_ref() {
                    let (branch, unpack, new_bindings) = self.analyze_pat_match_case(case)?;
                    if variants.insert(branch, unpack).is_some() {
                        bail_on!(cases, "duplicated adt variant");
                    }
                    if ref_bindings != new_bindings {
                        bail_on!(case, "case patterns do not bind the same variable set");
                    }
                }
                (MatchAtom::Binding(variants), ref_bindings)
            }
            _ => {
                let (branch, unpack, bindings) = self.analyze_pat_match_case(pat)?;
                (
                    MatchAtom::Binding(std::iter::once((branch, unpack)).collect()),
                    bindings,
                )
            }
        };
        Ok((atom, bindings))
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
                    if self.get_var_type(&name).is_some() {
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
                    self.new_vars.insert(name.clone(), body_expr.ty().clone());
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
    fn convert_expr(self, expr: &Exp) -> Result<Expr> {
        use Intrinsic::*;
        use TypeTag::*;

        // case on expression type
        let inst = match expr {
            Exp::Path(expr_path) => {
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = expr_path;

                bail_if_exists!(qself.as_ref().map(|q| &q.ty));
                let Path {
                    leading_colon,
                    segments,
                } = path;
                bail_if_exists!(leading_colon);

                // get the variable name
                let mut iter = segments.iter();
                let name = match iter.next() {
                    None => bail_on!(segments, "not a local variable name"),
                    Some(segment) => {
                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected path arguments");
                        }
                        ident.try_into()?
                    }
                };
                if let Some(segment) = iter.next() {
                    bail_on!(segment, "not a local variable name");
                }

                // look up the type
                let ty = match self.get_var_type(&name) {
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
                        User(name) => match self.ctxt.get_type(name) {
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
                let mut parsed_arms = vec![];
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
                        let (atom, partial) = self.analyze_pat_match_head(elem)?;
                        for (var_name, var_ty) in partial {
                            if bindings.insert(var_name, var_ty).is_some() {
                                bail_on!(elem, "duplicated variable name");
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
                    parsed_arms.push(MatchArm {
                        atoms,
                        body: body_expr,
                    });
                }

                // check that all arms share the same type
                if parsed_arms.is_empty() {
                    bail_on!(expr_match, "no arms in match");
                }
                let ref_ty = parsed_arms.first().unwrap().body.ty().clone();
                for item in &parsed_arms {
                    if item.body.ty() != &ref_ty {
                        bail_on!(expr_match, "match arm type mismatch");
                    }
                }

                // return the expression
                let combo = self.organize_match_arms(expr_match, &heads_options, &parsed_arms)?;
                let op = Op::Match { heads, combo };
                Inst {
                    op: op.into(),
                    ty: ref_ty,
                }
            }
            Exp::Call(expr_call) => {
                // extract the path
                let ExprCall {
                    attrs: _,
                    func,
                    paren_token: _,
                    args,
                } = expr_call;

                let call_path = match func.as_ref() {
                    Exp::Path(p) => p,
                    _ => bail_on!(func, "unrecognized callee"),
                };
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = call_path;
                bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));

                let Path {
                    leading_colon,
                    segments,
                } = path;
                bail_if_exists!(leading_colon);

                // extract the callee
                let mut seg_iter = segments.iter().rev();
                let method = match seg_iter.next() {
                    None => bail_on!(segments, "invalid path"),
                    Some(segment) => {
                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected path arguments");
                        }
                        FuncName::try_from(ident)?
                    }
                };
                let class = match seg_iter.next() {
                    None => "Self".to_string(),
                    Some(segment) => {
                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected path arguments");
                        }
                        ident.to_string()
                    }
                };

                // now parse arguments
                let args_len = args.len();
                let mut args_iter = args.iter();

                let check_args_len = |expected| {
                    if args_len != expected {
                        bail_on!(
                            args,
                            "argument number mismatch: expect {} | actual {}",
                            args_len,
                            expected
                        );
                    }
                    Ok(())
                };
                let check_type = |ety: &TypeTag, aty: &TypeTag| {
                    if ety != aty {
                        bail_on!(expr_call, "type mismatch: expect {} | actual {}", ety, aty);
                    }
                    Ok(())
                };

                macro_rules! bail_on_type_mismatch {
                    ($ety:expr, $aty:literal) => {
                        bail_on!(
                            expr_call,
                            "type mismatch: expect {} | actual {}",
                            $ety,
                            $aty
                        )
                    };
                    ($ety:literal, $aty:expr) => {
                        bail_on!(
                            expr_call,
                            "type mismatch: expect {} | actual {}",
                            $ety,
                            $aty
                        )
                    };
                }

                let (op, ty) = if class.as_str() == "Self" {
                    // user-defined function
                    let callee = self.get_func_sig(&method, path)?;
                    let params = callee.param_vec();

                    check_args_len(params.len())?;
                    let mut new_args = vec![];
                    for (arg_expr, (_, arg_ty)) in args_iter.zip(callee.param_vec().iter()) {
                        let converted = self.dup(Some(arg_ty.clone())).convert_expr(arg_expr)?;
                        new_args.push(converted);
                    }
                    (
                        Op::Procedure {
                            name: method,
                            args: new_args,
                        },
                        callee.ret_ty().clone(),
                    )
                } else {
                    // intrinsics
                    let (intrinsic, ty) = match (class.as_str(), method.as_ref()) {
                        // boolean
                        ("Boolean", "from") => {
                            check_args_len(1)?;
                            // argument must be a literal
                            let literal = Self::expect_expr_lit_bool(args_iter.next().unwrap())?;
                            (BoolVal(literal), Boolean)
                        }
                        ("Boolean", "not") => {
                            check_args_len(1)?;
                            let a1 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            (Not(a1), Boolean)
                        }
                        ("Boolean", "and") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            (And(a1, a2), Boolean)
                        }
                        ("Boolean", "or") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            (Or(a1, a2), Boolean)
                        }
                        ("Boolean", "xor") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Boolean))
                                .convert_expr(args_iter.next().unwrap())?;
                            (Xor(a1, a2), Boolean)
                        }

                        // integer
                        ("Integer", "from") => {
                            check_args_len(1)?;
                            // argument must be a literal
                            let literal = Self::expect_expr_lit_int(args_iter.next().unwrap())?;
                            (IntVal(literal), Integer)
                        }
                        ("Integer", "add") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntAdd(a1, a2), Integer)
                        }
                        ("Integer", "sub") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntSub(a1, a2), Integer)
                        }
                        ("Integer", "mul") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntMul(a1, a2), Integer)
                        }
                        ("Integer", "div") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntDiv(a1, a2), Integer)
                        }
                        ("Integer", "rem") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntRem(a1, a2), Integer)
                        }
                        ("Integer", "eq") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntEq(a1, a2), Boolean)
                        }
                        ("Integer", "ne") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntNe(a1, a2), Boolean)
                        }
                        ("Integer", "lt") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntLt(a1, a2), Boolean)
                        }
                        ("Integer", "le") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntLe(a1, a2), Boolean)
                        }
                        ("Integer", "ge") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntGe(a1, a2), Boolean)
                        }
                        ("Integer", "gt") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (IntGt(a1, a2), Boolean)
                        }

                        // rational
                        ("Rational", "from") => {
                            check_args_len(1)?;
                            // argument must be a literal
                            let literal = Self::expect_expr_lit_int(args_iter.next().unwrap())?;
                            (NumVal(literal), Rational)
                        }
                        ("Rational", "add") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumAdd(a1, a2), Rational)
                        }
                        ("Rational", "sub") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Integer))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumSub(a1, a2), Rational)
                        }
                        ("Rational", "mul") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumMul(a1, a2), Rational)
                        }
                        ("Rational", "div") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumDiv(a1, a2), Rational)
                        }
                        ("Rational", "eq") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumEq(a1, a2), Boolean)
                        }
                        ("Rational", "ne") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumNe(a1, a2), Boolean)
                        }
                        ("Rational", "lt") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumLt(a1, a2), Boolean)
                        }
                        ("Rational", "le") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumLe(a1, a2), Boolean)
                        }
                        ("Rational", "ge") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumGe(a1, a2), Boolean)
                        }
                        ("Rational", "gt") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Rational))
                                .convert_expr(args_iter.next().unwrap())?;
                            (NumGt(a1, a2), Boolean)
                        }

                        // string
                        ("Text", "from") => {
                            check_args_len(1)?;
                            // argument must be a literal
                            let literal = Self::expect_expr_lit_str(args_iter.next().unwrap())?;
                            (StrVal(literal), Text)
                        }
                        ("Text", "eq") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            (StrEq(a1, a2), Boolean)
                        }
                        ("Text", "ne") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            (StrNe(a1, a2), Boolean)
                        }
                        ("Text", "lt") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            (StrLt(a1, a2), Boolean)
                        }
                        ("Text", "le") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Text))
                                .convert_expr(args_iter.next().unwrap())?;
                            (StrLe(a1, a2), Boolean)
                        }

                        // seq
                        ("Seq", "empty") => {
                            check_args_len(0)?;
                            let rty = match &self.expected {
                                None => bail_on!(path, "type annotation needed"),
                                Some(t @ Seq(_)) => t.clone(),
                                Some(t) => bail_on_type_mismatch!(t, "Seq<?>"),
                            };
                            (SeqEmpty, rty)
                        }
                        ("Seq", "append") => {
                            check_args_len(2)?;
                            match &self.expected {
                                None => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let (t1, t2) = (a1.ty(), a2.ty());
                                    let inferred = match t1 {
                                        Seq(sub) => {
                                            check_type(sub, t2)?;
                                            t1.clone()
                                        }
                                        _ => bail_on_type_mismatch!("Seq<?>", t1),
                                    };
                                    (SeqAppend(a1, a2), inferred)
                                }
                                Some(t @ Seq(inner)) => {
                                    let a1 = self
                                        .dup(Some(t.clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let a2 = self
                                        .dup(Some(inner.as_ref().clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    (SeqAppend(a1, a2), t.clone())
                                }
                                Some(t) => bail_on_type_mismatch!(t, "Seq<?>"),
                            }
                        }
                        ("Seq", "length") => {
                            check_args_len(1)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let t1 = a1.ty();
                            if !matches!(t1, Seq(_)) {
                                bail_on_type_mismatch!("Seq<?>", t1);
                            }
                            (SeqLength(a1), Integer)
                        }
                        ("Seq", "contains") => {
                            check_args_len(2)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let a2 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let (t1, t2) = (a1.ty(), a2.ty());
                            match t1 {
                                Seq(sub) => {
                                    check_type(sub, t2)?;
                                }
                                _ => bail_on_type_mismatch!("Seq<?>", t1),
                            }
                            (SeqContains(a1, a2), Boolean)
                        }
                        ("Seq", "at_unchecked") => {
                            check_args_len(2)?;
                            match &self.expected {
                                None => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 = self
                                        .dup(Some(Integer))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let inferred = match a1.ty() {
                                        Seq(sub) => sub.as_ref().clone(),
                                        t => bail_on_type_mismatch!("Seq<?>", t),
                                    };
                                    (SeqAt(a1, a2), inferred)
                                }
                                Some(t) => {
                                    let a1 = self
                                        .dup(Some(Seq(t.clone().into())))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let a2 = self
                                        .dup(Some(Integer))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    (SeqAt(a1, a2), t.clone())
                                }
                            }
                        }

                        // set
                        ("Set", "empty") => {
                            check_args_len(0)?;
                            let rty = match &self.expected {
                                None => bail_on!(path, "type annotation needed"),
                                Some(t @ Set(_)) => t.clone(),
                                Some(t) => bail_on_type_mismatch!(t, "Set<?>"),
                            };
                            (SetEmpty, rty)
                        }
                        ("Set", "insert") => {
                            check_args_len(2)?;
                            match &self.expected {
                                None => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let (t1, t2) = (a1.ty(), a2.ty());
                                    let inferred = match t1 {
                                        Set(sub) => {
                                            check_type(sub, t2)?;
                                            t1.clone()
                                        }
                                        _ => bail_on_type_mismatch!("Set<?>", t1),
                                    };
                                    (SetInsert(a1, a2), inferred)
                                }
                                Some(t @ Set(inner)) => {
                                    let a1 = self
                                        .dup(Some(t.clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let a2 = self
                                        .dup(Some(inner.as_ref().clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    (SetInsert(a1, a2), t.clone())
                                }
                                Some(t) => bail_on_type_mismatch!(t, "Set<?>"),
                            }
                        }
                        ("Set", "length") => {
                            check_args_len(1)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let t1 = a1.ty();
                            if !matches!(t1, Set(_)) {
                                bail_on_type_mismatch!("Set<?>", t1);
                            }
                            (SetLength(a1), Integer)
                        }
                        ("Set", "contains") => {
                            check_args_len(2)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let a2 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let (t1, t2) = (a1.ty(), a2.ty());
                            match t1 {
                                Set(sub) => {
                                    check_type(sub, t2)?;
                                }
                                _ => bail_on_type_mismatch!("Set<?>", t1),
                            }
                            (SetContains(a1, a2), Boolean)
                        }

                        // map
                        ("Map", "empty") => {
                            check_args_len(0)?;
                            let rty = match &self.expected {
                                None => bail_on!(path, "type annotation needed"),
                                Some(t @ Map(_, _)) => t.clone(),
                                Some(t) => bail_on_type_mismatch!(t, "Set<?>"),
                            };
                            (MapEmpty, rty)
                        }
                        ("Map", "put_unchecked") => {
                            check_args_len(3)?;
                            match &self.expected {
                                None => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a3 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let (t1, t2, t3) = (a1.ty(), a2.ty(), a3.ty());
                                    let inferred = match t1 {
                                        Map(key, val) => {
                                            check_type(key, t2)?;
                                            check_type(val, t3)?;
                                            t1.clone()
                                        }
                                        _ => bail_on_type_mismatch!("Map<?, ?>", t1),
                                    };
                                    (MapPut(a1, a2, a3), inferred)
                                }
                                Some(t @ Map(key, val)) => {
                                    let a1 = self
                                        .dup(Some(t.clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let a2 = self
                                        .dup(Some(key.as_ref().clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    let a3 = self
                                        .dup(Some(val.as_ref().clone()))
                                        .convert_expr(args_iter.next().unwrap())?;
                                    (MapPut(a1, a2, a3), t.clone())
                                }
                                Some(t) => bail_on_type_mismatch!(t, "Map<?, ?>"),
                            }
                        }
                        ("Map", "get_unchecked") => {
                            check_args_len(2)?;
                            match &self.expected {
                                None => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let inferred = match a1.ty() {
                                        Map(key, val) => {
                                            check_type(key, a2.ty())?;
                                            val.as_ref().clone()
                                        }
                                        t => bail_on_type_mismatch!("Map<?, ?>", t),
                                    };
                                    (MapGet(a1, a2), inferred)
                                }
                                Some(t) => {
                                    let a1 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    let a2 =
                                        self.dup(None).convert_expr(args_iter.next().unwrap())?;
                                    match a1.ty() {
                                        Map(key, val) => {
                                            check_type(key, a2.ty())?;
                                            check_type(val, t)?;
                                        }
                                        t => bail_on_type_mismatch!("Map<?, ?>", t),
                                    };
                                    (MapGet(a1, a2), t.clone())
                                }
                            }
                        }
                        ("Map", "length") => {
                            check_args_len(1)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let t1 = a1.ty();
                            if !matches!(t1, Map(_, _)) {
                                bail_on_type_mismatch!("Map<?, ?>", t1);
                            }
                            (MapLength(a1), Integer)
                        }
                        ("Map", "contains_key") => {
                            check_args_len(2)?;
                            let a1 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let a2 = self.dup(None).convert_expr(args_iter.next().unwrap())?;
                            let (t1, t2) = (a1.ty(), a2.ty());
                            match t1 {
                                Map(key, _) => {
                                    check_type(key, t2)?;
                                }
                                _ => bail_on_type_mismatch!("Map<?, ?>", t1),
                            }
                            (MapContainsKey(a1, a2), Boolean)
                        }

                        // error
                        ("Error", "fresh") => {
                            check_args_len(0)?;
                            (ErrFresh, Error)
                        }
                        ("Error", "merge") => {
                            check_args_len(2)?;
                            let a1 = self
                                .dup(Some(Error))
                                .convert_expr(args_iter.next().unwrap())?;
                            let a2 = self
                                .dup(Some(Error))
                                .convert_expr(args_iter.next().unwrap())?;
                            (ErrMerge(a1, a2), Error)
                        }

                        // others
                        _ => bail_on!(path, "unknown callee"),
                    };
                    (Op::Intrinsic(intrinsic), ty)
                };

                // type check
                if let Some(ety) = self.expected.as_ref() {
                    check_type(ety, &ty)?;
                }

                // pack into expression
                Inst { op: op.into(), ty }
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

    /// Organize the arms of the match statement by permutation
    fn organize_match_arms(
        &self,
        expr: &ExprMatch,
        heads: &[(TypeName, BTreeSet<String>)],
        arms: &[MatchArm],
    ) -> Result<Vec<MatchCombo>> {
        // utility enum to indicate whether a match combo is concrete or abstract
        enum MatchComboStatus {
            None,
            Abstract(Vec<(usize, MatchCombo)>),
            Concrete(usize, MatchCombo),
        }

        // tracks how many combo are mapped to each arm
        let mut map_arms = BTreeMap::new();

        // sanity check, plus initialize the tracking
        for (i, arm) in arms.iter().enumerate() {
            map_arms.insert(i, 0_usize);
            if arm.atoms.len() != heads.len() {
                bail_on!(expr, "atoms and heads number mismatch");
            }
            for (atom, (adt_name, adt_variants)) in arm.atoms.iter().zip(heads.iter()) {
                match atom {
                    MatchAtom::Default => (),
                    MatchAtom::Binding(binding) => {
                        for branch in binding.keys() {
                            if adt_name != &branch.adt {
                                bail_on!(expr, "atoms and heads ADT name mismatch");
                            }
                            if !adt_variants.contains(&branch.branch) {
                                bail_on!(expr, "atoms and heads ADT variant mismatch");
                            }
                        }
                    }
                }
            }
        }

        // list all the combo
        let mut all_combinations = vec![];
        for combo in heads
            .iter()
            .map(|(_, names)| names.iter())
            .multi_cartesian_product()
        {
            // sanity check
            assert_eq!(combo.len(), heads.len());

            // conversion
            let combo_as_branch: Vec<_> = combo
                .into_iter()
                .zip(heads.iter())
                .map(|(variant_name, (type_name, _))| ADTBranch {
                    adt: type_name.clone(),
                    branch: variant_name.to_string(),
                })
                .collect();

            // go over each arm and check which is the match
            let mut found = MatchComboStatus::None;
            for (i, arm) in arms.iter().enumerate() {
                let mut variants = vec![];
                let mut is_matched = true;
                let mut is_abstract = false;
                for (combo_variant, arm_atom) in combo_as_branch.iter().zip(arm.atoms.iter()) {
                    match arm_atom {
                        MatchAtom::Default => {
                            is_abstract = true;
                        }
                        MatchAtom::Binding(binding) => match binding.get(combo_variant) {
                            None => {
                                is_matched = false;
                                break;
                            }
                            Some(unpack) => {
                                let variant = MatchVariant {
                                    adt: combo_variant.adt.clone(),
                                    branch: combo_variant.branch.clone(),
                                    unpack: unpack.clone(),
                                };
                                variants.push(variant);
                            }
                        },
                    }
                }

                // check if everything matches
                if !is_matched {
                    continue;
                }

                // assign the combo
                let combo = MatchCombo {
                    variants,
                    body: arm.body.clone(),
                };
                if is_abstract {
                    match found {
                        MatchComboStatus::None => {
                            found = MatchComboStatus::Abstract(vec![(i, combo)]);
                        }
                        MatchComboStatus::Abstract(existing) => {
                            found = MatchComboStatus::Abstract(
                                existing
                                    .into_iter()
                                    .chain(std::iter::once((i, combo)))
                                    .collect(),
                            )
                        }
                        MatchComboStatus::Concrete(..) => {
                            // do nothing, a concrete match takes priority
                        }
                    }
                } else {
                    // if two concrete arms match to the same combo, raise an error
                    if matches!(found, MatchComboStatus::Concrete(..)) {
                        bail_on!(expr, "two concrete match arms handles the same combination");
                    }
                    found = MatchComboStatus::Concrete(i, combo);
                }
            }

            // ensure that each combo is handled by one and only one match arm
            let (i, combo) = match found {
                MatchComboStatus::None => bail_on!(expr, "no match arms handles a combination"),
                MatchComboStatus::Abstract(candidates) => {
                    if candidates.len() != 1 {
                        bail_on!(expr, "ambiguous abstract match arms");
                    }
                    candidates.into_iter().next().unwrap()
                }
                MatchComboStatus::Concrete(i, combo) => (i, combo),
            };

            all_combinations.push(combo);
            map_arms.entry(i).and_modify(|c| *c += 1);
        }

        // check that every match arm is useful
        if map_arms.values().any(|v| *v == 0) {
            bail_on!(expr, "unused match arms");
        }

        Ok(all_combinations)
    }

    /// Convert an expression to a boolean literal
    fn expect_expr_lit_bool(expr: &Exp) -> Result<bool> {
        match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Bool(val) => Ok(val.value),
                    _ => bail_on!(lit, "not a boolean literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        }
    }

    /// Convert an expression to an integer literal
    fn expect_expr_lit_int(expr: &Exp) -> Result<i128> {
        match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Int(val) => match val.token().to_string().parse() {
                        Ok(v) => Ok(v),
                        Err(_) => bail_on!(val, "unable to parse"),
                    },
                    _ => bail_on!(lit, "not a integer literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        }
    }

    /// Convert an expression to a string literal
    fn expect_expr_lit_str(expr: &Exp) -> Result<String> {
        match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Str(val) => Ok(val.token().to_string()),
                    _ => bail_on!(lit, "not a integer literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        }
    }
}
