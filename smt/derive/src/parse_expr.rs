use syn::{
    Block, Expr as Exp, ExprBlock, ExprCall, ExprLit, ExprPath, Lit, Local, LocalInit, Pat,
    PatType, Path, PathArguments, PathSegment, Result, Stmt,
};

use std::collections::BTreeMap;

use crate::parse_ctxt::{bail_if_exists, bail_if_missing, bail_on, FuncName, TypeName, VarName};
use crate::parse_func::FuncSig;
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

/// Intrinsic procedure
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
pub struct PhiNode {
    cond: Expr,
    body: Expr,
}

/// Operations
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi { nodes: Vec<PhiNode>, default: Expr },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
    /// `<function>(<a1>, <a2>, ...)`
    Procedure { name: FuncName, args: Vec<Expr> },
}

/// Instructions (operations with type)
pub struct Inst {
    op: Box<Op>,
    ty: TypeTag,
}

/// Expressions
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

    /// Ensure the variable name is unique
    fn has_name(&self, name: &VarName) -> bool {
        self.vars.contains_key(name) || self.new_vars.contains_key(name)
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
                    if self.has_name(&name) {
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

                    // done
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
                                Some(Seq(inner)) => Seq(inner.clone()),
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
