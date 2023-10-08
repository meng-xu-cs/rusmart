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
        let mut parser = ExprParseCtxt::new(ctxt, Kind::Impl, sig);
        parser.convert_stmts(stmt)
    }

    /// Extract one from spec body
    pub fn from_spec<T: CtxtForExpr>(ctxt: &T, sig: &FuncSig, stmt: &[Stmt]) -> Result<Self> {
        let mut parser = ExprParseCtxt::new(ctxt, Kind::Spec, sig);
        parser.convert_stmts(stmt)
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
    fn convert_stmts(&mut self, stmts: &[Stmt]) -> Result<Expr> {
        let mut expr_found = None;
        for stmt in stmts {
            // one and only one expression is expected
            if expr_found.is_some() {
                bail_on!(stmt, "expect one and only one expression");
            }
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

                    let mut parser = self.dup(vty);
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
                }
                Stmt::Item(_) | Stmt::Macro(_) => bail_on!(stmt, "unexpected item"),
            }
        }

        // check we have an expression
        match expr_found {
            None => bail_on!(
                stmts.last().expect("at least one statement"),
                "unable to find the main expression"
            ),
            Some(e) => Ok(e),
        }
    }

    /// Convert an expression
    fn convert_expr(&mut self, expr: &Exp) -> Result<Expr> {
        use Intrinsic::*;

        // case on expression type
        match expr {
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
                self.convert_stmts(stmts)
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
                let (op, ty) = if class.as_str() == "Self" {
                    // user-defined function
                    let callee = self.get_func_sig(&method, path)?;
                    let params = callee.param_vec();
                    if args.len() != params.len() {
                        bail_on!(args, "number of arguments mismatch");
                    }

                    let mut new_args = vec![];
                    for (arg_expr, (_, arg_ty)) in args.iter().zip(callee.param_vec().iter()) {
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
                    let args_len = args.len();
                    let mut args_iter = args.iter();
                    let (intrinsic, ty) = match (class.as_str(), method.as_ref()) {
                        // boolean
                        ("Boolean", "from") => {
                            if args_len != 1 {
                                bail_on!(args, "number of arguments mismatch");
                            }
                            // argument must be a literal
                            let literal = Self::expect_expr_lit_bool(args_iter.next().unwrap())?;
                            (BoolVal(literal), TypeTag::Boolean)
                        }

                        // others
                        _ => bail_on!(path, "unknown callee"),
                    };
                    (Op::Intrinsic(intrinsic), ty)
                };

                // pack into expression
                let inst = Inst { op: op.into(), ty };
                Ok(Expr::Unit(inst))
            }
            _ => bail_on!(expr, "invalid expression"),
        }
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
}
