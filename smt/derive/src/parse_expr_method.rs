use std::collections::{BTreeMap, BTreeSet};

use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr as Exp, ExprMethodCall, Result};

use crate::err::bail_on;
use crate::parse_expr::{CtxtForExpr, Expr, ExprParseCtxt, Inst};
use crate::parse_path::{FuncName, TypeName};
use crate::parse_type::TypeTag;

/// A type variable representing either a concrete type or a symbolic (to be inferred) one
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum TypeVar {
    /// to be inferred
    Unknown(usize),
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// inductively defined type
    Box(Box<TypeVar>),
    /// SMT-sequence
    Seq(Box<TypeVar>),
    /// SMT-set
    Set(Box<TypeVar>),
    /// SMT-array
    Map(Box<TypeVar>, Box<TypeVar>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(TypeName),
}

/// A function type, with inference allowed
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeFn {
    params: Vec<TypeVar>,
    ret_ty: TypeVar,
}

impl TypeFn {
    /// Populate database for intrinsics
    pub fn populate_db_with_intrinsics() -> BTreeMap<FuncName, BTreeSet<TypeFn>> {
        use TypeVar::*;

        let mut db: BTreeMap<FuncName, BTreeSet<TypeFn>> = BTreeMap::new();

        // utility
        let mut register = |ident, params, ret_ty| {
            let name = FuncName::for_intrinsic(ident);
            let func = TypeFn { params, ret_ty };
            db.entry(name).or_default().insert(func);
        };

        // logical operators
        register("not", vec![Boolean], Boolean);
        register("and", vec![Boolean, Boolean], Boolean);
        register("or", vec![Boolean, Boolean], Boolean);
        register("xor", vec![Boolean, Boolean], Boolean);
        // arithmetic operators
        register("add", vec![Integer, Integer], Integer);
        register("add", vec![Rational, Rational], Rational);
        register("sub", vec![Integer, Integer], Integer);
        register("sub", vec![Rational, Rational], Rational);
        register("mul", vec![Integer, Integer], Integer);
        register("mul", vec![Rational, Rational], Rational);
        register("div", vec![Integer, Integer], Integer);
        register("div", vec![Rational, Rational], Rational);
        register("rem", vec![Integer, Integer], Integer);
        // comparison operators
        register("eq", vec![Integer, Integer], Boolean);
        register("eq", vec![Rational, Rational], Boolean);
        register("eq", vec![Text, Text], Boolean);
        register(
            "eq",
            vec![Box(Unknown(0).into()), Box(Unknown(0).into())],
            Boolean,
        );
        register(
            "eq",
            vec![Seq(Unknown(0).into()), Seq(Unknown(0).into())],
            Boolean,
        );
        register(
            "eq",
            vec![Set(Unknown(0).into()), Set(Unknown(0).into())],
            Boolean,
        );
        register(
            "eq",
            vec![
                Map(Unknown(0).into(), Unknown(1).into()),
                Map(Unknown(0).into(), Unknown(1).into()),
            ],
            Boolean,
        );
        register(
            "eq",
            vec![
                Map(Unknown(0).into(), Unknown(1).into()),
                Map(Unknown(0).into(), Unknown(1).into()),
            ],
            Boolean,
        );
        register("ne", vec![Integer, Integer], Boolean);
        register("ne", vec![Rational, Rational], Boolean);
        register("ne", vec![Text, Text], Boolean);

        // done
        db
    }
}

/// Helper on expr status
enum ExprStatus<'a> {
    Raw(&'a Exp),
    Typed(Expr),
}
impl ExprStatus<'_> {
    fn ty(&self) -> Option<&TypeTag> {
        match self {
            Self::Raw(_) => None,
            Self::Typed(e) => Some(e.ty()),
        }
    }
}

impl<'a, T: CtxtForExpr> ExprParseCtxt<'a, T> {
    /// Convert an unpacked a method call into an operation
    pub fn expect_expr_method_call(
        &self,
        receiver: &Exp,
        method: &FuncName,
        args: &Punctuated<Exp, Comma>,
        spanned: &ExprMethodCall,
    ) -> Result<Inst> {
        // attempt to convert everything, allow failure at this point of time
        let self_status = match self.dup(None).convert_expr(receiver) {
            Ok(expr) => ExprStatus::Typed(expr),
            Err(_) => ExprStatus::Raw(receiver),
        };

        let mut arg_status = vec![];
        for arg in args {
            let status = match self.dup(None).convert_expr(arg) {
                Ok(expr) => ExprStatus::Typed(expr),
                Err(_) => ExprStatus::Raw(receiver),
            };
            arg_status.push(status);
        }

        // all other cases are function calls
        todo!()
    }
}

// helpers on unpacking arguments
fn unpack_status_0(args: Vec<ExprStatus>, spanned: &ExprMethodCall) -> Result<()> {
    if !args.is_empty() {
        bail_on!(spanned, "argument number mismatch");
    }
    Ok(())
}

fn unpack_status_1<'a>(
    args: Vec<ExprStatus<'a>>,
    spanned: &ExprMethodCall,
) -> Result<ExprStatus<'a>> {
    if args.len() != 1 {
        bail_on!(spanned, "argument number mismatch");
    }
    Ok(args.into_iter().next().unwrap())
}

fn unpack_status_2<'a>(
    args: Vec<ExprStatus<'a>>,
    spanned: &ExprMethodCall,
) -> Result<(ExprStatus<'a>, ExprStatus<'a>)> {
    if args.len() != 2 {
        bail_on!(spanned, "argument number mismatch");
    }
    let mut iter = args.into_iter();
    Ok((iter.next().unwrap(), iter.next().unwrap()))
}
