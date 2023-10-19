use anyhow::bail;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr as Exp, ExprLit, Lit, Result};

use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::Expr;
use crate::parser::infer::TypeRef;
use crate::parser::name::UsrFuncName;
use crate::parser::ty::SysTypeName;

/// Intrinsic procedure
#[derive(Clone)]
pub enum Intrinsic {
    /// `Boolean::from`
    BoolVal(bool),
    /// `Boolean::not`
    BoolNot(Expr),
    /// `Boolean::and`
    BoolAnd(Expr, Expr),
    /// `Boolean::or`
    BoolOr(Expr, Expr),
    /// `Boolean::xor`
    BoolXor(Expr, Expr),
    /// `Integer::from`
    IntVal(i128),
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
    /// `Rational::from`
    NumVal(f64),
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
    /// `Text::from`
    StrVal(String),
    /// `Text::lt`
    StrLt(Expr, Expr),
    /// `Text::le`
    StrLe(Expr, Expr),
    /// `Cloak::shield`
    BoxShield(Expr),
    /// `Cloak::reveal`
    BoxReveal(Expr),
    /// `Seq::empty`
    SeqEmpty,
    /// `Seq::length`
    SeqLength(Expr),
    /// `Seq::append`
    SeqAppend(Expr, Expr),
    /// `Seq::at_unchecked`
    SeqAt(Expr, Expr),
    /// `Seq::includes`
    SeqIncludes(Expr, Expr),
    /// `Set::empty`
    SetEmpty,
    /// `Set::length`
    SetLength(Expr),
    /// `Set::insert`
    SetInsert(Expr, Expr),
    /// `Set::contains`
    SetContains(Expr, Expr),
    /// `Map::empty`
    MapEmpty,
    /// `Map::length`
    MapLength(Expr),
    /// `Map::put_unchecked`
    MapPut(Expr, Expr, Expr),
    /// `Map::get_unchecked`
    MapGet(Expr, Expr),
    /// `Map::contains_key`
    MapContainsKey(Expr, Expr),
    /// `Error::fresh`
    ErrFresh,
    /// `Error::merge`
    ErrMerge(Expr, Expr),
    /// `<any-smt-type>::eq`
    SmtEq(Expr, Expr),
    /// `<any-smt-type>::ne`
    SmtNe(Expr, Expr),
}

macro_rules! mk0 {
    ($op:ident, $args:expr) => {{
        Intrinsic::unpack_expr_0($args)?;
        Intrinsic::$op
    }};
}

macro_rules! mk1 {
    ($op:ident, $args:expr) => {{
        let e1 = Intrinsic::unpack_expr_1($args)?;
        Intrinsic::$op(e1)
    }};
}

macro_rules! mk2 {
    ($op:ident, $args:expr) => {{
        let (e1, e2) = Intrinsic::unpack_expr_2($args)?;
        Intrinsic::$op(e1, e2)
    }};
}

macro_rules! mk3 {
    ($op:ident, $args:expr) => {{
        let (e1, e2, e3) = Intrinsic::unpack_expr_3($args)?;
        Intrinsic::$op(e1, e2, e3)
    }};
}

impl Intrinsic {
    /// Convert an argument list to a boolean literal
    pub fn unpack_lit_bool(args: &Punctuated<Exp, Comma>) -> Result<bool> {
        let mut iter = args.iter();
        let expr = bail_if_missing!(iter.next(), args, "argument");
        let parsed = match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Bool(val) => val.value,
                    _ => bail_on!(lit, "not a boolean literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        };
        bail_if_exists!(iter.next());
        Ok(parsed)
    }

    /// Convert an argument list to an integer literal
    pub fn unpack_lit_int(args: &Punctuated<Exp, Comma>) -> Result<i128> {
        let mut iter = args.iter();
        let expr = bail_if_missing!(iter.next(), args, "argument");
        let parsed = match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Int(val) => match val.token().to_string().parse() {
                        Ok(v) => v,
                        Err(_) => bail_on!(val, "unable to parse"),
                    },
                    _ => bail_on!(lit, "not a integer literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        };
        bail_if_exists!(iter.next());
        Ok(parsed)
    }

    /// Convert an argument list to a floating-point literal
    pub fn unpack_lit_float(args: &Punctuated<Exp, Comma>) -> Result<f64> {
        let mut iter = args.iter();
        let expr = bail_if_missing!(iter.next(), args, "argument");
        let parsed = match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Float(val) => match val.token().to_string().parse() {
                        Ok(v) => v,
                        Err(_) => bail_on!(val, "unable to parse"),
                    },
                    _ => bail_on!(lit, "not a float literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        };
        bail_if_exists!(iter.next());
        Ok(parsed)
    }

    /// Convert an argument list to a string literal
    pub fn unpack_lit_str(args: &Punctuated<Exp, Comma>) -> Result<String> {
        let mut iter = args.iter();
        let expr = bail_if_missing!(iter.next(), args, "argument");
        let parsed = match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Str(val) => val.token().to_string(),
                    _ => bail_on!(lit, "not a string literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        };
        bail_if_exists!(iter.next());
        Ok(parsed)
    }

    /// Convert an expression to a literal
    pub fn parse_literal_into(receiver: &Exp) -> Result<(Self, TypeRef)> {
        let (intrinsic, ty) = match receiver {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Bool(val) => (Self::BoolVal(val.value), TypeRef::Boolean),
                    Lit::Int(val) => {
                        let parsed = match val.token().to_string().parse() {
                            Ok(v) => v,
                            Err(_) => bail_on!(val, "unable to parse"),
                        };
                        (Self::IntVal(parsed), TypeRef::Integer)
                    }
                    Lit::Float(val) => {
                        let parsed = match val.token().to_string().parse() {
                            Ok(v) => v,
                            Err(_) => bail_on!(val, "unable to parse"),
                        };
                        (Self::NumVal(parsed), TypeRef::Rational)
                    }
                    Lit::Str(val) => (Self::StrVal(val.token().to_string()), TypeRef::Text),
                    _ => bail_on!(lit, "not an expected literal"),
                }
            }
            _ => bail_on!(receiver, "not a literal"),
        };
        Ok((intrinsic, ty))
    }

    /// Create an intrinsic
    pub fn new(
        ty_name: &SysTypeName,
        fn_name: &UsrFuncName,
        args: Vec<Expr>,
    ) -> anyhow::Result<Self> {
        use SysTypeName as Q;

        let intrinsic = match (ty_name, fn_name.as_ref()) {
            // boolean
            (Q::Boolean, "not") => mk1!(BoolNot, args),
            (Q::Boolean, "and") => mk2!(BoolAnd, args),
            (Q::Boolean, "or") => mk2!(BoolOr, args),
            (Q::Boolean, "xor") => mk2!(BoolXor, args),
            // integer
            (Q::Integer, "add") => mk2!(IntAdd, args),
            (Q::Integer, "sub") => mk2!(IntSub, args),
            (Q::Integer, "mul") => mk2!(IntMul, args),
            (Q::Integer, "div") => mk2!(IntDiv, args),
            (Q::Integer, "rem") => mk2!(IntRem, args),
            (Q::Integer, "lt") => mk2!(IntLt, args),
            (Q::Integer, "le") => mk2!(IntLe, args),
            (Q::Integer, "ge") => mk2!(IntGe, args),
            (Q::Integer, "gt") => mk2!(IntGt, args),
            // rational
            (Q::Rational, "add") => mk2!(NumAdd, args),
            (Q::Rational, "sub") => mk2!(NumSub, args),
            (Q::Rational, "mul") => mk2!(NumMul, args),
            (Q::Rational, "div") => mk2!(NumDiv, args),
            (Q::Rational, "lt") => mk2!(NumLt, args),
            (Q::Rational, "le") => mk2!(NumLe, args),
            (Q::Rational, "ge") => mk2!(NumGe, args),
            (Q::Rational, "gt") => mk2!(NumGt, args),
            // text
            (Q::Text, "lt") => mk2!(StrLt, args),
            (Q::Text, "le") => mk2!(StrLe, args),
            // cloak
            (Q::Cloak, "shield") => mk1!(BoxShield, args),
            (Q::Cloak, "reveal") => mk1!(BoxReveal, args),
            // seq
            (Q::Seq, "empty") => mk0!(SeqEmpty, args),
            (Q::Seq, "length") => mk1!(SeqLength, args),
            (Q::Seq, "append") => mk2!(SeqAppend, args),
            (Q::Seq, "at_unchecked") => mk2!(SeqAt, args),
            (Q::Seq, "includes") => mk2!(SeqIncludes, args),
            // set
            (Q::Set, "empty") => mk0!(SetEmpty, args),
            (Q::Set, "length") => mk1!(SetLength, args),
            (Q::Set, "insert") => mk2!(SetInsert, args),
            (Q::Set, "contains") => mk2!(SetContains, args),
            // map
            (Q::Map, "empty") => mk0!(MapEmpty, args),
            (Q::Map, "length") => mk1!(MapLength, args),
            (Q::Map, "put_unchecked") => mk3!(MapPut, args),
            (Q::Map, "get_unchecked") => mk2!(MapGet, args),
            (Q::Map, "contains_key") => mk2!(MapContainsKey, args),
            // error
            (Q::Error, "fresh") => mk0!(ErrFresh, args),
            (Q::Error, "merge") => mk2!(ErrMerge, args),
            // others
            _ => bail!("no such intrinsic"),
        };
        Ok(intrinsic)
    }

    /// Utility to unpack 0 argument
    fn unpack_expr_0(exprs: Vec<Expr>) -> anyhow::Result<()> {
        let mut iter = exprs.into_iter();
        if iter.next().is_some() {
            bail!("expect 0 argument");
        }
        Ok(())
    }

    /// Utility to unpack 1 argument
    fn unpack_expr_1(exprs: Vec<Expr>) -> anyhow::Result<Expr> {
        let mut iter = exprs.into_iter();
        let e1 = match iter.next() {
            None => bail!("expect 1 argument"),
            Some(e) => e,
        };
        if iter.next().is_some() {
            bail!("expect 1 argument");
        }
        Ok(e1)
    }

    /// Utility to unpack 2 arguments
    fn unpack_expr_2(exprs: Vec<Expr>) -> anyhow::Result<(Expr, Expr)> {
        let mut iter = exprs.into_iter();
        let e1 = match iter.next() {
            None => bail!("expect 2 arguments"),
            Some(e) => e,
        };
        let e2 = match iter.next() {
            None => bail!("expect 2 arguments"),
            Some(e) => e,
        };
        if iter.next().is_some() {
            bail!("expect 2 arguments");
        }
        Ok((e1, e2))
    }

    /// Utility to unpack 3 arguments
    fn unpack_expr_3(exprs: Vec<Expr>) -> anyhow::Result<(Expr, Expr, Expr)> {
        let mut iter = exprs.into_iter();
        let e1 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some(e) => e,
        };
        let e2 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some(e) => e,
        };
        let e3 = match iter.next() {
            None => bail!("expect 3 arguments"),
            Some(e) => e,
        };
        if iter.next().is_some() {
            bail!("expect 3 arguments");
        }
        Ok((e1, e2, e3))
    }
}
