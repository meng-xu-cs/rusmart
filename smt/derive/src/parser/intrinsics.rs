use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr as Exp, ExprLit, Lit, Result};

use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::Expr;
use crate::parser::infer::TypeRef;

/// Intrinsic procedure
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
}
