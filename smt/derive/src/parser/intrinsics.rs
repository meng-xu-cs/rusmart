use std::fmt::{Display, Formatter};

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
    BoolNot { val: Expr },
    /// `Boolean::and`
    BoolAnd { lhs: Expr, rhs: Expr },
    /// `Boolean::or`
    BoolOr { lhs: Expr, rhs: Expr },
    /// `Boolean::xor`
    BoolXor { lhs: Expr, rhs: Expr },
    /// `Boolean::implies`
    BoolImplies { lhs: Expr, rhs: Expr },
    /// `Integer::from`
    IntVal(i128),
    /// `Integer::lt`
    IntLt { lhs: Expr, rhs: Expr },
    /// `Integer::le`
    IntLe { lhs: Expr, rhs: Expr },
    /// `Integer::ge`
    IntGe { lhs: Expr, rhs: Expr },
    /// `Integer::gt`
    IntGt { lhs: Expr, rhs: Expr },
    /// `Integer::add`
    IntAdd { lhs: Expr, rhs: Expr },
    /// `Integer::sub`
    IntSub { lhs: Expr, rhs: Expr },
    /// `Integer::mul`
    IntMul { lhs: Expr, rhs: Expr },
    /// `Integer::div`
    IntDiv { lhs: Expr, rhs: Expr },
    /// `Integer::rem`
    IntRem { lhs: Expr, rhs: Expr },
    /// `Rational::from`
    NumVal(i128),
    /// `Rational::lt`
    NumLt { lhs: Expr, rhs: Expr },
    /// `Rational::le`
    NumLe { lhs: Expr, rhs: Expr },
    /// `Rational::ge`
    NumGe { lhs: Expr, rhs: Expr },
    /// `Rational::gt`
    NumGt { lhs: Expr, rhs: Expr },
    /// `Rational::add`
    NumAdd { lhs: Expr, rhs: Expr },
    /// `Rational::sub`
    NumSub { lhs: Expr, rhs: Expr },
    /// `Rational::mul`
    NumMul { lhs: Expr, rhs: Expr },
    /// `Rational::div`
    NumDiv { lhs: Expr, rhs: Expr },
    /// `Text::from`
    StrVal(String),
    /// `Text::lt`
    StrLt { lhs: Expr, rhs: Expr },
    /// `Text::le`
    StrLe { lhs: Expr, rhs: Expr },
    /// `Cloak::shield`
    BoxShield { t: TypeRef, val: Expr },
    /// `Cloak::reveal`
    BoxReveal { t: TypeRef, val: Expr },
    /// `Seq::empty`
    SeqEmpty { t: TypeRef },
    /// `Seq::length`
    SeqLength { t: TypeRef, seq: Expr },
    /// `Seq::append`
    SeqAppend { t: TypeRef, seq: Expr, item: Expr },
    /// `Seq::at_unchecked`
    SeqAt { t: TypeRef, seq: Expr, idx: Expr },
    /// `Seq::includes`
    SeqIncludes { t: TypeRef, seq: Expr, item: Expr },
    /// `Set::empty`
    SetEmpty { t: TypeRef },
    /// `Set::length`
    SetLength { t: TypeRef, set: Expr },
    /// `Set::insert`
    SetInsert { t: TypeRef, set: Expr, item: Expr },
    /// `Set::remove`
    SetRemove { t: TypeRef, set: Expr, item: Expr },
    /// `Set::contains`
    SetContains { t: TypeRef, set: Expr, item: Expr },
    /// `Map::empty`
    MapEmpty { k: TypeRef, v: TypeRef },
    /// `Map::length`
    MapLength { k: TypeRef, v: TypeRef, map: Expr },
    /// `Map::put_unchecked`
    MapPut {
        k: TypeRef,
        v: TypeRef,
        map: Expr,
        key: Expr,
        val: Expr,
    },
    /// `Map::get_unchecked`
    MapGet {
        k: TypeRef,
        v: TypeRef,
        map: Expr,
        key: Expr,
    },
    /// `Map::del_unchecked`
    MapDel {
        k: TypeRef,
        v: TypeRef,
        map: Expr,
        key: Expr,
    },
    /// `Map::contains_key`
    MapContainsKey {
        k: TypeRef,
        v: TypeRef,
        map: Expr,
        key: Expr,
    },
    /// `Error::fresh`
    ErrFresh,
    /// `Error::merge`
    ErrMerge { lhs: Expr, rhs: Expr },
    /// `<any-smt-type>::eq`
    SmtEq { t: TypeRef, lhs: Expr, rhs: Expr },
    /// `<any-smt-type>::ne`
    SmtNe { t: TypeRef, lhs: Expr, rhs: Expr },
}

macro_rules! mk0 {
    ($op:ident, $ty_args:expr, $args:expr) => {{
        Intrinsic::unpack_ty_arg_0($ty_args)?;
        Intrinsic::unpack_expr_0($args)?;
        Intrinsic::$op
    }};
}

macro_rules! mk1 {
    ($op:ident, $ty_args:expr, $args:expr) => {{
        Intrinsic::unpack_ty_arg_0($ty_args)?;
        let e1 = Intrinsic::unpack_expr_1($args)?;
        Intrinsic::$op { val: e1 }
    }};
}

macro_rules! mk2 {
    ($op:ident, $ty_args:expr, $args:expr) => {{
        Intrinsic::unpack_ty_arg_0($ty_args)?;
        let (e1, e2) = Intrinsic::unpack_expr_2($args)?;
        Intrinsic::$op { lhs: e1, rhs: e2 }
    }};
}

macro_rules! mk0_t {
    ($op:ident, $ty_args:expr, $args:expr) => {{
        let t1 = Intrinsic::unpack_ty_arg_1($ty_args)?;
        Intrinsic::unpack_expr_0($args)?;
        Intrinsic::$op { t: t1 }
    }};
}

macro_rules! mk1_t {
    ($op:ident, $ty_args:expr, $args:expr, $n1:ident) => {{
        let t1 = Intrinsic::unpack_ty_arg_1($ty_args)?;
        let e1 = Intrinsic::unpack_expr_1($args)?;
        Intrinsic::$op { t: t1, $n1: e1 }
    }};
}

macro_rules! mk2_t {
    ($op:ident, $ty_args:expr, $args:expr, $n1:ident, $n2:ident) => {{
        let t1 = Intrinsic::unpack_ty_arg_1($ty_args)?;
        let (e1, e2) = Intrinsic::unpack_expr_2($args)?;
        Intrinsic::$op {
            t: t1,
            $n1: e1,
            $n2: e2,
        }
    }};
}

macro_rules! mk0_kv {
    ($op:ident, $ty_args:expr, $args:expr) => {{
        let (t1, t2) = Intrinsic::unpack_ty_arg_2($ty_args)?;
        Intrinsic::unpack_expr_0($args)?;
        Intrinsic::$op { k: t1, v: t2 }
    }};
}

macro_rules! mk1_kv {
    ($op:ident, $ty_args:expr, $args:expr, $n1:ident) => {{
        let (t1, t2) = Intrinsic::unpack_ty_arg_2($ty_args)?;
        let e1 = Intrinsic::unpack_expr_1($args)?;
        Intrinsic::$op {
            k: t1,
            v: t2,
            $n1: e1,
        }
    }};
}

macro_rules! mk2_kv {
    ($op:ident, $ty_args:expr, $args:expr, $n1:ident, $n2:ident) => {{
        let (t1, t2) = Intrinsic::unpack_ty_arg_2($ty_args)?;
        let (e1, e2) = Intrinsic::unpack_expr_2($args)?;
        Intrinsic::$op {
            k: t1,
            v: t2,
            $n1: e1,
            $n2: e2,
        }
    }};
}

macro_rules! mk3_kv {
    ($op:ident, $ty_args:expr, $args:expr, $n1:ident, $n2:ident, $n3:ident) => {{
        let (t1, t2) = Intrinsic::unpack_ty_arg_2($ty_args)?;
        let (e1, e2, e3) = Intrinsic::unpack_expr_3($args)?;
        Intrinsic::$op {
            k: t1,
            v: t2,
            $n1: e1,
            $n2: e2,
            $n3: e3,
        }
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
                    _ => bail_on!(lit, "not an integer literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        };
        bail_if_exists!(iter.next());
        Ok(parsed)
    }

    /// Convert an argument list to a floating-point literal
    pub fn unpack_lit_float(args: &Punctuated<Exp, Comma>) -> Result<i128> {
        Self::unpack_lit_int(args)
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
        ty_args: Vec<TypeRef>,
        args: Vec<Expr>,
    ) -> anyhow::Result<Self> {
        use SysTypeName as Q;

        let intrinsic = match (ty_name, fn_name.as_ref()) {
            // boolean
            (Q::Boolean, "not") => mk1!(BoolNot, ty_args, args),
            (Q::Boolean, "and") => mk2!(BoolAnd, ty_args, args),
            (Q::Boolean, "or") => mk2!(BoolOr, ty_args, args),
            (Q::Boolean, "xor") => mk2!(BoolXor, ty_args, args),
            (Q::Boolean, "implies") => mk2!(BoolImplies, ty_args, args),
            // integer
            (Q::Integer, "add") => mk2!(IntAdd, ty_args, args),
            (Q::Integer, "sub") => mk2!(IntSub, ty_args, args),
            (Q::Integer, "mul") => mk2!(IntMul, ty_args, args),
            (Q::Integer, "div") => mk2!(IntDiv, ty_args, args),
            (Q::Integer, "rem") => mk2!(IntRem, ty_args, args),
            (Q::Integer, "lt") => mk2!(IntLt, ty_args, args),
            (Q::Integer, "le") => mk2!(IntLe, ty_args, args),
            (Q::Integer, "ge") => mk2!(IntGe, ty_args, args),
            (Q::Integer, "gt") => mk2!(IntGt, ty_args, args),
            // rational
            (Q::Rational, "add") => mk2!(NumAdd, ty_args, args),
            (Q::Rational, "sub") => mk2!(NumSub, ty_args, args),
            (Q::Rational, "mul") => mk2!(NumMul, ty_args, args),
            (Q::Rational, "div") => mk2!(NumDiv, ty_args, args),
            (Q::Rational, "lt") => mk2!(NumLt, ty_args, args),
            (Q::Rational, "le") => mk2!(NumLe, ty_args, args),
            (Q::Rational, "ge") => mk2!(NumGe, ty_args, args),
            (Q::Rational, "gt") => mk2!(NumGt, ty_args, args),
            // text
            (Q::Text, "lt") => mk2!(StrLt, ty_args, args),
            (Q::Text, "le") => mk2!(StrLe, ty_args, args),
            // cloak
            (Q::Cloak, "shield") => mk1_t!(BoxShield, ty_args, args, val),
            (Q::Cloak, "reveal") => mk1_t!(BoxReveal, ty_args, args, val),
            // seq
            (Q::Seq, "empty") => mk0_t!(SeqEmpty, ty_args, args),
            (Q::Seq, "length") => mk1_t!(SeqLength, ty_args, args, seq),
            (Q::Seq, "append") => mk2_t!(SeqAppend, ty_args, args, seq, item),
            (Q::Seq, "at_unchecked") => mk2_t!(SeqAt, ty_args, args, seq, idx),
            (Q::Seq, "includes") => mk2_t!(SeqIncludes, ty_args, args, seq, item),
            // set
            (Q::Set, "empty") => mk0_t!(SetEmpty, ty_args, args),
            (Q::Set, "length") => mk1_t!(SetLength, ty_args, args, set),
            (Q::Set, "insert") => mk2_t!(SetInsert, ty_args, args, set, item),
            (Q::Set, "remove") => mk2_t!(SetRemove, ty_args, args, set, item),
            (Q::Set, "contains") => mk2_t!(SetContains, ty_args, args, set, item),
            // map
            (Q::Map, "empty") => mk0_kv!(MapEmpty, ty_args, args),
            (Q::Map, "length") => mk1_kv!(MapLength, ty_args, args, map),
            (Q::Map, "put_unchecked") => mk3_kv!(MapPut, ty_args, args, map, key, val),
            (Q::Map, "get_unchecked") => mk2_kv!(MapGet, ty_args, args, map, key),
            (Q::Map, "del_unchecked") => mk2_kv!(MapDel, ty_args, args, map, key),
            (Q::Map, "contains_key") => mk2_kv!(MapContainsKey, ty_args, args, map, key),
            // error
            (Q::Error, "fresh") => mk0!(ErrFresh, ty_args, args),
            (Q::Error, "merge") => mk2!(ErrMerge, ty_args, args),
            // others
            _ => bail!("no such intrinsic"),
        };
        Ok(intrinsic)
    }

    /// Utility to unpack 0 type argument
    fn unpack_ty_arg_0(ty_args: Vec<TypeRef>) -> anyhow::Result<()> {
        let mut iter = ty_args.into_iter();
        if iter.next().is_some() {
            bail!("expect 0 type argument");
        }
        Ok(())
    }

    /// Utility to unpack 1 type argument
    fn unpack_ty_arg_1(ty_args: Vec<TypeRef>) -> anyhow::Result<TypeRef> {
        let mut iter = ty_args.into_iter();
        let t1 = match iter.next() {
            None => bail!("expect 1 type argument"),
            Some(t) => t,
        };
        if iter.next().is_some() {
            bail!("expect 1 type argument");
        }
        Ok(t1)
    }

    /// Utility to unpack 2 type arguments
    fn unpack_ty_arg_2(ty_args: Vec<TypeRef>) -> anyhow::Result<(TypeRef, TypeRef)> {
        let mut iter = ty_args.into_iter();
        let t1 = match iter.next() {
            None => bail!("expect 2 type arguments"),
            Some(t) => t,
        };
        let t2 = match iter.next() {
            None => bail!("expect 2 type arguments"),
            Some(t) => t,
        };
        if iter.next().is_some() {
            bail!("expect 2 type arguments");
        }
        Ok((t1, t2))
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

impl Display for Intrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BoolVal(v) => write!(f, "{}", v),
            Self::BoolNot { val } => write!(f, "!{}", val),
            Self::BoolAnd { lhs, rhs } => write!(f, "{} & {}", lhs, rhs),
            Self::BoolOr { lhs, rhs } => write!(f, "{} | {}", lhs, rhs),
            Self::BoolXor { lhs, rhs } => write!(f, "{} ^ {}", lhs, rhs),
            Self::BoolImplies { lhs, rhs } => write!(f, "{} => {}", lhs, rhs),
            Self::IntVal(v) => write!(f, "{}", v),
            Self::NumVal(v) => write!(f, "{}", v),
            Self::IntLt { lhs, rhs } | Self::NumLt { lhs, rhs } | Self::StrLt { lhs, rhs } => {
                write!(f, "{} < {}", lhs, rhs)
            }
            Self::IntLe { lhs, rhs } | Self::NumLe { lhs, rhs } | Self::StrLe { lhs, rhs } => {
                write!(f, "{} <= {}", lhs, rhs)
            }
            Self::IntGe { lhs, rhs } | Self::NumGe { lhs, rhs } => write!(f, "{} >= {}", lhs, rhs),
            Self::IntGt { lhs, rhs } | Self::NumGt { lhs, rhs } => write!(f, "{} > {}", lhs, rhs),
            Self::IntAdd { lhs, rhs } | Self::NumAdd { lhs, rhs } => write!(f, "{} + {}", lhs, rhs),
            Self::IntSub { lhs, rhs } | Self::NumSub { lhs, rhs } => write!(f, "{} - {}", lhs, rhs),
            Self::IntMul { lhs, rhs } | Self::NumMul { lhs, rhs } => write!(f, "{} * {}", lhs, rhs),
            Self::IntDiv { lhs, rhs } | Self::NumDiv { lhs, rhs } => write!(f, "{} / {}", lhs, rhs),
            Self::IntRem { lhs, rhs } => write!(f, "{} % {}", lhs, rhs),
            Self::StrVal(v) => write!(f, "{}", v),
            Self::BoxShield { t, val } => write!(f, "&<{}>({})", t, val),
            Self::BoxReveal { t, val } => write!(f, "*<{}>({})", t, val),
            Self::SeqEmpty { t } => write!(f, "vec<{}>[]", t),
            Self::SeqLength { t, seq } => write!(f, "{}.len<{}>(vec)", seq, t),
            Self::SeqAppend { t, seq, item } => write!(f, "{}.append<{}>({})", seq, t, item),
            Self::SeqAt { t, seq, idx } => write!(f, "{}.at<{}>({})", seq, t, idx),
            Self::SeqIncludes { t, seq, item } => write!(f, "{}.includes<{}>({})", seq, t, item),
            Self::SetEmpty { t } => write!(f, "set<{}>[]", t),
            Self::SetLength { t, set } => write!(f, "{}.len<{}>(set)", set, t),
            Self::SetInsert { t, set, item } => write!(f, "{}.insert<{}>({})", set, t, item),
            Self::SetRemove { t, set, item } => write!(f, "{}.remove<{}>({})", set, t, item),
            Self::SetContains { t, set, item } => write!(f, "{}.contains<{}>({})", set, t, item),
            Self::MapEmpty { k, v } => write!(f, "map<{},{}>[]", k, v),
            Self::MapLength { k, v, map } => write!(f, "{}.len<{},{}>(map)", map, k, v),
            Self::MapPut {
                k,
                v,
                map,
                key,
                val,
            } => write!(f, "{}.put<{},{}>({},{})", map, k, v, key, val),
            Self::MapGet { k, v, map, key } => write!(f, "{}.get<{},{}>({})", map, k, v, key),
            Self::MapDel { k, v, map, key } => write!(f, "{}.del<{},{}>({})", map, k, v, key),
            Self::MapContainsKey { k, v, map, key } => {
                write!(f, "{}.contains_key<{},{}>({})", map, k, v, key)
            }
            Self::ErrFresh => write!(f, "error"),
            Self::ErrMerge { lhs, rhs } => write!(f, "{} ~ {}", lhs, rhs),
            Self::SmtEq { t: _, lhs, rhs } => write!(f, "{} == {}", lhs, rhs),
            Self::SmtNe { t: _, lhs, rhs } => write!(f, "{} != {}", lhs, rhs),
        }
    }
}
