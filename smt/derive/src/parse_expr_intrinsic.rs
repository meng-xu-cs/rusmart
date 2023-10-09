use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr as Exp, ExprLit, Lit, Result};

use crate::err::bail_on;
use crate::parse_expr::{CtxtForExpr, Expr, ExprParseCtxt};
use crate::parse_type::TypeTag;

/// Intrinsic procedure
#[derive(Clone)]
pub enum Intrinsic {
    /// `Boolean::from`
    BoolVal(bool),
    /// `Boolean::not`
    Not(Expr),
    /// `Boolean::and`
    And(Expr, Expr),
    /// `Boolean::or`
    Or(Expr, Expr),
    /// `Boolean::xor`
    Xor(Expr, Expr),
    /// `Integer::from`
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
    /// `Rational::from`
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
    /// `Text::from`
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

/// Helper macro for reporting type mismatch
macro_rules! bail_on_type_mismatch {
    ($func:expr, $ety:expr, $aty:expr) => {
        bail_on!($func, "type mismatch: expect {} | actual {}", $ety, $aty)
    };
}

impl Intrinsic {
    /// Convert an unpacked expression into intrinsics
    pub fn convert_and_infer_type<T: CtxtForExpr>(
        parser: &ExprParseCtxt<'_, T>,
        class: &str,
        method: &str,
        func: &Exp,
        args: &Punctuated<Exp, Comma>,
    ) -> Result<(Self, TypeTag)> {
        use Intrinsic::*;
        use TypeTag::*;

        let check_type = |ety: &TypeTag, aty: &TypeTag| {
            if ety != aty {
                bail_on!(func, "type mismatch: expect {} | actual {}", ety, aty);
            }
            Ok(())
        };

        let (intrinsic, ty) = match (class, method) {
            // boolean
            ("Boolean", "from") => {
                // argument must be a literal
                let val = Self::unpack_lit_bool(args)?;
                (BoolVal(val), Boolean)
            }
            ("Boolean", "not") => {
                let a1 = Self::unpack_arg_1(parser, args, Some(Boolean))?;
                (Not(a1), Boolean)
            }
            ("Boolean", "and") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Boolean), Some(Boolean))?;
                (And(a1, a2), Boolean)
            }
            ("Boolean", "or") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Boolean), Some(Boolean))?;
                (Or(a1, a2), Boolean)
            }
            ("Boolean", "xor") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Boolean), Some(Boolean))?;
                (Xor(a1, a2), Boolean)
            }

            // integer
            ("Integer", "from") => {
                // argument must be a literal
                let val = Self::unpack_lit_int(args)?;
                (IntVal(val), Integer)
            }
            ("Integer", "add") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntAdd(a1, a2), Integer)
            }
            ("Integer", "sub") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntSub(a1, a2), Integer)
            }
            ("Integer", "mul") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntMul(a1, a2), Integer)
            }
            ("Integer", "div") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntDiv(a1, a2), Integer)
            }
            ("Integer", "rem") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntRem(a1, a2), Integer)
            }
            ("Integer", "eq") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntEq(a1, a2), Boolean)
            }
            ("Integer", "ne") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntNe(a1, a2), Boolean)
            }
            ("Integer", "lt") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntLt(a1, a2), Boolean)
            }
            ("Integer", "le") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntLe(a1, a2), Boolean)
            }
            ("Integer", "ge") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntGe(a1, a2), Boolean)
            }
            ("Integer", "gt") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Integer), Some(Integer))?;
                (IntGt(a1, a2), Boolean)
            }

            // rational
            ("Rational", "from") => {
                // argument must be a literal
                let val = Self::unpack_lit_int(args)?;
                (NumVal(val), Rational)
            }
            ("Rational", "add") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumAdd(a1, a2), Rational)
            }
            ("Rational", "sub") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumSub(a1, a2), Rational)
            }
            ("Rational", "mul") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumMul(a1, a2), Rational)
            }
            ("Rational", "div") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumDiv(a1, a2), Rational)
            }
            ("Rational", "eq") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumEq(a1, a2), Boolean)
            }
            ("Rational", "ne") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumNe(a1, a2), Boolean)
            }
            ("Rational", "lt") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumLt(a1, a2), Boolean)
            }
            ("Rational", "le") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumLe(a1, a2), Boolean)
            }
            ("Rational", "ge") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumGe(a1, a2), Boolean)
            }
            ("Rational", "gt") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Rational), Some(Rational))?;
                (NumGt(a1, a2), Boolean)
            }

            // text
            ("Text", "from") => {
                // argument must be a literal
                let val = Self::unpack_lit_str(args)?;
                (StrVal(val), Text)
            }
            ("Text", "eq") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Text), Some(Text))?;
                (StrEq(a1, a2), Boolean)
            }
            ("Text", "ne") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Text), Some(Text))?;
                (StrNe(a1, a2), Boolean)
            }
            ("Text", "lt") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Text), Some(Text))?;
                (StrLt(a1, a2), Boolean)
            }
            ("Text", "le") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Text), Some(Text))?;
                (StrLe(a1, a2), Boolean)
            }

            // seq
            ("Seq", "empty") => {
                Self::unpack_arg_0(args)?;
                let rty = match parser.expected_type() {
                    None => bail_on!(func, "type annotation needed"),
                    Some(t @ Seq(_)) => t.clone(),
                    Some(t) => bail_on_type_mismatch!(func, t, "Seq<?>"),
                };
                (SeqEmpty, rty)
            }
            ("Seq", "append") => match parser.expected_type() {
                None => {
                    let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                    let (t1, t2) = (a1.ty(), a2.ty());
                    let inferred = match t1 {
                        Seq(sub) => {
                            check_type(sub, t2)?;
                            t1.clone()
                        }
                        _ => bail_on_type_mismatch!(func, format!("Seq<{}>", t2), t1),
                    };
                    (SeqAppend(a1, a2), inferred)
                }
                Some(t @ Seq(sub)) => {
                    let (a1, a2) = Self::unpack_arg_2(
                        parser,
                        args,
                        Some(t.clone()),
                        Some(sub.as_ref().clone()),
                    )?;
                    (SeqAppend(a1, a2), t.clone())
                }
                Some(t) => bail_on_type_mismatch!(func, t, "Seq<?>"),
            },
            ("Seq", "length") => {
                let a1 = Self::unpack_arg_1(parser, args, None)?;
                let t1 = a1.ty();
                if !matches!(t1, Seq(_)) {
                    bail_on_type_mismatch!(func, "Seq<?>", t1);
                }
                (SeqLength(a1), Integer)
            }
            ("Seq", "contains") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                let (t1, t2) = (a1.ty(), a2.ty());
                match t1 {
                    Seq(sub) => {
                        check_type(sub, t2)?;
                    }
                    _ => bail_on_type_mismatch!(func, format!("Seq<{}>", t2), t1),
                }
                (SeqContains(a1, a2), Boolean)
            }
            ("Seq", "at_unchecked") => match parser.expected_type() {
                None => {
                    let (a1, a2) = Self::unpack_arg_2(parser, args, None, Some(Integer))?;
                    let inferred = match a1.ty() {
                        Seq(sub) => sub.as_ref().clone(),
                        t => bail_on_type_mismatch!(func, "Seq<?>", t),
                    };
                    (SeqAt(a1, a2), inferred)
                }
                Some(t) => {
                    let (a1, a2) = Self::unpack_arg_2(
                        parser,
                        args,
                        Some(Seq(t.clone().into())),
                        Some(Integer),
                    )?;
                    (SeqAt(a1, a2), t.clone())
                }
            },

            // set
            ("Set", "empty") => {
                Self::unpack_arg_0(args)?;
                let rty = match parser.expected_type() {
                    None => bail_on!(func, "type annotation needed"),
                    Some(t @ Set(_)) => t.clone(),
                    Some(t) => bail_on_type_mismatch!(func, t, "Set<?>"),
                };
                (SetEmpty, rty)
            }
            ("Set", "insert") => match parser.expected_type() {
                None => {
                    let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                    let (t1, t2) = (a1.ty(), a2.ty());
                    let inferred = match t1 {
                        Set(sub) => {
                            check_type(sub, t2)?;
                            t1.clone()
                        }
                        _ => bail_on_type_mismatch!(func, format!("Set<{}>", t2), t1),
                    };
                    (SetInsert(a1, a2), inferred)
                }
                Some(t @ Set(sub)) => {
                    let (a1, a2) = Self::unpack_arg_2(
                        parser,
                        args,
                        Some(t.clone()),
                        Some(sub.as_ref().clone()),
                    )?;
                    (SetInsert(a1, a2), t.clone())
                }
                Some(t) => bail_on_type_mismatch!(func, t, "Set<?>"),
            },
            ("Set", "length") => {
                let a1 = Self::unpack_arg_1(parser, args, None)?;
                let t1 = a1.ty();
                if !matches!(t1, Set(_)) {
                    bail_on_type_mismatch!(func, "Set<?>", t1);
                }
                (SetLength(a1), Integer)
            }
            ("Set", "contains") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                let (t1, t2) = (a1.ty(), a2.ty());
                match t1 {
                    Set(sub) => {
                        check_type(sub, t2)?;
                    }
                    _ => bail_on_type_mismatch!(func, format!("Set<{}>", t2), t1),
                }
                (SetContains(a1, a2), Boolean)
            }

            // map
            ("Map", "empty") => {
                Self::unpack_arg_0(args)?;
                let rty = match parser.expected_type() {
                    None => bail_on!(func, "type annotation needed"),
                    Some(t @ Map(_, _)) => t.clone(),
                    Some(t) => bail_on_type_mismatch!(func, t, "Map<?>"),
                };
                (MapEmpty, rty)
            }
            ("Map", "put_unchecked") => match parser.expected_type() {
                None => {
                    let (a1, a2, a3) = Self::unpack_arg_3(parser, args, None, None, None)?;
                    let (t1, t2, t3) = (a1.ty(), a2.ty(), a3.ty());
                    let inferred = match t1 {
                        Map(key, val) => {
                            check_type(key, t2)?;
                            check_type(val, t2)?;
                            t1.clone()
                        }
                        _ => bail_on_type_mismatch!(func, format!("Map<{},{}>", t2, t3), t1),
                    };
                    (MapPut(a1, a2, a3), inferred)
                }
                Some(t @ Map(key, val)) => {
                    let (a1, a2, a3) = Self::unpack_arg_3(
                        parser,
                        args,
                        Some(t.clone()),
                        Some(key.as_ref().clone()),
                        Some(val.as_ref().clone()),
                    )?;
                    (MapPut(a1, a2, a3), t.clone())
                }
                Some(t) => bail_on_type_mismatch!(func, t, "Map<?,?>"),
            },
            ("Map", "get_unchecked") => match parser.expected_type() {
                None => {
                    let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                    let (t1, t2) = (a1.ty(), a2.ty());
                    let inferred = match t1 {
                        Map(key, val) => {
                            check_type(key, t2)?;
                            val.as_ref().clone()
                        }
                        _ => bail_on_type_mismatch!(func, format!("Map<{},?>", t2), t1),
                    };
                    (MapGet(a1, a2), inferred)
                }
                Some(t) => {
                    let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                    let (t1, t2) = (a1.ty(), a2.ty());
                    match t1 {
                        Map(key, val) => {
                            check_type(key, t2)?;
                            check_type(val, t)?;
                        }
                        _ => bail_on_type_mismatch!(func, format!("Map<{},{}>", t2, t), t1),
                    };
                    (MapGet(a1, a2), t.clone())
                }
            },
            ("Map", "length") => {
                let a1 = Self::unpack_arg_1(parser, args, None)?;
                let t1 = a1.ty();
                if !matches!(t1, Map(_, _)) {
                    bail_on_type_mismatch!(func, "Map<?,?>", t1);
                }
                (MapLength(a1), Integer)
            }
            ("Map", "contains") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, None, None)?;
                let (t1, t2) = (a1.ty(), a2.ty());
                match t1 {
                    Map(key, _) => {
                        check_type(key, t2)?;
                    }
                    _ => bail_on_type_mismatch!(func, format!("Map<{},?>", t2), t1),
                }
                (MapContainsKey(a1, a2), Boolean)
            }

            // error
            ("Error", "fresh") => {
                Self::unpack_arg_0(args)?;
                (ErrFresh, Error)
            }
            ("Error", "merge") => {
                let (a1, a2) = Self::unpack_arg_2(parser, args, Some(Error), Some(Error))?;
                (ErrMerge(a1, a2), Error)
            }

            // others
            _ => bail_on!(func, "not an intrinsic call"),
        };

        // type check
        if let Some(ety) = parser.expected_type() {
            check_type(ety, &ty)?;
        }

        // done with the conversion
        Ok((intrinsic, ty))
    }

    /// Unpack 0 arguments
    fn unpack_arg_0(args: &Punctuated<Exp, Comma>) -> Result<()> {
        let mut iter = args.iter();
        match iter.next() {
            None => Ok(()),
            Some(arg) => bail_on!(arg, "unexpected argument"),
        }
    }

    /// Unpack 1 arguments
    fn unpack_arg_1<T: CtxtForExpr>(
        parser: &ExprParseCtxt<'_, T>,
        args: &Punctuated<Exp, Comma>,
        p1: Option<TypeTag>,
    ) -> Result<Expr> {
        let mut iter = args.iter();
        let a1 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p1).convert_expr(arg)?,
        };
        match iter.next() {
            None => Ok(a1),
            Some(arg) => bail_on!(arg, "unexpected argument"),
        }
    }

    /// Unpack 2 arguments
    fn unpack_arg_2<T: CtxtForExpr>(
        parser: &ExprParseCtxt<'_, T>,
        args: &Punctuated<Exp, Comma>,
        p1: Option<TypeTag>,
        p2: Option<TypeTag>,
    ) -> Result<(Expr, Expr)> {
        let mut iter = args.iter();
        let a1 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p1).convert_expr(arg)?,
        };
        let a2 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p2).convert_expr(arg)?,
        };
        match iter.next() {
            None => Ok((a1, a2)),
            Some(arg) => bail_on!(arg, "unexpected argument"),
        }
    }

    /// Unpack 3 arguments
    fn unpack_arg_3<T: CtxtForExpr>(
        parser: &ExprParseCtxt<'_, T>,
        args: &Punctuated<Exp, Comma>,
        p1: Option<TypeTag>,
        p2: Option<TypeTag>,
        p3: Option<TypeTag>,
    ) -> Result<(Expr, Expr, Expr)> {
        let mut iter = args.iter();
        let a1 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p1).convert_expr(arg)?,
        };
        let a2 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p2).convert_expr(arg)?,
        };
        let a3 = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => parser.dup(p3).convert_expr(arg)?,
        };
        match iter.next() {
            None => Ok((a1, a2, a3)),
            Some(arg) => bail_on!(arg, "unexpected argument"),
        }
    }

    /// Convert an argument list to a boolean literal
    fn unpack_lit_bool(args: &Punctuated<Exp, Comma>) -> Result<bool> {
        let mut iter = args.iter();
        let expr = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => arg,
        };
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

    /// Convert an argument list to an integer literal
    fn unpack_lit_int(args: &Punctuated<Exp, Comma>) -> Result<i128> {
        let mut iter = args.iter();
        let expr = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => arg,
        };
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

    /// Convert an argument list to a string literal
    fn unpack_lit_str(args: &Punctuated<Exp, Comma>) -> Result<String> {
        let mut iter = args.iter();
        let expr = match iter.next() {
            None => bail_on!(args, "expect more arguments"),
            Some(arg) => arg,
        };
        match expr {
            Exp::Lit(expr_lit) => {
                let ExprLit { attrs: _, lit } = expr_lit;
                match lit {
                    Lit::Str(val) => Ok(val.token().to_string()),
                    _ => bail_on!(lit, "not a string literal"),
                }
            }
            _ => bail_on!(expr, "not a literal"),
        }
    }
}
