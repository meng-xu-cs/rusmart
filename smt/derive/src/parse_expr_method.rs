use crate::err::bail_on;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{Expr as Exp, ExprMethodCall, Result};

use crate::parse_expr::{CtxtForExpr, Expr, ExprParseCtxt, Inst};
use crate::parse_path::FuncName;
use crate::parse_type::TypeTag;

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

/// Helper on known logical operators
enum OpLogic {
    Not,
    And,
    Or,
    Xor,
}

/// Helper on known arithmetic operators
enum OpArith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

/// Helper on known comparison operators
enum OpCmp {
    Eq,
    Ne,
    Lt,
    Le,
    Ge,
    Gt,
}

enum PrimitiveOperator {
    // error
    Merge,
    // collection
    Length,
    Contains,
    ContainsKey,
    Append,
    Insert,
    AtUnchecked,
    PutUnchecked,
    GetUnchecked,
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
        match method.as_ref() {
            // arithmetics
            "add" => match parser.expected_type() {
                None => (),
                Some(ety) => match ety {
                    TypeTag::Integer | TypeTag::Rational => {}
                    _ => bail_on!(method, "unexpected type"),
                },
            },

            // numerical
            _ => bail_on!(method, "unknown method"),
        }
    }

    /// Handle arithmetic operator
    fn handle_op_arith(
        &self,
        op: OpArith,
        receiver: ExprStatus,
        args: Vec<ExprStatus>,
        spanned: &ExprMethodCall,
    ) -> Result<()> {
        let a1 = unpack_status_1(args, spanned)?;
        match (self.expected_type(), receiver.ty(), a1.ty()) {}
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