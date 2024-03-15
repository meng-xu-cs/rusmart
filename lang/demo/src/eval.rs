use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, Map, SMT};

/// A term *in its valid state* is defined by the following ADT
#[smt_type]
pub enum Value {
    Null,
    Boolean(Boolean),
    Integer(Integer),
}

/// A term *in any state* is defined by the following ADT
#[smt_type]
pub enum Expr {
    Undef,
    Value(Value),
    Error(Error),
}

#[smt_impl(method = lt)]
fn lt_value(lhs: Value, rhs: Value) -> Expr {
    match (lhs, rhs) {
        // null will make every operation null
        (Value::Null, Value::Null) => Expr::Value(Value::Null),
        (Value::Null, Value::Boolean(_)) => Expr::Value(Value::Null),
        (Value::Null, Value::Integer(_)) => Expr::Value(Value::Null),
        (Value::Boolean(_), Value::Null) => Expr::Value(Value::Null),
        (Value::Integer(_), Value::Null) => Expr::Value(Value::Null),

        // boolean leads to a type error
        (Value::Boolean(_), Value::Boolean(_)) => Expr::Error(Error::fresh()),
        (Value::Boolean(_), Value::Integer(_)) => Expr::Error(Error::fresh()),
        (Value::Integer(_), Value::Boolean(_)) => Expr::Error(Error::fresh()),

        // integer
        (Value::Integer(l), Value::Integer(r)) => Expr::Value(Value::Boolean(l.lt(r))),
    }
}

#[smt_impl(method = lt)]
pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
    match (lhs, rhs) {
        // undef will cause an error
        (Expr::Undef, Expr::Undef) => Expr::Error(Error::fresh()),
        (Expr::Undef, Expr::Value(_)) => Expr::Error(Error::fresh()),
        (Expr::Undef, Expr::Error(e)) => Expr::Error(Error::fresh().merge(e)),
        (Expr::Value(_), Expr::Undef) => Expr::Error(Error::fresh()),
        (Expr::Error(e), Expr::Undef) => Expr::Error(Error::fresh().merge(e)),

        // error will be propagated
        (Expr::Error(l), Expr::Error(r)) => Expr::Error(l.merge(r)),
        (Expr::Error(e), Expr::Value(_)) => Expr::Error(e),
        (Expr::Value(_), Expr::Error(e)) => Expr::Error(e),

        // value op is delegated
        (Expr::Value(l), Expr::Value(r)) => l.lt(r),
    }
}

#[smt_type]
pub struct Variable;

#[smt_type]
pub struct Namespace {
    map: Map<Variable, Expr>,
}

#[smt_impl(method = assign)]
pub fn assign(ns: Namespace, var: Variable, expr: Expr) -> Namespace {
    Namespace {
        map: ns.map.put_unchecked(var, expr),
    }
}

#[smt_impl(method = retrieve)]
pub fn retrieve(ns: Namespace, var: Variable) -> Expr {
    if *ns.map.contains_key(var) {
        ns.map.get_unchecked(var)
    } else {
        Expr::Undef
    }
}
