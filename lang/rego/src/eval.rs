use rusmart_smt_remark::{smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, Map, Rational, Seq, Set, Text, SMT};
use rusmart_smt_stdlib::smt_expr;

/// A term *in its valid state* is defined by the following ADT
#[smt_type]
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub enum Value {
    #[default]
    Null,
    Boolean(Boolean),
    Number(Rational),
    String(Text),
    Seq(Seq<Value>),
    Map(Map<Value, Value>),
    Set(Set<Value>),
}

impl SMT for Value {}

/// A term *in any state* is defined by the following ADT
#[smt_type]
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub enum State {
    #[default]
    Undef,
    Value(Value),
    Error(Error),
}

impl SMT for State {}

#[smt_impl]
pub fn seq_lt_recursive(l: Seq<Value>, r: Seq<Value>, i: Integer) -> Boolean {
    #[allow(clippy::if_same_then_else)]
    if Seq::length(r) == i {
        false.into()
    } else if Seq::length(r) == i {
        true.into()
    } else if *lt(Seq::get_unchecked(l, i), Seq::get_unchecked(r, i)) {
        true.into()
    } else {
        seq_lt_recursive(l, r, Integer::add(i, 1.into()))
    }
}

#[smt_impl]
pub fn seq_lt(l: Seq<Value>, r: Seq<Value>) -> Boolean {
    seq_lt_recursive(l, r, 0.into())
}

#[smt_impl]
pub fn lt(lhs: Value, rhs: Value) -> Boolean {
    match (lhs, rhs) {
        // null
        (
            Value::Null,
            Value::Boolean(_)
            | Value::Number(_)
            | Value::String(_)
            | Value::Seq(_)
            | Value::Map(_)
            | Value::Set(_),
        ) => true.into(),

        // boolean
        (Value::Boolean(v_lhs), Value::Boolean(v_rhs)) => Boolean::and(v_lhs, Boolean::not(v_rhs)),
        (
            Value::Boolean(_),
            Value::Number(_) | Value::String(_) | Value::Seq(_) | Value::Map(_) | Value::Set(_),
        ) => true.into(),

        // number
        (Value::Number(v_lhs), Value::Number(v_rhs)) => (v_lhs < v_rhs).into(),
        (Value::Number(_), Value::String(_) | Value::Seq(_) | Value::Map(_) | Value::Set(_)) => {
            true.into()
        }

        // string
        (Value::String(v_lhs), Value::String(v_rhs)) => (v_lhs < v_rhs).into(),
        (Value::String(_), Value::Seq(_) | Value::Map(_) | Value::Set(_)) => true.into(),

        // seq
        (Value::Seq(v_lhs), Value::Seq(v_rhs)) => seq_lt(v_lhs, v_rhs),
        (Value::Seq(_), Value::Map(_) | Value::Set(_)) => true.into(),

        // TODO (others)

        // default
        _ => false.into(),
    }
}

#[smt_spec(lt)]
pub fn spec_lt(_lhs: Value, _rhs: Value) -> Boolean {
    false.into()
}

#[smt_spec(seq_lt)]
pub fn spec_seq_lt(lhs: Seq<Value>, rhs: Seq<Value>) -> Boolean {
    /*
    (|k: Integer| {
        Boolean::from(k >= 0.into())
            & (k < Seq::length(rhs)).into()
            & (|i: Integer| {
                Boolean::from(i >= 0.into())
                    & (i < k).into()
                    & spec_lt(Seq::at_unchecked(lhs, i), Seq::at_unchecked(rhs, k))
            })(Integer::forall())
    })(Integer::exists())
     */
    todo!()
}

pub fn test() -> Boolean {
    let v = Value::any();
    smt_expr!(
    (~ v
        | Value::Null => (b true)
        _ => (b false)
    ))
}
