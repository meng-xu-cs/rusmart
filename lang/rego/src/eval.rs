use rusmart_smt_remark::{smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, Map, Rational, Seq, Set, Text, SMT};
use rusmart_smt_stdlib::exp::forall;

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
    if *l.length().eq(i) {
        Boolean::from(false)
    } else if *r.length().eq(i) {
        Boolean::from(true)
    } else if *l.at_unchecked(i).lt(r.at_unchecked(i)) {
        Boolean::from(true)
    } else if *r.at_unchecked(i).lt(l.at_unchecked(i)) {
        Boolean::from(false)
    } else {
        seq_lt_recursive(l, r, i.add(1.into()))
    }
}

#[smt_impl]
pub fn seq_lt(l: Seq<Value>, r: Seq<Value>) -> Boolean {
    seq_lt_recursive(l, r, Integer::from(0))
}

#[smt_impl(method = lt)]
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
        (Value::Number(v_lhs), Value::Number(v_rhs)) => Rational::lt(v_lhs, v_rhs),
        (Value::Number(_), Value::String(_) | Value::Seq(_) | Value::Map(_) | Value::Set(_)) => {
            true.into()
        }

        // string
        (Value::String(v_lhs), Value::String(v_rhs)) => Text::lt(v_lhs, v_rhs),
        (Value::String(_), Value::Seq(_) | Value::Map(_) | Value::Set(_)) => true.into(),

        // seq
        (Value::Seq(v_lhs), Value::Seq(v_rhs)) => seq_lt(v_lhs, v_rhs),
        (Value::Seq(_), Value::Map(_) | Value::Set(_)) => true.into(),

        // TODO (others)

        // default
        (_, _) => false.into(),
    }
}

#[smt_spec(impls = [lt])]
pub fn spec_lt(_lhs: Value, _rhs: Value) -> Boolean {
    false.into()
}

#[smt_spec(impls = [seq_lt])]
pub fn spec_seq_lt(lhs: Seq<Value>, rhs: Seq<Value>) -> Boolean {
    forall!(|k: Integer| Integer::ge(k, Integer::from(0)))
}
