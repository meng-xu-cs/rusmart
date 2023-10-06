use rusmart_smt_remark::{smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, Map, Rational, Seq, Set, Text, SMT};

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
    } else if *lt(Seq::at_unchecked(l, i), Seq::at_unchecked(r, i)) {
        true.into()
    } else {
        seq_lt_recursive(l, r, i + 1.into())
    }
}

#[smt_impl]
pub fn seq_lt(l: Seq<Value>, r: Seq<Value>) -> Boolean {
    seq_lt_recursive(l, r, 0.into())
}

#[smt_impl]
pub fn lt(_lhs: Value, _rhs: Value) -> Boolean {
    // TODO: match
    false.into()
}

#[smt_spec(lt)]
pub fn spec_lt(_lhs: Value, _rhs: Value) -> Boolean {
    // TODO: match
    false.into()
}
