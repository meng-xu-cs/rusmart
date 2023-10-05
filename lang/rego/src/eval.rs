use rusmart_smt_remark::{smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, Map, Quantified, Rational, Seq, Set, Text};

/// A term *in its valid state* is defined by the following ADT
#[smt_type]
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
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

impl Quantified for Value {}

/// A term *in any state* is defined by the following ADT
#[smt_type]
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub enum State {
    #[default]
    Undef,
    Value(Value),
    Error(Error),
}

impl Quantified for State {}

#[smt_impl]
pub fn seq_lt_recursive(l: &Seq<Value>, r: &Seq<Value>, i: &Integer) -> Boolean {
    #[allow(clippy::if_same_then_else)]
    if *Integer::eq(&Seq::length(r), i) {
        Boolean::new(false)
    } else if *Integer::eq(&Seq::length(l), i) {
        Boolean::new(true)
    } else if *lt(&Seq::at_unchecked(l, i), &Seq::at_unchecked(r, i)) {
        Boolean::new(true)
    } else {
        seq_lt_recursive(l, r, &Integer::add(i, &Integer::new(1)))
    }
}

#[smt_impl]
pub fn seq_lt(l: &Seq<Value>, r: &Seq<Value>) -> Boolean {
    seq_lt_recursive(l, r, &Integer::new(0))
}

#[smt_impl]
pub fn lt(_lhs: &Value, _rhs: &Value) -> Boolean {
    // TODO: match
    Boolean::new(false)
}

#[smt_spec(lt)]
pub fn spec_lt(_lhs: &Value, _rhs: &Value) -> Boolean {
    // TODO: match
    Boolean::new(false)
}
