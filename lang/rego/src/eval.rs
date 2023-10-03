use rusmart_smt_remark::smt_type;
use rusmart_smt_stdlib::dt::{Boolean, Error, Map, Rational, Seq, Set, Text};

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

/// A term *in any state* is defined by the following ADT
#[smt_type]
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub enum State {
    #[default]
    Undef,
    Value(Value),
    Error(Error),
}
