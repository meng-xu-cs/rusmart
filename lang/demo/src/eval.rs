use rusmart_smt_remark::{smt_impl, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Error, Integer, SMT};

/// A term *in its valid state* is defined by the following ADT
#[smt_type]
pub enum Value {
    Null,
    Boolean(Boolean),
    Integer(Integer),
}

/// A term *in any state* is defined by the following ADT
#[smt_type]
pub enum State {
    Undef,
    Value(Value),
    Error(Error),
}

#[smt_impl(method = lt)]
fn lt_value(lhs: Value, rhs: Value) -> State {
    match (lhs, rhs) {
        // null will make every operation null
        (Value::Null, Value::Null) => State::Value(Value::Null),
        (Value::Null, Value::Boolean(_)) => State::Value(Value::Null),
        (Value::Null, Value::Integer(_)) => State::Value(Value::Null),
        (Value::Boolean(_), Value::Null) => State::Value(Value::Null),
        (Value::Integer(_), Value::Null) => State::Value(Value::Null),

        // boolean leads to a type error
        (Value::Boolean(_), Value::Boolean(_)) => State::Error(Error::fresh()),
        (Value::Boolean(_), Value::Integer(_)) => State::Error(Error::fresh()),
        (Value::Integer(_), Value::Boolean(_)) => State::Error(Error::fresh()),

        // integer
        (Value::Integer(l), Value::Integer(r)) => State::Value(Value::Boolean(l.lt(r))),
    }
}

#[smt_impl(method = lt)]
pub fn lt(lhs: State, rhs: State) -> State {
    match (lhs, rhs) {
        // undef will cause an error
        (State::Undef, State::Undef) => State::Error(Error::fresh()),
        (State::Undef, State::Value(_)) => State::Error(Error::fresh()),
        (State::Undef, State::Error(e)) => State::Error(Error::fresh().merge(e)),
        (State::Value(_), State::Undef) => State::Error(Error::fresh()),
        (State::Error(e), State::Undef) => State::Error(Error::fresh().merge(e)),

        // error will be propagated
        (State::Error(l), State::Error(r)) => State::Error(l.merge(r)),
        (State::Error(e), State::Value(_)) => State::Error(e),
        (State::Value(_), State::Error(e)) => State::Error(e),

        // value op is delegated
        (State::Value(l), State::Value(r)) => l.lt(r),
    }
}
