use rusmart_smt_remark_derive::{smt_axiom, smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::{Boolean, Error, Integer, Map, Rational, Seq, Set, Text, SMT};
use rusmart_smt_stdlib::{choose, forall};

/// A term *in its valid state* is defined by the following ADT
#[smt_type]
pub enum Value {
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
pub enum State {
    Undef,
    Value(Value),
    Error(Error),
}

#[smt_impl]
fn seq_lt_recursive(l: Seq<Value>, r: Seq<Value>, i: Integer) -> Boolean {
    if *l.length().eq(i) {
        r.length().ne(i)
    } else if *r.length().eq(i) {
        false.into()
    } else {
        let v_l = l.at_unchecked(i);
        let v_r = r.at_unchecked(i);
        if *v_l.lt(v_r) {
            true.into()
        } else if *v_r.lt(v_l) {
            false.into()
        } else {
            seq_lt_recursive(l, r, i.add(1.into()))
        }
    }
}

#[smt_impl]
fn seq_lt(l: Seq<Value>, r: Seq<Value>) -> Boolean {
    seq_lt_recursive(l, r, Integer::from(0))
}

#[smt_impl]
fn set_min(set: Set<Value>) -> Value {
    choose!(v in set => forall!(e in set => v.eq(e).or(v.lt(e))))
}

#[smt_type]
enum SetCmpResult {
    Done(Boolean),
    Next(Set<Value>, Set<Value>),
}

#[smt_impl]
fn set_lt_recursive(l: Set<Value>, r: Set<Value>) -> SetCmpResult {
    if *l.length().eq(0.into()) {
        SetCmpResult::Done(r.length().ne(0.into()))
    } else if *r.length().eq(0.into()) {
        SetCmpResult::Done(false.into())
    } else {
        let min_l = set_min(l);
        let min_r = set_min(r);
        if *min_l.lt(min_r) {
            SetCmpResult::Done(true.into())
        } else if *min_r.lt(min_l) {
            SetCmpResult::Done(false.into())
        } else {
            SetCmpResult::Next(l.remove(min_l), r.remove(min_r))
        }
    }
}

#[smt_impl]
fn set_lt(l: Set<Value>, r: Set<Value>) -> Boolean {
    match set_lt_recursive(l, r) {
        SetCmpResult::Done(result) => result,
        SetCmpResult::Next(next_l, next_r) => set_lt(next_l, next_r),
    }
}

#[smt_impl]
fn map_key_min(map: Map<Value, Value>) -> Value {
    choose!(v in map => forall!(e in map => v.eq(e).or(v.lt(e))))
}

#[smt_type]
enum MapCmpResult {
    Done(Boolean),
    Next(Map<Value, Value>, Map<Value, Value>),
}

#[smt_impl]
fn map_lt_recursive(l: Map<Value, Value>, r: Map<Value, Value>) -> MapCmpResult {
    if *l.length().eq(0.into()) {
        MapCmpResult::Done(r.length().ne(0.into()))
    } else if *r.length().eq(0.into()) {
        MapCmpResult::Done(false.into())
    } else {
        let min_key_l = map_key_min(l);
        let min_key_r = map_key_min(r);
        if *min_key_l.lt(min_key_r) {
            MapCmpResult::Done(true.into())
        } else if *min_key_r.lt(min_key_l) {
            MapCmpResult::Done(false.into())
        } else {
            let val_l = l.get_unchecked(min_key_l);
            let val_r = r.get_unchecked(min_key_r);
            if *val_l.lt(val_r) {
                MapCmpResult::Done(true.into())
            } else if *val_r.lt(val_l) {
                MapCmpResult::Done(false.into())
            } else {
                MapCmpResult::Next(l.del_unchecked(min_key_l), r.del_unchecked(min_key_r))
            }
        }
    }
}

#[smt_impl]
fn map_lt(l: Map<Value, Value>, r: Map<Value, Value>) -> Boolean {
    match map_lt_recursive(l, r) {
        MapCmpResult::Done(result) => result,
        MapCmpResult::Next(next_l, next_r) => map_lt(next_l, next_r),
    }
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
        (Value::Boolean(v_lhs), Value::Boolean(v_rhs)) => v_lhs.not().and(v_rhs),
        (
            Value::Boolean(_),
            Value::Number(_) | Value::String(_) | Value::Seq(_) | Value::Map(_) | Value::Set(_),
        ) => true.into(),

        // number
        (Value::Number(v_lhs), Value::Number(v_rhs)) => v_lhs.lt(v_rhs),
        (Value::Number(_), Value::String(_) | Value::Seq(_) | Value::Map(_) | Value::Set(_)) => {
            true.into()
        }

        // string
        (Value::String(v_lhs), Value::String(v_rhs)) => v_lhs.lt(v_rhs),
        (Value::String(_), Value::Seq(_) | Value::Map(_) | Value::Set(_)) => true.into(),

        // seq
        (Value::Seq(v_lhs), Value::Seq(v_rhs)) => seq_lt(v_lhs, v_rhs),
        (Value::Seq(_), Value::Map(_) | Value::Set(_)) => true.into(),

        // map
        (Value::Map(v_lhs), Value::Map(v_rhs)) => map_lt(v_lhs, v_rhs),
        (Value::Map(_), Value::Set(_)) => true.into(),

        // set
        (Value::Set(v_lhs), Value::Set(v_rhs)) => set_lt(v_lhs, v_rhs),

        // default
        (_, _) => false.into(),
    }
}

#[smt_spec(impls = [lt])]
pub fn spec_lt(_lhs: Value, _rhs: Value) -> Boolean {
    unimplemented!()
}

// strict total orders

// - rule 1: irreflexive
#[smt_axiom]
pub fn axiom_lt_irreflexive(v: Value) -> Boolean {
    spec_lt(v, v).not()
}

// - rule 2: asymmetric
#[smt_axiom]
pub fn axiom_lt_asymmetric(v1: Value, v2: Value) -> Boolean {
    spec_lt(v1, v2).implies(spec_lt(v2, v1).not())
}

// - rule 3: transitive
#[smt_axiom]
pub fn axiom_lt_transitive(v1: Value, v2: Value, v3: Value) -> Boolean {
    (spec_lt(v1, v2).and(spec_lt(v2, v3))).implies(spec_lt(v1, v3))
}

// - rule 4: connected
#[smt_axiom]
pub fn axiom_lt_connected(v1: Value, v2: Value) -> Boolean {
    v1.ne(v2).implies(spec_lt(v1, v2).or(spec_lt(v2, v1)))
}
