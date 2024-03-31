use crate::ir::index::ExpId;
use crate::ir::sort::Sort;

/// Intrinsic procedure
pub enum Intrinsic {
    /// `Boolean::from`
    BoolVal(bool),
    /// `Boolean::not`
    BoolNot { val: ExpId },
    /// `Boolean::and`
    BoolAnd { lhs: ExpId, rhs: ExpId },
    /// `Boolean::or`
    BoolOr { lhs: ExpId, rhs: ExpId },
    /// `Boolean::xor`
    BoolXor { lhs: ExpId, rhs: ExpId },
    /// `Boolean::implies`
    BoolImplies { lhs: ExpId, rhs: ExpId },
    /// `Integer::from`
    IntVal(i128),
    /// `Integer::lt`
    IntLt { lhs: ExpId, rhs: ExpId },
    /// `Integer::le`
    IntLe { lhs: ExpId, rhs: ExpId },
    /// `Integer::ge`
    IntGe { lhs: ExpId, rhs: ExpId },
    /// `Integer::gt`
    IntGt { lhs: ExpId, rhs: ExpId },
    /// `Integer::add`
    IntAdd { lhs: ExpId, rhs: ExpId },
    /// `Integer::sub`
    IntSub { lhs: ExpId, rhs: ExpId },
    /// `Integer::mul`
    IntMul { lhs: ExpId, rhs: ExpId },
    /// `Integer::div`
    IntDiv { lhs: ExpId, rhs: ExpId },
    /// `Integer::rem`
    IntRem { lhs: ExpId, rhs: ExpId },
    /// `Rational::from`
    NumVal(i128),
    /// `Rational::lt`
    NumLt { lhs: ExpId, rhs: ExpId },
    /// `Rational::le`
    NumLe { lhs: ExpId, rhs: ExpId },
    /// `Rational::ge`
    NumGe { lhs: ExpId, rhs: ExpId },
    /// `Rational::gt`
    NumGt { lhs: ExpId, rhs: ExpId },
    /// `Rational::add`
    NumAdd { lhs: ExpId, rhs: ExpId },
    /// `Rational::sub`
    NumSub { lhs: ExpId, rhs: ExpId },
    /// `Rational::mul`
    NumMul { lhs: ExpId, rhs: ExpId },
    /// `Rational::div`
    NumDiv { lhs: ExpId, rhs: ExpId },
    /// `Text::from`
    StrVal(String),
    /// `Text::lt`
    StrLt { lhs: ExpId, rhs: ExpId },
    /// `Text::le`
    StrLe { lhs: ExpId, rhs: ExpId },
    /// `Cloak::shield`
    BoxShield { t: Sort, val: ExpId },
    /// `Cloak::reveal`
    BoxReveal { t: Sort, val: ExpId },
    /// `Seq::empty`
    SeqEmpty { t: Sort },
    /// `Seq::length`
    SeqLength { t: Sort, seq: ExpId },
    /// `Seq::append`
    SeqAppend { t: Sort, seq: ExpId, item: ExpId },
    /// `Seq::at_unchecked`
    SeqAt { t: Sort, seq: ExpId, idx: ExpId },
    /// `Seq::includes`
    SeqIncludes { t: Sort, seq: ExpId, item: ExpId },
    /// `Set::empty`
    SetEmpty { t: Sort },
    /// `Set::length`
    SetLength { t: Sort, set: ExpId },
    /// `Set::insert`
    SetInsert { t: Sort, set: ExpId, item: ExpId },
    /// `Set::remove`
    SetRemove { t: Sort, set: ExpId, item: ExpId },
    /// `Set::contains`
    SetContains { t: Sort, set: ExpId, item: ExpId },
    /// `Map::empty`
    MapEmpty { k: Sort, v: Sort },
    /// `Map::length`
    MapLength { k: Sort, v: Sort, map: ExpId },
    /// `Map::put_unchecked`
    MapPut {
        k: Sort,
        v: Sort,
        map: ExpId,
        key: ExpId,
        val: ExpId,
    },
    /// `Map::get_unchecked`
    MapGet {
        k: Sort,
        v: Sort,
        map: ExpId,
        key: ExpId,
    },
    /// `Map::del_unchecked`
    MapDel {
        k: Sort,
        v: Sort,
        map: ExpId,
        key: ExpId,
    },
    /// `Map::contains_key`
    MapContainsKey {
        k: Sort,
        v: Sort,
        map: ExpId,
        key: ExpId,
    },
    /// `Error::fresh`
    ErrFresh,
    /// `Error::merge`
    ErrMerge { lhs: ExpId, rhs: ExpId },
    /// `<any-smt-type>::eq`
    SmtEq { t: Sort, lhs: ExpId, rhs: ExpId },
    /// `<any-smt-type>::ne`
    SmtNe { t: Sort, lhs: ExpId, rhs: ExpId },
}
