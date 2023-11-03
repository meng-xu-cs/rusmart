use crate::ir::name::name;

name! {
    /// Type parameter treated as uninterpreted sort
    TypeParam
}

name! {
    /// Name of a user-defined sort
    UsrSortName
}

/// A unique and complete reference to an SMT sort
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Sort {
    /// uninterpreted sort
    Param(TypeParam),
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// SMT-sequence
    Seq(Box<Sort>),
    /// SMT-set
    Set(Box<Sort>),
    /// SMT-array
    Map(Box<Sort>, Box<Sort>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(UsrSortName, Vec<Sort>),
}
