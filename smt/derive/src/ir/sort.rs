use crate::ir::name::name;
use crate::parser::name::TypeParamName;

name! {
    /// Name of a type parameter that implements the SMT trait
    SmtSortName
}

impl SmtSortName {
    /// Name for an uninterpreted sort
    pub fn new(name: &TypeParamName) -> Self {
        Self {
            ident: name.to_string(),
        }
    }
}

name! {
    /// Name of a user-defined sort
    UsrSortName
}

/// A unique and complete reference to an SMT sort
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Sort {
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
    /// a tuple of types
    Pack(Vec<Sort>),
    /// user-defined type
    User(UsrSortName, Vec<Sort>),
    /// uninterpreted
    Uninterpreted(SmtSortName),
}
