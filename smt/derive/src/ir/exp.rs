use std::collections::BTreeMap;

use crate::ir::name::index;
use crate::ir::sort::{Sort, UsrSortId};
use crate::parser::name::VarName;

index! {
    /// Index of a variable
    VarId
}

index! {
    /// Index of an expression
    ExpId
}

/// The origin of a variable
pub enum VarKind {
    /// function parameter
    Param,
    /// free variable used in a quantifier
    Quant { decl: ExpId },
    /// let-binding to an expression
    Bound { bind: ExpId },
    /// match-introduced
    Match { decl: ExpId, bind: ExpId },
    /// axiomatized (through a list of predicates)
    Axiom { decl: ExpId, pred: Vec<ExpId> },
}

/// Information about a variable
pub struct Variable {
    name: String,
    kind: VarKind,
    sort: Sort,
}

/// Denotes how to construct an enum variant
pub enum VariantCtor {
    Unit,
    Tuple(Vec<ExpId>),
    Record(BTreeMap<String, ExpId>),
}

/// Denotes how to destruct an enum variant and bind variables
pub enum VariantDtor {
    Unit,
    Tuple(Vec<Option<VarId>>),
    Record(BTreeMap<String, Option<VarId>>),
}

/// One atom in the match case to unpack
pub struct MatchAtom {
    head: ExpId,
    sort: UsrSortId,
    branch: String,
    variant: VariantDtor,
}

/// One match case
pub struct MatchCase {
    atoms: Vec<MatchAtom>,
    body: ExpId,
}

/// One phi case (i.e., conditional branch)
pub struct PhiCase {
    cond: ExpId,
    body: ExpId,
}

/// An expression
pub enum Expression {
    /// `<var>`
    Var(VarId),
    /// `(v1, v2, ...)`
    Pack { elems: Vec<ExpId> },
    /// `<tuple-name>(<inst>?)(v1, v2. ...)`
    Tuple { sort: UsrSortId, slots: Vec<ExpId> },
    /// `<record-name>(<inst>?){ f1: v1, f2: v2, ... }`
    Record {
        sort: UsrSortId,
        fields: BTreeMap<String, ExpId>,
    },
    /// `<adt-name>(<inst>?)::<branch>(<ctor>)`
    Enum {
        sort: UsrSortId,
        branch: String,
        variant: VariantCtor,
    },
    /// `<base>.<index>`
    AccessSlot { base: ExpId, slot: usize },
    /// `<base>.<field>`
    AccessField { base: ExpId, field: String },
    /// `match (v1, v2, ...) { (a1, a2, ...) => <body1> } ...`
    Match { cases: Vec<MatchCase> },
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi { cases: Vec<PhiCase>, default: ExpId },
}

/// A builder for expressions
pub struct ExprBuilder {
    /// a map from variable id to variables
    vars: BTreeMap<VarId, Variable>,
    /// a map from expression id to expressions
    exps: BTreeMap<ExpId, Expression>,
}

pub struct Func {
    var_params: BTreeMap<VarName, Sort>,
    var_axioms: Vec<(Vec<VarName>, Vec<Predicate>)>,
    body: Exp,
}
