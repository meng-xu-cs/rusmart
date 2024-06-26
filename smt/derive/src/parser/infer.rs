use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use itertools::Itertools;

use crate::parser::name::{TypeParamName, UsrTypeName};
use crate::parser::ty::TypeTag;

/// An error for type inference
pub enum TIError {
    CyclicUnification,
}

pub type TIResult<T> = Result<T, TIError>;

macro_rules! ti_unwrap {
    ($item:expr) => {
        match ($item)? {
            None => return Ok(None),
            Some(__v) => __v,
        }
    };
}

/// Try to unify the two types, bail on the spanned element if not unified
macro_rules! ti_unify {
    ($unifier:expr, $lhs:expr, $rhs:expr, $spanned:expr) => {
        match $unifier.unify($lhs, $rhs) {
            Err($crate::parser::infer::TIError::CyclicUnification) => {
                $crate::parser::err::bail_on!($spanned, "cyclic type unification");
            }
            Ok(None) => {
                $crate::parser::err::bail_on!($spanned, "no viable type");
            }
            Ok(Some(__v)) => __v,
        }
    };
}
pub(crate) use ti_unify;

/// Represents a type variable participating in type unification
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeVar(usize);

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "?{}", self.0)
    }
}

/// Like `TypeTag`, but allows type variable for type unification
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum TypeRef {
    /// variable
    Var(TypeVar),
    /// boolean
    Boolean,
    /// integer (unlimited precision)
    Integer,
    /// rational numbers (unlimited precision)
    Rational,
    /// string
    Text,
    /// inductively defined type
    Cloak(Box<TypeRef>),
    /// SMT-sequence
    Seq(Box<TypeRef>),
    /// SMT-set
    Set(Box<TypeRef>),
    /// SMT-array
    Map(Box<TypeRef>, Box<TypeRef>),
    /// dynamic error type
    Error,
    /// user-defined type
    User(UsrTypeName, Vec<TypeRef>),
    /// a tuple of types
    Pack(Vec<TypeRef>),
    /// parameter
    Parameter(TypeParamName),
}

impl From<&TypeTag> for TypeRef {
    fn from(ty: &TypeTag) -> Self {
        match ty {
            TypeTag::Boolean => Self::Boolean,
            TypeTag::Integer => Self::Integer,
            TypeTag::Rational => Self::Rational,
            TypeTag::Text => Self::Text,
            TypeTag::Cloak(sub) => Self::Cloak(Box::new(sub.as_ref().into())),
            TypeTag::Seq(sub) => Self::Seq(Box::new(sub.as_ref().into())),
            TypeTag::Set(sub) => Self::Set(Box::new(sub.as_ref().into())),
            TypeTag::Map(key, val) => {
                Self::Map(Box::new(key.as_ref().into()), Box::new(val.as_ref().into()))
            }
            TypeTag::Error => Self::Error,
            TypeTag::User(name, tags) => {
                Self::User(name.clone(), tags.iter().map(|t| t.into()).collect())
            }
            TypeTag::Pack(elems) => Self::Pack(elems.iter().map(|t| t.into()).collect()),
            TypeTag::Parameter(name) => Self::Parameter(name.clone()),
        }
    }
}

impl TypeRef {
    /// Validate whether the type is complete
    pub fn validate(&self) -> bool {
        match self {
            Self::Var(_) => false,
            Self::Boolean
            | Self::Integer
            | Self::Rational
            | Self::Text
            | Self::Error
            | Self::Parameter(_) => true,
            Self::Cloak(sub) | Self::Seq(sub) | Self::Set(sub) => sub.validate(),
            Self::Map(key, val) => key.validate() && val.validate(),
            Self::Pack(elems) => elems.iter().all(|t| t.validate()),
            Self::User(_, args) => args.iter().all(|t| t.validate()),
        }
    }

    /// Reverse the `TypeRef` to a `TypeTag`
    pub fn reverse(&self) -> Option<TypeTag> {
        let reversed = match self {
            Self::Var(_) => return None,
            Self::Boolean => TypeTag::Boolean,
            Self::Integer => TypeTag::Integer,
            Self::Rational => TypeTag::Rational,
            Self::Text => TypeTag::Text,
            Self::Error => TypeTag::Error,
            Self::Parameter(name) => TypeTag::Parameter(name.clone()),
            Self::Cloak(sub) => TypeTag::Cloak(sub.as_ref().reverse()?.into()),
            Self::Seq(sub) => TypeTag::Seq(sub.as_ref().reverse()?.into()),
            Self::Set(sub) => TypeTag::Set(sub.as_ref().reverse()?.into()),
            Self::Map(key, val) => TypeTag::Map(
                key.as_ref().reverse()?.into(),
                val.as_ref().reverse()?.into(),
            ),
            Self::Pack(elems) => {
                TypeTag::Pack(elems.iter().map(|t| t.reverse()).collect::<Option<_>>()?)
            }
            Self::User(name, args) => TypeTag::User(
                name.clone(),
                args.iter().map(|t| t.reverse()).collect::<Option<_>>()?,
            ),
        };
        Some(reversed)
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(var) => var.fmt(f),
            Self::Boolean => write!(f, "Boolean"),
            Self::Integer => write!(f, "Integer"),
            Self::Rational => write!(f, "Rational"),
            Self::Text => write!(f, "Text"),
            Self::Cloak(sub) => write!(f, "Cloak<{}>", sub),
            Self::Seq(sub) => write!(f, "Seq<{}>", sub),
            Self::Set(sub) => write!(f, "Set<{}>", sub),
            Self::Map(key, val) => write!(f, "Map<{},{}>", key, val),
            Self::Error => write!(f, "Error"),
            Self::User(name, args) => {
                if args.is_empty() {
                    name.fmt(f)
                } else {
                    write!(f, "{}<{}>", name, args.iter().format(","))
                }
            }
            Self::Pack(elems) => {
                write!(f, "({})", elems.iter().format(","))
            }
            Self::Parameter(name) => name.fmt(f),
        }
    }
}

/// An equivalence group of type variables
#[derive(Clone)]
struct TypeEquivGroup {
    vars: BTreeSet<usize>,
    sort: Option<TypeRef>,
}

impl TypeEquivGroup {
    /// Represent this group with a type variable at the minimum index
    fn var(&self) -> TypeRef {
        let var = *self.vars.first().expect("at least one type variable");
        TypeRef::Var(TypeVar(var))
    }

    /// Extract a type representing this group
    pub fn repr(&self) -> TypeRef {
        match self.sort.as_ref() {
            None => self.var(),
            Some(t) => t.clone(),
        }
    }
}

/// A type unification instance
#[derive(Clone)]
struct Typing {
    /// holds the set of possible candidates associated with each type parameter
    params: BTreeMap<usize, usize>,
    /// hold the equivalence groups
    groups: Vec<TypeEquivGroup>,
}

impl Typing {
    /// Create an empty type unification context
    pub fn new() -> Self {
        Self {
            params: BTreeMap::new(),
            groups: vec![],
        }
    }

    /// Make a new type variable
    pub fn mk_var(&mut self) -> TypeVar {
        let var_id = self.params.len();

        // assign a fresh equivalence group to the type variable
        let group = TypeEquivGroup {
            vars: std::iter::once(var_id).collect(),
            sort: None,
        };
        let group_index = self.groups.len();

        // register the param and the group
        self.groups.push(group);
        let existing = self.params.insert(var_id, group_index);
        assert!(existing.is_none());

        // done
        TypeVar(var_id)
    }

    /// Merge the type constraints
    fn merge_group(
        &mut self,
        l: &TypeVar,
        h: &TypeVar,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        let idx_l = *self.params.get(&l.0).unwrap();
        let idx_h = *self.params.get(&h.0).unwrap();

        // obtain groups
        let mut group_l = self.groups.get(idx_l).unwrap().clone();
        if !involved.is_disjoint(&group_l.vars) {
            return Err(TIError::CyclicUnification);
        }

        let group_h = self.groups.get(idx_h).unwrap().clone();
        if !involved.is_disjoint(&group_h.vars) {
            return Err(TIError::CyclicUnification);
        }

        // prevent recursive typing
        involved.extend(group_l.vars.iter().copied());
        involved.extend(group_h.vars.iter().copied());

        // nothing to do if they belong to the same group
        if idx_l == idx_h {
            return Ok(Some(group_l.repr()));
        }

        // unify the equivalence set, after a sanity checking
        if !group_l.vars.is_disjoint(&group_h.vars) {
            panic!("non-disjoint equivalence set");
        }
        group_l.vars.extend(group_h.vars);

        // check whether they unity to the same type, if any
        match (group_l.sort.as_ref(), group_h.sort.as_ref()) {
            (None, None) => {
                // none of the groups have type inferred
            }
            (Some(_), None) => {
                // the lower group already has candidates
            }
            (None, Some(sort_h)) => {
                // propagate the type candidates to the lower group
                group_l.sort = Some(sort_h.clone());
            }
            (Some(sort_l), Some(sort_h)) => {
                // further unity (refine) the types, also check for mismatches
                let unified = ti_unwrap!(self.unify(sort_l, sort_h, involved));
                group_l.sort = Some(unified);
            }
        };

        // pre-calculate the inferred type
        let inferred = group_l.repr();

        // redirect the group for the type variable at a higher index
        *self.groups.get_mut(idx_l).unwrap() = group_l;
        *self.params.get_mut(&h.0).unwrap() = idx_l;

        // return the inferred type
        Ok(Some(inferred))
    }

    /// Assign the constraint
    fn update_group(
        &mut self,
        v: &TypeVar,
        t: &TypeRef,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        // obtain the group
        let idx = *self.params.get(&v.0).unwrap();
        let mut group = self.groups.get(idx).unwrap().clone();

        // decides on whether further unification is needed
        let inferred = match group.sort.as_ref() {
            None => {
                // propagate the type to the group
                t.clone()
            }
            Some(e) => {
                // further unity (refine) the types, also check for mismatches
                ti_unwrap!(self.unify(e, t, involved))
            }
        };

        // update the type in this group
        group.sort = Some(inferred.clone());

        // reset the equivalence of group for the type variable at a lower index
        *self.groups.get_mut(idx).unwrap() = group;

        // return the inferred type
        Ok(Some(inferred))
    }

    /// Unify two types
    pub fn unify(
        &mut self,
        lhs: &TypeRef,
        rhs: &TypeRef,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        use TypeRef::*;

        let inferred = match (lhs, rhs) {
            // variable
            (Var(l), Var(r)) => match Ord::cmp(&l.0, &r.0) {
                Ordering::Equal => {
                    // no knowledge gain in this case
                    Var(l.clone())
                }
                Ordering::Less => ti_unwrap!(self.merge_group(l, r, involved)),
                Ordering::Greater => ti_unwrap!(self.merge_group(r, l, involved)),
            },
            (Var(l), _) => ti_unwrap!(self.update_group(l, rhs, involved)),
            (_, Var(r)) => ti_unwrap!(self.update_group(r, lhs, involved)),
            // concrete types
            (Boolean, Boolean) => Boolean,
            (Integer, Integer) => Integer,
            (Rational, Rational) => Rational,
            (Text, Text) => Text,
            (Error, Error) => Error,
            // variadic types
            (Cloak(sub_lhs), Cloak(sub_rhs)) => {
                Cloak(ti_unwrap!(self.unify(sub_lhs, sub_rhs, involved)).into())
            }
            (Seq(sub_lhs), Seq(sub_rhs)) => {
                Seq(ti_unwrap!(self.unify(sub_lhs, sub_rhs, involved)).into())
            }
            (Set(sub_lhs), Set(sub_rhs)) => {
                Set(ti_unwrap!(self.unify(sub_lhs, sub_rhs, involved)).into())
            }
            (Map(key_lhs, val_lhs), Map(key_rhs, val_rhs)) => Map(
                ti_unwrap!(self.unify(key_lhs, key_rhs, involved)).into(),
                ti_unwrap!(self.unify(val_lhs, val_rhs, involved)).into(),
            ),
            // user-define types
            (User(name_lhs, args_lhs), User(name_rhs, args_rhs)) => {
                // filter obvious type mismatches
                if name_lhs != name_rhs {
                    return Ok(None);
                }

                // invariant checking
                if args_lhs.len() != args_rhs.len() {
                    panic!("type argument number mismatch");
                }

                // try to unify the type arguments
                let mut new_args = vec![];
                for (v_lhs, v_rhs) in args_lhs.iter().zip(args_rhs) {
                    new_args.push(ti_unwrap!(self.unify(v_lhs, v_rhs, involved)));
                }
                User(name_lhs.clone(), new_args)
            }
            // packs (i.e., type tuples)
            (Pack(pack_lhs), Pack(pack_rhs)) => {
                // filter obvious type mismatches
                if pack_lhs.len() != pack_rhs.len() {
                    return Ok(None);
                }

                // try to unify the elements
                let mut new_elems = vec![];
                for (v_lhs, v_rhs) in pack_lhs.iter().zip(pack_rhs) {
                    new_elems.push(ti_unwrap!(self.unify(v_lhs, v_rhs, involved)));
                }
                Pack(new_elems)
            }
            // type parameters
            (Parameter(name_lhs), Parameter(name_rhs)) => {
                if name_lhs != name_rhs {
                    return Ok(None);
                }
                Parameter(name_lhs.clone())
            }
            // all other cases are considered mismatch
            _ => return Ok(None),
        };

        // return the inferred type
        Ok(Some(inferred))
    }

    /// Retrieve the type behind the type variable
    pub fn retrieve_type(&self, var: &TypeVar) -> TypeRef {
        let idx = *self.params.get(&var.0).unwrap();
        self.groups.get(idx).unwrap().repr()
    }
}

/// Context manager for type unification
#[derive(Clone)]
pub struct TypeUnifier {
    /// the actual typing worker
    typing: Typing,
}

impl TypeUnifier {
    /// Create a unifier with one instance only
    pub fn new() -> Self {
        Self {
            typing: Typing::new(),
        }
    }

    /// Make a new type variable
    pub fn mk_var(&mut self) -> TypeVar {
        self.typing.mk_var()
    }

    /// Unify two types
    pub fn unify(&mut self, lhs: &TypeRef, rhs: &TypeRef) -> TIResult<Option<TypeRef>> {
        let mut involved = BTreeSet::new();
        self.typing.unify(lhs, rhs, &mut involved)
    }

    /// Retrieve either an assigned type or the variable itself (if multiple options available)
    fn retrieve_type(&self, var: &TypeVar) -> TypeRef {
        self.typing.retrieve_type(var)
    }

    /// Try to instantiate a type when needed
    pub fn refresh_type(&self, ty: &TypeRef) -> TypeRef {
        match ty {
            TypeRef::Var(var) => self.retrieve_type(var),
            TypeRef::Boolean => TypeRef::Boolean,
            TypeRef::Integer => TypeRef::Integer,
            TypeRef::Rational => TypeRef::Rational,
            TypeRef::Text => TypeRef::Text,
            TypeRef::Cloak(sub) => TypeRef::Cloak(self.refresh_type(sub).into()),
            TypeRef::Seq(sub) => TypeRef::Seq(self.refresh_type(sub).into()),
            TypeRef::Set(sub) => TypeRef::Set(self.refresh_type(sub).into()),
            TypeRef::Map(key, val) => {
                TypeRef::Map(self.refresh_type(key).into(), self.refresh_type(val).into())
            }
            TypeRef::Error => TypeRef::Error,
            TypeRef::User(name, args) => TypeRef::User(
                name.clone(),
                args.iter().map(|t| self.refresh_type(t)).collect(),
            ),
            TypeRef::Pack(elems) => {
                TypeRef::Pack(elems.iter().map(|t| self.refresh_type(t)).collect())
            }
            TypeRef::Parameter(name) => TypeRef::Parameter(name.clone()),
        }
    }
}
