use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use crate::parser::apply::TypeFn;
use crate::parser::func::FuncSig;
use crate::parser::name::{TypeParamName, UsrTypeName};
use crate::parser::path::GenericsInstantiated;
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
            Ok(Some(())) => (),
        }
    };
}
pub(crate) use ti_unify;

/// An error for type substitution
pub enum TSError {
    NoSuchParameter,
}

pub type TSResult<T> = Result<T, TSError>;

/// Try to substitute a type, bail on the spanned element if not unified
macro_rules! bail_on_ts_err {
    ($result:expr, $spanned:expr) => {
        match $result {
            Ok(__v) => __v,
            Err($crate::parser::infer::TSError::NoSuchParameter) => {
                $crate::parser::err::bail_on!($spanned, "no such type parameter");
            }
        }
    };
}
pub(crate) use bail_on_ts_err;

/// Represents a type variable participating in type unification
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct TypeVar(usize);

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
            Self::User(_, args) => args.iter().all(|t| t.validate()),
        }
    }

    /// Apply type parameter substitution
    pub fn substitute_params(
        unifier: &mut TypeUnifier,
        tag: &TypeTag,
        substitute: &GenericsInstantiated,
    ) -> TSResult<TypeRef> {
        let updated = match tag {
            TypeTag::Boolean => TypeRef::Boolean,
            TypeTag::Integer => TypeRef::Integer,
            TypeTag::Rational => TypeRef::Rational,
            TypeTag::Text => TypeRef::Text,
            TypeTag::Cloak(sub) => {
                TypeRef::Cloak(Self::substitute_params(unifier, sub, substitute)?.into())
            }
            TypeTag::Seq(sub) => {
                TypeRef::Seq(Self::substitute_params(unifier, sub, substitute)?.into())
            }
            TypeTag::Set(sub) => {
                TypeRef::Set(Self::substitute_params(unifier, sub, substitute)?.into())
            }
            TypeTag::Map(key, val) => TypeRef::Map(
                Self::substitute_params(unifier, key, substitute)?.into(),
                Self::substitute_params(unifier, val, substitute)?.into(),
            ),
            TypeTag::Error => TypeRef::Error,
            TypeTag::User(name, args) => TypeRef::User(
                name.clone(),
                args.iter()
                    .map(|t| Self::substitute_params(unifier, t, substitute))
                    .collect::<TSResult<_>>()?,
            ),
            TypeTag::Parameter(name) => match substitute
                .get(name)
                .ok_or_else(|| TSError::NoSuchParameter)?
            {
                None => TypeRef::Var(unifier.mk_var()),
                Some(t) => t.into(),
            },
        };
        Ok(updated)
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
            (User(name_lhs, vars_lhs), User(name_rhs, vars_rhs)) => {
                // filter obvious type mismatches
                if name_lhs != name_rhs {
                    return Ok(None);
                }

                // invariant checking
                if vars_rhs.len() != vars_rhs.len() {
                    panic!("type argument number mismatch");
                }

                // try to unify the type arguments
                let mut new_vars = vec![];
                for (v_lhs, v_rhs) in vars_lhs.iter().zip(vars_rhs.iter()) {
                    let unified = ti_unwrap!(self.unify(v_lhs, v_rhs, involved));
                    new_vars.push(unified);
                }
                User(name_lhs.clone(), new_vars)
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
pub struct TypeUnifier {
    /// instances surviving so far
    instances: Vec<Typing>,
}

impl TypeUnifier {
    /// Create a unifier with one instance only
    pub fn new() -> Self {
        Self {
            instances: vec![Typing::new()],
        }
    }

    /// Make a new type variable
    pub fn mk_var(&mut self) -> TypeVar {
        let mut iter = self.instances.iter_mut();
        let ref_var = iter.next().expect("at least one instance").mk_var();
        for item in iter.by_ref() {
            let v = item.mk_var();
            if ref_var != v {
                panic!("variable out of sync");
            }
        }
        ref_var
    }

    /// Unify two types
    pub fn unify(&mut self, lhs: &TypeRef, rhs: &TypeRef) -> TIResult<Option<()>> {
        let instances = std::mem::take(&mut self.instances);
        for mut typing in instances {
            let mut involved = BTreeSet::new();
            match typing.unify(lhs, rhs, &mut involved)? {
                None => {
                    // type unification failed, drop this instance
                }
                Some(_) => {
                    self.instances.push(typing);
                }
            }
        }

        // check if we have any surviving instances
        let survival = if self.instances.is_empty() {
            None
        } else {
            Some(())
        };
        Ok(survival)
    }

    /// Instantiate a function type
    pub fn instantiate_func_ty(
        &mut self,
        fty: &TypeFn,
        subst: &GenericsInstantiated,
    ) -> TSResult<(Vec<TypeRef>, TypeRef)> {
        let params: Vec<_> = fty
            .params()
            .iter()
            .map(|t| TypeRef::substitute_params(self, t, subst))
            .collect::<TSResult<_>>()?;
        let ret_ty = TypeRef::substitute_params(self, fty.ret_ty(), subst)?;
        Ok((params, ret_ty))
    }

    /// Instantiate a function signature
    pub fn instantiate_func_sig(
        &mut self,
        sig: &FuncSig,
        subst: &GenericsInstantiated,
    ) -> TSResult<(Vec<TypeRef>, TypeRef)> {
        let fty = TypeFn::from(sig);
        self.instantiate_func_ty(&fty, subst)
    }

    /// Retrieve either an assigned type or the variable itself (if multiple options available)
    fn retrieve_type(&self, var: &TypeVar) -> TypeRef {
        let mut unified = BTreeSet::new();
        for typing in &self.instances {
            unified.insert(typing.retrieve_type(var));
        }
        match unified.len() {
            0 => panic!("type unification failure not captured"),
            1 => unified.into_iter().next().unwrap(),
            _ => TypeRef::Var(var.clone()),
        }
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
            TypeRef::User(name, tys) => TypeRef::User(
                name.clone(),
                tys.iter().map(|t| self.refresh_type(t)).collect(),
            ),
            TypeRef::Parameter(name) => TypeRef::Parameter(name.clone()),
        }
    }
}
