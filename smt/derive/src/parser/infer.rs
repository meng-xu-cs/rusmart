//! Type inference and unification for the SMT parser.

use std::cmp::Ordering; // Imported for comparing type variables.
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter}; // Imported for implementing the Display trait.

use itertools::Itertools; // imported to use the format method on iterators (std::slice::Iter types). This method does not exist on iterators by default, but itertools provides it. The format method takes an iterator and returns a string with the elements of the iterator separated by a separator string. The separator string is optional and defaults to ", ".

use crate::parser::name::{TypeParamName, UsrTypeName}; // generic type parameter name and user-defined type name.
use crate::parser::ty::TypeTag; // TypeTag provides the variants: Boolean, Integer, Rational, Text, Cloak, Seq, Set, Map, Error, User, Pack, Parameter.

/// An error for type inference
pub enum TIError {
    /// Error indicating a cyclic unification was detected.
    /// This occurs when a type variable is unified with a type that contains itself. for example, "Type A is equal to a list of Type A." example: type A = [A]
    /// Cyclic unification errors usually mean that the type relationships are self-referential and cannot be resolved, leading to an infinite loop.
    CyclicUnification,
}

/// A specialized `Result` type for type inference operations, where the error type is `TIError` and the success type is a generic `T`.
pub type TIResult<T> = Result<T, TIError>;

/// A declarative macro that unwraps a `TIResult<Option<T>>`. item? first checks if `item` is an TIError. If it is, it returns the error. If it is not, then Option<T> is checked. If it is None, it early returns Ok(None). If it is Some(value), the value is used.
///
/// This macro simplifies error handling during type unification.
/// - If the result is `Ok(Some(value))`, it returns `value`.
/// - If the result is `Ok(None)`, it returns `Ok(None)` immediately.
/// - If the result is `Err(err)`, where `err` is `TIError::CyclicUnification`, it bails with an error message.
macro_rules! ti_unwrap {
    ($item:expr) => {
        match ($item)? {
            None => return Ok(None),
            Some(__v) => __v,
        }
    };
}

/// Try to unify the two types, bail on the spanned element if not unified.
///
/// If unification fails due to a cyclic unification, it bails with an error message.
/// If unification yields `None`, it bails with an error message.
/// Otherwise, it returns the unified type.
///
/// # Arguments
///
/// * `$unifier` - The type unifier to use, it is of type `&mut TypeUnifier`.
/// * `$lhs` - The left-hand side type to unify, it is of type `&TypeRef`.
/// * `$rhs` - The right-hand side type to unify, it is of type `&TypeRef`.
/// * `$spanned` - The spanned element (e.g., for error reporting).
///
/// The return type of `unify` is `TIResult<Option<TypeRef>>`.
macro_rules! ti_unify {
    ($unifier:expr, $lhs:expr, $rhs:expr, $spanned:expr) => {
        match $unifier.unify($lhs, $rhs) {
            Err($crate::parser::infer::TIError::CyclicUnification) => {
                $crate::parser::err::bail_on!($spanned, "cyclic type unification");
            }
            Ok(None) => {
                $crate::parser::err::bail_on!($spanned, "no viable type");
            }
            Ok(Some(__v)) => __v, // __v is of type TypeRef and is the result of the unification.
        }
    };
}
pub(crate) use ti_unify; // this makes the ti_unify! macro available to other modules in the crate. But it is not available to external crates that depend on this crate. to make it available to external crates, #[macro_export] should be added before the macro definition.

/// Represents a type variable used in type unification.
///
/// Each `TypeVar` is identified by a unique `usize` index.
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub struct TypeVar(usize); // usize can only store non-negative values (it cannot represent negative numbers).
                           // The exact size of usize depends on whether the system is 32-bit or 64-bit.

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "?{}", self.0) // the display is ?<index> where index is the usize value of the TypeVar.
    }
}

/// Represents a type reference in the type unification process.
///
/// Like TypeTag, but allows type variable for type unification.
/// # Examples
/// let var = TypeVar(0usize);
/// let type_ref = TypeRef::Var(var);
/// println!("Type reference: {}", type_ref); // Type reference: ?0
/// ? What I am guessing is that TypeRef is a reference to a type. For unification, we need a reference to a type. TypeTag is a concrete type. ???
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
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
    /// parameter like generics
    Parameter(TypeParamName),
}

impl From<&TypeTag> for TypeRef {
    /// Converts a `TypeTag` reference into a `TypeRef`.
    ///
    /// This allows for converting concrete types into types that can participate in unification.
    fn from(ty: &TypeTag) -> Self {
        match ty {
            TypeTag::Boolean => Self::Boolean,
            TypeTag::Integer => Self::Integer,
            TypeTag::Rational => Self::Rational,
            TypeTag::Text => Self::Text,
            TypeTag::Cloak(sub) => Self::Cloak(Box::new(sub.as_ref().into())), // as_ref() dereferences the `sub` twice and adds a pointer. The first dereference eliminates the & reference, and the second dereference eliminates the Box reference. Therefore, sub.as_ref() gives &TypeTag, and sub.as_ref().into() gives TypeRef. The into() method is the same as the From trait. so sub.as_ref().into() is the same as TypeRef::from(sub.as_ref()).
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
    /// Validates whether the type is complete (i.e., contains no type variables).
    ///
    /// Returns `true` if the type contains no `Var` variants; `false` otherwise.
    ///
    /// # Examples
    /// let int_type = TypeRef::Integer;
    /// assert!(int_type.validate());
    ///
    /// let var = TypeVar(0usize);
    /// let var_type = TypeRef::Var(var);
    /// assert!(!var_type.validate());
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

    /// Converts the `TypeRef` back into a `TypeTag`, if possible.
    ///
    /// Returns `Some(TypeTag)` if the type contains no type variables; `None` otherwise.
    /// This is because the Var(TypeVar) variant cannot be converted back into a TypeTag.
    ///
    /// # Examples
    /// let int_type = TypeRef::Integer;
    /// let type_tag = int_type.reverse();
    /// assert_eq!(type_tag, Some(TypeTag::Integer));
    pub fn reverse(&self) -> Option<TypeTag> {
        let reversed = match self {
            Self::Var(_) => return None, // Cannot reverse a type variable into a concrete type.
            Self::Boolean => TypeTag::Boolean,
            Self::Integer => TypeTag::Integer,
            Self::Rational => TypeTag::Rational,
            Self::Text => TypeTag::Text,
            Self::Error => TypeTag::Error,
            Self::Parameter(name) => TypeTag::Parameter(name.clone()),
            Self::Cloak(sub) => TypeTag::Cloak(Box::new(sub.as_ref().reverse()?)),
            Self::Seq(sub) => TypeTag::Seq(Box::new(sub.as_ref().reverse()?)),
            Self::Set(sub) => TypeTag::Set(Box::new(sub.as_ref().reverse()?)),
            Self::Map(key, val) => TypeTag::Map(
                Box::new(key.as_ref().reverse()?),
                Box::new(val.as_ref().reverse()?),
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

/// An equivalence group of type variables.
///
/// Types that are unified together are stored in an equivalence group.
/// The group may have a reference type assigned to it, which is the inferred type.
#[derive(Clone)]
struct TypeEquivGroup {
    /// Set of type variable indices that are equivalent.
    vars: BTreeSet<usize>,
    /// The reference type assigned to this equivalence group, if any.
    sort: Option<TypeRef>,
}

impl TypeEquivGroup {
    /// Represent this group with a type variable at the minimum index and returns a `TypeRef::Var`.
    ///
    /// example if vars = BTreeSet::from([4,2,3,4]), then the minimum index is 2.
    fn var(&self) -> TypeRef {
        let var = *self
            .vars
            .first()
            .expect("at least one type variable is expected in the equivalence group");
        TypeRef::Var(TypeVar(var))
    }

    /// Extracts the representative type of this group.
    ///
    /// If a concrete type is assigned (`sort` is `Some`), returns that type.
    /// Otherwise, returns a `TypeRef::Var` using the minimum type variable index.
    pub fn repr(&self) -> TypeRef {
        match self.sort.as_ref() {
            // Converts from `&Option<T>` to `Option<&T>`.
            None => self.var(),
            Some(t) => t.clone(),
        }
    }
}

/// Represents a type unification instance.
#[derive(Clone)]
pub struct Typing {
    /// Holds the set of possible candidates associated with each type parameter.
    params: BTreeMap<usize, usize>,
    /// Holds the equivalence groups.
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

    /// Creates a new type variable and returns it.
    ///
    /// # Examples
    ///
    /// let mut typing = Typing::new();
    /// let var = typing.mk_var();
    pub fn mk_var(&mut self) -> TypeVar {
        let var_id = self.params.len();
        let group_index = self.groups.len();

        // Assign a fresh equivalence group to the type variable.
        let group = TypeEquivGroup {
            vars: std::iter::once(var_id).collect(),
            sort: None,
        };

        // Register the param and the group
        self.groups.push(group);
        let existing = self.params.insert(var_id, group_index);
        assert!(existing.is_none(), "Type variable already exists");

        // Return the new type variable.
        TypeVar(var_id)
    }

    /// Merge the type constraints
    ///
    /// Unifies the groups of `l` and `h`, updating their types accordingly.
    /// Returns the inferred type after merging.
    ///
    /// # Errors
    ///
    /// Returns `TIError::CyclicUnification` if a cyclic unification is detected.
    fn merge_group(
        &mut self,
        l: &TypeVar,
        h: &TypeVar,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        let idx_l = *self
            .params
            .get(&l.0)
            .expect("type variable not found for l in merge_group");
        let idx_h = *self
            .params
            .get(&h.0)
            .expect("type variable not found for h in merge_group");

        // Obtain groups.
        let mut group_l = self
            .groups
            .get(idx_l)
            .expect("equivalence group not found for l in merge_group")
            .clone();
        if !involved.is_disjoint(&group_l.vars) {
            return Err(TIError::CyclicUnification);
        }

        let group_h = self
            .groups
            .get(idx_h)
            .expect("equivalence group not found for h in merge_group")
            .clone();
        if !involved.is_disjoint(&group_h.vars) {
            return Err(TIError::CyclicUnification);
        }

        // Prevent recursive typing.
        involved.extend(group_l.vars.iter().copied());
        involved.extend(group_h.vars.iter().copied());

        // Nothing to do if they belong to the same group.
        if idx_l == idx_h {
            return Ok(Some(group_l.repr()));
        }

        // Unify the equivalence set, after a sanity checking.
        if !group_l.vars.is_disjoint(&group_h.vars) {
            panic!("Non-disjoint equivalence sets");
        }
        group_l.vars.extend(group_h.vars);

        // check whether they unify to the same type, if any
        match (group_l.sort.as_ref(), group_h.sort.as_ref()) {
            (None, None) => {
                // Neither group has a concrete type; nothing to unify.
            }
            (Some(_), None) => {
                // The lower group already has candidates; keep it.
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

        // Pre-calculate the inferred type.
        let inferred = group_l.repr();

        // Redirect the group for the type variable at a higher index
        *self.groups.get_mut(idx_l).unwrap() = group_l;
        *self.params.get_mut(&h.0).unwrap() = idx_l;

        // Return the inferred type.
        Ok(Some(inferred))
    }

    /// Assign the constraint
    ///
    /// Updates the group of `v` with the new type `t`, unifying as necessary.
    /// Returns the inferred type after updating.
    ///
    /// # Errors
    ///
    /// Returns `TIError::CyclicUnification` if a cyclic unification is detected.
    fn update_group(
        &mut self,
        v: &TypeVar,
        t: &TypeRef,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        // Obtain the group index and group.
        let idx = *self.params.get(&v.0).unwrap();
        let mut group = self.groups.get(idx).unwrap().clone();

        // Decide whether further unification is needed.
        let inferred = match group.sort.as_ref() {
            None => {
                // propagate the type to the group; assign `t`.
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

        // Return the inferred type.
        Ok(Some(inferred))
    }

    /// Unifies two types and returns the unified type.
    ///
    /// This is the core unification algorithm.
    ///
    /// # Arguments
    ///
    /// * `lhs` - The left-hand side type.
    /// * `rhs` - The right-hand side type.
    /// * `involved` - A set of type variable indices involved in unification to detect cycles.
    ///
    /// # Errors
    ///
    /// Returns `TIError::CyclicUnification` if a cyclic unification is detected.
    pub fn unify(
        &mut self,
        lhs: &TypeRef,
        rhs: &TypeRef,
        involved: &mut BTreeSet<usize>,
    ) -> TIResult<Option<TypeRef>> {
        use TypeRef::*;

        let inferred = match (lhs, rhs) {
            // Both are type variables.
            (Var(l), Var(r)) => match Ord::cmp(&l.0, &r.0) {
                Ordering::Equal => {
                    // no knowledge gain in this case
                    Var(l.clone())
                }
                Ordering::Less => ti_unwrap!(self.merge_group(l, r, involved)),
                Ordering::Greater => ti_unwrap!(self.merge_group(r, l, involved)),
            },
            // One is a variable.
            (Var(l), _) => ti_unwrap!(self.update_group(l, rhs, involved)),
            (_, Var(r)) => ti_unwrap!(self.update_group(r, lhs, involved)),
            // Both are concrete types.
            // Unify base types.
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
            // Unify user-defined types.
            (User(name_lhs, args_lhs), User(name_rhs, args_rhs)) => {
                // If names are different, types cannot be unified.
                if name_lhs != name_rhs {
                    return Ok(None);
                }

                // invariant checking
                if args_lhs.len() != args_rhs.len() {
                    panic!("Type argument number mismatch");
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
                // If tuple sizes differ, cannot unify.
                if pack_lhs.len() != pack_rhs.len() {
                    return Ok(None);
                }

                // Unify each element.
                let mut new_elems = vec![];
                for (v_lhs, v_rhs) in pack_lhs.iter().zip(pack_rhs) {
                    new_elems.push(ti_unwrap!(self.unify(v_lhs, v_rhs, involved)));
                }
                Pack(new_elems)
            }
            // Unify type parameters.
            (Parameter(name_lhs), Parameter(name_rhs)) => {
                if name_lhs != name_rhs {
                    return Ok(None);
                }
                Parameter(name_lhs.clone())
            }
            // All other cases are considered a type mismatch.
            _ => return Ok(None),
        };

        // Return the inferred type.
        Ok(Some(inferred))
    }

    /// Retrieve the type behind the type variable
    ///
    /// Returns the concrete type assigned to the variable's equivalence group, or the variable itself.
    pub fn retrieve_type(&self, var: &TypeVar) -> TypeRef {
        let idx = *self.params.get(&var.0).unwrap();
        self.groups.get(idx).unwrap().repr()
    }
}

/// Context manager for type unification
///
/// let mut unifier = TypeUnifier::new();
/// let var = unifier.mk_var();
/// let int_type = TypeRef::Integer;
/// let result = unifier.unify(&TypeRef::Var(var.clone()), &int_type).
#[derive(Clone)]
pub struct TypeUnifier {
    /// The internal typing worker that performs unification.
    typing: Typing,
}

impl TypeUnifier {
    /// Create a unifier with one instance only
    pub fn new() -> Self {
        Self {
            typing: Typing::new(),
        }
    }

    /// Creates a new type variable.
    ///
    /// # Returns
    /// A new `TypeVar` instance.
    pub fn mk_var(&mut self) -> TypeVar {
        self.typing.mk_var()
    }

    /// Unifies two types and returns the unified type.
    ///
    /// # Parameters
    /// - `lhs`: The left-hand side type.
    /// - `rhs`: The right-hand side type.
    ///
    /// # Returns
    /// A `TIResult` containing the unified type or `None` if unification fails.
    pub fn unify(&mut self, lhs: &TypeRef, rhs: &TypeRef) -> TIResult<Option<TypeRef>> {
        let mut involved = BTreeSet::new();
        self.typing.unify(lhs, rhs, &mut involved)
    }

    /// Retrieve either an assigned type or the variable itself (if multiple options available)
    ///
    /// # Parameters
    /// - `var`: The type variable to retrieve the type for.
    ///
    /// # Returns
    /// The `TypeRef` representing the type of the variable.
    fn retrieve_type(&self, var: &TypeVar) -> TypeRef {
        self.typing.retrieve_type(var)
    }

    /// Refreshes a type by replacing any type variables with their inferred types.
    ///
    /// # Parameters
    /// - `ty`: The type to refresh.
    ///
    /// # Returns
    /// A new `TypeRef` with type variables replaced.
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
