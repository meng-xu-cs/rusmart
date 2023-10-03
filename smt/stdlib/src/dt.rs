use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Add, Div, Mul, Rem, Sub};

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;

/// Derive the arbitrary values as long as the type implements default
macro_rules! smt_arbitrary {
    () => {
        /// variable: any
        pub fn any() -> Self {
            Self::default()
        }

        /// variable: forall
        pub fn forall() -> Self {
            Self::default()
        }

        /// variable: exists
        pub fn exists() -> Self {
            Self::default()
        }
    };
}

/// SMT boolean
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Boolean {
    inner: bool,
}

impl Boolean {
    smt_arbitrary!();

    /// create a boolean constant, requires `inner` to be a literal
    pub fn new(c: bool) -> Self {
        Self { inner: c }
    }

    /// operation: !
    pub fn not(v: &Self) -> Self {
        Self { inner: !v.inner }
    }

    /// operation: &
    pub fn and(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner & r.inner,
        }
    }

    /// operation: |
    pub fn or(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner | r.inner,
        }
    }

    /// operation: ^
    pub fn xor(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner ^ r.inner,
        }
    }

    /// Unpack to primitive type
    pub fn unpack(v: &Self) -> bool {
        v.inner
    }
}

/// Arbitrary precision integer
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Integer {
    inner: BigInt,
}

impl Integer {
    smt_arbitrary!();

    /// create a integer constant, requires `inner` to be a literal
    pub fn new(c: i128) -> Self {
        Self {
            inner: BigInt::from(c),
        }
    }

    /// operation: ==
    pub fn eq(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner == r.inner,
        }
    }

    /// operation: !=
    pub fn ne(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner != r.inner,
        }
    }

    /// operation: <
    pub fn lt(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner < r.inner,
        }
    }

    /// operation: <=
    pub fn le(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner <= r.inner,
        }
    }

    /// operation: >=
    pub fn ge(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner >= r.inner,
        }
    }

    /// operation: >
    pub fn gt(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner > r.inner,
        }
    }

    /// operation: +
    pub fn add(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().add(r.inner.clone()),
        }
    }

    /// operation: -
    pub fn sub(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().sub(r.inner.clone()),
        }
    }

    /// operation: *
    pub fn mul(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().mul(r.inner.clone()),
        }
    }

    /// operation: /
    pub fn div(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().div(r.inner.clone()),
        }
    }

    /// operation: %
    pub fn rem(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().rem(r.inner.clone()),
        }
    }
}

/// Arbitrary precision rational number
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Rational {
    inner: BigRational,
}

impl Rational {
    smt_arbitrary!();

    /// create a rational constant, requires `inner` to be a **integer** literal
    pub fn new(c: i128) -> Self {
        Self {
            inner: BigRational::from(BigInt::from(c)),
        }
    }

    /// operation: ==
    pub fn eq(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner == r.inner,
        }
    }

    /// operation: !=
    pub fn ne(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner != r.inner,
        }
    }

    /// operation: <
    pub fn lt(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner < r.inner,
        }
    }

    /// operation: <=
    pub fn le(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner <= r.inner,
        }
    }

    /// operation: >=
    pub fn ge(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner >= r.inner,
        }
    }

    /// operation: >
    pub fn gt(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner > r.inner,
        }
    }

    /// operation: +
    pub fn add(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().add(r.inner.clone()),
        }
    }

    /// operation: -
    pub fn sub(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().sub(r.inner.clone()),
        }
    }

    /// operation: *
    pub fn mul(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().mul(r.inner.clone()),
        }
    }

    /// operation: /
    pub fn div(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.clone().div(r.inner.clone()),
        }
    }
}

/// SMT string
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Text {
    inner: String,
}

impl Text {
    smt_arbitrary!();

    /// create a string constant, requires `inner` to be a literal
    pub fn new(c: String) -> Self {
        Self { inner: c }
    }

    /// operation: ==
    pub fn eq(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner == r.inner,
        }
    }

    /// operation: !=
    pub fn ne(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner != r.inner,
        }
    }

    /// operation: < (lexicographically)
    pub fn lt(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner < r.inner,
        }
    }

    /// operation: <= (lexicographically)
    pub fn le(l: &Self, r: &Self) -> Boolean {
        Boolean {
            inner: l.inner <= r.inner,
        }
    }
}

/// SMT sequence
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Seq<T: Clone + Ord + Default> {
    inner: Vec<T>,
}

impl<T: Clone + Ord + Default> Seq<T> {
    smt_arbitrary!();

    /// create an empty sequence
    pub fn empty() -> Self {
        Self { inner: Vec::new() }
    }

    /// operation: `v.append(e)`
    pub fn append(v: &Self, e: &T) -> Self {
        Self {
            inner: v.inner.iter().chain(std::iter::once(e)).cloned().collect(),
        }
    }

    /// operation: `v.length()`
    pub fn length(v: &Self) -> Integer {
        Integer {
            inner: BigInt::from(v.inner.len()),
        }
    }

    /// operation: `v.contains(e)`
    pub fn contains(v: &Self, e: &T) -> Boolean {
        Boolean {
            inner: v.inner.contains(e),
        }
    }

    /// operation: `v[i]` with partial semantics (valid only when `i` is in bound)
    pub fn at_unchecked(v: &Self, i: &Integer) -> T {
        v.inner
            .get(i.inner.to_usize().expect("index out of usize range"))
            .expect("index out of bound")
            .clone()
    }
}

/// SMT set
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Set<T: Clone + Ord + Default> {
    inner: BTreeSet<T>,
}

impl<T: Clone + Ord + Default> Set<T> {
    smt_arbitrary!();

    /// create an empty set
    pub fn empty() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }

    /// operation: `s.insert(e)`
    pub fn insert(s: &Self, e: &T) -> Self {
        Self {
            inner: s.inner.iter().chain(std::iter::once(e)).cloned().collect(),
        }
    }

    /// operation: `s.length()`
    pub fn length(s: &Self) -> Integer {
        Integer {
            inner: BigInt::from(s.inner.len()),
        }
    }

    /// operation: `v.contains(e)`
    pub fn contains(s: &Self, e: &T) -> Boolean {
        Boolean {
            inner: s.inner.contains(e),
        }
    }
}

/// SMT array
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Map<K: Clone + Ord + Default, V: Clone + Ord + Default> {
    inner: BTreeMap<K, V>,
}

impl<K: Clone + Ord + Default, V: Clone + Ord + Default> Map<K, V> {
    smt_arbitrary!();

    /// create an empty map
    pub fn empty() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    /// operation: `m.put(k, v)`, will override `v` if `k` already exists
    pub fn put_unchecked(m: &Self, k: &K, v: &V) -> Self {
        Self {
            inner: m
                .inner
                .iter()
                .chain(std::iter::once((k, v)))
                .map(|(i, e)| (i.clone(), e.clone()))
                .collect(),
        }
    }

    /// operation: `m.get(k)` with partial semantics (valid only when `k` exists)
    pub fn get_unchecked(m: &Self, k: &K) -> V {
        m.inner.get(k).expect("key does not exist").clone()
    }

    /// operation: `m.length()`
    pub fn length(m: &Self) -> Integer {
        Integer {
            inner: BigInt::from(m.inner.len()),
        }
    }

    /// operation: `v.contains(e)`
    pub fn contains_key(m: &Self, k: &K) -> Boolean {
        Boolean {
            inner: m.inner.contains_key(k),
        }
    }
}

/// Dynamically assigned error
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct Error {
    inner: BTreeSet<usize>,
}

impl Error {
    smt_arbitrary!();

    /// Create a fresh error
    pub fn fresh() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }

    /// Merge two errors
    pub fn merge(l: &Self, r: &Self) -> Self {
        Self {
            inner: l.inner.iter().chain(r.inner.iter()).cloned().collect(),
        }
    }
}
