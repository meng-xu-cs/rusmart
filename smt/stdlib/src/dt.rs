use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hash;
use std::ops::{Add, Deref, Div, Mul, Rem, Sub};

use internment::Intern;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;

/// Pre-defined order (including equality and comparison) operators
macro_rules! order_operator {
    ($l:ty $(,$op: tt)*) => {
        impl $l {
            $(
                pub fn $op(self, rhs: Self) -> Boolean {
                    self.inner.as_ref().$op(rhs.inner.as_ref()).into()
            }
            )*
        }
    };
}

/// Pre-defined arithmetic operators
macro_rules! arith_operator {
    ($l:ty $(,$op: tt)*) => {
        impl $l {
            $(
                #[allow(clippy::should_implement_trait)]
                pub fn $op(self, rhs: Self) -> Self {
                    Self {
                        inner: Intern::new(
                            self.inner.as_ref().$op(rhs.inner.as_ref())
                        )
                    }
                }
            )*
        }
    };
}

/// Marks that this type is an SMT-enabled type
pub trait SMT: 'static + Copy + Ord + Hash + Default + Sync + Send {
    /// SMT values of the same type are subject to equality testing
    fn eq(self, rhs: Self) -> Boolean {
        (self == rhs).into()
    }

    /// SMT values of the same type are subject to inequality testing
    fn ne(self, rhs: Self) -> Boolean {
        (self != rhs).into()
    }
}

/// SMT boolean
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Boolean {
    inner: bool,
}

impl From<bool> for Boolean {
    fn from(c: bool) -> Self {
        Self { inner: c }
    }
}

impl Deref for Boolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.inner
    }
}

impl Boolean {
    #[allow(clippy::should_implement_trait)]
    pub fn not(self) -> Self {
        Self { inner: !self.inner }
    }

    pub fn and(self, rhs: Self) -> Self {
        Self {
            inner: self.inner & rhs.inner,
        }
    }

    pub fn or(self, rhs: Self) -> Self {
        Self {
            inner: self.inner | rhs.inner,
        }
    }

    pub fn xor(self, rhs: Self) -> Self {
        Self {
            inner: self.inner ^ rhs.inner,
        }
    }
}

impl SMT for Boolean {}

/// Arbitrary precision integer
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Integer {
    inner: Intern<BigInt>,
}

/// Convert to integer from literals
macro_rules! integer_from_literal {
    ($l:ty $(,$e:ty)* $(,)?) => {
        impl From<$l> for Integer {
            fn from(c: $l) -> Self {
                Self {
                    inner: Intern::new(BigInt::from(c)),
                }
            }
        }
        $(impl From<$e> for Integer {
            fn from(c: $e) -> Self {
                Self {
                    inner: Intern::new(BigInt::from(c)),
                }
            }
        })+
    };
}

integer_from_literal!(i8, i16, i32, i64, i128, isize);
integer_from_literal!(u8, u16, u32, u64, u128, usize);

arith_operator!(Integer, add, sub, mul, div, rem);
order_operator!(Integer, eq, ne, lt, le, ge, gt);

impl SMT for Integer {}

/// Arbitrary precision rational number
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Rational {
    inner: Intern<BigRational>,
}
/// Convert to integer from literals
macro_rules! rational_from_literal {
    ($l:ty $(,$e:ty)* $(,)?) => {
        impl From<$l> for Rational {
            fn from(c: $l) -> Self {
                Self {
                    inner: Intern::new(BigRational::from(BigInt::from(c))),
                }
            }
        }
        $(impl From<$e> for Rational {
            fn from(c: $e) -> Self {
                Self {
                    inner: Intern::new(BigRational::from(BigInt::from(c))),
                }
            }
        })+
    };
}

rational_from_literal!(i8, i16, i32, i64, i128, isize);
rational_from_literal!(u8, u16, u32, u64, u128, usize);

arith_operator!(Rational, add, sub, mul, div);
order_operator!(Rational, eq, ne, lt, le, ge, gt);

impl SMT for Rational {}

/// SMT string
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Text {
    inner: Intern<String>,
}

impl From<&'static str> for Text {
    fn from(c: &'static str) -> Self {
        Self {
            inner: Intern::new(c.to_string()),
        }
    }
}

order_operator!(Text, eq, ne, lt, le);

impl SMT for Text {}

/// SMT cloak (for breaking cyclic dependency in ADT)
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Cloak<T: SMT> {
    inner: Intern<Box<T>>,
}

impl<T: SMT> Cloak<T> {
    /// shield an object behind a cloak
    pub fn shield(t: T) -> Self {
        Self {
            inner: Intern::new(Box::new(t)),
        }
    }

    /// reveal an object behind a cloak
    pub fn reveal(self) -> T {
        *(*self.inner).as_ref()
    }
}

impl<T: SMT> SMT for Cloak<T> {}

/// SMT sequence
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Seq<T: SMT> {
    inner: Intern<Vec<T>>,
}

impl<T: SMT> Seq<T> {
    /// create an empty sequence
    pub fn empty() -> Self {
        Self {
            inner: Intern::new(vec![]),
        }
    }

    /// operation: `v.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// operation: `v.append(e)`
    pub fn append(self, e: T) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .copied()
                    .chain(std::iter::once(e))
                    .collect(),
            ),
        }
    }

    /// operation: `v[i]` with partial semantics (valid only when `i` is in bound)
    pub fn at_unchecked(self, i: Integer) -> T {
        *self
            .inner
            .get(i.inner.to_usize().expect("index out of usize range"))
            .expect("index out of bound")
    }

    /// operation: `v.includes(e)`
    pub fn includes(self, e: T) -> Boolean {
        self.inner.contains(&e).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<Integer> {
        (0..self.inner.len()).map(Integer::from).collect()
    }
}

impl<T: SMT> SMT for Seq<T> {}

/// SMT set
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Set<T: SMT> {
    inner: Intern<BTreeSet<T>>,
}

impl<T: SMT> Set<T> {
    /// create an empty set
    pub fn empty() -> Self {
        Self {
            inner: Intern::new(BTreeSet::new()),
        }
    }

    /// operation: `s.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// operation: `s.insert(e)`
    pub fn insert(self, e: T) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .copied()
                    .chain(std::iter::once(e))
                    .collect(),
            ),
        }
    }

    /// operation: `s.remove(e)`
    pub fn remove(self, e: T) -> Self {
        Self {
            inner: Intern::new(self.inner.iter().filter(|i| *i != &e).copied().collect()),
        }
    }

    /// operation: `v.contains(e)`
    pub fn contains(self, e: T) -> Boolean {
        self.inner.contains(&e).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<T> {
        self.inner.iter().copied().collect()
    }
}

impl<T: SMT> SMT for Set<T> {}

/// SMT array
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Map<K: SMT, V: SMT> {
    inner: Intern<BTreeMap<K, V>>,
}

impl<K: SMT, V: SMT> Map<K, V> {
    /// create an empty map
    pub fn empty() -> Self {
        Self {
            inner: Intern::new(BTreeMap::new()),
        }
    }

    /// operation: `m.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// operation: `m.put(k, v)`, will override `v` if `k` already exists
    pub fn put_unchecked(self, k: K, v: V) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .map(|(k, v)| (*k, *v))
                    .chain(std::iter::once((k, v)))
                    .collect(),
            ),
        }
    }

    /// operation: `m.get(k)` with partial semantics (valid only when `k` exists)
    pub fn get_unchecked(self, k: K) -> V {
        *self.inner.get(&k).expect("key does not exist")
    }

    /// operation: `m.del(k, v)`, will delete the (`k`, `v`) pair only when `k` exists
    pub fn del_unchecked(self, k: K) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .filter_map(|(i, v)| if i == &k { None } else { Some((*i, *v)) })
                    .collect(),
            ),
        }
    }

    /// operation: `v.contains_key(e)`
    pub fn contains_key(self, k: K) -> Boolean {
        self.inner.contains_key(&k).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<K> {
        self.inner.keys().copied().collect()
    }
}

impl<K: SMT, V: SMT> SMT for Map<K, V> {}

/// Dynamically assigned error
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Error {
    inner: Intern<BTreeSet<usize>>,
}

impl Error {
    /// Create a fresh error
    pub fn fresh() -> Self {
        Self {
            inner: Intern::new(BTreeSet::new()),
        }
    }

    /// Merge two errors
    pub fn merge(self, r: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.iter().chain(r.inner.iter()).copied().collect()),
        }
    }
}

impl SMT for Error {}
