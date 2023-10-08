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
pub trait SMT: 'static + Copy + Ord + Hash + Default + Sync + Send {}

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

    /// operation: `v.append(e)`
    pub fn append(v: Self, e: T) -> Self {
        Self {
            inner: Intern::new(v.inner.iter().copied().chain(std::iter::once(e)).collect()),
        }
    }

    /// operation: `v.length()`
    pub fn length(v: Self) -> Integer {
        v.inner.len().into()
    }

    /// operation: `v.contains(e)`
    pub fn contains(v: Self, e: T) -> Boolean {
        v.inner.contains(&e).into()
    }

    /// operation: `v[i]` with partial semantics (valid only when `i` is in bound)
    pub fn at_unchecked(v: Self, i: Integer) -> T {
        *v.inner
            .get(i.inner.to_usize().expect("index out of usize range"))
            .expect("index out of bound")
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

    /// operation: `s.insert(e)`
    pub fn insert(s: Self, e: T) -> Self {
        Self {
            inner: Intern::new(s.inner.iter().copied().chain(std::iter::once(e)).collect()),
        }
    }

    /// operation: `s.length()`
    pub fn length(s: Self) -> Integer {
        s.inner.len().into()
    }

    /// operation: `v.contains(e)`
    pub fn contains(s: Self, e: T) -> Boolean {
        s.inner.contains(&e).into()
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

    /// operation: `m.put(k, v)`, will override `v` if `k` already exists
    pub fn put_unchecked(m: Self, k: K, v: V) -> Self {
        Self {
            inner: Intern::new(
                m.inner
                    .iter()
                    .map(|(k, v)| (*k, *v))
                    .chain(std::iter::once((k, v)))
                    .collect(),
            ),
        }
    }

    /// operation: `m.get(k)` with partial semantics (valid only when `k` exists)
    pub fn get_unchecked(m: Self, k: K) -> V {
        *m.inner.get(&k).expect("key does not exist")
    }

    /// operation: `m.length()`
    pub fn length(m: Self) -> Integer {
        m.inner.len().into()
    }

    /// operation: `v.contains_key(e)`
    pub fn contains_key(m: Self, k: K) -> Boolean {
        m.inner.contains_key(&k).into()
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
    pub fn merge(l: Self, r: Self) -> Self {
        Self {
            inner: Intern::new(l.inner.iter().chain(r.inner.iter()).copied().collect()),
        }
    }
}

impl SMT for Error {}
