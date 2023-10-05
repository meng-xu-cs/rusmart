use internment::Intern;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hash;
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, Div, Mul, Not, Rem, Sub};

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;

/// Marks that this type is an SMT-enabled type
pub trait SMT: Copy + Ord + Hash + Default {
    /// variable: any
    fn any() -> Self {
        Self::default()
    }

    /// variable: forall
    fn forall() -> Self {
        Self::any()
    }

    /// variable: exists
    fn exists() -> Self {
        Self::any()
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

impl Not for Boolean {
    type Output = Self;
    fn not(self) -> Self {
        Self { inner: !self.inner }
    }
}

impl BitAnd for Boolean {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self {
            inner: self.inner & rhs.inner,
        }
    }
}

impl BitOr for Boolean {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self {
            inner: self.inner | rhs.inner,
        }
    }
}

impl BitXor for Boolean {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Self {
            inner: self.inner ^ rhs.inner,
        }
    }
}

impl Deref for Boolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.inner
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

impl Add for Integer {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.add(rhs.inner.as_ref())),
        }
    }
}

impl Sub for Integer {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.sub(rhs.inner.as_ref())),
        }
    }
}

impl Mul for Integer {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.mul(rhs.inner.as_ref())),
        }
    }
}

impl Div for Integer {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.div(rhs.inner.as_ref())),
        }
    }
}

impl Rem for Integer {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.rem(rhs.inner.as_ref())),
        }
    }
}

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

impl Add for Rational {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.add(rhs.inner.as_ref())),
        }
    }
}

impl Sub for Rational {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.sub(rhs.inner.as_ref())),
        }
    }
}

impl Mul for Rational {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.mul(rhs.inner.as_ref())),
        }
    }
}

impl Div for Rational {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.div(rhs.inner.as_ref())),
        }
    }
}

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
            inner: v.inner.iter().chain(std::iter::once(e)).cloned().collect(),
        }
    }

    /// operation: `v.length()`
    pub fn length(v: &Self) -> Integer {
        v.inner.len().into()
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

impl<T: SMT> SMT for Seq<T> {}

/// SMT set
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Set<T: SMT> {
    inner: BTreeSet<T>,
}

impl<T: SMT> Set<T> {
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
        s.inner.len().into()
    }

    /// operation: `v.contains(e)`
    pub fn contains(s: &Self, e: &T) -> Boolean {
        Boolean {
            inner: s.inner.contains(e),
        }
    }
}

impl<T: SMT> SMT for Set<T> {}

/// SMT array
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Map<K: SMT, V: SMT> {
    inner: BTreeMap<K, V>,
}

impl<K: SMT, V: SMT> Map<K, V> {
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
        m.inner.len().into()
    }

    /// operation: `v.contains(e)`
    pub fn contains_key(m: &Self, k: &K) -> Boolean {
        Boolean {
            inner: m.inner.contains_key(k),
        }
    }
}

impl<K: SMT, V: SMT> SMT for Map<K, V> {}

/// Dynamically assigned error
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct Error {
    inner: BTreeSet<usize>,
}

impl Error {
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

impl SMT for Error {}
