use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, Div, Mul, Rem, Sub};
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;

use internment::Intern;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use paste::paste;

/// Default implementation of the SNT trait
macro_rules! smt_impl {
    ($l:ty) => {
        impl SMT for $l {
            fn _cmp(self, rhs: Self) -> Ordering {
                self.inner.cmp(&rhs.inner)
            }
        }
    };
    ($l:ident, $t0:ident $(, $tn:ident)*) => {
        impl<$t0: SMT $(, $tn: SMT)*> SMT for $l<$t0 $(, $tn)*> {
            fn _cmp(self, rhs: Self) -> Ordering {
                self.inner.cmp(&rhs.inner)
            }
        }
    };
}

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
pub trait SMT: 'static + Copy + Default + Hash + Send + Sync {
    /// SMT values of the same type are totally ordered
    fn _cmp(self, rhs: Self) -> Ordering;

    /// SMT values of the same type are subject to equality testing
    fn eq(self, rhs: Self) -> Boolean {
        (Self::_cmp(self, rhs) == Ordering::Equal).into()
    }
    /// SMT values of the same type are subject to inequality testing
    fn ne(self, rhs: Self) -> Boolean {
        (Self::_cmp(self, rhs) != Ordering::Equal).into()
    }
}

/// Wrap for an SMT type for Rust-semantics enrichment
#[derive(Debug, Clone, Copy, Default)]
struct SMTWrap<T: SMT>(T);

impl<T: SMT> PartialEq for SMTWrap<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0).inner
    }
}
impl<T: SMT> Eq for SMTWrap<T> {}

impl<T: SMT> PartialOrd for SMTWrap<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: SMT> Ord for SMTWrap<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        T::_cmp(self.0, other.0)
    }
}

impl<T: SMT> Hash for SMTWrap<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

/// SMT boolean
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Boolean {
    inner: bool,
}
smt_impl!(Boolean);

impl Deref for Boolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.inner
    }
}

impl From<bool> for Boolean {
    fn from(c: bool) -> Self {
        Self { inner: c }
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

    pub fn implies(self, rhs: Self) -> Self {
        Self {
            inner: !self.inner | rhs.inner,
        }
    }
}

/// Arbitrary precision integer
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Integer {
    inner: Intern<BigInt>,
}
smt_impl!(Integer);

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
order_operator!(Integer, lt, le, ge, gt);

/// Arbitrary precision rational number
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Rational {
    inner: Intern<BigRational>,
}
smt_impl!(Rational);

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
order_operator!(Rational, lt, le, ge, gt);

/// SMT string
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Text {
    inner: Intern<String>,
}
smt_impl!(Text);

impl From<&'static str> for Text {
    fn from(c: &'static str) -> Self {
        Self {
            inner: Intern::new(c.to_string()),
        }
    }
}

order_operator!(Text, lt, le);

/// SMT cloak (for breaking cyclic dependency in ADT)
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Cloak<T: SMT> {
    inner: Intern<SMTWrap<T>>,
}
smt_impl!(Cloak, T);

impl<T: SMT> Cloak<T> {
    /// shield an object behind a cloak
    pub fn shield(t: T) -> Self {
        Self {
            inner: Intern::new(SMTWrap(t)),
        }
    }

    /// reveal an object behind a cloak
    pub fn reveal(self) -> T {
        self.inner.0
    }
}

/// SMT sequence
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Seq<T: SMT> {
    inner: Intern<Vec<SMTWrap<T>>>,
}
smt_impl!(Seq, T);

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
                    .chain(std::iter::once(SMTWrap(e)))
                    .collect(),
            ),
        }
    }

    /// operation: `v[i]` with partial semantics (valid only when `i` is in bound)
    pub fn at_unchecked(self, i: Integer) -> T {
        self.inner
            .get(i.inner.to_usize().expect("index out of usize range"))
            .expect("index out of bound")
            .0
    }

    /// operation: `v.includes(e)`
    pub fn includes(self, e: T) -> Boolean {
        self.inner.iter().any(|i| *T::eq(i.0, e)).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<Integer> {
        (0..self.inner.len()).map(Integer::from).collect()
    }
}

/// SMT set
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Set<T: SMT> {
    inner: Intern<BTreeSet<SMTWrap<T>>>,
}
smt_impl!(Set, T);

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
                    .chain(std::iter::once(SMTWrap(e)))
                    .collect(),
            ),
        }
    }

    /// operation: `s.remove(e)`
    pub fn remove(self, e: T) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .filter(|i| *T::ne(i.0, e))
                    .copied()
                    .collect(),
            ),
        }
    }

    /// operation: `v.contains(e)`
    pub fn contains(self, e: T) -> Boolean {
        self.inner.iter().any(|i| *T::eq(i.0, e)).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<T> {
        self.inner.iter().map(|i| i.0).collect()
    }
}

/// SMT array
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Map<K: SMT, V: SMT> {
    inner: Intern<BTreeMap<SMTWrap<K>, SMTWrap<V>>>,
}
smt_impl!(Map, K, V);

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
                    .chain(std::iter::once((SMTWrap(k), SMTWrap(v))))
                    .collect(),
            ),
        }
    }

    /// operation: `m.get(k)` with partial semantics (valid only when `k` exists)
    pub fn get_unchecked(self, k: K) -> V {
        self.inner.get(&SMTWrap(k)).expect("key does not exist").0
    }

    /// operation: `m.del(k, v)`, will delete the (`k`, `v`) pair only when `k` exists
    pub fn del_unchecked(self, k: K) -> Self {
        Self {
            inner: Intern::new(
                self.inner
                    .iter()
                    .filter_map(|(i, v)| if *K::eq(i.0, k) { None } else { Some((*i, *v)) })
                    .collect(),
            ),
        }
    }

    /// operation: `v.contains_key(e)`
    pub fn contains_key(self, k: K) -> Boolean {
        self.inner.contains_key(&SMTWrap(k)).into()
    }

    /// iterator
    pub fn iterator(self) -> Vec<K> {
        self.inner.keys().map(|i| i.0).collect()
    }
}

/// Dynamically assigned error
#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Error {
    inner: Intern<BTreeSet<usize>>,
}
smt_impl!(Error);

static _ERROR_COUNTER_: AtomicUsize = AtomicUsize::new(0);

impl Error {
    /// Create a fresh error
    pub fn fresh() -> Self {
        let mut set = BTreeSet::new();
        set.insert(_ERROR_COUNTER_.fetch_add(1, atomic::Ordering::SeqCst));
        Self {
            inner: Intern::new(set),
        }
    }

    /// Merge two errors
    pub fn merge(self, r: Self) -> Self {
        Self {
            inner: Intern::new(self.inner.iter().chain(r.inner.iter()).copied().collect()),
        }
    }
}

/// Trait implementation
macro_rules! smt_tuple_impls {
    ( $n0:ident $($nx:ident)+ ) => {
        impl<$n0: SMT $(, $nx: SMT)+> SMT for ($n0 $(, $nx)+) {
            #[allow(non_snake_case)]
            fn _cmp(self, rhs: Self) -> Ordering {
                let (paste!{[<l_ $n0>]} $(, paste!{[<l_ $nx>]})+) = self;
                let (paste!{[<r_ $n0>]} $(, paste!{[<r_ $nx>]})+) = rhs;
                match $n0::_cmp(paste!{[<l_ $n0>]}, paste!{[<r_ $n0>]}) {
                    Ordering::Less => return Ordering::Less,
                    Ordering::Greater => return Ordering::Greater,
                    Ordering::Equal => {},
                };
                $(match $nx::_cmp(paste!{[<l_ $nx>]}, paste!{[<r_ $nx>]}) {
                    Ordering::Less => return Ordering::Less,
                    Ordering::Greater => return Ordering::Greater,
                    Ordering::Equal => {},
                };)+
                Ordering::Equal
            }
        }
    };
}

smt_tuple_impls! { A B }
smt_tuple_impls! { A B C }
smt_tuple_impls! { A B C D }
smt_tuple_impls! { A B C D E }
smt_tuple_impls! { A B C D E F }
smt_tuple_impls! { A B C D E F G }
smt_tuple_impls! { A B C D E F G H }
smt_tuple_impls! { A B C D E F G H I }
smt_tuple_impls! { A B C D E F G H I J }
smt_tuple_impls! { A B C D E F G H I J K }
smt_tuple_impls! { A B C D E F G H I J K L }
