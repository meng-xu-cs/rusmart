//! Standard library for SMT types in Rusmart
//!
//! This module provides the following SMT types:
//!
//! * `Boolean` - SMT boolean
//!
//! * `Integer` - SMT integer
//!
//! * `Rational` - SMT rational
//!
//! * `Text` - SMT string
//!
//! * `Cloak<T>` - SMT cloak - A wrapper over T to allow recursive data types to be defined.
//!
//! * `Seq<T>` - SMT sequence - A sequence (list) of SMT values of type T
//!
//! * `Set<T>` - SMT set - A set of SMT values of type T.
//!
//! * `Map<K,V>` - SMT map - SMT array of key type K and value type Option<V> with None as the default values.
//!
//! * `Error` - SMT error - A special marker to indicate error states.

//----------------------------------------DEPENDENCIES----------------------------------------------------//

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hash;
use std::ops::{Add, Deref, Div, Mul, Rem, Sub};
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;

use internment::Intern;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use paste::paste;

//------------------------------------------MACROS-------------------------------------------------------//

/// Arithmetic operators
/// In the below example, the operators add, sub, mul, div, and rem are implemented for the Integer type.
/// Note 1: The type needs to be a struct with an `inner` field that is wrapped inside an `Intern`.
/// Note 2: The designated operators are required to be defined for the type inside the Intern.
/// 
/// Example: arith_operator!(Integer, add, sub, mul, div, rem);
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

/// Default implementation of the SMT trait for a type.
/// This has three patterns to be matched:
/// 1. non-generic types (Integer, Error, Rational, Text, Boolean), fall in the first pattern.
/// 2. generic types (Cloak, Seq, Set, Map), fall in the second pattern.
/// 3. the third pattern is the smt implementation for tuples of size 2 to 12.
/// In generic types, the type parameters need to implement the SMT trait.
/// In both cases, the types need to be a struct with a field `inner` that implements the Ord trait.
/// 
/// Example: smt_impl!(Integer);
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

// In the first example, the macro implements the SMT trait for tuples of two elements A and B.
// Both A and B need to implement the SMT trait.
smt_impl! { A B }
smt_impl! { A B C }
smt_impl! { A B C D }
smt_impl! { A B C D E }
smt_impl! { A B C D E F }
smt_impl! { A B C D E F G }
smt_impl! { A B C D E F G H }
smt_impl! { A B C D E F G H I }
smt_impl! { A B C D E F G H I J }
smt_impl! { A B C D E F G H I J K }
smt_impl! { A B C D E F G H I J K L }

/// Pre-defined order (including equality and comparison) operators.
/// This macro implments the order operators only for the Integer, Rational, and Text types.
/// Order operators are: lt, le, ge, gt.
/// `lt` is less than
/// `le` is less than or equal to
/// `ge` is greater than or equal to
/// `gt` is greater than
/// 
/// Example: order_operator!(Integer, lt, le, ge, gt);
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

/// Convert to the Integer type from literals
/// This macro provides the following functionalities (in the examples `a` is of type Integer):
/// let a = Integer::from(1);
/// let a:Integer = 1.into(); // this needs to be annotated
/// let a:Integer = From::from(1); // this needs to be annotated
/// 
/// Example: 
///     1) integer_from_literal!(i8, i16, i32, i64, i128, isize); 
///     2) integer_from_literal!(u8, u16, u32, u64, u128, usize);
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
        })*
    };
}

/// Convert to Rational from int literals
/// This macro provides the following functionalities (in the examples `a` is of type Rational):
/// let a = Rational::from(1);
/// let a:Rational = 1.into(); // this needs to be annotated
/// let a:Rational = From::from(1); // this needs to be annotated
/// 
/// Example: 
///     1) rational_from_literal_int!(i8, i16, i32, i64, i128, isize); 
///     2) rational_from_literal_int!(u8, u16, u32, u64, u128, usize);
macro_rules! rational_from_literal_int {
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
        })*
    };
}

#[allow(unused_macros)]
#[macro_export]
/// the cloak! macro for initializing a new cloak with an element
/// 
/// Example: cloak!(Integer::from(1));
macro_rules! cloak {
    ( $x:expr ) => {{
        let cloak = Cloak::shield($x);
        cloak
    }};
}

#[allow(unused_macros)]
#[macro_export]
/// the seq! macro for initializing a new sequence with elements
/// 
/// Example: seq!(Integer::from(1), Integer::from(2));
macro_rules! seq {
    ($($e:expr),*) => {
        {
            let mut seq = Seq::new();
            $(
                seq = seq.append($e);
            )*
            seq
        }
    };
}

#[allow(unused_macros)]
#[macro_export]
/// the set! macro for initializing a new set with elements
/// 
/// Example: set!(Integer::from(1), Integer::from(2));
macro_rules! set {
    ( $($e:expr),*) => {
        {
            let mut set = Set::new();
            $(
                set = set.insert($e);
            )*
            set
        }
    };
}

#[allow(unused_macros)]
#[macro_export]
/// the map! macro for initializing a new map with key-value pairs
/// 
/// Example: map!((Integer::from(1), Text::from("one")), (Integer::from(2), Text::from("two")));
macro_rules! map {
($( ($e1:expr, $e2:expr) ),*) => {
        {
            let mut map = Map::new();
            $(
                map = map.put_unchecked($e1, $e2);
            )*
            map
        }
    };
}

//------------------------------------------SMT----------------------------------------------------------//
/// A type that implements the SMT trait must implement the following methods:
///
/// * `_cmp` - Compare two values of the same type
/// * `eq` - Check if two values of the same type are equal
/// * `ne` - Check if two values of the same type are not equal
///
/// The first method does not have a default implementation and must be implemented by the type.
/// The second and third methods have default implementations that use the `_cmp` method.
/// The following traits are the supertraits of the SMT trait, thus any type that implements the SMT trait must also implement these traits.
///
/// * `Copy` - The type must be copyable
/// * `Default` - The type must have a default value
/// * `Hash` - The type must be hashable
/// * `Send` - The type must be able to be sent between threads
/// * `Sync` - The type must be able to be access from multiple threads
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

/// This trait allows the methods of the SMT trait to be used between Integer and Rational types.
/// The trait is only implemented for the Integer and Rational types.
// pub trait Nums: SMT {
//     fn into_rational(self) -> Rational;
// }

//----------------------------------------SIMPLE-TYPES----------------------------------------------------//
/// ** 1) SMT boolean: A wrapper around the Rust boolean type
///
/// This type implements the SMT trait.
/// This SMT type implements the PartialEq and Eq traits to allow equality comparison for assertions.
/// The debug trait is implemented to allow for printing the assertion results.
/// This SMT type must implement the Copy, Hash, and Default traits as the supertraits of the SMT trait.
/// The Clone trait is implemented because it is required by the Copy trait.
/// The Deref trait is implemented to allow for dereferencing the inner boolean value.
/// The From trait is implemented to allow for converting a Rust boolean value to the SMT boolean type.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Boolean {
    inner: bool,
}
smt_impl!(Boolean);

impl Deref for Boolean {
    type Target = bool;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl From<bool> for Boolean {
    fn from(c: bool) -> Self {
        Self { inner: c }
    }
}

/// SMT boolean operators which follow the logical semantics
impl Boolean {
    #[allow(clippy::should_implement_trait)]
    pub fn not(self) -> Self {
        Self { inner: !self.inner }
    }

    pub fn and(self, rhs: Self) -> Self {
        Self {
            inner: self.inner && rhs.inner,
        }
    }

    pub fn or(self, rhs: Self) -> Self {
        Self {
            inner: self.inner || rhs.inner,
        }
    }

    pub fn xor(self, rhs: Self) -> Self {
        Self {
            inner: self.inner ^ rhs.inner,
        }
    }

    pub fn implies(self, rhs: Self) -> Self {
        Self {
            inner: !self.inner || rhs.inner,
        }
    }
}

/// ** 2) Arbitrary precision integer (SMT integer)
/// Most explanations are the same as the Boolean type.
/// The integer_from_literal! macro is used to implement the conversion from literals for the Integer type.
/// For example, we can write 1i16.into() or Integer::from(1i16) to convert 1 to the Integer type.
/// The arith_operator! macro is used to implement the arithmetic operators for the Integer type.
/// For example, we can write a.add(b) or Integer::add(a,b) to add two Integer values a and b.
/// The order_operator! macro is used to implement the order operators for the Integer type.
/// For example, we can write a.lt(b) or Integer::lt(a,b) to check if a is less than b.
// The Nums trait is implemented for the Integer type to allow for the conversion of Integer values to Rational values.
// The implementation for Integer has been added for the comparison between Rationals and Integers.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Integer {
    inner: Intern<BigInt>,
}
smt_impl!(Integer);

integer_from_literal!(i8, i16, i32, i64, i128, isize);
integer_from_literal!(u8, u16, u32, u64, u128, usize);

arith_operator!(Integer, add, sub, mul, div, rem);
// The ne and eq operators are implemented by default in the SMT
order_operator!(Integer, lt, le, ge, gt);

// impl Nums for Integer {
//     fn into_rational(self) -> Rational {
//         let num = self.inner.as_ref().clone().to_i64().expect("Failed to convert BigInt to i64");
//         Rational::from(num)
//     }
// }

// impl Integer {
//     fn _cmp<T: Nums>(self, rhs: T) -> Ordering {
//         SMT::_cmp(self.into_rational(), rhs.into_rational())
//     }

//     fn eq<T: Nums + SMT>(self, rhs: T) -> Boolean {
//         SMT::eq(self.into_rational(), rhs.into_rational())
//     }

//     fn ne<T: Nums + SMT>(self, rhs: T) -> Boolean {
//         SMT::ne(self.into_rational(), rhs.into_rational())
//     }
// }

/// ** 3) Arbitrary precision rational number (SMT rational)
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Rational {
    inner: Intern<BigRational>,
}
smt_impl!(Rational);

rational_from_literal_int!(i8, i16, i32, i64, i128, isize);
rational_from_literal_int!(u8, u16, u32, u64, u128, usize);

arith_operator!(Rational, add, sub, mul, div);
// The ne and eq operators are implemented by default in the SMT
order_operator!(Rational, lt, le, ge, gt);

// These allow us to build rational numbers from f32/f64
impl From<f32> for Rational {
    fn from(value: f32) -> Self {
        Self {
            inner: Intern::new(
                BigRational::from_float(value).expect("Failed to convert float to BigRational"),
            ),
        }
    }
}
impl From<f64> for Rational {
    fn from(value: f64) -> Self {
        Self {
            inner: Intern::new(
                BigRational::from_float(value).expect("Failed to convert float to BigRational"),
            ),
        }
    }
}

// impl Nums for Rational {
//     fn into_rational(self) -> Rational {
//         self
//     }
// }

// impl Rational {
//     fn _cmp<T: SMT + Nums>(self, rhs: T) -> Ordering {
//         SMT::_cmp(self.into_rational(), rhs.into_rational())
//     }
//     fn eq<T: Nums + SMT>(self, rhs: T) -> Boolean {
//         SMT::eq(self.into_rational(), rhs.into_rational())
//     }
//     fn ne<T: Nums + SMT>(self, rhs: T) -> Boolean {
//         SMT::ne(self.into_rational(), rhs.into_rational())
//     }
// }

/// ** 4) SMT string
/// The String inside the interns are compared in a lexicographical order when calling the cmp method.
/// For example, "a" < "b" and "aa" < "ab" and "a" < "aa" etc.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
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

order_operator!(Text, lt, le, ge, gt);

/// ** 5) Dynamically assigned error
/// This type is used to represent an error state in the SMT system.
/// The error state is created by calling the Error::fresh() function.
/// Every time the new() method is called, a new error state is created with a unique inner value.
/// The inner values are incremented by one each time a new error state is created.
/// The merge method is used to merge two error states where duplicates are not allowed.
///
/// let a = Error::fresh(); // a is of type Error with an inner value of 0
/// let a = Error::fresh(); // a is of type Error with an inner value of 1
/// let b = Error::fresh(); // b is of type Error with an inner value of 2
/// let c = a.merge(b); // c is of type Error with an inner value of 1, 2
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Error {
    inner: Intern<BTreeSet<usize>>,
}
smt_impl!(Error);

static _ERROR_COUNTER_: AtomicUsize = AtomicUsize::new(0);

impl Error {
    /// Create a new error
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

//---------------------------------------WRAPPER-TYPES---------------------------------------------------//

/// Wrap for an SMT type for Rust-semantics enrichment
/// The SMTWrap is a tuple struct that wraps an SMT type.
// In SMTWrap, instead of using #[derive(Eq)] we implement the trait manually to avoid imposing the T: Eq constraint.
#[derive(Debug, Clone, Copy, Default, Hash)]
struct SMTWrap<T: SMT>(T);

impl<T: SMT> PartialEq for SMTWrap<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0).inner
    }
}

// implementation for SMTWrap<Rational> for equality between Rationals and integers inside wrappers.
// impl PartialEq<SMTWrap<Integer>> for SMTWrap<Rational> {
//     fn eq(&self, other: &SMTWrap<Integer>) -> bool {
//         self.0.into_rational().eq(other.0.into_rational()).inner
//     }
// }

// implementation for SMTWrap<Integer> for equality between Rationals and integers inside wrappers.
// impl PartialEq<SMTWrap<Rational>> for SMTWrap<Integer> {
//     fn eq(&self, other: &SMTWrap<Rational>) -> bool {
//         self.0.into_rational().eq(other.0.into_rational()).inner
//     }
// }

impl<T: SMT> Eq for SMTWrap<T> {}

impl<T: SMT> PartialOrd for SMTWrap<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: SMT> Ord for SMTWrap<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0._cmp(other.0)
    }
}

/// ** 6) `Cloak` is used to prevent cyclic dependencies in Abstract Data Types (ADTs).
/// Cyclic dependencies lead to issues like infinite recursion (stack overflow) or memory leaks.
/// They act as a wrapper around the SMT type T.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Cloak<T: SMT> {
    inner: Intern<SMTWrap<T>>,
}
smt_impl!(Cloak, T);

impl<T: SMT> Cloak<T> {
    /// operation: `Cloak::shield(Integer::from(1))`
    pub fn shield(t: T) -> Self {
        Self {
            inner: Intern::new(SMTWrap(t)),
        }
    }
    /// operation: `let a = Cloak::shield(Integer::from(1)).reveal(); // a is of type Integer with value 1`
    pub fn reveal(self) -> T {
        self.inner.0
    }
}

/// ** 7) SMT sequence
/// This is a sequence (list) of SMT values of type T where T is a type that implements the SMT trait.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Seq<T: SMT> {
    inner: Intern<Vec<SMTWrap<T>>>,
}
smt_impl!(Seq, T);

impl<T: SMT> Seq<T> {
    /// create an new sequence
    /// operation: `Seq::new()`
    pub fn new() -> Self {
        Self {
            inner: Intern::new(vec![]),
        }
    }

    /// get the length of the sequence
    /// operation: `v.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// append an element to the sequence
    /// operation: `let mut v = Seq::new(); v = v.append(Integer::from(1));`
    /// note that the operaton is not in-place
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
    /// The method will panic if the index is too large or is out of bound.
    /// the too large can happen because the inner is BigInt or BigRational which may not fit in usize.
    pub fn at_unchecked(self, i: Integer) -> T {
        self.inner
            .get(i.inner.to_usize().expect("index out of usize range"))
            .expect(
                format!(
                    "index {:?} out of bound for Seq with type {}",
                    i,
                    std::any::type_name::<T>()
                )
                .as_str(),
            )
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

    /// checks if the sequence is empty
    /// operation: `v.is_empty()`
    pub fn is_empty(self) -> Boolean {
        self.inner.is_empty().into()
    }
}

/// ** 8) SMT set
/// This is a set of SMT values of type T where T is a type that implements the SMT trait.
/// The methods defined in the SMT Set type correspond to the operations performed on a set data structure.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Set<T: SMT> {
    inner: Intern<BTreeSet<SMTWrap<T>>>,
}
smt_impl!(Set, T);

impl<T: SMT> Set<T> {
    /// create an new set
    /// operation: `Set::new()`
    pub fn new() -> Self {
        Self {
            inner: Intern::new(BTreeSet::new()),
        }
    }

    /// return the length of the set
    /// operation: `s.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// a non in-place operation to insert an element into the set
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

    /// a non in-place operation to remove an element from the set
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
    /// It gives the elements of the set in a vector
    pub fn iterator(self) -> Vec<T> {
        self.inner.iter().map(|i| i.0).collect()
    }

    /// checks if the set is empty
    /// operation: `s.is_empty()`
    pub fn is_empty(self) -> Boolean {
        self.inner.is_empty().into()
    }
}

/// ** 9) SMT array
/// This is an array of key type K and value type V where K and V are types that implement the SMT trait.
#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub struct Map<K: SMT, V: SMT> {
    inner: Intern<BTreeMap<SMTWrap<K>, SMTWrap<V>>>,
}
smt_impl!(Map, K, V);

impl<K: SMT, V: SMT> Map<K, V> {
    /// create an new map
    /// operation: `Map::new()`
    pub fn new() -> Self {
        Self {
            inner: Intern::new(BTreeMap::new()),
        }
    }

    /// return the length of the map
    /// operation: `m.length()`
    pub fn length(self) -> Integer {
        self.inner.len().into()
    }

    /// a non in-place operation to insert a key-value pair into the map
    /// operation: `m.put(k, v)`, will override `v` if `k` already exists
    /// operation: `let mut m = Map::new(); m = m.put(Integer::from(1), Integer::from(2));`
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

    /// receive the value for a key and panic if the key does not exist
    /// operation: `m.get(k)` with partial semantics (valid only when `k` exists)
    pub fn get_unchecked(self, k: K) -> V {
        self.inner
            .get(&SMTWrap(k))
            .expect(
                format!(
                    "key does not exist for SMT Array with key types {}",
                    std::any::type_name::<K>()
                )
                .as_str(),
            )
            .0
    }

    /// a non in-place operation to delete a key-value pair from the map
    /// if the key does not exist, the operation will not do anything
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
    /// It gives the keys of the map in a vector
    pub fn iterator(self) -> Vec<K> {
        self.inner.keys().map(|i| i.0).collect()
    }

    /// checks if the map is empty
    /// operation: `m.is_empty()`
    pub fn is_empty(self) -> Boolean {
        self.inner.is_empty().into()
    }
}

//------------------------------------------TESTS--------------------------------------------------------//

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    /// This test is for checking the implementation of the the arith_operator macro on Integer types
    /// basically the func of add, div, mul, rem, and sub operators are checked on Integer types
    fn test_arith_operator_macro_integer() {
        let num1 = Integer::from(7);
        let num2 = Integer::from(2);

        let res = num1.add(num2);
        assert_eq!(res, Integer::from(9)); // 7 + 2 = 9

        let res = num1.div(num2);
        assert_eq!(res, Integer::from(3)); // 7 / 2 = 3

        let res = num1.mul(num2);
        assert_eq!(res, Integer::from(14)); // 7 * 2 = 14

        let res = num1.rem(num2);
        assert_eq!(res, Integer::from(1)); // 7 % 2 = 1

        let res = num1.sub(num2);
        assert_eq!(res, Integer::from(5)); // 7 - 2 = 5
    }

    #[test]
    /// This test is for checking the implementation of the the arith_operator macro on Rational types
    /// The functionality of add, div, mul, and sub operators are checked on Rational types
    fn test_arith_operator_macro_rational() {
        let num1 = Rational::from(7.5);
        let num2 = Rational::from(2);

        let res = num1.add(num2);
        assert_eq!(res, Rational::from(9.5)); // 7.5 + 2 = 9.5

        let res = num1.div(num2);
        assert_eq!(res, Rational::from(3.75)); // 7.5 / 2 = 3.75

        let res = num1.mul(num2);
        assert_eq!(res, Rational::from(15)); // 7.5 * 2 = 15

        let res = num1.sub(num2);
        assert_eq!(res, Rational::from(5.5)); // 7.5 - 2 = 5.5
    }

    #[test]
    /// This tests the smt_impl macro for the Boolean type
    /// testing the cmp method of the SMT trait on the Boolean type
    fn test_smt_impl_macro_boolean() {
        let var1 = Boolean::from(true);
        let var2 = Boolean::from(false);

        let res = var1._cmp(var2); // true > false
        assert_eq!(res, Ordering::Greater);
    }

    #[test]
    /// This tests the smt_impl macro for the Integer type
    /// testing the cmp method of the SMT trait on the Integer type
    fn test_smt_impl_macro_integer() {
        let var1 = Integer::from(1);
        let var2 = Integer::from(3);

        let res = var1._cmp(var2); // 1 < 3
        assert_eq!(res, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for the Rational type
    /// testing the cmp method of the SMT trait on the Rational type
    fn test_smt_impl_macro_rational() {
        let var1 = Rational::from(1.75);
        let var2 = Rational::from(3);

        let res = var1._cmp(var2); // 1.75 < 3
        assert_eq!(res, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for the Text type
    /// testing the cmp method of the SMT trait on the Text type
    /// This happens in a lexicographical order
    fn test_smt_impl_macro_text() {
        let var1 = Text::from("more");
        let var2 = Text::from("less");

        let res = var1._cmp(var2); // "more" > "less"
        assert_eq!(res, Ordering::Greater);
    }

    #[test]
    /// This tests the smt_impl macro for the Error type
    /// testing the cmp method of the SMT trait on the Error type
    fn test_smt_impl_macro_error() {
        let var1 = Error::fresh(); // 0
        let var2 = Error::fresh(); // 1
        let var3 = Error::fresh(); // 2

        let res1 = var1._cmp(var2); // 0 < 1
        let res2 = var2._cmp(var3); // 1 < 2
        let res3 = var1._cmp(var3); // 0 < 2

        assert_eq!(res1, Ordering::Less);
        assert_eq!(res2, Ordering::Less);
        assert_eq!(res3, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for the Cloak type
    /// testing the cmp method of the SMT trait on the Cloak type
    fn test_smt_impl_macro_cloak() {
        let var1 = Cloak::shield(Integer::from(2));
        let var2 = Cloak::shield(Integer::from(3));

        let res = var1._cmp(var2); // 2 < 3

        assert_eq!(res, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for Seq type
    /// testing the cmp method of the SMT trait on the Seq type
    /// elements are compared in order
    fn test_smt_impl_macro_seq() {
        let mut var1: Seq<Integer> = Seq::new();
        var1 = var1.append(Integer::from(1));
        var1 = var1.append(Integer::from(2));
        var1 = var1.append(Integer::from(3));
        var1 = var1.append(Integer::from(4));

        let mut var2: Seq<Integer> = Seq::new();
        var2 = var2.append(Integer::from(10));

        let res1 = var1._cmp(var2); // 1 < 10 (first element comparison)
        assert_eq!(res1, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for Set type
    /// testing the cmp method of the SMT trait on the Set type
    fn test_smt_impl_macro_set() {
        let mut var1 = Set::new();
        var1 = var1.insert(Text::from("hello"));
        var1 = var1.insert(Text::from("world"));

        let mut var2 = Set::new();
        var2 = var2.insert(Text::from("mehrad"));

        let res = var1._cmp(var2); // "hello" > "mehrad"
        assert_eq!(res, Ordering::Less);
    }

    #[test]
    /// This tests the smt_impl macro for Map type
    /// The keys are compared in lexicographical order
    fn test_smt_impl_macro_map() {
        let mut var1 = Map::new();

        var1 = var1.put_unchecked(Integer::from(1), Text::from("one"));
        var1 = var1.put_unchecked(Integer::from(2), Text::from("two"));
        var1 = var1.put_unchecked(Integer::from(3), Text::from("three"));

        let mut var2 = Map::new();
        var2 = var2.put_unchecked(Integer::from(0), Text::from("zero"));

        let res = var1._cmp(var2); // 1 > 0
        assert_eq!(res, Ordering::Greater);
    }

    #[test]
    /// This test is for checking the implementation of the the order_operator macro on Integer types
    /// The functionality of lt, le, and ge operators are checked on Integer types
    fn test_order_operator_macro_integer() {
        let var1 = Integer::from(1);
        let var2 = Integer::from(3);

        let res1 = var1.lt(var2); // 1 < 3
        let res2 = var1.le(var2); // 1 <= 3
        let res3 = var1.ge(var2); // 1 >= 3 (false)

        assert_eq!(res1, Boolean::from(true));
        assert_eq!(res2, Boolean::from(true));
        assert_eq!(res3, false.into());
    }

    #[test]
    /// This test is for checking the implementation of the the order_operator macro on Rational types
    /// The functionality of lt, le, and ge operators are checked on Rational types
    fn test_order_operator_macro_rational() {
        let var1 = Rational::from(1.5);
        let var2 = Rational::from(3);

        let res1 = var1.lt(var2); // 1.5 < 3
        let res2 = var1.le(var2); // 1.5 <= 3

        assert_eq!(res1, Boolean::from(true));
        assert_eq!(res2, Boolean::from(true));
    }

    #[test]
    /// This test is for the order_operator macro on Text types
    /// The functionality of lt, le, and gt operators are checked on Text types
    fn test_order_operator_macro_text() {
        let var1 = Text::from("1.5");
        let var2 = Text::from("9a");
        let var3 = Text::from("apple");

        let res1 = var1.lt(var2); // "1.5" < "9a"
        let res2 = var1.le(var2); // "1.5" <= "9a"
        let res3 = var3.gt(var2); // "apple" > "9a"

        assert_eq!(res1, Boolean::from(true));
        assert_eq!(res2, Boolean::from(true));
        assert_eq!(res3, Boolean::from(true));
    }

    #[test]
    /// Testing the integer_from_literal macro which gives access to the from method for Integer types
    fn test_integer_from_literal() {
        let var1 = Integer::from(1i8);
        let var2 = Integer::from(10u8);

        assert!(var1.inner.to_u8().expect("Failed to convert BigInt to u8") == 1);
        assert!(var2.inner.to_u8().expect("Failed to convert BigInt to u8") == 10);
    }

    #[test]
    /// This test is for checking the implementation of the rational_from_literal_int macro
    /// The macro gives access to the `from` method for Rational types
    fn test_rational_from_literal_int_macro() {
        let a = Rational::from(1i8);
        let b = Rational::from(1u8);
        let c = Rational::from(1i32);
        let d = Rational::from(1i64);
        let e = Rational::from(1i128);
        let f = Rational::from(1isize);
        let g = Rational::from(1f32);

        assert!(a == b && b == c && c == d && d == e && e == f && f == g);
    }

    #[test]
    /// This test is for checking the implementation of the smt_impl macro for tuples
    /// The macro allows the use of the cmp method of the SMT trait on tuples
    fn test_smt_impl_macro() {
        let var1 = (Integer::from(1), Integer::from(2));
        let var2 = (Integer::from(1), Integer::from(2));
        let var3 = (Integer::from(1), Integer::from(3));
        let var4 = (Integer::from(10), Integer::from(1));

        let res1 = var1._cmp(var2); // (1, 2) == (1, 2)
        let res2 = var1._cmp(var3); // (1, 2) < (1, 3)
        let res3 = var3._cmp(var2); // (1, 3) > (1, 2)
        let res4 = var1._cmp(var4); // (1, 2) < (10, 1)
        let res5 = var4._cmp(var3); // (10, 1) > (1, 3)

        assert_eq!(res1, Ordering::Equal);
        assert_eq!(res2, Ordering::Less);
        assert_eq!(res3, Ordering::Greater);
        assert_eq!(res4, Ordering::Less);
        assert_eq!(res5, Ordering::Greater);
    }

    /// This tests the cloak! macro.
    /// it basically initializes a Cloak type with the given value
    #[test]
    fn test_cloak_macro() {
        let var1 = cloak!(Integer::from(3));

        let var2 = Cloak::shield(Integer::from(3));
        let var3 = Cloak::shield(Integer::from(0));

        assert_eq!(var1, var2); // 3 == 3
        assert_ne!(var1, var3); // 3 != 0
    }

    /// This tests the seq! macro
    /// it basically initializes a Seq type with the given values
    #[test]
    fn test_seq_macro() {
        let var1 = seq![Integer::from(1), Integer::from(0)];

        let mut var2 = Seq::new();
        var2 = var2.append(Integer::from(1));
        var2 = var2.append(Integer::from(0));

        assert_eq!(var1, var2); // [1, 0] == [1, 0]
    }

    /// This tests the set! macro
    #[test]
    fn test_set_macro() {
        let var1 = set![Integer::from(1), Integer::from(0)];

        let mut var2 = Set::new();
        var2 = var2.insert(Integer::from(0));
        var2 = var2.insert(Integer::from(1));

        assert_eq!(var1, var2); // {0, 1} == {0, 1}
    }

    /// This tests the map! macro
    #[test]
    fn test_map_macro() {
        let var1 = map![
            (Integer::from(1), Text::from("Value 1")),
            (Integer::from(2), Text::from("Value 2"))
        ];

        let mut var2 = Map::new();
        var2 = var2.put_unchecked(Integer::from(1), Text::from("Value 1"));
        var2 = var2.put_unchecked(Integer::from(2), Text::from("Value 2"));

        assert_eq!(var1, var2); // {1: "Value 1", 2: "Value 2"} == {1: "Value 1", 2: "Value 2"}
    }

    /// testing the three methods (_cmp, eq, ne) of the SMT trait
    #[test]
    fn test_smt_1() {
        let var1 = Integer::from(1);
        let var2 = Integer::from(3);

        assert!(var1._cmp(var2) == Ordering::Less);
        assert!(var1.eq(var2) == false.into());
        assert!(var1.ne(var2) == true.into());
    }

    #[test]
    fn test_smt_2() {
        let var1 = Text::from("a");
        let mut var2 = Text::from("b");

        assert!(var1._cmp(var2) == Ordering::Less);
        assert!(var1.eq(var2) == false.into());
        assert!(var1.ne(var2) == true.into());

        var2 = var1; // after this var1 and var2 should be equal
        assert!(var1.eq(var2) == true.into());
    }

    /// testing the deref function of boolean
    #[test]
    fn test_deref_boolean() {
        let var1 = Boolean::from(true);

        assert!(*var1);
    }
    #[test]
    /// testing the from function on Boolean
    fn test_from_boolean() {
        let var1 = Boolean::from(false);

        assert_eq!(var1, Boolean { inner: false });
    }
    /// testing the boolean operators
    #[test]
    fn test_not_boolean() {
        let mut var1 = Boolean::from(true);
        assert!(*var1);

        var1 = var1.not();
        assert!(!(*var1));
    }
    #[test]
    /// it is true only if both are true
    fn test_and_boolean() {
        let var1 = Boolean::from(true);
        let var2 = Boolean::from(false);

        assert!(!(*(var1.and(var2)))); // true && false = false
        assert!(!(*(var2.and(var2)))); // false && false = false
        assert!(*(var1.and(var1))); // true && true = true
    }
    #[test]
    /// it is true if at least one is true
    fn test_or_boolean() {
        let var1 = Boolean::from(true);
        let var2 = Boolean::from(false);

        assert!(*(var1.or(var2))); // true || false = true
        assert!(!(*(var2.or(var2)))); // false || false = false
        assert!(*(var1.or(var1))); // true || true = true
    }
    #[test]
    /// it is true if the two operands do not match
    fn test_xor_boolean() {
        let var1 = Boolean::from(true);
        let var2 = Boolean::from(false);

        assert!(*var1.xor(var2)); // true xor false = true
        assert!(!(*var2.xor(var2))); // false xor false = false
        assert!(!(*var1.xor(var1))); // true xor true = false
    }
    #[test]
    /// it is true unless a -> b where a is true and b is false
    fn test_implies_boolean() {
        let var1 = Boolean::from(true);
        let var2 = Boolean::from(false);

        assert!(!(*var1.implies(var2))); // true -> false = false
        assert!(*var1.implies(var1)); // true -> true = true
        assert!(*var2.implies(var1)); // false -> true = true
        assert!(*var2.implies(var2)); // false -> false = true
    }

    /// testing the into_rational method on integers
    // #[test]
    // fn test_into_rational_integer() {
    //     let var1 = Integer::from(1);
    //     let var2 = var1.into_rational();

    //     // If this passes, var2 is of type Rational
    //     let _: Rational = var2;
    //     assert_eq!(var1.eq(var2), true.into()); // 1 == 1
    // }

    /// testing the cmp method for comparison between rationals and integers
    // #[test]
    // fn test_cmp_integer_rational() {
    //     let var1 = Integer::from(1);
    //     let var2 = Rational::from(1.2);
    //     let var3 = Rational::from(1);
    //     let var4 = Rational::from(0.8);

    //     assert_eq!(var1._cmp(var1), Ordering::Equal); // 1 == 1
    //     assert_eq!(var1._cmp(var2), Ordering::Less); // 1 < 1.2
    //     assert_eq!(var3._cmp(var1), Ordering::Equal); // 1 == 1
    //     assert_eq!(var1._cmp(var4), Ordering::Greater); // 1 > 0.8
    // }
    /// testing the eq method for integers and rational
    // #[test]
    // fn test_eq_integer() {
    //     let var1 = Integer::from(1);
    //     let var2 = Rational::from(1.2);
    //     let var3 = Rational::from(1);
    //     let var4 = Rational::from(0.8);

    //     assert_eq!(var1.eq(var1), true.into()); // 1 == 1
    //     assert_eq!(var1.eq(var2), false.into()); // 1 != 1.2
    //     assert_eq!(var3.eq(var1), true.into()); // 1 == 1
    //     assert_eq!(var1.eq(var4), false.into()); // 1 != 0.8
    // }

    /// testing the ne method for integers and rational
    // #[test]
    // fn test_ne_integer() {
    //     let var1 = Integer::from(1);
    //     let var2 = Rational::from(1.2);
    //     let var3 = Rational::from(1);
    //     let var4 = Rational::from(0.8);

    //     assert_eq!(var1.ne(var1), false.into()); // 1 == 1
    //     assert_eq!(var1.ne(var2), true.into()); // 1 != 1.2
    //     assert_eq!(var3.ne(var1), false.into()); // 1 == 1
    //     assert_eq!(var4.ne(var1), true.into()); // 1 != 0.8
    // }

    /// testing the from method of Rational
    #[test]
    fn test_from_rational() {
        let var1 = Rational::from(1.5f32);
        let var2 = Rational::from(1.6f64);

        assert_eq!(
            var1,
            Rational {
                inner: Intern::new(BigRational::from_float(1.5).expect("Failed to convert float to BigRational"))
            }
        );
        assert_eq!(
            var2,
            Rational {
                inner: Intern::new(BigRational::from_float(1.6).expect("Failed to convert float to BigRational"))
            }
        );
    }
    /// testing the into_rational method of rational
    // #[test]
    // fn test_into_rational_rational() {
    //     let var1 = Rational::from(1.4);
    //     let var2 = var1.into_rational();

    //     // If this passes, var2 is of type Rational
    //     let _: Rational = var2;
    //     assert!(true);
    // }

    /// testing the ne/eq/_cmp methods of rational
    #[test]
    fn test_cmp_rational() {
        let var1 = Rational::from(1);
        let var2 = Rational::from(243.3);

        assert_eq!(var1._cmp(var2), Ordering::Less);
    }
    #[test]
    fn test_eq_rational() {
        let var1 = Rational::from(1);
        let var2 = Rational::from(243.3);

        assert_eq!(var1.eq(var2), false.into());
    }
    #[test]
    fn test_ne_rational() {
        let var1 = Rational::from(1);
        let var2 = Rational::from(243.3);

        assert_eq!(var1.ne(var2), true.into());
    }

    #[test]
    /// testing the from method of Text
    fn test_from_text() {
        let var1 = Text::from("value");
        assert_eq!(
            var1,
            Text {
                inner: Intern::new(String::from("value"))
            }
        );
    }

    #[test]
    /// testing the new method of Error
    fn test_error() {
        let var1 = Error::fresh();
        let var2 = Error::fresh();

        // each newly created error only has one element.
        assert_eq!(var1.inner.len(), 1);
        assert_eq!(var2.inner.len(), 1);

        // the first one is created with a value of zero and each new one is incremented by one.
        assert_eq!(*var1.inner.iter().next().expect("Error is empty"), 0);
        assert_eq!(*var2.inner.iter().next().expect("Error is empty"), 1);

        // in merging, the elements are included in one set, thus {0,1} is inside var3.
        let var3 = var1.merge(var2);
        assert_eq!(var3.inner.len(), 2);

        // the first element is 0 and the second element is 1.
        assert_eq!(*var3.inner.iter().next().expect("Error is empty"), 0);
        assert_eq!(*var3.inner.iter().nth(1).expect("Error does not have 2 elements"), 1);

        // in merging var3 {0,1} and var1 {0} because the inner value is a set the values are not duplicated.
        let var4 = var3.merge(var1);
        assert_eq!(var4.inner.len(), 2);
    }

    #[test]
    /// testing the eq method for SMTWrap
    fn test_eq_smtwrap() {
        let var1 = SMTWrap(Integer::from(1));
        let var2 = SMTWrap(Integer::from(15));

        assert!(!var1.eq(&var2));
        assert!(var1.ne(&var2));
    }

    #[test]
    /// testing the partial_cmp for smt wrap
    fn test_partial_cmp_smtwrap() {
        let var1 = SMTWrap(cloak!(Integer::from(1)));
        let var2 = SMTWrap(cloak!(Integer::from(10)));

        assert_eq!(var2.partial_cmp(&var1).expect("Failed to compare"), Ordering::Greater);
    }

    #[test]
    /// testing the _cmp for smt wrap
    fn test_cmp_smtwrap() {
        let var1 = SMTWrap(cloak!(Integer::from(1)));
        let var2 = SMTWrap(cloak!(Integer::from(10)));

        assert_eq!(var2.cmp(&var1), Ordering::Greater);
    }

    #[test]
    /// Adding SMtWrap to a HashSet
    fn test_hash_smtwrap() {
        let var1 = SMTWrap(Integer::from(1));
        let var2 = SMTWrap(Integer::from(2));
        let var3 = SMTWrap(Integer::from(1));
        let var4 = SMTWrap(Integer::from(2));

        let mut set = std::collections::HashSet::new();
        set.insert(var1);
        set.insert(var2);
        set.insert(var3);
        set.insert(var4);

        assert_eq!(set.len(), 2);
    }

    #[test]
    /// This test is for checking the implementation of the shield and reveal methods on the Cloak type.
    fn test_shield_reveal_cloak() {
        let var1 = Cloak::shield(Integer::from(1));
        let var2 = var1.reveal();
        assert_eq!(var2, 1.into());
    }

    #[test]
    /// These tests the implementation of the Seq type
    /// A new sequence has an initial length of 0
    fn test_new_seq() {
        let seq: Seq<Integer> = Seq::new();
        assert_eq!(seq.length(), Integer::from(0));
    }

    #[test]
    /// The length of the sequence is 3 after appending 3 elements (duplicate elements are allowed)
    fn test_append_seq() {
        let seq = Seq::new();
        let seq = seq.append(Integer::from(1));
        let seq = seq.append(Integer::from(2));
        let seq = seq.append(Integer::from(1));
        assert_eq!(seq.length(), Integer::from(3));
    }

    #[test]
    /// The value at index 1 is 2
    fn test_at_unchecked_seq() {
        let seq = Seq::new();
        let seq = seq.append(Integer::from(1));
        let seq = seq.append(Integer::from(2));
        let seq = seq.append(Integer::from(3));
        assert_eq!(seq.at_unchecked(Integer::from(1)), Integer::from(2));
    }

    #[test]
    #[should_panic]
    /// This test checks that the at_unchecked method panics when the index is out of bounds
    fn test_at_unchecked_seq_out_of_bounds() {
        let seq = Seq::new();
        let seq = seq.append(Integer::from(1));
        let seq = seq.append(Integer::from(2));
        let seq = seq.append(Integer::from(3));
        seq.at_unchecked(Integer::from(3));
    }

    #[test]
    /// The sequence includes the value 2
    /// The sequence does not include the value 4
    fn test_includes_seq() {
        let seq = Seq::new();
        let seq = seq.append(Integer::from(1));
        let seq = seq.append(Integer::from(2));
        let seq = seq.append(Integer::from(3));
        assert_eq!(seq.includes(Integer::from(2)), Boolean::from(true));
        assert_eq!(seq.includes(Integer::from(4)), Boolean::from(false));
    }

    #[test]
    /// The iterator returns the values 0, 1, 2
    fn test_iterator_seq() {
        let seq = Seq::new();
        let seq = seq.append(Text::from("one"));
        let seq = seq.append(Text::from("two"));
        let seq = seq.append(Text::from("three"));
        assert_eq!(
            seq.iterator(),
            vec![Integer::from(0), Integer::from(1), Integer::from(2)]
        );
    }

    #[test]
    /// This tests the new method for the Set type
    /// The new method is used to create a new empty set.
    fn test_new_set() {
        let set = Set::<Integer>::new();
        assert_eq!(set.length(), 0.into());
    }

    #[test]
    /// This tests the length method for the Set type
    fn test_length_set() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(2));
        assert_eq!(set.length(), 2.into());
    }

    #[test]
    /// The set does not contain duplicates
    fn test_length_dup_set() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(1));
        assert_eq!(set.length(), 1.into());
    }

    #[test]
    /// After removing the element, the length of the set should decrease by 1
    fn test_remove_set() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(2));
        assert_eq!(set.length(), 2.into());
        let set = set.remove(Integer::from(1));
        assert_eq!(set.length(), 1.into());
    }

    #[test]
    /// After removing a non-existent element, the length of the set should not change
    fn test_remove_set2() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(2));
        assert_eq!(set.length(), 2.into());
        let set = set.remove(Integer::from(10));
        assert_eq!(set.length(), 2.into());
    }

    #[test]
    /// The set should contain the element that was inserted
    fn test_contains_set() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(2));
        assert_eq!(set.contains(Integer::from(1)), true.into());
        assert_eq!(set.contains(Integer::from(20)), false.into());
    }

    #[test]
    /// when the element is removed, the set should not contain the element anymore
    fn test_contains_set2() {
        let set = Set::new();
        let set = set.insert(Integer::from(1));
        let set = set.insert(Integer::from(2));
        assert_eq!(set.contains(Integer::from(1)), true.into());
        let set = set.remove(Integer::from(1));
        assert_eq!(set.contains(Integer::from(1)), false.into());
    }

    #[test]
    /// The iterator should return the elements in the set
    fn test_iterator_set() {
        let set = Set::new();
        let set = set.insert(Text::from("one"));
        let set = set.insert(Text::from("two"));
        let set = set.insert(Text::from("three"));

        let iter = set.iterator();
        assert_eq!(iter.len(), 3);
        assert_eq!(iter[0], Text::from("one"));
        assert_eq!(iter[1], Text::from("three")); // it is in lexicographical order
        assert_eq!(iter[2], Text::from("two"));

    }

    #[test]
    /// A new map is created and the length of the map should be 0
    fn test_map_length() {
        let map: Map<Integer, Integer> = Map::new();
        assert_eq!(map.length(), 0.into());
    }

    #[test]
    /// When adding a key-value pair to the map, the length of the map should increase by 1
    fn test_map_put() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        assert_eq!(map.length(), 1.into());
    }

    #[test]
    /// When adding a key-value pair to the map, the key should exist in the map
    /// and the value should be the same as the one that was added
    fn test_map_get() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        assert_eq!(map.get_unchecked(Integer::from(1)), Integer::from(2));
    }

    #[test]
    /// When deleting an existent key from the map, the length of the map should decrease by 1
    fn test_map_del() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        let map = map.del_unchecked(Integer::from(1));
        assert_eq!(map.length(), 0.into());
    }

    #[test]
    /// Deleting a key that does not exist in the map should not change the map.
    fn test_map_del_non_existent() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        assert_eq!(map.length(), 1.into());
        let map = map.del_unchecked(Integer::from(2));
        assert_eq!(map.length(), 1.into());
    }

    #[test]
    #[should_panic]
    /// getting an element that does not exist in the map should panic
    fn test_map_get_non_existent() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        map.get_unchecked(Integer::from(2));
    }

    #[test]
    /// checking if a key exists in the map
    /// a key should only exist if it was added to the map
    fn test_map_contains_key() {
        let map = Map::new().put_unchecked(Integer::from(1), Integer::from(2));
        assert_eq!(map.contains_key(Integer::from(1)), true.into());
        assert_eq!(map.contains_key(Integer::from(2)), false.into());
    }

    #[test]
    /// getting an iterator over the keys of the map
    fn test_map_iterator() {
        let map = Map::new().put_unchecked(Text::from("one"), Integer::from(1));
        assert_eq!(map.iterator(), vec![Text::from("one")]);
    }

    #[test]
    /// the default value of Integer is 0
    fn test_default_integer() {
        let var1 = Integer::default();
        let var2 = Integer::from(0);
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Boolean is false
    fn test_default_boolean() {
        let var1 = Boolean::default();
        let var2 = Boolean::from(false);
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Rational is numerator 0 and denominator 1
    fn test_default_rational() {
        let var1 = Rational::default();
        let var2 = Rational::from(0);
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Text is an empty string
    fn test_default_text() {
        let var1 = Text::default();
        let var2 = Text::from("");
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Error is an empty set
    fn test_default_error() {
        let var1 = Error::default();
        let var2 = Error { inner: Intern::new(BTreeSet::new()) };
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Cloak is the default value of the inner type
    fn test_default_cloak() {
        let var1 = Cloak::default();
        let var2 = Cloak::shield(Integer::default());
        assert_eq!(var1, var2);
    }

    #[test]
    /// the default value of Seq is an empty sequence
    fn test_default_seq() {
        let var1 = Seq::<Integer>::default();
        let var2 = Seq { inner: Intern::new(Vec::new()) };
        assert_eq!(var1, var2);
        assert!(*var1.is_empty());
    }

    #[test]
    /// the default value of SMT Set is an empty set
    fn test_default_set() {
        let var1 = Set::<Integer>::default();
        let var2 = Set { inner: Intern::new(BTreeSet::new()) };
        assert_eq!(var1, var2);
        assert!(*var1.is_empty());
    }

    #[test]
    /// the default value of SMT Map is an empty map
    fn test_default_map() {
        let var1 = Map::<Integer, Integer>::default();
        let var2 = Map { inner: Intern::new(BTreeMap::<SMTWrap<Integer>, SMTWrap<Integer>>::new()) };
        assert_eq!(var1, var2);
        assert!(*var1.is_empty());
    }  
}