//! Standard library for SMT expressions in Rusmart
//!
//! This module provides the following operators:
//!
//! * `forall` operator
//! * `exists` operator
//! * `choose` operator
//!

//---------------------------------------DEPENDENCIES----------------------------------------------------//

pub use itertools::iproduct;

//----------------------------------------MACROS---------------------------------------------------------//

/// Operator: forall
/// There are two patterns:
/// 1) This is used in the format of forall!(|x:Integer, y:Integer| x.eq(y))
/// So basically it initializes the variables (in this case x,y) with their default values (0 for int)
/// 2) This is used in the format of forall!(x in m, y in n => x.gt(y))
/// Here m and n are values that have the iterator() method. For example:
/// let m = set!(Integer::from(1), Integer::from(2))
/// let n = set!(Integer::from(10), Integer::from(20))
/// The iproduct! macro creates a cartesian product out of the result of m.iterator() and n.iterator():
/// The result will be: (Integer::from(1), Integer::from(10)), (Integer::from(1), Integer::from(20)), (Integer::from(2), Integer::from(10)), (Integer::from(2), Integer::from(20))
/// The forall macro then iterates over this product and applies the constraint (here x.gt(y)) to each pair of values.
/// If the constraint is true for all pairs, the result is true, otherwise false.
#[macro_export]
macro_rules! forall {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $constraint:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| -> $crate::Boolean {
            $constraint
        })(<$t0>::default() $(, <$tn>::default())*)
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        (|| -> $crate::Boolean {
            // let iterators = $crate::iproduct!($c0.iterator() $(, $cn.iterator())*);
            // // consume the iterators
            // dbg!(iterators.collect::<Vec<_>>()); to debug
            $crate::Boolean::from(
                $crate::iproduct!($c0.iterator() $(, $cn.iterator())*).all(
                    |($v0, $($vn, )*)| *$constraint
                )
            )
        })()
    };
}

/// Operator: exists
/// The same as forall with the difference that the any() method is used instead of all()
/// Meaning that the result is true if the constraint is true for at least one pair of values
#[macro_export]
macro_rules! exists {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $constraint:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| -> $crate::Boolean {
            $constraint
        })(<$t0>::default() $(, <$tn>::default())*)
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        $crate::Boolean::from(
            $crate::exp::iproduct!($c0.iterator() $(, $cn.iterator())*).any(
                |($v0, $($vn, )*)| *$constraint
            )
        )
    };
}

/// Operator: choose
/// If there exists a valid set of values that satisfy the constraint, the values are returned
/// In the first pattern, the default values are returned if the constraint is satisfied
/// In the second pattern, the cartesian product of the iterators is created and the constraint is applied to each pair of values
/// The first pair of values that satisfies the constraint is returned
/// If no pair of values satisfies the constraint, a panic is thrown
#[macro_export]
macro_rules! choose {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $constraint:expr) => {
        if *$crate::exists!(|$v0 : $t0 $(, $vn : $tn)*| $constraint) {
            (<$t0>::default() $(, <$tn>::default())*)
        } else {
            panic!("no valid choice");
        }
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        (|| {
            for ($v0, $($vn, )*) in $crate::iproduct!($c0.iterator() $(, $cn.iterator())*) {
                if *$constraint {
                    return ($v0 $(, $vn)*);
                }
            }
            panic!("no valid choice");
        }) ()
    };
}

//----------------------------------------TESTS-----------------------------------------------------------//

#[cfg(test)]
mod test {
    use crate::{dt::*, map, set};

    #[test]
    /// testing the first pattern of the forall macro.
    /// All integers are by default zero so var1 = 0.into(); and var2 = 0.into()
    fn test_pattern_one_forall_one() {
        let v = forall!(|var1: Integer, var2: Integer| var1.eq(var2));
        assert!(*v);
    }

    #[test]
    /// By default a bool is set to false
    fn test_pattern_one_forall_two() {
        let v = forall!(|var1: Boolean| var1);
        assert!(!*v)
    }

    #[test]
    /// the cartesian product will be (1, 10), (2, 10)
    /// 1 < 10 and 2 < 10 so the result should be true
    /// Note that the iterator() method on map returns a list of keys
    /// and the iterator() method on set returns a list of its elements
    fn test_pattern_two_forall_one() {
        let m = map!(
            (Integer::from(1), Text::from("one")),
            (Integer::from(2), Text::from("two"))
        );
        let s = set!(Integer::from(10));

        let v = forall!(var1 in m, var2 in s => var1.lt(var2));
        assert!(*v);
    }

    #[test]
    /// The functionality of the first pattern of the exists macro is similar to the forall macro
    /// The only difference is the use cases.
    fn test_pattern_one_exists() {
        let v = exists!(|var1: Integer, var2: Integer| var1.eq(var2));
        assert!(*v);
    }

    #[test]
    /// the output of the cartesian product will be (1, 10), (20, 10)
    /// 1 < 10 so the result should be true because at least one pair of values satisfies the constraint
    fn test_pattern_two_exists() {
        let m = map!(
            (Integer::from(1), Text::from("one")),
            (Integer::from(20), Text::from("twenty"))
        );
        let s = set!(Integer::from(10));

        let v = exists!(var1 in m, var2 in s => var1.lt(var2));
        assert!(*v);
    }

    #[test]
    /// the output of the cartesian product will be (1, 10), (20, 10)
    /// 20 < 10 so the result should be false because not all pairs of values satisfy the constraint
    fn test_pattern_two_forall_two() {
        let m = map!(
            (Integer::from(1), Text::from("one")),
            (Integer::from(20), Text::from("twenty"))
        );
        let s = set!(Integer::from(10));

        let v = forall!(var1 in m, var2 in s => var1.lt(var2));
        assert!(!*v);
    }

    #[test]
    fn test_pattern_one_choose() {
        let v = choose!(|var1: Integer, var2: Integer| var1.eq(var2));
        assert_eq!(v, (Integer::from(0), Integer::from(0)));
    }

    #[test]
    #[should_panic(expected = "no valid choice")]
    fn test_pattern_one_choose_panic() {
        let _ = choose!(|var1: Integer, var2: Integer| var1.gt(var2));
    }

    #[test]
    /// the output of the cartesian product will be (1, 10), (20, 10)
    /// 1 < 10 so the result should be (1, 10) because the first pair of values that satisfies the constraint is returned
    fn test_pattern_two_choose() {
        let m = map!(
            (Integer::from(1), Text::from("one")),
            (Integer::from(20), Text::from("twenty"))
        );
        let s = set!(Integer::from(10));

        let v = choose!(var1 in m, var2 in s => var1.lt(var2));
        assert_eq!(v, (Integer::from(1), Integer::from(10)));
    }

    #[test]
    #[should_panic(expected = "no valid choice")]
    /// the output of the cartesian product will be (10, 10), (20, 10)
    /// no pair of values satisfies the constraint so a panic is thrown because nothing is returned
    fn test_pattern_two_choose_panic() {
        let m = map!(
            (Integer::from(10), Text::from("ten")),
            (Integer::from(20), Text::from("twenty"))
        );
        let s = set!(Integer::from(10));

        let _ = choose!(var1 in m, var2 in s => var1.lt(var2));
    }

    #[test]
    /// The forall and choose macros have been combined to set the minimum value of a set in the eval.rs file
    /// fn set_min(set: Set<Value>) -> Value {
    ///     choose!(v in set => forall!(e in set => v.eq(e).or(v.lt(e))))
    /// }
    fn test_set_min() {
        let s = set!(Integer::from(1), Integer::from(2), Integer::from(3));
        let v = choose!(v in s => forall!(e in s => v.eq(e).or(v.lt(e))));
        assert_eq!(v, Integer::from(1));
    }

    #[test]
    /// The forall and choose macros have been combined to set the minimum key of a map in the eval.rs file
    /// fn map_key_min(map: Map<Value, Value>) -> Value {
    ///    choose!(v in map => forall!(e in map => v.eq(e).or(v.lt(e))))
    /// }
    fn test_map_key_min() {
        let m = map!(
            (Integer::from(1), Text::from("one")),
            (Integer::from(2), Text::from("two")),
            (Integer::from(3), Text::from("three"))
        );
        let v = choose!(v in m => forall!(e in m => v.eq(e).or(v.lt(e))));
        assert_eq!(v, Integer::from(1));
    }
}

