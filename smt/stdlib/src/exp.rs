pub use itertools::iproduct;

/// Operator: forall
#[macro_export]
macro_rules! forall {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $constraint:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| $constraint)(<$t0>::default() $(, <$tn>::default())*)
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        $crate::dt::Boolean::from(
            $crate::exp::iproduct!($c0.iterator() $(, $cn.iterator())*).all(
                |($v0 $(, $vn)*)| *$constraint
            )
        )
    };
}

/// Operator: exists
#[macro_export]
macro_rules! exists {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $e:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| $e)(<$t0>::default() $(, <$tn>::default())*)
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        $crate::dt::Boolean::from(
            $crate::exp::iproduct!($c0.iterator() $(, $cn.iterator())*).any(
                |($v0 $(, $vn)*)| *$constraint
            )
        )
    };
}

/// Operator: choose
#[macro_export]
macro_rules! choose {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $constraint:expr) => {
        let ($v0 $(, $vn)*) = if *$crate::exists!(|$v0 : $t0 $(, $vn : $tn)*| $constraint) {
            (<$t0>::default() $(, <$tn>::default())*)
        } else {
            panic!("no valid choice");
        };
    };
    ($v0:ident in $c0:expr $(, $vn:ident in $cn:expr)* => $constraint:expr) => {
        let ($v0 $(, $vn)*) = (|| {
            for ($v0 $(, $vn)*) in $crate::exp::iproduct!($c0.iterator() $(, $cn.iterator())*) {
                if *$constraint {
                    return ($v0 $(, $vn)*);
                }
            }
            panic!("no valid choice");
        }) ();
    };
}
