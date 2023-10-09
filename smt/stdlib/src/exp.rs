/// Annotation for the forall operator
#[macro_export]
macro_rules! forall {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $e:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| $e)(<$t0>::default() $(, <$tn>::default())*)
    };
}
pub use forall;

/// Annotation for the exists operator
#[macro_export]
macro_rules! exists {
    (|$v0:ident : $t0:ty $(, $vn:ident : $tn:ty)* $(,)?| $e:expr) => {
        (|$v0 : $t0 $(, $vn : $tn)*| $e)(<$t0>::default() $(, <$tn>::default())*)
    };
}
pub use exists;
