use crate::dt::{Boolean, SMT};

pub struct Quantified;

impl Quantified {
    /// Annotation for the forall operator
    pub fn forall<T: SMT, F: FnOnce(T) -> Boolean>(f: F) -> Boolean {
        f(T::default())
    }

    /// Annotation for the exists operator
    pub fn exists<T: SMT, F: FnOnce(T) -> Boolean>(f: F) -> Boolean {
        f(T::default())
    }
}
