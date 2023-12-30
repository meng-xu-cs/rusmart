/// Utility macro to define an index
macro_rules! index {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
        pub struct $name {
            pub index: usize,
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.index)
            }
        }
    };
}

index! {
    /// A unique identifier for user-defined sort
    UsrSortId
}

index! {
    /// A unique identifier for user-defined function
    UsrFunId
}

index! {
    /// Index of a variable
    VarId
}

index! {
    /// Index of an expression
    ExpId
}

index! {
    /// A unique identifier for an axiom
    UsrAxiomId
}
