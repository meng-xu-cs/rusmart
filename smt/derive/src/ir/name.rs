/// Utility macro to define a name
macro_rules! name {
    ($(#[$meta:meta])* $name:ident : $parent:ty) => {
        $(#[$meta])*
        #[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
        pub struct $name {
            ident: String,
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                &self.ident
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.ident)
            }
        }

        impl From<$parent> for $name {
            fn from(name: $parent) -> Self {
                Self {
                    ident: name.to_string(),
                }
            }
        }

        impl From<&$parent> for $name {
            fn from(name: &$parent) -> Self {
                Self {
                    ident: name.to_string(),
                }
            }
        }
    };
}

name! {
    /// Name of a type parameter that implements the SMT trait
    SmtSortName
        : crate::parser::name::TypeParamName
}

name! {
    /// Name of a user-defined sort
    UsrSortName
        : crate::parser::name::UsrTypeName
}

name! {
    /// Name of a user-defined function
    UsrFunName
        : crate::parser::name::UsrFuncName
}

name! {
    /// Name of a variable
    Symbol
        : crate::parser::name::VarName
}

name! {
    /// Name of an axiom
    UsrAxiomName
        : crate::parser::name::AxiomName
}
