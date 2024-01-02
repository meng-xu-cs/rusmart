/// Utility macro to define a name
macro_rules! name {
    ($(#[$meta:meta])* $name:ident) => {
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
    };

    ($(#[$meta:meta])* $name:ident : $parent:ty) => {
        name!($(#[$meta])* $name);

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
}

impl SmtSortName {
    /// Create an uninterpreted sort for function param
    pub fn new_func_param(
        func: &crate::parser::name::UsrFuncName,
        param: &crate::parser::name::TypeParamName,
    ) -> Self {
        Self {
            ident: format!("{}_{}", func, param),
        }
    }

    /// Create an uninterpreted sort for axiom param
    pub fn new_axiom_param(
        axiom: &crate::parser::name::AxiomName,
        param: &crate::parser::name::TypeParamName,
    ) -> Self {
        Self {
            ident: format!("{}_{}", axiom, param),
        }
    }
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
