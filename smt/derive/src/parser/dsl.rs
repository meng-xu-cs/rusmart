use crate::parser::name::ReservedIdent;

/// Reserved macro name
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum SysMacroName {
    Exists,
    Forall,
}

impl ReservedIdent for SysMacroName {
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident.to_string().as_str() {
            "exists" => Self::Exists,
            "forall" => Self::Forall,
            _ => return None,
        };
        Some(matched)
    }
}
