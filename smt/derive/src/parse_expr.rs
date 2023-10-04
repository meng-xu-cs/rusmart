use crate::parse_ctxt::{FuncName, TypeName};
use crate::parse_func::FuncSig;
use crate::parse_type::TypeDef;

/// A context suitable for expression analysis
pub trait CtxtForExpr {
    /// Retrieve the type definition
    fn get_type(&self, name: &TypeName) -> Option<&TypeDef>;

    /// Retrieve the function signature for impl
    fn get_impl_sig(&self, name: &FuncName) -> Option<&FuncSig>;

    /// Retrieve the function signature for spec
    fn get_spec_sig(&self, name: &FuncName) -> Option<&FuncSig>;
}
