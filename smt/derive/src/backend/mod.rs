use crate::backend::codegen::CodeGen;
use crate::backend::z3::common::CodeGenZ3;
use crate::backend::z3::engine_chc::BackendZ3CHC;

pub mod error;

mod codegen;

mod cvc5;
mod z3;

/// Available list of backend solvers
pub fn solvers() -> Vec<Box<dyn CodeGen>> {
    vec![Box::new(CodeGenZ3::new(BackendZ3CHC::new()))]
}
