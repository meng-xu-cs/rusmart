use crate::backend::codegen::CodeGen;
use crate::backend::cvc5::common::CodeGenCVC5;
use crate::backend::cvc5::engine_smt::BackendCVC5SMT;
use crate::backend::z3::common::CodeGenZ3;
use crate::backend::z3::engine_chc::BackendZ3CHC;

pub mod error;

mod codegen;
pub mod exec;

mod cvc5;
mod z3;

/// Available list of backend solvers
pub fn solvers() -> Vec<Box<dyn CodeGen>> {
    vec![
        Box::new(CodeGenZ3::new(BackendZ3CHC::new())),
        Box::new(CodeGenCVC5::new(BackendCVC5SMT::new())),
    ]
}
