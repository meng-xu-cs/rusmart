//! stdlib
//! 
//! This is Rusmart standard library (stdlib) that contains language constructs that cannot be expressed readily in Rust as they have special semantics in SMT.
//! 
//! The library crate contains the following modules:
//! 
//! * dt - SMT-related data types
//! * exp - SMT-related expressions


/// SMT-related data types
mod dt;
/// SMT-related expressions
mod exp;

/// Re-export SMT-related data types
/// This allows users to call `rusmart_stdlib::Boolean` instead of `rusmart_stdlib::dt::Boolean`
pub use dt::*;
/// Re-export SMT-related expressions
/// This allows users to call `rusmart_stdlib::forall` instead of `rusmart_stdlib::exp::forall`
pub use exp::*;