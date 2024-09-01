//! stdlib
//! 
//! This is Rusmart standard library (short for stdlib) that contains language constructs that cannot be expressed readily in Rust as they have special semantics in SMT.
//! 
//! It provides the following modules:
//! 
//! * dt - SMT-related data types
//! * exp - SMT-related expressions


/// SMT-related data types
pub mod dt;

/// Re-export SMT-related data types
/// This allows users to call `rusmart_stdlib::Boolean` instead of `rusmart_stdlib::dt::Boolean`
pub use self::dt::*;

/// SMT-related expressions
pub mod exp;

/// Re-export SMT-related expressions
/// This allows users to call `rusmart_stdlib::forall` instead of `rusmart_stdlib::exp::forall`
pub use self::exp::*;