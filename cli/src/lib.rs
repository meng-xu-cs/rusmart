//! # package rusmart_cli 
//! ## library crate: This crate provides the module tree system for the CLI.
//! 
//! The following modules are available in this crate:
//! 
//! * git - a Git module
//! * dep - a general dependency module
//! * dep_cvc5 - a CVC5 dependency module
//! * dep_z3 - a Z3 dependency module
//! * cli - a CLI module
//! 
//! Only the CLI module is public, as it is used in the binary crate to define the executable entry point.
//! // this macro is used to put warnings when the documentation is missing.

// this macro checks the missing documentation in all the `public` modules in the mdoule tree of the library crate. In this case, the only public module in the module tree is `cli`.
#![deny(missing_docs)]

/// The module tree. These modules can be accessed from within the library crate.
mod git;
mod dep;
mod dep_cvc5;
mod dep_z3;

/// The CLI module. This module can be accessed from the binary crate (and other external crates).
pub mod cli;
