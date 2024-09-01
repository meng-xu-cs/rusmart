#![deny(missing_docs)]

//! # Utils
//! 
//! `utils` is a collection of utilities for the other crates in the rustmart project.
//! 
//! It provides the following modules:
//! 
//! * config - a configuration module
//! * lib - a library module


/// Configuration module
/// This module contains all the configuration settings for the application
pub mod config;


/// Re-export config module
/// This allows users to call `rusmart_utils::Mode` instead of `rusmart_utils::config::Mode`
pub use self::config::*;