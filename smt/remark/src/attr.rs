//! Attribute parsing utilities.
//!
//! This module provides the `parse_dict` function to parse key-value mappings from a token stream.
//! The `parse_dict` function is used inside the `derive_for_func` of the `func` module.
//! The `derive_for_func` function is used inside the `derive_for_impl` and `derive_for_spec` functions of the `func` module.
//! The `derive_for_impl` and `derive_for_spec` functions are used inside the `smt_impl` and `smt_spec` procedural macros of the `lib` module.
//!
//! Use cases of the `parse_dict` function include parsing attributes and annotations in Rust macros. for example:
//!
//! #[my_attr(key1 = value1, key2 = [value2, value3])]
//! fn my_function() {}
//!
//! The (key1 = value1, key2 = [value2, value3]) part is a key-value mapping that can be parsed using the `parse_dict` function.
//!

use crate::{bail_if_missing, bail_on};
use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use std::collections::{BTreeMap, BTreeSet};
use syn::Result;

/// Annotation value: represents a value in a key-value mapping, which can be either a single identifier
/// or a set of identifiers.
///
/// This enum is used to store the value associated with a key when parsing
/// a token stream.
///
/// The `MetaValue::One` variant is used to store a single identifier value.
/// The `MetaValue::Set` variant is used to store a set of identifier values.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MetaValue {
    /// A single identifier value.
    One(Ident),
    /// A set of identifier values.
    Set(BTreeSet<Ident>),
}

/// Parses a key-value mapping from a token stream.
///
/// This function takes a `TokenStream` and attempts to parse it into a
/// `BTreeMap<String, MetaValue>`. The expected format is a series of
/// key-value pairs, where keys are identifiers and values are either
/// identifiers or lists of identifiers enclosed in square brackets.
/// Key-value pairs are separated by commas.
///
/// # Arguments
///
/// * `stream` - A reference to a `TokenStream` containing the key-value pairs.
///
/// # Returns
///
/// * `Result<BTreeMap<String, MetaValue>>` - A map of keys to values, or an error if parsing fails.
///
/// # Errors
///
/// This function will return an error if:
/// - A key or value is missing.
/// - A key or item is duplicated.
/// - The syntax does not match the expected format.
pub fn parse_dict(stream: &TokenStream) -> Result<BTreeMap<String, MetaValue>> {
    let mut store: BTreeMap<String, MetaValue> = BTreeMap::new(); // Stores the parsed key-value pairs
    let mut iter = stream.clone().into_iter(); // Creates an iterator over the token stream
    // this will be a None value if the stream is empty
    let mut cursor = iter.next(); // Current token

    while cursor.is_some() {
        // Extract key
        let token = bail_if_missing!(cursor.as_ref(), stream, "key"); // this will never lead to a compile error because cursor is checked to be Some at the beginning of the loop.
        // Extract the key as an identifier
        // A key must be an identifier for example in #[my_attr(key = value)]
        // #[my_attr(1 = value)] leads to an error
        let key = match token {
            TokenTree::Ident(ident) => ident.to_string(),
            _ => bail_on!(token, "key not an identifier"), // return an error if the token is not an identifier
        };
        // Check for duplicated keys and return an error if found
        // for example #[my_attr(key1 = value1, key1 = value2)] leads to an error
        if store.contains_key(&key) {
            bail_on!(token, "duplicated key");
        }

        // Extract equal sign (the format is key = value)
        // if the next token is empty (None), it will be caught in the next iteration
        // the error message will be "expect =" for the stream.
        let token = bail_if_missing!(iter.next(), stream, "=");
        // Check if the token is an equal sign
        match &token {
            TokenTree::Punct(punct) if punct.as_char() == '=' => (),
            _ => bail_on!(token, "expect ="), // return an error if the token is not an equal sign
        }

        // Extract value (the format is key = value)
        // if the next token is empty (None), it will be caught in the next iteration
        let token = bail_if_missing!(iter.next(), stream, "val");
        // Extract the value as an identifier or a set of identifiers
        let val = match token {
            // Single identifier value
            TokenTree::Ident(ident) => MetaValue::One(ident),
            // Set of identifiers enclosed in brackets key = [value1, value2]
            TokenTree::Group(group) if matches!(group.delimiter(), Delimiter::Bracket) => {
                let mut set = BTreeSet::new(); // Stores the set of identifiers

                let sub = group.stream(); // creates a TokenStream
                let mut sub_iter = sub.into_iter(); // Iterator over the sub-stream
                                                    // sub_cursor will be a None value if we have #[my_attr(key = [])]
                let mut sub_cursor = sub_iter.next(); // Current token in sub-stream
                while sub_cursor.is_some() {
                    // Extract the item. This will never lead to a compile error because sub_cursor is checked to be Some at the beginning of the loop. So it will only unwrap a Some value.
                    let token = bail_if_missing!(sub_cursor.as_ref(), group, "item");
                    // Extract the item as an identifier.
                    let item = match token {
                        TokenTree::Ident(ident) => ident.clone(),
                        _ => bail_on!(token, "item not an identifier"), // return an error if the token is not an identifier for example #[my_attr(key = [1, 2])]
                    };

                    // Check for duplicated items and return an error if found
                    // for example #[my_attr(key = [value1, value1])] leads to an error
                    if !set.insert(item.clone()) {
                        bail_on!(group, "duplicated item");
                    }

                    // Advance the cursor
                    sub_cursor = sub_iter.next();
                    // Skip commas between items
                    if matches!(sub_cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',') {
                        sub_cursor = sub_iter.next();
                    } else if !sub_cursor.is_none() {
                        // Return an error if a comma is missing between items
                        bail_on!(sub_cursor, "expect comma between items");
                    }
                }

                MetaValue::Set(set)
            }
            _ => bail_on!(token, "expect value as identifier or set of identifiers"),
        };

        // Add to the key-value store
        store.insert(key, val);

        // Advance the cursor
        cursor = iter.next();

        // Skip commas between key-value pairs
        if matches!(cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',') {
            cursor = iter.next();
        } else if !cursor.is_none() {
            // Return an error if a comma is missing between key-value pairs
            bail_on!(cursor, "expect comma between key-value pairs");
        }
    }

    // if the stream is empty, return an empty store map
    Ok(store)
}

#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::Span;
    use quote::quote;

    /// Helper function to create an identifier with a given name.
    fn ident(name: &str) -> Ident {
        Ident::new(name, Span::call_site())
    }

    #[test]
    fn test_parse_single_key_value() {
        // Test parsing a single key-value pair: key = value
        let tokens = quote! { key = value };
        let result = parse_dict(&tokens).expect("Failed to parse key-value pair");

        assert_eq!(result.len(), 1);
        match result.get("key") {
            Some(MetaValue::One(id)) => assert_eq!(id, &ident("value")),
            _ => panic!("Expected MetaValue::One with identifier 'value'"),
        }
    }

    #[test]
    fn test_parse_multiple_key_values() {
        // Test parsing multiple key-value pairs: key1 = value1, key2 = value2
        let tokens = quote! { key1 = value1,key2 = value2 };
        let result = parse_dict(&tokens).unwrap();

        assert_eq!(result.len(), 2);
        match result.get("key1") {
            Some(MetaValue::One(id)) => assert_eq!(id, &ident("value1")),
            _ => panic!("Expected MetaValue::One with identifier 'value1'"),
        }
        match result.get("key2") {
            Some(MetaValue::One(id)) => assert_eq!(id, &ident("value2")),
            _ => panic!("Expected MetaValue::One with identifier 'value2'"),
        }
    }

    #[test]
    fn test_parse_key_with_set_value() {
        // Test parsing a key with a set value: key = [value1, value2]
        let tokens = quote! { key = [value1, value2] };
        let result = parse_dict(&tokens).unwrap();

        assert_eq!(result.len(), 1);
        match result.get("key") {
            Some(MetaValue::Set(set)) => {
                let expected: BTreeSet<_> = BTreeSet::from([ident("value1"), ident("value2")]);
                assert_eq!(set, &expected);
            }
            _ => panic!("Expected MetaValue::Set with identifiers 'value1' and 'value2'"),
        }
    }

    #[test]
    fn test_parse_mixed_values() {
        // Test parsing mixed key-value pairs: key1 = value1, key2 = [value2, value3]
        let tokens = quote! { key1 = value1, key2 = [value2, value3] };
        let result = parse_dict(&tokens).unwrap();

        assert_eq!(result.len(), 2);
        // Check key1
        match result.get("key1") {
            Some(MetaValue::One(id)) => assert_eq!(id, &ident("value1")),
            _ => panic!("Expected MetaValue::One with identifier 'value1'"),
        }
        // Check key2
        match result.get("key2") {
            Some(MetaValue::Set(set)) => {
                let expected: BTreeSet<_> = BTreeSet::from([ident("value2"), ident("value3")]);
                assert_eq!(set, &expected);
            }
            _ => panic!("Expected MetaValue::Set with identifiers 'value2' and 'value3'"),
        }
    }

    #[test]
    fn test_parse_empty_input() {
        // Test parsing an empty input
        let tokens = quote! {};
        let result = parse_dict(&tokens).unwrap();

        assert!(result.is_empty()); // store is empty
    }

    #[test]
    fn test_not_a_key() {
        // Test parsing when the key is not an identifier: 1 = value
        // this gets invoked here: let key = match token { TokenTree::Ident(ident) => ident.to_string(), _ => bail_on!(token, "key not an identifier") };
        let tokens = quote! { 1 = value };
        let result = parse_dict(&tokens);

        assert!(result.is_err());
        
        let result = result.unwrap_err().to_string();
        assert_eq!(result, "key not an identifier");
    }

    #[test]
    fn test_parse_duplicate_keys() {
        // Test parsing with duplicate keys: key = value1, key = value2
        // this gets invoked here: if store.contains_key(&key) { bail_on!(token, "duplicated key"); }
        let tokens = quote! { key = value1, key = value2 };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "duplicated key");
    }

    #[test]
    fn test_no_tokens_after_key() {
        // Test parsing when there are no tokens after the key: key
        // this gets invoked here: let token = bail_if_missing!(iter.next(), stream, "=");
        let tokens = quote! { key };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect =");
    }

    #[test]
    fn test_parse_invalid_syntax() {
        // Test parsing invalid syntax: key value1 value2
        // this gets invoked here: bail_on!(token, "expect =")
        let tokens = quote! { key value1 value2 };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect =");
    }

    #[test]
    fn test_parse_missing_equal_sign() {
        // Test parsing when the equal sign is missing: key value
        // this gets invoked here: bail_on!(token, "expect =")
        let tokens = quote! { key value };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect =");
    }

    #[test]
    fn test_parse_missing_value() {
        // Test parsing when the value is missing: key =
        // this gets invoked here: let token = bail_if_missing!(iter.next(), stream, "val");
        let tokens = quote! { key = };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect val");

    }

    #[test]
    fn test_parse_invalid_value() {
        // Test parsing when the value is not an identifier or a set: key = 1
        // This gets invoked here: _ => bail_on!(token, "expect value as identifier or set of identifiers")
        let tokens = quote! { key = 1 };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect value as identifier or set of identifiers");
    }

    #[test]
    fn test_group_not_identifier() {
        // Test parsing when the group item is not an identifier: key = [1, 2]
        // This gets invoked here: _ => bail_on!(token, "item not an identifier")
        let tokens = quote! { key = [1, 2] };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "item not an identifier");
    }

    #[test]
    fn test_parse_duplicate_items_in_set() {
        // Test parsing with duplicate items in set: key = [value1, value1]
        // This gets invoked here: if !set.insert(item.clone()) { bail_on!(group, "duplicated item"); }
        let tokens = quote! { key = [value1, value1] };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "duplicated item");
    }

    #[test]
    fn test_expect_comma_between_items() {
        // Test parsing when a comma is missing between items in a set: key = [value1 value2]
        // This gets invoked here: if matches!(sub_cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() != ',')
        let tokens = quote! { key = [value1 value2] };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect comma between items");
    }

    #[test]
    fn test_expect_comma_between_key_values() {
        // Test parsing when a comma is missing between key-value pairs: key1 = value1 key2 = value2
        // This gets invoked here: if matches!(cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() != ',')
        let tokens = quote! { key1 = value1 key2 = value2 };
        let result = parse_dict(&tokens);

        assert!(result.is_err());

        let result = result.unwrap_err().to_string();
        assert_eq!(result, "expect comma between key-value pairs");
    }
}
