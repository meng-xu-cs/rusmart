use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use syn::Result;

use crate::err::{bail_if_missing, bail_on};

/// Annotation value
pub enum MetaValue {
    One(Ident),
    Set(#[allow(dead_code)] BTreeSet<Ident>), // TODO: use the MetaValue::Set variant
}
 
/// Parse a key-value mapping from a token stream
pub fn parse_dict(stream: &TokenStream) -> Result<BTreeMap<String, MetaValue>> {
    let mut store = BTreeMap::new();

    let mut iter = stream.clone().into_iter();
    let mut cursor = iter.next();

    while cursor.is_some() {
        // extract key
        let token = bail_if_missing!(cursor.as_ref(), stream, "key");
        let key = match token {
            TokenTree::Ident(ident) => ident.to_string(),
            _ => bail_on!(token, "not a key"),
        };
        if store.contains_key(&key) {
            bail_on!(token, "duplicated key");
        }

        // equal sign
        let token = bail_if_missing!(iter.next(), stream, "=");
        match &token {
            TokenTree::Punct(punct) if punct.as_char() == '=' => (),
            _ => bail_on!(token, "expect ="),
        }

        // extract value
        let token = bail_if_missing!(iter.next(), stream, "val");
        let val = match token {
            TokenTree::Ident(ident) => MetaValue::One(ident),
            TokenTree::Group(group) if matches!(group.delimiter(), Delimiter::Bracket) => {
                let mut set = BTreeSet::new();

                let sub = group.stream();
                let mut sub_iter = sub.into_iter();
                let mut sub_cursor = sub_iter.next();
                while sub_cursor.is_some() {
                    // extract the item
                    let token = bail_if_missing!(sub_cursor.as_ref(), group, "item");
                    let item = match token {
                        TokenTree::Ident(ident) => ident.clone(),
                        _ => bail_on!(token, "not an item"),
                    };
                    if !set.insert(item.clone()) {
                        bail_on!(group, "duplicated item");
                    }

                    // advance the cursor
                    sub_cursor = sub_iter.next();
                    if matches!(sub_cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',')
                    {
                        sub_cursor = sub_iter.next();
                    }
                }

                MetaValue::Set(set)
            }
            _ => bail_on!(token, "expect value"),
        };

        // add to the key-value store
        store.insert(key, val);

        // check for more tokens
        cursor = iter.next();
        if matches!(cursor.as_ref(), Some(TokenTree::Punct(punct)) if punct.as_char() == ',') {
            cursor = iter.next();
        }
    }

    Ok(store)
}
