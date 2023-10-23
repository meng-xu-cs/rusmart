use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::{Delimiter, Ident, TokenStream, TokenTree};
use syn::{AttrStyle, Attribute, MacroDelimiter, Meta, MetaList, MetaNameValue, Path, Result};

use crate::parser::err::{bail_if_missing, bail_on};
use crate::parser::name::UsrFuncName;

/// Allowed annotation
enum Annotation {
    Type,
    Impl,
    Spec,
}

/// Annotation value
enum MetaValue {
    One(Ident),
    Set(BTreeSet<Ident>),
}

impl Annotation {
    /// Convert a path to an annotation
    pub fn parse_path(path: &Path) -> Option<Self> {
        match path.get_ident()?.to_string().as_str() {
            "smt_type" => Some(Self::Type),
            "smt_impl" => Some(Self::Impl),
            "smt_spec" => Some(Self::Spec),
            _ => None,
        }
    }
}

/// A mark for an annotated impl function
pub struct ImplMark {
    /// whether to derive a receiver-style method for this function
    pub method: Option<UsrFuncName>,
    /// which spec functions this impl should conform to
    pub specs: BTreeSet<UsrFuncName>,
}

/// A mark for an annotated spec function
pub struct SpecMark {
    /// whether to derive a receiver-style method for this function
    pub method: Option<UsrFuncName>,
    /// which impl functions this spec should target at
    pub impls: BTreeSet<UsrFuncName>,
}

/// SMT-related marking
pub enum Mark {
    Type,
    Impl(ImplMark),
    Spec(SpecMark),
}

impl Mark {
    /// Parse a key-value mapping from a token stream
    fn parse_dict(stream: &TokenStream) -> Result<BTreeMap<String, MetaValue>> {
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

    /// Test whether this attribute represents a mark
    fn parse_attr(attr: &Attribute) -> Result<Option<Self>> {
        let Attribute {
            pound_token: _,
            style,
            bracket_token: _,
            meta,
        } = attr;

        // early filtering
        if !matches!(style, AttrStyle::Outer) {
            return Ok(None);
        }

        let mark = match meta {
            Meta::Path(path) => match Annotation::parse_path(path) {
                None => return Ok(None),
                Some(Annotation::Type) => Self::Type,
                Some(Annotation::Impl) => Self::Impl(ImplMark {
                    method: None,
                    specs: BTreeSet::new(),
                }),
                Some(Annotation::Spec) => Self::Spec(SpecMark {
                    method: None,
                    impls: BTreeSet::new(),
                }),
            },
            Meta::List(MetaList {
                path,
                delimiter,
                tokens,
            }) => match Annotation::parse_path(path) {
                None => return Ok(None),
                Some(Annotation::Type) => bail_on!(attr, "unexpected list"),
                Some(Annotation::Impl) => {
                    if !matches!(delimiter, MacroDelimiter::Paren(_)) {
                        bail_on!(attr, "not a parenthesis-enclosed list");
                    }

                    let mut store = Self::parse_dict(tokens)?;
                    let method = match store.remove("method") {
                        None => None,
                        Some(MetaValue::One(ref item)) => Some(item.try_into()?),
                        Some(_) => bail_on!(tokens, "invalid method"),
                    };

                    let mut specs = BTreeSet::new();
                    match store.remove("specs") {
                        None => (),
                        Some(MetaValue::One(ref item)) => {
                            specs.insert(item.try_into()?);
                        }
                        Some(MetaValue::Set(items)) => {
                            for item in items.iter() {
                                specs.insert(UsrFuncName::try_from(item)?);
                            }
                        }
                    };

                    if !store.is_empty() {
                        bail_on!(tokens, "unrecognized entries");
                    }
                    Self::Impl(ImplMark { method, specs })
                }
                Some(Annotation::Spec) => {
                    if !matches!(delimiter, MacroDelimiter::Paren(_)) {
                        bail_on!(attr, "not a parenthesis-enclosed list");
                    }

                    let mut store = Self::parse_dict(tokens)?;
                    let method = match store.remove("method") {
                        None => None,
                        Some(MetaValue::One(ref item)) => Some(item.try_into()?),
                        Some(_) => bail_on!(tokens, "invalid method"),
                    };

                    let mut impls = BTreeSet::new();
                    match store.remove("impls") {
                        None => (),
                        Some(MetaValue::One(ref item)) => {
                            impls.insert(item.try_into()?);
                        }
                        Some(MetaValue::Set(items)) => {
                            for item in items.iter() {
                                impls.insert(UsrFuncName::try_from(item)?);
                            }
                        }
                    };

                    if !store.is_empty() {
                        bail_on!(tokens, "unrecognized entries");
                    }
                    Self::Spec(SpecMark { method, impls })
                }
            },
            Meta::NameValue(MetaNameValue {
                path,
                eq_token: _,
                value: _,
            }) => match Annotation::parse_path(path) {
                None => return Ok(None),
                Some(_) => bail_on!(attr, "unexpected dict"),
            },
        };
        Ok(Some(mark))
    }

    /// Extract a mark, if any, from the list of attributes
    pub fn parse_attrs(attrs: &[Attribute]) -> Result<Option<Self>> {
        let mut mark = None;
        for attr in attrs {
            match Self::parse_attr(attr)? {
                None => continue,
                Some(parsed) => {
                    if mark.is_some() {
                        bail_on!(attr, "multiple marks specified");
                    }
                    mark = Some(parsed);
                }
            }
        }
        Ok(mark)
    }
}
