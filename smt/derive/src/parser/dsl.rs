//! Module for parsing quantifiers in the macro system.
//!
//! This module defines the AST structures and parsing logic for quantifiers
//! such as `exists`, `forall`, and `choose`. It supports both typed quantifiers
//! using closure syntax and iterated quantifiers using foreach syntax.

use proc_macro2::TokenTree; // token tree is a single token or a sequence of tokens. It is the building block of a token stream.
use syn::{
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    token::Paren,
    Expr,
    ExprClosure,
    ExprMacro,
    Ident,
    Macro,
    MacroDelimiter,
    Pat,
    PatType,
    Result,
    ReturnType,
    Token, // Token![:] for example, is a macro that expands to a Token::Colon.
};

use crate::parser::err::{bail_if_exists, bail_on};
use crate::parser::expr::CtxtForExpr;
use crate::parser::name::{ReservedIdent, VarName};
use crate::parser::ty::TypeTag;

/// Represents reserved macro names.
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum SysMacroName {
    /// The `exists` quantifier.
    Exists,
    /// The `forall` quantifier.
    Forall,
    /// The `choose` quantifier.
    Choose,
}

// the macro names are reserved identifiers
impl ReservedIdent for SysMacroName {
    /// Attempts to parse a string into a `SysMacroName`.
    ///
    /// Returns `Some(SysMacroName)` if the string matches one of the reserved macro names,
    /// otherwise returns `None`.
    /// This function is used in the `validate_user_ident` method of `name` module.
    /// The `validate_user_ident` method is used to check if the user-defined identifier is a reserved keyword. If it is, the method returns an error.
    fn from_str(ident: &str) -> Option<Self> {
        let matched = match ident {
            "exists" => Self::Exists,
            "forall" => Self::Forall,
            "choose" => Self::Choose,
            _ => return None,
        };
        Some(matched)
    }
}

/// AST node representing a variable declaration in an iterative quantifier.
///
/// For example, in `x in xs`, `ident` would be `x`, and `collection` would be `xs`.
struct IterVar {
    /// The identifier of the variable.
    ident: Ident,
    #[allow(dead_code)] //? should be removed
    /// The `in` token.
    in_token: Token![in],
    /// The collection expression the variable iterates over.
    collection: Expr,
}

impl Parse for IterVar {
    /// Parses an `IterVar` from the given parse stream.
    ///
    /// Expects the syntax `ident in collection`.
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            in_token: input.parse()?,
            collection: input.parse()?,
        })
    }
}

/// AST node representing an iterative quantifier.
///
/// For example, `x in xs, y in ys => body`.
struct IterQuant {
    /// The list of variable declarations delimited by commas.
    vars: Punctuated<IterVar, Token![,]>,
    #[allow(dead_code)] //? should be removed
    /// The `=>` token separating the variables and the body.
    imply_token: Token![=>],
    /// The body expression of the quantifier.
    body: Expr,
}

impl Parse for IterQuant {
    /// Parses an `IterQuant` from the given parse stream.
    ///
    /// Expects the syntax `vars => body`, where `vars` is a non-empty
    /// list of `IterVar` separated by commas.
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            vars: Punctuated::parse_separated_nonempty(input)?, // input.parse()? is used to parse a single token from the input stream not multiple tokens.
            imply_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

/// Represents a quantified operation in the macro system.
///
/// A quantifier can be either typed (using closure syntax) or iterated (using foreach syntax).
pub enum Quantifier {
    /// A typed quantifier using closure syntax.
    ///
    /// For example: `exists(|x: i32| x > 0)`.
    /// This falls under the first pattern of forall!, exists! and choose! macros in the stdlib/exp.rs file.
    Typed {
        /// The name of the quantifier (`exists`, `forall`, `choose`).
        name: SysMacroName,
        /// The list of variable declarations with their types.
        vars: Vec<(VarName, TypeTag)>,
        /// The body expression of the quantifier.
        body: Expr,
    },
    /// An iterated quantifier using foreach syntax.
    ///
    /// For example: `exists(x in xs => x > 0)`.
    /// This falls under the second pattern of forall!, exists! and choose! macros in the stdlib/exp.rs file.
    Iterated {
        /// The name of the quantifier (`exists`, `forall`, `choose`).
        name: SysMacroName,
        /// The list of variable declarations with their collections.
        vars: Vec<(VarName, Expr)>,
        /// The body expression of the quantifier.
        body: Expr,
    },
}

impl Quantifier {
    /// Parses a `Quantifier` from an `ExprMacro`.
    ///
    /// The macro can be in either closure style or iterated style.
    ///
    /// - Closure style: `quantifier(|x: Type| body)`
    /// - Iterated style: `quantifier(x in collection => body)`
    ///
    /// # Arguments
    ///
    /// * `ctxt` - The parsing context, suitable for expr analysis.
    /// * `expr` - The macro invocation expression; for example: `format!("{}", q)`.
    ///
    /// # Returns
    ///
    /// Returns a `Quantifier` if parsing succeeds, or a `syn::Error` otherwise.
    pub fn parse<T: CtxtForExpr>(ctxt: &T, expr: &ExprMacro) -> Result<Self> {
        // Destructure the macro expression to extract the macro path, delimiter, and tokens
        let ExprMacro {
            attrs: _,
            mac:
                Macro {
                    path,
                    bang_token: _,
                    delimiter,
                    tokens,
                },
        } = expr;

        // Ensure that the macro is invoked with parentheses
        if !matches!(delimiter, MacroDelimiter::Paren(Paren { .. })) {
            bail_on!(expr, "expect macro invocation with parenthesis");
        }

        // Parse the macro name from the path
        // This will return an error if the path has leading colons, or the path does not have one and only one segment, or the path has arguments. It will also return an error if the path is not `exists`, `forall`, or `choose`.
        let name = SysMacroName::parse_path(path)?;

        // Clone the tokens to inspect the first token
        let tester = tokens.clone();

        // Determine the style of the quantifier based on the first token
        let parsed = match tester.into_iter().next() {
            // token stream needs an owned iterator (into_iter())to iterate over the tokens.
            None => bail_on!(expr, "expect content"), // if the token stream is empty, return an error
            Some(TokenTree::Punct(punct)) if punct.as_char() == '|' => {
                // Closure style quantifier
                let stream = tokens.clone();

                // Parse the tokens as a closure expression
                let closure = parse2::<ExprClosure>(stream)?;
                // example of a parsed closure to ExprClosure with all the fields:
                // let x  = parse_quote! {
                //     |x: i32| x > 0
                // };
                // let closure = parse2::<ExprClosure>(x).unwrap();

                let ExprClosure {
                    attrs: _,
                    lifetimes,
                    constness,
                    movability,
                    asyncness,
                    capture,
                    or1_token: _,
                    inputs,
                    or2_token: _,
                    output,
                    body,
                } = closure;

                // Ensure that unsupported closure features are not used
                bail_if_exists!(lifetimes);
                bail_if_exists!(constness);
                bail_if_exists!(movability);
                bail_if_exists!(asyncness);
                bail_if_exists!(capture);

                // Collect the parameter declarations
                let mut param_decls = vec![];
                for param in inputs {
                    match param {
                        Pat::Type(typed) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = typed;

                            // Convert the pattern to a variable name
                            // this returns an error if the pattern is not an identifier pattern, or has a ref keyword, or has a mut keyword, or has a subpattern.
                            let name: VarName = pat.as_ref().try_into()?;

                            // Check for conflicting variable names
                            if param_decls.iter().any(|(n, _)| n == &name) {
                                bail_on!(pat, "conflicting quantifier variable name");
                            }

                            // Resolve the type tag from the type
                            let ty = TypeTag::from_type(ctxt, &ty)?; //? what does this do?
                            param_decls.push((name, ty));
                        }
                        _ => bail_on!(param, "invalid quantifier variable declaration"),
                    }
                }

                // Expect no return type in the closure
                // a ReturnType enum has two variants: Default and Type. Default is `()` for functions without a return type, and closures default to type inference. Type is a tuple struct with two fields: Arrow and Type - Type(Token![->], Box<Type>).
                match output {
                    ReturnType::Default => (),
                    ReturnType::Type(_, rty) => bail_on!(rty, "unexpected return type"),
                };

                // Return the parsed typed quantifier
                Self::Typed {
                    name,
                    vars: param_decls,
                    body: *body,
                }
            }
            Some(_) => {
                // Iterated style quantifier
                let stream = tokens.clone();

                // Parse the tokens as an iterative quantifier
                let syntax = parse2::<IterQuant>(stream)?;
                let IterQuant {
                    vars,
                    imply_token: _,
                    body,
                } = syntax;

                // Collect the variable declarations
                let mut var_decls = vec![];
                for var in vars {
                    let IterVar {
                        ident,
                        in_token: _,
                        collection,
                    } = var;

                    // Convert the identifier to a variable name
                    // this returns an error if the identifier is a reserved keyword.
                    let name: VarName = (&ident).try_into()?;

                    // Check for conflicting variable names
                    if var_decls.iter().any(|(n, _)| n == &name) {
                        bail_on!(&ident, "conflicting quantifier variable name");
                    }
                    var_decls.push((name, collection));
                }

                // Return the parsed iterated quantifier
                Self::Iterated {
                    name,
                    vars: var_decls,
                    body,
                }
            }
        };
        Ok(parsed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::ToTokens;
    use syn::parse_quote;

    // simple test to check if the from_str method works
    #[test]
    fn test_sys_macro_name_from_str_exists() {
        let par = SysMacroName::from_str("exists");
        assert!(par.is_some_and(|v| v == SysMacroName::Exists));
    }

    #[test]
    fn test_sys_macro_name_from_str_forall() {
        let par = SysMacroName::from_str("forall");
        assert!(par.is_some_and(|v| v == SysMacroName::Forall));
    }

    #[test]
    fn test_sys_macro_name_from_str_choose() {
        let par = SysMacroName::from_str("choose");
        assert!(par.is_some_and(|v| v == SysMacroName::Choose));
    }

    #[test]
    fn test_sys_macro_name_from_str_invalid() {
        let par = SysMacroName::from_str("invalid");
        assert!(par.is_none());
    }

    // simple test to check if the parse method works for IterQuant and IterVar
    #[test]
    fn test_parse_iter() {
        let par: IterQuant = parse_quote!(x in x_collection, y in y_collection => x > y);

        let IterQuant {
            vars,
            imply_token:_,
            body,
        } = par;

        // now destructure vars to get IterVar
        let IterVar {
            ident: x_ident,
            in_token: _,
            collection: x_collection,
        } = &vars[0];

        let IterVar {
            ident: y_ident,
            in_token: _,
            collection: y_collection,
        } = &vars[1];

        assert_eq!(x_ident.to_string(), "x");
        assert_eq!(x_collection.to_token_stream().to_string(), "x_collection");

        assert_eq!(y_ident.to_string(), "y");
        assert_eq!(y_collection.to_token_stream().to_string(), "y_collection");

        // check the body
        assert_eq!(body.to_token_stream().to_string(), "x > y");
    }

    //? parse method for Quantifier not tested because a generics type that implements CtxtForExpr is needed...
}
