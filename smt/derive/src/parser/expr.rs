use std::collections::BTreeMap;

use syn::{
    Block, Expr as Exp, ExprBlock, ExprMethodCall, Local, LocalInit, Pat, PatType, Result, Stmt,
};

use crate::parser::ctxt::ContextWithSig;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on};
use crate::parser::func::{FuncName, FuncSig, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, TypeTag};
use crate::parser::util::{ExprUtil, PatUtil};

/// Operations
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
}

/// Instructions (operations with type)
pub struct Inst {
    op: Box<Op>,
    ty: TypeRef,
}

/// Expressions
pub enum Expr {
    /// a single instruction
    Unit(Inst),
    /// `{ let <v1> = ...; let <v2> = ...; ...; <op>(<v1>, <v2>, ...) }`
    Block {
        lets: Vec<(VarName, Expr)>,
        body: Inst,
    },
}

impl Expr {
    /// Retrieve the type of an expression
    pub fn ty(&self) -> &TypeRef {
        let i = match self {
            Self::Unit(inst) => inst,
            Self::Block { lets: _, body } => body,
        };
        &i.ty
    }
}

/// Marks whether this expression is for impl or spec
#[derive(Copy, Clone)]
pub enum Kind {
    /// actual implementation
    Impl,
    /// formal specification
    Spec,
}

/// Root expression parser for function body
pub struct ExprParserRoot<'ctx> {
    /// context provider
    ctxt: &'ctx ContextWithSig,
    /// the expression is in spec or impl
    kind: Kind,
    /// function generics
    generics: Generics,
    /// function parameters
    params: BTreeMap<VarName, TypeRef>,
    /// function return type
    ret_ty: TypeRef,
}

/// Derived expression parser (for one and only one expression)
struct ExprParserCursor<'r, 'ctx: 'r> {
    /// parent of the parser
    root: &'r ExprParserRoot<'ctx>,
    /// expected type
    exp_ty: TypeRef,
    /// variables in scope and their types
    vars: BTreeMap<VarName, TypeRef>,
    /// new let-bindings created, if any
    bindings: Vec<(VarName, Expr)>,
}

impl<'ctx> ExprParserRoot<'ctx> {
    /// Creating a new context for parsing the function body expression
    pub fn new(ctxt: &'ctx ContextWithSig, kind: Kind, sig: &FuncSig) -> Self {
        Self {
            ctxt,
            kind,
            generics: sig.generics().clone(),
            params: sig
                .param_map()
                .into_iter()
                .map(|(k, v)| (k, (&v).into()))
                .collect(),
            ret_ty: sig.ret_ty().into(),
        }
    }

    /// Parse the body of the function
    pub fn parse(self, stmts: &[Stmt]) -> Result<Expr> {
        let mut unifier = TypeUnifier::new();

        // derive a parser
        let exp_ty = self.ret_ty.clone();
        let parser = ExprParserCursor {
            root: &self,
            exp_ty,
            vars: self.params.clone(),
            bindings: vec![],
        };
        let parsed = parser.convert_stmts(&mut unifier, stmts)?;

        // TODO: check type completeness

        // things look good
        Ok(parsed)
    }
}

impl CtxtForType for ExprParserRoot<'_> {
    fn generics(&self) -> &Generics {
        &self.generics
    }

    fn get_type_generics(&self, name: &UsrTypeName) -> Option<&Generics> {
        self.ctxt.get_type_generics(name)
    }
}

impl<'r, 'ctx: 'r> ExprParserCursor<'r, 'ctx> {
    /// Fork a child parser
    fn fork(&self, exp_ty: TypeRef) -> Self {
        Self {
            root: self.root,
            exp_ty,
            vars: self.vars.clone(),
            bindings: vec![],
        }
    }

    /// Consume a stream of statements
    fn convert_stmts(mut self, unifier: &mut TypeUnifier, stmts: &[Stmt]) -> Result<Expr> {
        let mut expr_found = None;
        let mut iter = stmts.iter();
        for stmt in iter.by_ref() {
            // parse the statement
            match stmt {
                Stmt::Local(binding) => {
                    // this is a let-binding
                    let Local {
                        attrs: _,
                        let_token: _,
                        pat,
                        init,
                        semi_token: _,
                    } = binding;

                    // find the name and type (optionally)
                    let (name, vty) = match pat {
                        Pat::Ident(_) => (PatUtil::expect_name(pat)?, None),
                        Pat::Type(pty) => {
                            let PatType {
                                attrs: _,
                                pat,
                                colon_token: _,
                                ty,
                            } = pty;
                            (
                                PatUtil::expect_name(pat)?,
                                Some(TypeTag::from_type(self.root, ty.as_ref())?),
                            )
                        }
                        _ => bail_on!(pat, "unrecognized binding"),
                    };
                    if self.vars.get(&name).is_some() {
                        bail_on!(pat, "name conflict");
                    }

                    // extract the body
                    let body = bail_if_missing!(init, binding, "expect initializer");
                    let LocalInit {
                        eq_token: _,
                        expr,
                        diverge,
                    } = body;
                    bail_if_exists!(diverge.as_ref().map(|(_, div)| div));

                    // parse the body with a new parser
                    let exp_ty = match vty.as_ref() {
                        None => {
                            // assign a new type var to this variable (i.e., to be inferred)
                            TypeRef::Var(unifier.mk_var())
                        }
                        Some(t) => t.into(),
                    };
                    let body_expr = self.fork(exp_ty).convert_expr(unifier, expr)?;

                    // done, continue to next statement
                    self.vars.insert(name.clone(), body_expr.ty().clone());
                    self.bindings.push((name, body_expr));
                }
                Stmt::Expr(expr, semi_token) => {
                    // expecting a unit expression
                    bail_if_exists!(semi_token);

                    // mark that we have found the main expression in the statements
                    expr_found = Some(expr);

                    // end of loop
                    break;
                }
                Stmt::Item(_) | Stmt::Macro(_) => bail_on!(stmt, "unexpected item"),
            }
        }

        // bail if there are more statements
        bail_if_exists!(iter.next());

        // check that we have an expression and convert it if so
        match expr_found {
            None => bail_on!(
                stmts.last().expect("at least one statement"),
                "unable to find the main expression"
            ),
            Some(expr) => self.convert_expr(unifier, expr),
        }
    }

    /// Parse an expression
    fn convert_expr(self, unifier: &mut TypeUnifier, target: &Exp) -> Result<Expr> {
        // case on expression type
        let (op, ty) = match target {
            Exp::Path(expr_path) => {
                let name = ExprUtil::expect_name_from_path(expr_path)?;
                let ty = match self.vars.get(&name) {
                    None => bail_on!(expr_path, "unknown local variable"),
                    Some(t) => t.clone(),
                };
                (Op::Var(name), ty)
            }
            Exp::Block(expr_block) => {
                let ExprBlock {
                    attrs: _,
                    label,
                    block:
                        Block {
                            brace_token: _,
                            stmts,
                        },
                } = expr_block;

                bail_if_exists!(label);
                return self.convert_stmts(unifier, stmts);
            }
            Exp::MethodCall(expr_method) => {
                let ExprMethodCall {
                    attrs: _,
                    receiver,
                    dot_token: _,
                    method,
                    turbofish,
                    paren_token: _,
                    args,
                } = expr_method;

                match FuncName::try_from(method)? {
                    FuncName::Sys(SysFuncName::Into) => {
                        // handle literal conversion
                        bail_if_exists!(turbofish);
                        bail_if_non_empty!(args);
                        let (intrinsic, ty) = Intrinsic::parse_literal_into(receiver)?;
                        (Op::Intrinsic(intrinsic), ty)
                    }
                    FuncName::Sys(SysFuncName::From) => bail_on!(expr_method, "unexpected"),
                    FuncName::Usr(name) => {
                        todo!()
                    }
                }
            }
            _ => todo!(),
        };

        // unity the type and then build the instruction
        let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
            None => bail_on!(target, "type unification"),
            Some(unified) => unified,
        };
        let inst = Inst {
            op: op.into(),
            ty: ty_unified,
        };

        // decide to go with an instruction or a block
        let converted = if self.bindings.is_empty() {
            Expr::Unit(inst)
        } else {
            Expr::Block {
                lets: self.bindings,
                body: inst,
            }
        };
        Ok(converted)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test::unit_test;

    unit_test!(forward_arg_plain, {
        #[smt_impl]
        fn foo(x: Boolean) -> Boolean {
            x
        }
    });

    unit_test!(forward_arg_ty_param, {
        #[smt_impl]
        fn foo<T: SMT>(x: T) -> T {
            x
        }
    });

    unit_test!(let_bind_arg_plain, {
        #[smt_impl]
        fn foo<T: SMT>(x: Boolean) -> Boolean {
            let y = x;
            y
        }
    });

    unit_test!(let_bind_arg_ty_param, {
        #[smt_impl]
        fn foo<T: SMT>(x: T) -> T {
            let y = x;
            y
        }
    });
}
