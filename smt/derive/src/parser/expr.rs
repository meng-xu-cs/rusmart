use std::collections::BTreeMap;

use syn::{
    Block, Expr as Exp, ExprBlock, ExprCall, ExprIf, ExprMethodCall, ExprPath, ExprUnary, Local,
    LocalInit, Pat, PatType, Path, PathArguments, PathSegment, Result, Stmt, UnOp,
};

use crate::parser::ctxt::ContextWithSig;
use crate::parser::err::{bail_if_exists, bail_if_missing, bail_if_non_empty, bail_on};
use crate::parser::func::{FuncName, FuncSig, SysFuncName};
use crate::parser::generics::Generics;
use crate::parser::infer::{TypeRef, TypeUnifier};
use crate::parser::intrinsics::Intrinsic;
use crate::parser::name::{UsrFuncName, UsrTypeName, VarName};
use crate::parser::ty::{CtxtForType, SysTypeName, TypeName, TypeTag};
use crate::parser::util::{ExprUtil, PatUtil};

/// Phi node, guarded by condition
pub struct PhiNode {
    cond: Expr,
    body: Expr,
}

/// Operations
pub enum Op {
    /// `<var>`
    Var(VarName),
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi { nodes: Vec<PhiNode>, default: Expr },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
    /// `<function>(<a1>, <a2>, ...)`
    Procedure { name: UsrFuncName, args: Vec<Expr> },
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

    /// Set the type
    pub fn set_ty(&mut self, ty: TypeRef) {
        match self {
            Self::Unit(inst) => inst.ty = ty,
            Self::Block { lets: _, body } => body.ty = ty,
        }
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
        let inst = match target {
            Exp::Path(expr_path) => {
                let name = ExprUtil::expect_name_from_path(expr_path)?;
                let ty = match self.vars.get(&name) {
                    None => bail_on!(expr_path, "unknown local variable"),
                    Some(t) => t.clone(),
                };

                // unity the type and then build the instruction
                let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                    Ok(unified) => unified,
                    Err(e) => bail_on!(target, "{}", e),
                };
                Inst {
                    op: Op::Var(name).into(),
                    ty: ty_unified,
                }
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
            Exp::If(expr_if) => {
                let mut phi_nodes = vec![];
                let mut cursor = expr_if;
                let default_expr = loop {
                    let ExprIf {
                        attrs: _,
                        if_token: _,
                        cond,
                        then_branch,
                        else_branch,
                    } = cursor;

                    // convert the condition
                    let new_cond = match cond.as_ref() {
                        Exp::Unary(expr_unary) => {
                            let ExprUnary { attrs: _, op, expr } = expr_unary;
                            if !matches!(op, UnOp::Deref(_)) {
                                bail_on!(cond, "not a Boolean deref");
                            }
                            self.fork(TypeRef::Boolean).convert_expr(unifier, expr)?
                        }
                        _ => bail_on!(cond, "not a Boolean deref"),
                    };

                    // convert the then block
                    let new_then = self
                        .fork(self.exp_ty.clone())
                        .convert_stmts(unifier, &then_branch.stmts)?;

                    // now we have a node
                    phi_nodes.push(PhiNode {
                        cond: new_cond,
                        body: new_then,
                    });

                    // convert the else block
                    let else_expr = bail_if_missing!(
                        else_branch.as_ref().map(|(_, e)| e.as_ref()),
                        expr_if,
                        "expect else branch"
                    );

                    match else_expr {
                        Exp::If(elseif_expr) => {
                            cursor = elseif_expr;
                        }
                        _ => {
                            break self
                                .fork(self.exp_ty.clone())
                                .convert_expr(unifier, else_expr)?;
                        }
                    }
                };

                // check consistency of body type
                let ref_ty = default_expr.ty().clone();
                for item in &phi_nodes {
                    if item.body.ty() != &ref_ty {
                        bail_on!(expr_if, "phi node type mismatch");
                    }
                }

                // construct the operator
                let op = Op::Phi {
                    nodes: phi_nodes,
                    default: default_expr,
                };
                Inst {
                    op: op.into(),
                    ty: ref_ty,
                }
            }
            Exp::Call(expr_call) => {
                let ExprCall {
                    attrs: _,
                    func,
                    paren_token: _,
                    args,
                } = expr_call;

                // extract the callee
                let mut iter = match func.as_ref() {
                    Exp::Path(p) => {
                        let ExprPath {
                            attrs: _,
                            qself,
                            path:
                                Path {
                                    leading_colon,
                                    segments,
                                },
                        } = p;
                        bail_if_exists!(qself.as_ref().map(|q| q.ty.as_ref()));
                        bail_if_exists!(leading_colon);
                        segments.iter().rev()
                    }
                    _ => bail_on!(func, "invalid callee"),
                };

                // extract method and class
                let segment_method = bail_if_missing!(iter.next(), func, "callee");
                let class = match iter.next() {
                    None => None,
                    Some(segment) => {
                        let PathSegment { ident, arguments } = segment;
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected type arguments");
                        }
                        Some((TypeName::try_from(self.root.generics(), ident)?, segment))
                    }
                };
                bail_if_exists!(iter.next());

                // collect type arguments, if any
                let PathSegment { ident, arguments } = segment_method;

                // case on function names
                match FuncName::try_from(ident)? {
                    FuncName::Sys(name) => {
                        // qualifier required
                        let (qualifier, qsegment) = match class {
                            None => bail_on!(ident, "expect qualifier"),
                            Some(q) => q,
                        };

                        // none of the smt-system functions take path arguments
                        if !matches!(arguments, PathArguments::None) {
                            bail_on!(arguments, "unexpected type arguments");
                        }

                        match name {
                            SysFuncName::From => {
                                // handle literal conversion
                                let (intrinsic, ty) = match qualifier {
                                    TypeName::Param(_) | TypeName::Usr(_) => {
                                        bail_on!(qsegment, "expect an intrinsic qualifier");
                                    }
                                    TypeName::Sys(tname) => match tname {
                                        SysTypeName::Boolean => (
                                            Intrinsic::BoolVal(Intrinsic::unpack_lit_bool(args)?),
                                            TypeRef::Boolean,
                                        ),
                                        SysTypeName::Integer => (
                                            Intrinsic::IntVal(Intrinsic::unpack_lit_int(args)?),
                                            TypeRef::Integer,
                                        ),
                                        SysTypeName::Rational => (
                                            Intrinsic::NumVal(Intrinsic::unpack_lit_float(args)?),
                                            TypeRef::Rational,
                                        ),
                                        SysTypeName::Text => (
                                            Intrinsic::StrVal(Intrinsic::unpack_lit_str(args)?),
                                            TypeRef::Text,
                                        ),
                                        _ => {
                                            bail_on!(qsegment, "expect a literal-enabled qualifier")
                                        }
                                    },
                                };

                                // unity the type and then build the instruction
                                let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                                    Ok(unified) => unified,
                                    Err(e) => bail_on!(target, "{}", e),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: ty_unified,
                                }
                            }
                            SysFuncName::Into => bail_on!(expr_call, "unexpected"),
                            SysFuncName::Eq | SysFuncName::Ne => {
                                // collect arguments
                                let mut iter = args.iter();
                                let lhs = bail_if_missing!(iter.next(), args, "expect 2 arguments");
                                let rhs = bail_if_missing!(iter.next(), args, "expect 2 arguments");
                                bail_if_exists!(iter.next());

                                // process it
                                let (parsed_lhs, parsed_rhs) =
                                    self.handle_equality_inequality(unifier, lhs, rhs, target)?;

                                // check qualifier consistency
                                let consistent = match (&qualifier, parsed_lhs.ty()) {
                                    (TypeName::Sys(SysTypeName::Boolean), TypeRef::Boolean)
                                    | (TypeName::Sys(SysTypeName::Integer), TypeRef::Integer)
                                    | (TypeName::Sys(SysTypeName::Rational), TypeRef::Rational)
                                    | (TypeName::Sys(SysTypeName::Text), TypeRef::Text)
                                    | (TypeName::Sys(SysTypeName::Cloak), TypeRef::Cloak(_))
                                    | (TypeName::Sys(SysTypeName::Seq), TypeRef::Seq(_))
                                    | (TypeName::Sys(SysTypeName::Set), TypeRef::Set(_))
                                    | (TypeName::Sys(SysTypeName::Map), TypeRef::Map(_, _))
                                    | (TypeName::Sys(SysTypeName::Error), TypeRef::Error) => true,
                                    (TypeName::Usr(name1), TypeRef::User(name2, _)) => {
                                        name1 == name2
                                    }
                                    (TypeName::Param(name1), TypeRef::Parameter(name2)) => {
                                        name1 == name2
                                    }
                                    _ => false,
                                };
                                if !consistent {
                                    bail_on!(
                                        expr_call,
                                        "qualifier and argument type does not match"
                                    );
                                }

                                // construct the operator
                                let intrinsic = match name {
                                    SysFuncName::Eq => Intrinsic::SmtEq(parsed_lhs, parsed_rhs),
                                    SysFuncName::Ne => Intrinsic::SmtNe(parsed_lhs, parsed_rhs),
                                    _ => panic!("invariant violation"),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: TypeRef::Boolean,
                                }
                            }
                        }
                    }
                    FuncName::Usr(name) => {
                        // qualifier should never be a type parameter or an intrinsic type
                        let (qualifier, qsegment) = match class {
                            None => bail_on!(ident, "expect qualifier"),
                            Some(q) => q,
                        };
                        if !matches!(qualifier, TypeName::Usr(_)) {
                            bail_on!(qsegment, "user-defined type qualifier only");
                        }

                        // collect type arguments
                        let ty_args_opt = match arguments {
                            PathArguments::None => None,
                            PathArguments::AngleBracketed(ty_args) => {
                                Some(TypeTag::from_generics(self.root, ty_args)?)
                            }
                            PathArguments::Parenthesized(_) => {
                                bail_on!(arguments, "invalid type arguments")
                            }
                        };

                        // parse the arguments by tentatively marking their types as variables
                        let mut parsed_args = vec![];
                        for arg in args {
                            let parsed = self
                                .fork(TypeRef::Var(unifier.mk_var()))
                                .convert_expr(unifier, arg)?;
                            parsed_args.push(parsed);
                        }

                        // choose the function from function database
                        let inferred = self.root.ctxt.infer.get_inference(
                            unifier,
                            &name,
                            Some(qualifier).as_ref(),
                            ty_args_opt.as_deref(),
                            parsed_args,
                            &self.exp_ty,
                        );
                        match inferred {
                            Ok((op, unified_ret)) => Inst {
                                op: op.into(),
                                ty: unified_ret,
                            },
                            Err(e) => bail_on!(expr_call, "{}", e),
                        }
                    }
                }
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
                    FuncName::Sys(name) => {
                        // none of the smt-system functions take path arguments
                        bail_if_exists!(turbofish);

                        match name {
                            SysFuncName::Into => {
                                bail_if_non_empty!(args);
                                let (intrinsic, ty) = Intrinsic::parse_literal_into(receiver)?;

                                // unity the type and then build the instruction
                                let ty_unified = match unifier.unify(&ty, &self.exp_ty) {
                                    Ok(unified) => unified,
                                    Err(e) => bail_on!(target, "{}", e),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: ty_unified,
                                }
                            }
                            SysFuncName::From => bail_on!(expr_method, "unexpected"),
                            SysFuncName::Eq | SysFuncName::Ne => {
                                // collect arguments
                                let mut iter = args.iter();
                                let rhs = bail_if_missing!(iter.next(), args, "expect 1 argument");
                                bail_if_exists!(iter.next());

                                // process it
                                let (parsed_lhs, parsed_rhs) = self
                                    .handle_equality_inequality(unifier, receiver, rhs, target)?;

                                // construct the operator
                                let intrinsic = match name {
                                    SysFuncName::Eq => Intrinsic::SmtEq(parsed_lhs, parsed_rhs),
                                    SysFuncName::Ne => Intrinsic::SmtNe(parsed_lhs, parsed_rhs),
                                    _ => panic!("invariant violation"),
                                };
                                Inst {
                                    op: Op::Intrinsic(intrinsic).into(),
                                    ty: TypeRef::Boolean,
                                }
                            }
                        }
                    }
                    FuncName::Usr(name) => {
                        // collect type arguments, if any
                        let ty_args_opt = match turbofish {
                            None => None,
                            Some(ty_args) => Some(TypeTag::from_generics(self.root, ty_args)?),
                        };

                        // parse the arguments by tentatively marking their types as variables
                        let mut parsed_args = vec![];
                        let parsed_receiver = self
                            .fork(TypeRef::Var(unifier.mk_var()))
                            .convert_expr(unifier, receiver)?;
                        parsed_args.push(parsed_receiver);

                        for arg in args {
                            let parsed = self
                                .fork(TypeRef::Var(unifier.mk_var()))
                                .convert_expr(unifier, arg)?;
                            parsed_args.push(parsed);
                        }

                        // choose the function from function database
                        let inferred = self.root.ctxt.infer.get_inference(
                            unifier,
                            &name,
                            None,
                            ty_args_opt.as_deref(),
                            parsed_args,
                            &self.exp_ty,
                        );
                        match inferred {
                            Ok((op, unified_ret)) => Inst {
                                op: op.into(),
                                ty: unified_ret,
                            },
                            Err(e) => bail_on!(expr_method, "{}", e),
                        }
                    }
                }
            }
            _ => todo!(),
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

    /// Utility function on parsing equality and inequalities
    fn handle_equality_inequality(
        &self,
        unifier: &mut TypeUnifier,
        lhs: &Exp,
        rhs: &Exp,
        host: &Exp,
    ) -> Result<(Expr, Expr)> {
        // parse the arguments tentatively by marking their types as variables
        let mut parsed_lhs = self
            .fork(TypeRef::Var(unifier.mk_var()))
            .convert_expr(unifier, lhs)?;
        let mut parsed_rhs = self
            .fork(TypeRef::Var(unifier.mk_var()))
            .convert_expr(unifier, rhs)?;

        // unify the types
        match unifier.unify(&TypeRef::Boolean, &self.exp_ty) {
            Ok(t) => t,
            Err(e) => bail_on!(host, "{}", e),
        };
        let arg_ty = match unifier.unify(parsed_lhs.ty(), parsed_rhs.ty()) {
            Ok(t) => t,
            Err(e) => bail_on!(host, "{}", e),
        };
        parsed_lhs.set_ty(arg_ty.clone());
        parsed_rhs.set_ty(arg_ty);

        // return the expressions
        Ok((parsed_lhs, parsed_rhs))
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

    unit_test!(bool_basics, {
        #[smt_impl]
        fn foo(x: Boolean, y: Boolean) -> Boolean {
            x.not().and(false.into()).or(true.into()).xor(y).eq(x.ne(y))
        }
    });

    unit_test!(type_param_equal, {
        #[smt_impl]
        fn foo<T: SMT>(x: T, y: T) -> Boolean {
            x.eq(y).ne(T::ne(x, y))
        }
    });
}
