use std::collections::BTreeMap;

use anyhow::{bail, Result};

use crate::ir::ctxt::IRBuilder;
use crate::ir::fun::{Function, UsrFunId};
use crate::ir::intrinsics::Intrinsic;
use crate::ir::name::{index, name};
use crate::ir::sort::{DataType, Sort, UsrSortId};
use crate::parser::expr::{Expr, LetBinding, Op, VarDecl};
use crate::parser::name::VarName;

name! {
    /// Name of a variable
    Symbol
}

impl From<&VarName> for Symbol {
    fn from(name: &VarName) -> Self {
        Self {
            ident: name.to_string(),
        }
    }
}

index! {
    /// Index of a variable
    VarId
}

index! {
    /// Index of an expression
    ExpId
}

/// The origin of a variable
pub enum VarKind {
    /// function parameter
    Param,
    /// free variable used in a quantifier
    Quant { decl: ExpId },
    /// let-binding to an expression
    Bound { bind: ExpId },
    /// match-introduced
    Match { decl: ExpId, bind: ExpId },
    /// axiomatized (through a list of predicates)
    Axiom { decl: ExpId, pred: Vec<ExpId> },
}

/// Information about a variable
pub struct Variable {
    pub name: Symbol,
    pub kind: VarKind,
    pub sort: Sort,
}

/// Denotes how to construct an enum variant
pub enum VariantCtor {
    Unit,
    Tuple(Vec<ExpId>),
    Record(BTreeMap<String, ExpId>),
}

/// Denotes how to destruct an enum variant and bind variables
pub enum VariantDtor {
    Unit,
    Tuple(Vec<Option<VarId>>),
    Record(BTreeMap<String, Option<VarId>>),
}

/// One atom in the match case to unpack
pub struct MatchAtom {
    head: ExpId,
    sort: UsrSortId,
    branch: String,
    variant: VariantDtor,
}

/// One match case
pub struct MatchCase {
    atoms: Vec<MatchAtom>,
    body: ExpId,
}

/// One phi case (i.e., conditional branch)
pub struct PhiCase {
    cond: ExpId,
    body: ExpId,
}

/// An expression
pub enum Expression {
    /// `<var>`
    Var(VarId),
    /// `(v1, v2, ...)`
    Pack { elems: Vec<ExpId> },
    /// `<tuple-name>(<inst>?)(v1, v2. ...)`
    Tuple { sort: UsrSortId, slots: Vec<ExpId> },
    /// `<record-name>(<inst>?){ f1: v1, f2: v2, ... }`
    Record {
        sort: UsrSortId,
        fields: BTreeMap<String, ExpId>,
    },
    /// `<adt-name>(<inst>?)::<branch>(<ctor>)`
    Enum {
        sort: UsrSortId,
        branch: String,
        variant: VariantCtor,
    },
    /// `<base>.<index>`
    AccessSlot { base: ExpId, slot: usize },
    /// `<base>.<field>`
    AccessField { base: ExpId, field: String },
    /// `match (v1, v2, ...) { (a1, a2, ...) => <body1> } ...`
    Match { cases: Vec<MatchCase> },
    /// `if (<c1>) { <v1> } else if (<c2>) { <v2> } ... else { <default> }`
    Phi { cases: Vec<PhiCase>, default: ExpId },
    /// `forall!(|<v>: <t>| {<expr>})`
    Forall {
        vars: BTreeMap<VarId, Sort>,
        body: ExpId,
    },
    /// `exists!(|<v>: <t>| {<expr>})`
    Exists {
        vars: BTreeMap<VarId, Sort>,
        body: ExpId,
    },
    /// `choose!(|<v>: <t>| {<expr>})`
    Choose {
        vars: BTreeMap<VarId, Sort>,
        body: ExpId,
        rets: Vec<VarId>,
    },
    /// `forall!(<v> in <c> ... => <expr>)`
    IterForall {
        vars: BTreeMap<VarId, ExpId>,
        body: ExpId,
    },
    /// `exists!(<v> in <c> ... => <expr>)`
    IterExists {
        vars: BTreeMap<VarId, ExpId>,
        body: ExpId,
    },
    /// `choose!(<v> in <c> ... => <expr>)`
    IterChoose {
        vars: BTreeMap<VarId, ExpId>,
        body: ExpId,
    },
    /// `<class>::<method>(<a1>, <a2>, ...)`
    Intrinsic(Intrinsic),
    /// `<function>(<a1>, <a2>, ...)`
    Procedure { callee: UsrFunId, args: Vec<ExpId> },
}

/// A registry of expressions (organized around a function body)
pub struct ExpRegistry {
    /// a map from variable id to variables
    vars: BTreeMap<VarId, Variable>,
    /// a map from expression id to expressions
    exps: BTreeMap<ExpId, Expression>,
}

impl ExpRegistry {
    /// Create an empty registry
    pub fn new() -> Self {
        Self {
            vars: BTreeMap::new(),
            exps: BTreeMap::new(),
        }
    }

    /// Add a new parameter to the registry
    fn add_param(&mut self, name: Symbol, sort: Sort) -> VarId {
        let id = VarId {
            index: self.vars.len(),
        };
        self.vars.insert(
            id,
            Variable {
                name,
                kind: VarKind::Param,
                sort,
            },
        );
        id
    }

    /// Add a new let-binding to the registry
    fn add_bound(&mut self, name: Symbol, sort: Sort, bind: ExpId) -> VarId {
        let id = VarId {
            index: self.vars.len(),
        };
        self.vars.insert(
            id,
            Variable {
                name,
                kind: VarKind::Bound { bind },
                sort,
            },
        );
        id
    }

    /// Register an expression
    fn register(&mut self, exp: Expression) -> ExpId {
        let id = ExpId {
            index: self.exps.len(),
        };
        self.exps.insert(id, exp);
        id
    }

    /// Retrieve the variable
    fn lookup_var(&self, idx: VarId) -> &Variable {
        self.vars.get(&idx).expect("no such var id")
    }

    /// Retrieve the expression
    fn lookup_exp(&self, idx: ExpId) -> &Expression {
        self.exps.get(&idx).expect("no such exp id")
    }
}

/// A context builder originated from a refinement relation
pub struct ExpBuilder<'b, 'ir: 'b, 'a: 'ir, 'ctx: 'a> {
    /// the parent IR builder
    pub parent: &'ir mut IRBuilder<'a, 'ctx>,
    /// a map from variable id to variables
    pub registry: &'b mut ExpRegistry,
    /// a set of valid variable ids in the current expression
    pub namespace: BTreeMap<Symbol, VarId>,
}

impl<'b, 'ir: 'b, 'a: 'ir, 'ctx: 'a> ExpBuilder<'b, 'ir, 'a, 'ctx> {
    /// Create a new expression builder
    fn new(
        parent: &'ir mut IRBuilder<'a, 'ctx>,
        registry: &'b mut ExpRegistry,
        params: &[(Symbol, Sort)],
    ) -> Result<Self> {
        let mut namespace = BTreeMap::new();
        for (name, sort) in params {
            let id = registry.add_param(name.clone(), sort.clone());
            match namespace.insert(name.clone(), id) {
                None => (),
                Some(_) => bail!("symbol conflict: {}", name),
            }
        }
        Ok(Self {
            parent,
            registry,
            namespace,
        })
    }

    /// Bind a variable declaration to an expression
    fn bind(&mut self, decl: &VarDecl, ety: Sort, exp: ExpId) -> Result<()> {
        match decl {
            VarDecl::One(name, ty) => {
                let sort = self.parent.resolve_type(ty)?;
                if sort != ety {
                    bail!("type mismatch");
                }
                let sym = Symbol::from(name);
                let vid = self.registry.add_bound(sym.clone(), sort, exp);
                match self.namespace.insert(sym, vid) {
                    None => (),
                    Some(_) => bail!("naming conflict"),
                }
            }
            VarDecl::Pack(elems) => {
                let tuple = match ety {
                    Sort::User(sort_id) => {
                        let dt = self.parent.ir.ty_registry.retrieve(sort_id);
                        match dt {
                            DataType::Tuple(tuple) => tuple.clone(),
                            _ => bail!("type mismatch"),
                        }
                    }
                    _ => bail!("type mismatch"),
                };
                if elems.len() != tuple.len() {
                    bail!("type mismatch");
                }
                for (elem_decl, elem_sort) in elems.iter().zip(tuple) {
                    self.bind(elem_decl, elem_sort, exp)?;
                }
            }
        }
        Ok(())
    }

    /// Process an expression
    pub fn resolve(&mut self, expr: &Expr, exp_ty: Sort) -> Result<ExpId> {
        // save the namespace
        let old_namespace = self.namespace.clone();

        // handle let bindings
        let inst = match expr {
            Expr::Unit(inst) => inst,
            Expr::Block { lets, body } => {
                for LetBinding { decl, bind } in lets {
                    let bind_ty = self.parent.resolve_type(&decl.ty())?;
                    let bind_exp = self.resolve(bind, bind_ty.clone())?;
                    self.bind(decl, bind_ty, bind_exp)?;
                }
                body
            }
        };

        // resolve type and check consistency
        let sort = self.parent.resolve_type(&inst.ty)?;
        if exp_ty != sort {
            bail!("type mismatch");
        }

        // parse the expression
        let expression = match inst.op.as_ref() {
            Op::Var(name) => {
                let vid = match self.namespace.get(&name.into()) {
                    None => bail!("no such variable"),
                    Some(id) => *id,
                };
                Expression::Var(vid)
            }
            Op::Pack { elems } => {
                let tuple = match sort {
                    Sort::User(sort_id) => {
                        let dt = self.parent.ir.ty_registry.retrieve(sort_id);
                        match dt {
                            DataType::Tuple(tuple) => tuple.clone(),
                            _ => bail!("type mismatch"),
                        }
                    }
                    _ => bail!("type mismatch"),
                };
                if elems.len() != tuple.len() {
                    bail!("type mismatch");
                }

                let mut elem_exps = vec![];
                for (elem_expr, elem_sort) in elems.iter().zip(tuple) {
                    let elem_eid = self.resolve(elem_expr, elem_sort.clone())?;
                    elem_exps.push(elem_eid);
                }
                Expression::Pack { elems: elem_exps }
            }
            _ => todo!(),
        };

        // register the expression
        let eid = self.registry.register(expression);

        // restore the namespace
        self.namespace = old_namespace;

        // done
        Ok(eid)
    }

    /// Materialize the entire function (signature + body, if any)
    pub fn materialize(
        mut parent: IRBuilder<'a, 'ctx>,
        params: Vec<(&VarName, Sort)>,
        ret_ty: Sort,
        body: Option<&Expr>,
    ) -> Result<Function> {
        let params: Vec<_> = params.into_iter().map(|(n, t)| (n.into(), t)).collect();
        let body = match body {
            None => None,
            Some(expr) => {
                // initialize the registry and builder
                let mut registry = ExpRegistry::new();
                let mut builder = ExpBuilder::new(&mut parent, &mut registry, &params)?;

                // build the expression
                let id = builder.resolve(expr, ret_ty.clone())?;

                // done
                Some((registry, id))
            }
        };
        Ok(Function {
            params,
            ret_ty,
            body,
        })
    }
}
