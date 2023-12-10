use std::collections::BTreeMap;

use crate::ir::ctxt::IRBuilder;
use crate::ir::fun::{Function, UsrFunId};
use crate::ir::intrinsics::Intrinsic;
use crate::ir::name::{index, name};
use crate::ir::sort::{DataType, Sort, UsrSortId, Variant};
use crate::parser::adt::ADTBranch;
use crate::parser::expr::{Expr, LetBinding, Op, Unpack, VarDecl};
use crate::parser::name::VarName;
use crate::parser::ty::TypeTag;

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
    Quant,
    /// axiomatized (through a list of predicates)
    Axiom,
    /// let-binding to an expression
    Bound { bind: ExpId },
    /// match-introduced
    Match {
        head: ExpId,
        sort: UsrSortId,
        branch: String,
        selector: EnumSelector,
    },
}

/// Information about a variable
pub struct Variable {
    pub name: Symbol,
    pub kind: VarKind,
    pub sort: Sort,
}

/// Denotes how a variable gets match-bounded
pub enum EnumSelector {
    Tuple(usize),
    Record(String),
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
    Pack { sort: UsrSortId, elems: Vec<ExpId> },
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
        rets: Vec<VarId>,
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

    /// Add a new quantified variable (free variable) to the registry
    fn add_quant_var(&mut self, name: Symbol, sort: Sort) -> VarId {
        let id = VarId {
            index: self.vars.len(),
        };
        self.vars.insert(
            id,
            Variable {
                name,
                kind: VarKind::Quant,
                sort,
            },
        );
        id
    }

    /// Add a new axiomatized variable to the registry
    fn add_axiom_var(&mut self, name: Symbol, sort: Sort) -> VarId {
        let id = VarId {
            index: self.vars.len(),
        };
        self.vars.insert(
            id,
            Variable {
                name,
                kind: VarKind::Axiom,
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

    /// Add a new match-binding to the registry
    fn add_match(
        &mut self,
        name: Symbol,
        sort: Sort,
        head: ExpId,
        sid: UsrSortId,
        branch: String,
        selector: EnumSelector,
    ) -> VarId {
        let id = VarId {
            index: self.vars.len(),
        };
        self.vars.insert(
            id,
            Variable {
                name,
                kind: VarKind::Match {
                    head,
                    sort: sid,
                    branch,
                    selector,
                },
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
    ) -> Self {
        let mut namespace = BTreeMap::new();
        for (name, sort) in params {
            let id = registry.add_param(name.clone(), sort.clone());
            match namespace.insert(name.clone(), id) {
                None => (),
                Some(_) => panic!("symbol conflict: {}", name),
            }
        }
        Self {
            parent,
            registry,
            namespace,
        }
    }

    /// Utility: expect type match
    fn check_sort(expect: &Sort, actual: &Sort) {
        if expect != actual {
            panic!("type mismatch: expect {} | actual {}", expect, actual);
        }
    }

    /// Utility: resolve user sort id from the sort
    fn expect_sort_user(sort: &Sort) -> UsrSortId {
        match sort {
            Sort::User(sid) => *sid,
            _ => panic!("type mismatch: expect $? | actual {}", sort),
        }
    }

    /// Utility: retrieve a tuple data type from a sort id
    fn expect_type_tuple(&self, sort_id: UsrSortId) -> Vec<Sort> {
        match self.parent.ir.ty_registry.retrieve(sort_id) {
            DataType::Tuple(tuple) => tuple.clone(),
            dt => panic!("type mismatch: expect <tuple> | actual {}", dt),
        }
    }

    /// Utility: retrieve a record data type from a sort id
    fn expect_type_record(&self, sort_id: UsrSortId) -> BTreeMap<String, Sort> {
        match self.parent.ir.ty_registry.retrieve(sort_id) {
            DataType::Record(record) => record.clone(),
            dt => panic!("type mismatch: expect <record> | actual {}", dt),
        }
    }

    /// Utility: retrieve an enum-unit data type from a sort id and branch name
    fn expect_type_enum_unit(&self, sort_id: UsrSortId, branch: &str) {
        match self.parent.ir.ty_registry.retrieve(sort_id) {
            DataType::Enum(adt) => match adt.get(branch) {
                None => panic!("no such branch: {}", branch),
                Some(Variant::Unit) => (),
                Some(variant) => panic!("type mismatch: expect <enum::unit> | actual {}", variant),
            },
            dt => panic!("type mismatch: expect <enum::unit> | actual {}", dt),
        }
    }

    /// Utility: retrieve an enum-tuple data type from a sort id and branch name
    fn expect_type_enum_tuple(&self, sort_id: UsrSortId, branch: &str) -> Vec<Sort> {
        match self.parent.ir.ty_registry.retrieve(sort_id) {
            DataType::Enum(adt) => match adt.get(branch) {
                None => panic!("no such branch: {}", branch),
                Some(Variant::Tuple(tuple)) => tuple.clone(),
                Some(variant) => panic!("type mismatch: expect <enum::tuple> | actual {}", variant),
            },
            dt => panic!("type mismatch: expect <enum::tuple> | actual {}", dt),
        }
    }

    /// Utility: retrieve an enum-record data type from a sort id and branch name
    fn expect_type_enum_record(
        &self,
        sort_id: UsrSortId,
        branch: &String,
    ) -> BTreeMap<String, Sort> {
        match self.parent.ir.ty_registry.retrieve(sort_id) {
            DataType::Enum(adt) => match adt.get(branch) {
                None => panic!("no such branch: {}", branch),
                Some(Variant::Record(record)) => record.clone(),
                Some(variant) => {
                    panic!("type mismatch: expect <enum::record> | actual {}", variant)
                }
            },
            dt => panic!("type mismatch: expect <enum::record> | actual {}", dt),
        }
    }

    /// Utility: resolve a tuple of expressions
    fn resolve_expr_tuple(&mut self, tuple: &[Sort], slots: &[Expr]) -> Vec<ExpId> {
        if tuple.len() != slots.len() {
            panic!(
                "tuple slot number mismatch: expect {} | actual {}",
                tuple.len(),
                slots.len()
            );
        }

        let mut converted = vec![];
        for (expr, sort) in slots.iter().zip(tuple) {
            let eid = self.resolve(expr, Some(sort));
            converted.push(eid);
        }
        converted
    }

    /// Utility: resolve a record of expressions
    fn resolve_expr_record(
        &mut self,
        record: &BTreeMap<String, Sort>,
        fields: &BTreeMap<String, Expr>,
    ) -> BTreeMap<String, ExpId> {
        if record.len() != fields.len() {
            panic!(
                "record field number mismatch: expect {} | actual {}",
                record.len(),
                fields.len()
            );
        }

        let mut converted = BTreeMap::new();
        for ((name_ref, expr), (name, sort)) in fields.iter().zip(record) {
            if name_ref != name {
                panic!(
                    "record field name mismatch: expect {} | actual {}",
                    name_ref, name
                );
            }
            let field_eid = self.resolve(expr, Some(sort));
            converted.insert(name.clone(), field_eid);
        }
        converted
    }

    /// Bind a variable declaration to an expression
    fn bind_decl(&mut self, decl: &VarDecl, ety: Sort, exp: ExpId) {
        match decl {
            VarDecl::One(name, ty) => {
                let sort = self.parent.resolve_type(ty);
                Self::check_sort(&ety, &sort);

                let sym = Symbol::from(name);
                let vid = self.registry.add_bound(sym.clone(), sort, exp);
                match self.namespace.insert(sym, vid) {
                    None => (),
                    Some(_) => panic!("naming conflict: {}", name),
                }
            }
            VarDecl::Pack(elems) => {
                let sort_id = Self::expect_sort_user(&ety);
                let tuple = self.expect_type_tuple(sort_id);
                if elems.len() != tuple.len() {
                    panic!(
                        "tuple slot number mismatch: expect {} | actual {}",
                        tuple.len(),
                        elems.len(),
                    );
                }
                for (elem_decl, elem_sort) in elems.iter().zip(tuple) {
                    self.bind_decl(elem_decl, elem_sort, exp);
                }
            }
        }
    }

    /// Bind a variable match-destruction
    fn bind_dtor(
        &mut self,
        name: &VarName,
        sort: Sort,
        head: ExpId,
        sid: UsrSortId,
        branch: String,
        selector: EnumSelector,
    ) -> VarId {
        let sym = Symbol::from(name);
        let vid = self
            .registry
            .add_match(sym.clone(), sort, head, sid, branch, selector);
        match self.namespace.insert(sym, vid) {
            None => (),
            Some(_) => panic!("naming conflict: {}", name),
        }
        vid
    }

    /// Add a quantified variable (i.e., free variable) induced by iteration
    fn iter_quant_var(&mut self, name: &VarName, expr: ExpId) -> VarId {
        let sym = Symbol::from(name);
        let sort = match self.derive_type(expr) {
            Sort::Seq(_) => Sort::Integer,
            Sort::Set(sub) => *sub,
            Sort::Map(key, _) => *key,
            s => panic!("iterating over a non-iterable type: {}", s),
        };
        let vid = self.registry.add_quant_var(sym.clone(), sort);
        match self.namespace.insert(sym, vid) {
            None => (),
            Some(_) => panic!("naming conflict: {}", name),
        }
        vid
    }

    /// Add a quantified variable (i.e., free variable)
    fn free_quant_var(&mut self, name: &VarName, tag: &TypeTag) -> (VarId, Sort) {
        let sort = self.parent.resolve_type(&tag.into());
        let sym = Symbol::from(name);
        let vid = self.registry.add_quant_var(sym.clone(), sort.clone());
        match self.namespace.insert(sym, vid) {
            None => (),
            Some(_) => panic!("naming conflict: {}", name),
        }
        (vid, sort)
    }

    /// Add an axiomatized variable induced by iteration
    fn iter_axiom_var(&mut self, name: &VarName, expr: ExpId) -> VarId {
        let sym = Symbol::from(name);
        let sort = match self.derive_type(expr) {
            Sort::Seq(_) => Sort::Integer,
            Sort::Set(sub) => *sub,
            Sort::Map(key, _) => *key,
            s => panic!("iterating over a non-iterable type: {}", s),
        };
        let vid = self.registry.add_axiom_var(sym.clone(), sort);
        match self.namespace.insert(sym, vid) {
            None => (),
            Some(_) => panic!("naming conflict: {}", name),
        }
        vid
    }

    /// Add an axiomatized variable
    fn free_axiom_var(&mut self, name: &VarName, tag: &TypeTag) -> (VarId, Sort) {
        let sort = self.parent.resolve_type(&tag.into());
        let sym = Symbol::from(name);
        let vid = self.registry.add_axiom_var(sym.clone(), sort.clone());
        match self.namespace.insert(sym, vid) {
            None => (),
            Some(_) => panic!("naming conflict: {}", name),
        }
        (vid, sort)
    }

    /// Process an expression
    pub fn resolve(&mut self, expr: &Expr, exp_ty: Option<&Sort>) -> ExpId {
        // save the namespace
        let old_namespace = self.namespace.clone();

        // handle let bindings
        let inst = match expr {
            Expr::Unit(inst) => inst,
            Expr::Block { lets, body } => {
                for LetBinding { decl, bind } in lets {
                    let bind_ty = self.parent.resolve_type(&decl.ty());
                    let bind_exp = self.resolve(bind, Some(&bind_ty));
                    self.bind_decl(decl, bind_ty, bind_exp);
                }
                body
            }
        };

        // resolve type and check consistency
        let sort = self.parent.resolve_type(&inst.ty);
        match exp_ty {
            None => (),
            Some(ety) => Self::check_sort(&sort, ety),
        }

        // parse the expression
        let expression = match inst.op.as_ref() {
            Op::Var(name) => {
                let vid = match self.namespace.get(&name.into()) {
                    None => panic!("no such variable: {}", name),
                    Some(id) => *id,
                };
                Expression::Var(vid)
            }
            Op::Pack { elems } => {
                let sort_id = Self::expect_sort_user(&sort);
                let tuple = self.expect_type_tuple(sort_id);
                let resolved = self.resolve_expr_tuple(&tuple, elems);
                Expression::Pack {
                    sort: sort_id,
                    elems: resolved,
                }
            }
            Op::Tuple { name, inst, slots } => {
                let sort_id = self.parent.register_type(Some(name), inst);
                let tuple = self.expect_type_tuple(sort_id);
                let resolved = self.resolve_expr_tuple(&tuple, slots);
                Expression::Tuple {
                    sort: sort_id,
                    slots: resolved,
                }
            }
            Op::Record { name, inst, fields } => {
                let sort_id = self.parent.register_type(Some(name), inst);
                let record = self.expect_type_record(sort_id);
                let resolved = self.resolve_expr_record(&record, fields);
                Expression::Record {
                    sort: sort_id,
                    fields: resolved,
                }
            }
            Op::EnumUnit {
                branch: ADTBranch { ty_name, variant },
                inst,
            } => {
                let sort_id = self.parent.register_type(Some(ty_name), inst);
                self.expect_type_enum_unit(sort_id, variant);
                Expression::Enum {
                    sort: sort_id,
                    branch: variant.clone(),
                    variant: VariantCtor::Unit,
                }
            }
            Op::EnumTuple {
                branch: ADTBranch { ty_name, variant },
                inst,
                slots,
            } => {
                let sort_id = self.parent.register_type(Some(ty_name), inst);
                let tuple = self.expect_type_enum_tuple(sort_id, variant);
                let resolved = self.resolve_expr_tuple(&tuple, slots);
                Expression::Enum {
                    sort: sort_id,
                    branch: variant.clone(),
                    variant: VariantCtor::Tuple(resolved),
                }
            }
            Op::EnumRecord {
                branch: ADTBranch { ty_name, variant },
                inst,
                fields,
            } => {
                let sort_id = self.parent.register_type(Some(ty_name), inst);
                let record = self.expect_type_record(sort_id);
                let resolved = self.resolve_expr_record(&record, fields);
                Expression::Enum {
                    sort: sort_id,
                    branch: variant.clone(),
                    variant: VariantCtor::Record(resolved),
                }
            }
            Op::AccessSlot { base, slot } => {
                let resolved = self.resolve(base, None);
                Expression::AccessSlot {
                    base: resolved,
                    slot: *slot,
                }
            }
            Op::AccessField { base, field } => {
                let resolved = self.resolve(base, None);
                Expression::AccessField {
                    base: resolved,
                    field: field.clone(),
                }
            }
            Op::Match { heads, combo } => {
                // resolve heads
                let resolved_heads: Vec<_> = heads.iter().map(|e| self.resolve(e, None)).collect();

                // resolve match arms
                let mut cases = vec![];
                for arm in combo {
                    // process atoms
                    if arm.variants.len() != resolved_heads.len() {
                        panic!(
                            "match atom number mismatch: expect {} | actual {}",
                            resolved_heads.len(),
                            arm.variants.len()
                        );
                    }

                    let mut atoms = vec![];
                    for (variant, &head) in arm.variants.iter().zip(resolved_heads.iter()) {
                        let head_type = self.derive_type(head);
                        let head_sid = Self::expect_sort_user(&head_type);
                        let branch = variant.branch.variant.clone();
                        let dtor = match &variant.unpack {
                            Unpack::Unit => {
                                self.expect_type_enum_unit(head_sid, &branch);
                                VariantDtor::Unit
                            }
                            Unpack::Tuple(bind_slots) => {
                                let sort_tuple = self.expect_type_enum_tuple(head_sid, &branch);
                                for &k in bind_slots.keys() {
                                    if k >= sort_tuple.len() {
                                        panic!(
                                            "type {} at branch {} does not have slot {}",
                                            head_type, branch, k
                                        );
                                    }
                                }
                                let mut binds = vec![];
                                for (i, s) in sort_tuple.into_iter().enumerate() {
                                    let item = match bind_slots.get(&i) {
                                        None => None,
                                        Some(var_name) => {
                                            let vid = self.bind_dtor(
                                                var_name,
                                                s,
                                                head,
                                                head_sid,
                                                branch.clone(),
                                                EnumSelector::Tuple(i),
                                            );
                                            Some(vid)
                                        }
                                    };
                                    binds.push(item);
                                }
                                VariantDtor::Tuple(binds)
                            }
                            Unpack::Record(bind_fields) => {
                                let sort_record = self.expect_type_enum_record(head_sid, &branch);
                                for k in bind_fields.keys() {
                                    if !sort_record.contains_key(k) {
                                        panic!(
                                            "type {} at branch {} does not have field {}",
                                            head_type, branch, k
                                        );
                                    }
                                }
                                let mut binds = BTreeMap::new();
                                for (i, s) in sort_record.into_iter() {
                                    let item = match bind_fields.get(&i) {
                                        None => None,
                                        Some(var_name) => {
                                            let vid = self.bind_dtor(
                                                var_name,
                                                s,
                                                head,
                                                head_sid,
                                                branch.clone(),
                                                EnumSelector::Record(i.clone()),
                                            );
                                            Some(vid)
                                        }
                                    };
                                    binds.insert(i, item);
                                }
                                VariantDtor::Record(binds)
                            }
                        };

                        let atom = MatchAtom {
                            head,
                            sort: head_sid,
                            branch,
                            variant: dtor,
                        };
                        atoms.push(atom);
                    }

                    // handle body
                    let body = self.resolve(&arm.body, Some(&sort));

                    // construct and register the case
                    cases.push(MatchCase { atoms, body });
                }
                Expression::Match { cases }
            }
            Op::Phi { nodes, default } => {
                let converted_default = self.resolve(default, Some(&sort));
                let mut cases = vec![];
                for node in nodes {
                    let cond = self.resolve(&node.cond, Some(&Sort::Boolean));
                    let body = self.resolve(&node.body, Some(&sort));
                    cases.push(PhiCase { cond, body });
                }
                Expression::Phi {
                    cases,
                    default: converted_default,
                }
            }
            Op::Forall { vars, body } => {
                let mut free_vars = BTreeMap::new();
                for (var_name, var_tag) in vars {
                    let (vid, var_sort) = self.free_quant_var(var_name, var_tag);
                    free_vars.insert(vid, var_sort);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::Forall {
                    vars: free_vars,
                    body: converted_body,
                }
            }
            Op::Exists { vars, body } => {
                let mut free_vars = BTreeMap::new();
                for (var_name, var_tag) in vars {
                    let (vid, var_sort) = self.free_quant_var(var_name, var_tag);
                    free_vars.insert(vid, var_sort);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::Exists {
                    vars: free_vars,
                    body: converted_body,
                }
            }
            Op::Choose { vars, body } => {
                let mut axiom_vars = BTreeMap::new();
                let mut axiom_rets = vec![];
                for (var_name, var_tag) in vars {
                    let (vid, var_sort) = self.free_axiom_var(var_name, var_tag);
                    axiom_vars.insert(vid, var_sort);
                    axiom_rets.push(vid);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::Choose {
                    vars: axiom_vars,
                    body: converted_body,
                    rets: axiom_rets,
                }
            }
            Op::IterForall { vars, body } => {
                let mut free_vars = BTreeMap::new();
                for (var_name, var_host) in vars {
                    let eid = self.resolve(var_host, None);
                    let vid = self.iter_quant_var(var_name, eid);
                    free_vars.insert(vid, eid);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::IterForall {
                    vars: free_vars,
                    body: converted_body,
                }
            }
            Op::IterExists { vars, body } => {
                let mut free_vars = BTreeMap::new();
                for (var_name, var_host) in vars {
                    let eid = self.resolve(var_host, None);
                    let vid = self.iter_quant_var(var_name, eid);
                    free_vars.insert(vid, eid);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::IterExists {
                    vars: free_vars,
                    body: converted_body,
                }
            }
            Op::IterChoose { vars, body } => {
                let mut axiom_vars = BTreeMap::new();
                let mut axiom_rets = vec![];
                for (var_name, var_host) in vars {
                    let eid = self.resolve(var_host, None);
                    let vid = self.iter_axiom_var(var_name, eid);
                    axiom_vars.insert(vid, eid);
                    axiom_rets.push(vid);
                }
                let converted_body = self.resolve(body, Some(&Sort::Boolean));
                Expression::IterChoose {
                    vars: axiom_vars,
                    body: converted_body,
                    rets: axiom_rets,
                }
            }
            Op::Procedure { name, inst, args } => {
                let callee = self.parent.register_func(name, inst);
                let params = self.parent.ir.fn_registry.retrieve(callee).params.clone();
                if params.len() != args.len() {
                    panic!(
                        "callee argument number mismatch: expect {} | actual {}",
                        params.len(),
                        args.len()
                    );
                }

                let mut converted_args = vec![];
                for ((_, arg_sort), arg_expr) in params.into_iter().zip(args) {
                    let eid = self.resolve(arg_expr, Some(&arg_sort));
                    converted_args.push(eid);
                }
                Expression::Procedure {
                    callee,
                    args: converted_args,
                }
            }
            _ => todo!(),
        };

        // register the expression
        let eid = self.registry.register(expression);

        // cross-check type consistency again (this is a paranoid check)
        let derived_type = self.derive_type(eid);
        Self::check_sort(&derived_type, &sort);

        // restore the namespace
        self.namespace = old_namespace;

        // done
        eid
    }

    /// Derive type of an expression
    fn derive_type(&self, eid: ExpId) -> Sort {
        let sort = match self.registry.lookup_exp(eid) {
            Expression::Var(vid) => self.registry.lookup_var(*vid).sort.clone(),
            Expression::Pack { sort, elems: _ }
            | Expression::Tuple { sort, slots: _ }
            | Expression::Record { sort, fields: _ }
            | Expression::Enum {
                sort,
                branch: _,
                variant: _,
            } => Sort::User(*sort),
            Expression::AccessSlot { base, slot } => {
                let base_sort = self.derive_type(*base);
                let base_tuple = self.expect_type_tuple(Self::expect_sort_user(&base_sort));
                base_tuple.into_iter().nth(*slot).unwrap_or_else(|| {
                    panic!("type mismatch: no slot {} in tuple {}", slot, base_sort)
                })
            }
            Expression::AccessField { base, field } => {
                let base_sort = self.derive_type(*base);
                let mut base_record = self.expect_type_record(Self::expect_sort_user(&base_sort));
                base_record
                    .remove(field)
                    .unwrap_or_else(|| {
                        panic!("type mismatch: no field {} in record {}", field, base_sort)
                    })
                    .clone()
            }
            Expression::Match { cases } => {
                let mut case_sort = None;
                for case in cases {
                    let sort = self.derive_type(case.body);
                    match &case_sort {
                        None => {
                            case_sort = Some(sort);
                        }
                        Some(s) => Self::check_sort(s, &sort),
                    }
                }
                match case_sort {
                    None => panic!("expect at least one match arm"),
                    Some(sort) => sort,
                }
            }
            Expression::Phi { cases, default } => {
                if cases.is_empty() {
                    panic!("expect at least one phi case");
                }
                let case_sort = self.derive_type(*default);
                for case in cases {
                    let sort = self.derive_type(case.body);
                    Self::check_sort(&case_sort, &sort);
                }
                case_sort
            }
            Expression::Forall { .. }
            | Expression::Exists { .. }
            | Expression::IterForall { .. }
            | Expression::IterExists { .. } => Sort::Boolean,
            Expression::Choose {
                vars,
                body: _,
                rets,
            } => {
                let mut inst = vec![];
                for vid in rets {
                    match vars.get(vid) {
                        None => panic!("invalid axiom variable to return"),
                        Some(sort) => {
                            inst.push(sort.clone());
                        }
                    }
                }
                let sid = self.parent.lookup_type(None, &inst);
                Sort::User(sid)
            }
            Expression::IterChoose {
                vars,
                body: _,
                rets,
            } => {
                let mut inst = vec![];
                for vid in rets {
                    match vars.get(vid) {
                        None => panic!("invalid axiom variable to return"),
                        Some(eid) => {
                            inst.push(self.derive_type(*eid));
                        }
                    }
                }
                let sid = self.parent.lookup_type(None, &inst);
                Sort::User(sid)
            }
            Expression::Procedure { callee, args: _ } => {
                self.parent.ir.fn_registry.retrieve(*callee).ret_ty.clone()
            }
            _ => todo!(),
        };
        sort
    }

    /// Materialize the entire function (signature + body, if any)
    pub fn materialize(
        mut parent: IRBuilder<'a, 'ctx>,
        params: Vec<(&VarName, Sort)>,
        ret_ty: Sort,
        body: Option<&Expr>,
    ) -> Function {
        let params: Vec<_> = params.into_iter().map(|(n, t)| (n.into(), t)).collect();
        let body = match body {
            None => None,
            Some(expr) => {
                // initialize the registry and builder
                let mut registry = ExpRegistry::new();
                let mut builder = ExpBuilder::new(&mut parent, &mut registry, &params);

                // build the expression
                let id = builder.resolve(expr, Some(&ret_ty));

                // done
                Some((registry, id))
            }
        };
        Function {
            params,
            ret_ty,
            body,
        }
    }
}
