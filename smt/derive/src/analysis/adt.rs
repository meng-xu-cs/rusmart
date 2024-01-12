use std::collections::{BTreeMap, BTreeSet};

use petgraph::algo::tarjan_scc;
use petgraph::graph::{DiGraph, NodeIndex};

use crate::ir::ctxt::IRContext;
use crate::ir::fun::FunDef;
use crate::ir::index::UsrSortId;
use crate::ir::sort::{DataType, Sort, TypeRegistry, Variant};

/// All potential ADTs fully specified out in SMT context
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
enum ExplicitADT {
    Simple(UsrSortId),
    Optional(Sort),
}

/// Dependency graph around explicit ADTs
struct ADTGraph<'a> {
    registry: &'a TypeRegistry,
    nodes: BTreeMap<ExplicitADT, NodeIndex>,
    graph: DiGraph<ExplicitADT, ()>,
}

impl<'a> ADTGraph<'a> {
    /// Create an empty dependency graph
    pub fn new(registry: &'a TypeRegistry) -> Self {
        Self {
            registry,
            nodes: BTreeMap::new(),
            graph: DiGraph::new(),
        }
    }

    /// Process a sort, make changes to the graph if not seen
    pub fn process(&mut self, sort: &Sort) {
        self.add_sort(sort);
    }

    /// Process a sort, make changes to the graph if not seen, return related indices
    fn add_sort(&mut self, sort: &Sort) -> BTreeSet<NodeIndex> {
        let mut related = BTreeSet::new();
        match sort {
            Sort::Boolean
            | Sort::Integer
            | Sort::Rational
            | Sort::Text
            | Sort::Error
            | Sort::Uninterpreted(_) => (),
            Sort::Seq(sub) | Sort::Set(sub) => {
                related.extend(self.add_sort(sub));
            }
            Sort::Map(key, val) => {
                related.extend(self.add_sort(key));
                related.insert(self.add_adt(ExplicitADT::Optional(val.as_ref().clone())));
            }
            Sort::User(sid) => {
                related.insert(self.add_adt(ExplicitADT::Simple(*sid)));
            }
        }
        related
    }

    /// Populate the dependency graph with a depth-first approach
    fn add_adt(&mut self, adt: ExplicitADT) -> NodeIndex {
        // check existence and short-circuit if we have seen it
        match self.nodes.get(&adt) {
            None => (),
            Some(idx) => return *idx,
        }

        // build the node
        let src_id = self.graph.add_node(adt.clone());
        self.nodes.insert(adt.clone(), src_id);

        // collect dependencies
        let deps = match adt {
            ExplicitADT::Simple(sid) => {
                let mut deps = BTreeSet::new();
                let dt = self.registry.retrieve(sid);
                match dt {
                    DataType::Tuple(slots) => {
                        for sort in slots {
                            deps.extend(self.add_sort(sort));
                        }
                    }
                    DataType::Record(fields) => {
                        for sort in fields.values() {
                            deps.extend(self.add_sort(sort));
                        }
                    }
                    DataType::Enum(variants) => {
                        for variant in variants.values() {
                            match variant {
                                Variant::Unit => (),
                                Variant::Tuple(slots) => {
                                    for sort in slots {
                                        deps.extend(self.add_sort(sort));
                                    }
                                }
                                Variant::Record(fields) => {
                                    for sort in fields.values() {
                                        deps.extend(self.add_sort(sort));
                                    }
                                }
                            }
                        }
                    }
                }
                deps
            }
            ExplicitADT::Optional(sort) => self.add_sort(&sort),
        };

        // build the edges
        for dst_id in deps {
            self.graph.add_edge(src_id, dst_id, ());
        }

        // done with the building
        src_id
    }

    /// Derive the type dependency chain
    pub fn analyze(&self) -> Vec<SortSCC> {
        let mut steps = vec![];
        for scc in tarjan_scc(&self.graph) {
            assert!(!scc.is_empty());
            let step = if scc.len() == 1 {
                let idx = scc.into_iter().next().unwrap();
                let adt = self.graph.node_weight(idx).unwrap().clone();
                if self.graph.contains_edge(idx, idx) {
                    // self-referencing inductive type
                    SortSCC::Inductive(std::iter::once(adt).collect())
                } else {
                    // simple and plain type definition
                    SortSCC::Simple(adt)
                }
            } else {
                // a multi-node SCC is always a mutually-inductive group
                let names = scc
                    .into_iter()
                    .map(|i| self.graph.node_weight(i).unwrap().clone())
                    .collect();
                SortSCC::Inductive(names)
            };
            steps.push(step);
        }
        steps
    }
}

/// One SCC in the type dependency chain
pub enum SortSCC {
    /// A single type
    Simple(ExplicitADT),
    /// An inductively defined type or type group
    Inductive(BTreeSet<ExplicitADT>),
}

impl IRContext {
    /// Utility on collecting all sorts that every appears in the IR context
    fn all_sorts(&self) -> BTreeSet<Sort> {
        let mut results = BTreeSet::new();

        // all user-defined data types
        for sid in self.ty_registry.data_types().keys() {
            results.insert(Sort::User(*sid));
        }

        // sorts appearing in functions
        for insts in self.fn_registry.lookup.values() {
            for (inst, fid) in insts {
                results.extend(inst.iter().cloned());

                let sig = self.fn_registry.retrieve_sig(*fid);
                results.extend(sig.params.iter().map(|(_, t)| t.clone()));
                results.insert(sig.ret_ty.clone());

                match self.fn_registry.retrieve_def(*fid) {
                    FunDef::Uninterpreted => (),
                    FunDef::Defined(exps, _) => {
                        todo!()
                    }
                }
            }
        }

        // sorts appearing in axioms
        for insts in self.axiom_registry.lookup.values() {
            for (inst, aid) in insts {
                results.extend(inst.iter().cloned());

                let axiom = self.axiom_registry.retrieve(*aid);
                results.extend(axiom.params.iter().map(|(_, t)| t.clone()));
                todo!()
            }
        }

        results
    }
}

/// Topologically order the user-defined types collected in type registry
pub fn sort_in_topological_order(ir: &IRContext) -> Vec<SortSCC> {
    let mut graph = ADTGraph::new(&ir.ty_registry);
    for sid in ir.ty_registry.data_types().keys() {
        graph.add_adt(ExplicitADT::Simple(*sid));
    }
    graph.analyze()
}
