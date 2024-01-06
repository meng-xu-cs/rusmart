use std::collections::{BTreeMap, BTreeSet};

use petgraph::algo::tarjan_scc;
use petgraph::graph::{DiGraph, NodeIndex};

use crate::ir::index::UsrSortId;
use crate::ir::sort::{DataType, Sort, TypeRegistry, Variant};

/// One SCC in the type dependency chain
pub enum SortDep {
    /// A single type
    Simple(UsrSortId),
    /// An inductively defined type or type group
    Inductive(BTreeSet<UsrSortId>),
}

/// Dependency graph around the user-defined data types
struct SortDepGraph<'a> {
    registry: &'a TypeRegistry,
    nodes: BTreeMap<UsrSortId, NodeIndex>,
    graph: DiGraph<UsrSortId, ()>,
}

impl<'a> SortDepGraph<'a> {
    /// Create an empty dependency graph
    pub fn new(registry: &'a TypeRegistry) -> Self {
        Self {
            registry,
            nodes: BTreeMap::new(),
            graph: DiGraph::new(),
        }
    }

    /// Add a user-defined data type to the graph, together with all its dependencies
    pub fn add(&mut self, sid: UsrSortId) {
        if !self.nodes.contains_key(&sid) {
            self.build_with_dfs(sid);
        }
    }

    /// Populate the dependency graph with a depth-first approach
    fn build_with_dfs(&mut self, sid: UsrSortId) -> NodeIndex {
        // build the node
        let src_id = self.graph.add_node(sid);
        let exists = self.nodes.insert(sid, src_id);
        assert!(exists.is_none());

        // build the edges (if not exist) or grab the destination node index if already explored
        let dt = self.registry.retrieve(sid);

        let mut deps = BTreeSet::new();
        direct_deps_datatype(dt, &mut deps);

        for dep in deps {
            let dst_id = match self.nodes.get(&dep) {
                None => self.build_with_dfs(dep),
                Some(dst_id) => *dst_id,
            };
            self.graph.add_edge(src_id, dst_id, ());
        }

        // done with the building
        src_id
    }

    /// Derive the type dependency chain
    pub fn analyze(&self) -> Vec<SortDep> {
        let mut steps = vec![];
        for scc in tarjan_scc(&self.graph) {
            assert!(!scc.is_empty());
            let step = if scc.len() == 1 {
                let idx = scc.into_iter().next().unwrap();
                let sid = *self.graph.node_weight(idx).unwrap();
                if self.graph.contains_edge(idx, idx) {
                    // self-referencing inductive type
                    SortDep::Inductive(std::iter::once(sid).collect())
                } else {
                    // simple and plain type definition
                    SortDep::Simple(sid)
                }
            } else {
                // a multi-node SCC is always a mutually-inductive group
                let names = scc
                    .into_iter()
                    .map(|i| *self.graph.node_weight(i).unwrap())
                    .collect();
                SortDep::Inductive(names)
            };
            steps.push(step);
        }
        steps
    }
}

/// List dependencies on user-defined types of this data type
fn direct_deps_datatype(dt: &DataType, deps: &mut BTreeSet<UsrSortId>) {
    match dt {
        DataType::Tuple(slots) => {
            for sort in slots {
                direct_deps_sort(sort, deps);
            }
        }
        DataType::Record(fields) => {
            for sort in fields.values() {
                direct_deps_sort(sort, deps);
            }
        }
        DataType::Enum(variants) => {
            for variant in variants.values() {
                match variant {
                    Variant::Unit => (),
                    Variant::Tuple(slots) => {
                        for sort in slots {
                            direct_deps_sort(sort, deps);
                        }
                    }
                    Variant::Record(fields) => {
                        for sort in fields.values() {
                            direct_deps_sort(sort, deps);
                        }
                    }
                }
            }
        }
    }
}

fn direct_deps_sort(sort: &Sort, deps: &mut BTreeSet<UsrSortId>) {
    match sort {
        Sort::Boolean
        | Sort::Integer
        | Sort::Rational
        | Sort::Text
        | Sort::Error
        | Sort::Uninterpreted(_) => (),
        Sort::Seq(sub) | Sort::Set(sub) => {
            direct_deps_sort(sub.as_ref(), deps);
        }
        Sort::Map(key, val) => {
            direct_deps_sort(key.as_ref(), deps);
            direct_deps_sort(val.as_ref(), deps);
        }
        Sort::User(sid) => {
            deps.insert(*sid);
        }
    }
}

/// Topologically order the user-defined types collected in type registry
pub fn topological_order(registry: &TypeRegistry) -> Vec<SortDep> {
    let mut graph = SortDepGraph::new(registry);
    for sid in registry.data_types().keys() {
        graph.add(*sid);
    }
    graph.analyze()
}
