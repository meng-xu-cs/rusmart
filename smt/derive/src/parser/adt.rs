use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

use itertools::Itertools;
use syn::{ExprMatch, ExprPath, FieldPat, Member, Pat, PatOr, PatStruct, PatTupleStruct, Result};

use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::{CtxtForExpr, Expr, MatchCombo, MatchVariant, Unpack};
use crate::parser::generics::GenericsInstFull;
use crate::parser::infer::{ti_unify, TypeRef, TypeUnifier};
use crate::parser::name::{UsrTypeName, VarName};
use crate::parser::path::ADTPath;
use crate::parser::ty::EnumVariant;

/// An identifier for a ADT variant
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ADTBranch {
    pub ty_name: UsrTypeName,
    pub variant: String,
}

impl Display for ADTBranch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.ty_name, self.variant)
    }
}

/// An atom for a specific variable in the match head
pub enum MatchAtom {
    Default,
    Binding(BTreeMap<ADTBranch, Unpack>),
}

/// A full match arm for all variables in the match head
struct MatchArm {
    atoms: Vec<MatchAtom>,
    body: Expr,
}

/// An analyzer for match expression
pub struct MatchAnalyzer;

impl MatchAnalyzer {
    /// Analyze a pattern for: match arm -> head -> case -> binding
    fn analyze_pat_match_binding(pat: &Pat) -> Result<Option<VarName>> {
        let binding = match pat {
            Pat::Wild(_) => None,
            _ => Some(pat.try_into()?),
        };
        Ok(binding)
    }

    /// Analyze a pattern for: match arm -> head -> case
    fn analyze_pat_match_case<T: CtxtForExpr>(
        ctxt: &T,
        unifier: &mut TypeUnifier,
        pat: &Pat,
    ) -> Result<(
        ADTBranch,
        GenericsInstFull,
        Unpack,
        BTreeMap<VarName, TypeRef>,
    )> {
        let mut bindings = BTreeMap::new();

        let (branch, inst, unpack) = match pat {
            Pat::Path(pat_path) => {
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = pat_path;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));

                let adt = ADTPath::from_path(ctxt, path)?;
                let (branch, inst) = adt.complete(unifier);
                let variant = match ctxt.get_adt_variant_details(&branch) {
                    None => bail_on!(path, "not a valid enum branch"),
                    Some(def) => def,
                };

                match variant {
                    EnumVariant::Unit => (),
                    _ => bail_on!(pat, "unexpected pattern"),
                }
                (branch, inst, Unpack::Unit)
            }
            Pat::TupleStruct(pat_tuple) => {
                let PatTupleStruct {
                    attrs: _,
                    qself,
                    path,
                    paren_token: _,
                    elems,
                } = pat_tuple;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));

                let adt = ADTPath::from_path(ctxt, path)?;
                let (branch, inst) = adt.complete(unifier);
                let variant = match ctxt.get_adt_variant_details(&branch) {
                    None => bail_on!(path, "not a valid enum branch"),
                    Some(def) => def,
                };

                match variant {
                    EnumVariant::Tuple(def_tuple) => {
                        let slots = &def_tuple.slots;
                        if elems.len() != slots.len() {
                            bail_on!(elems, "number of slots mismatch");
                        }

                        let mut unpack = BTreeMap::new();
                        for (i, (elem, slot)) in elems.iter().zip(slots.iter()).enumerate() {
                            match Self::analyze_pat_match_binding(elem)? {
                                None => (),
                                Some(var) => {
                                    let ty_substitute = match inst.instantiate(slot) {
                                        None => bail_on!(elem, "no such type parameter"),
                                        Some(instantiated) => instantiated,
                                    };
                                    match bindings.insert(var.clone(), ty_substitute) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(elem, "duplicated name");
                                        }
                                    }
                                    unpack.insert(i, var);
                                }
                            }
                        }
                        (branch, inst, Unpack::Tuple(unpack))
                    }
                    _ => bail_on!(pat, "unexpected pattern"),
                }
            }
            Pat::Struct(pat_struct) => {
                let PatStruct {
                    attrs: _,
                    qself,
                    path,
                    brace_token: _,
                    fields,
                    rest,
                } = pat_struct;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));
                bail_if_exists!(rest);

                let adt = ADTPath::from_path(ctxt, path)?;
                let (branch, inst) = adt.complete(unifier);
                let variant = match ctxt.get_adt_variant_details(&branch) {
                    None => bail_on!(path, "not a valid enum branch"),
                    Some(def) => def,
                };

                match variant {
                    EnumVariant::Record(def_record) => {
                        let records = &def_record.fields;
                        if fields.len() != records.len() {
                            bail_on!(fields, "number of fields mismatch");
                        }

                        let mut unpack = BTreeMap::new();
                        for field in fields {
                            let FieldPat {
                                attrs: _,
                                member,
                                colon_token: _,
                                pat,
                            } = field;
                            let field_name = match member {
                                Member::Named(name) => name.to_string(),
                                Member::Unnamed(_) => bail_on!(member, "unnamed field"),
                            };

                            let field_type = match records.get(&field_name) {
                                None => bail_on!(member, "no such field"),
                                Some(t) => t,
                            };
                            let ty_substitute = match inst.instantiate(field_type) {
                                None => bail_on!(member, "no such type parameter"),
                                Some(instantiated) => instantiated,
                            };

                            match Self::analyze_pat_match_binding(pat)? {
                                None => (),
                                Some(var) => {
                                    match bindings.insert(var.clone(), ty_substitute) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(pat, "duplicated name");
                                        }
                                    }
                                    unpack.insert(field_name, var);
                                }
                            }
                        }
                        (branch, inst, Unpack::Record(unpack))
                    }
                    _ => bail_on!(pat, "unexpected pattern"),
                }
            }
            _ => bail_on!(pat, "invalid case pattern"),
        };

        Ok((branch, inst, unpack, bindings))
    }

    /// Analyze a pattern for: match arm -> head
    pub fn analyze_pat_match_head<T: CtxtForExpr>(
        ctxt: &T,
        unifier: &mut TypeUnifier,
        ety: &TypeRef,
        pat: &Pat,
    ) -> Result<(MatchAtom, BTreeMap<VarName, TypeRef>)> {
        let (atom, bindings) = match pat {
            Pat::Wild(_) => (MatchAtom::Default, BTreeMap::new()),
            Pat::Or(pat_or) => {
                let PatOr {
                    attrs: _,
                    leading_vert,
                    cases,
                } = pat_or;
                bail_if_exists!(leading_vert);

                let mut variants = BTreeMap::new();
                let mut iter = cases.iter();
                let pat_case = bail_if_missing!(iter.next(), pat_or, "case patterns");

                // analyze the bindings
                let (branch, inst, unpack, ref_bindings) =
                    Self::analyze_pat_match_case(ctxt, unifier, pat_case)?;

                // unify the type
                let ty_ref = inst.make_ty(branch.ty_name.clone());
                ti_unify!(unifier, ety, &ty_ref, pat_case);

                // save it
                if variants.insert(branch, unpack).is_some() {
                    bail_on!(cases, "duplicated adt variant");
                }

                for pat_case in iter.by_ref() {
                    // analyze the bindings
                    let (branch, inst, unpack, new_bindings) =
                        Self::analyze_pat_match_case(ctxt, unifier, pat_case)?;

                    // unify the type
                    let ty_ref = inst.make_ty(branch.ty_name.clone());
                    ti_unify!(unifier, ety, &ty_ref, pat_case);

                    // check binding consistency
                    if ref_bindings != new_bindings {
                        bail_on!(pat_case, "case patterns do not bind the same variable set");
                    }
                    // save it
                    if variants.insert(branch, unpack).is_some() {
                        bail_on!(cases, "duplicated adt variant");
                    }
                }

                // done
                (MatchAtom::Binding(variants), ref_bindings)
            }
            _ => {
                // analyze the bindings
                let (branch, inst, unpack, bindings) =
                    Self::analyze_pat_match_case(ctxt, unifier, pat)?;

                // unify the type
                let ty_ref = inst.make_ty(branch.ty_name.clone());
                ti_unify!(unifier, ety, &ty_ref, pat);

                // done
                let variants = std::iter::once((branch, unpack)).collect();
                (MatchAtom::Binding(variants), bindings)
            }
        };
        Ok((atom, bindings))
    }
}

/// An organizer for the match arms
pub struct MatchOrganizer {
    arms: Vec<MatchArm>,
}

impl MatchOrganizer {
    /// Create a new organizer
    pub fn new() -> Self {
        Self { arms: vec![] }
    }

    /// Add a match arm
    pub fn add_arm(&mut self, atoms: Vec<MatchAtom>, body: Expr) {
        self.arms.push(MatchArm { atoms, body })
    }

    /// Organize the arms into per-combo-by-permutation format
    pub fn into_organized(
        self,
        expr: &ExprMatch,
        heads: &[(UsrTypeName, BTreeMap<String, Unpack>)],
    ) -> Result<Vec<MatchCombo>> {
        // utility enum to indicate whether a match combo is concrete or abstract
        enum MatchComboStatus {
            None,
            Abstract(Vec<(usize, MatchCombo)>),
            Concrete(usize, MatchCombo),
        }

        // tracks how many combo are mapped to each arm
        let mut map_arms = BTreeMap::new();

        // sanity check, plus initialize the tracking
        for (i, arm) in self.arms.iter().enumerate() {
            map_arms.insert(i, 0_usize);
            if arm.atoms.len() != heads.len() {
                bail_on!(expr, "atoms and heads number mismatch");
            }
            for (atom, (adt_name, adt_variants)) in arm.atoms.iter().zip(heads.iter()) {
                match atom {
                    MatchAtom::Default => (),
                    MatchAtom::Binding(binding) => {
                        for branch in binding.keys() {
                            if adt_name != &branch.ty_name {
                                bail_on!(expr, "atoms and heads ADT name mismatch");
                            }
                            if !adt_variants.contains_key(&branch.variant) {
                                bail_on!(expr, "atoms and heads ADT variant mismatch");
                            }
                        }
                    }
                }
            }
        }

        // list all the combo
        let mut all_combinations = vec![];
        for combo in heads
            .iter()
            .map(|(_, names)| names.iter())
            .multi_cartesian_product()
        {
            // sanity check
            assert_eq!(combo.len(), heads.len());

            // conversion
            let combo_as_branch: Vec<_> = combo
                .into_iter()
                .zip(heads.iter())
                .map(|((variant_name, variant_default), (type_name, _))| {
                    let branch = ADTBranch {
                        ty_name: type_name.clone(),
                        variant: variant_name.clone(),
                    };
                    (branch, variant_default)
                })
                .collect();

            // go over each arm and check which is the match
            let mut found = MatchComboStatus::None;
            for (i, arm) in self.arms.iter().enumerate() {
                let mut variants = vec![];
                let mut is_matched = true;
                let mut is_abstract = false;
                for ((combo_branch, default_unpack), arm_atom) in
                    combo_as_branch.iter().zip(arm.atoms.iter())
                {
                    let combo_unpack = match arm_atom {
                        MatchAtom::Default => {
                            is_abstract = true;
                            *default_unpack
                        }
                        MatchAtom::Binding(binding) => match binding.get(combo_branch) {
                            None => {
                                is_matched = false;
                                break;
                            }
                            Some(unpack) => unpack,
                        },
                    };
                    let variant = MatchVariant {
                        branch: combo_branch.clone(),
                        unpack: combo_unpack.clone(),
                    };
                    variants.push(variant);
                }

                // check if everything matches
                if !is_matched {
                    continue;
                }

                // assign the combo
                let combo = MatchCombo {
                    variants,
                    body: arm.body.clone(),
                };
                if is_abstract {
                    match found {
                        MatchComboStatus::None => {
                            found = MatchComboStatus::Abstract(vec![(i, combo)]);
                        }
                        MatchComboStatus::Abstract(existing) => {
                            found = MatchComboStatus::Abstract(
                                existing
                                    .into_iter()
                                    .chain(std::iter::once((i, combo)))
                                    .collect(),
                            )
                        }
                        MatchComboStatus::Concrete(..) => {
                            // do nothing, a concrete match takes priority
                        }
                    }
                } else {
                    // if two concrete arms match to the same combo, raise an error
                    if matches!(found, MatchComboStatus::Concrete(..)) {
                        bail_on!(expr, "two concrete match arms handles the same combination");
                    }
                    found = MatchComboStatus::Concrete(i, combo);
                }
            }

            // ensure that each combo is handled by one and only one match arm
            let (i, combo) = match found {
                MatchComboStatus::None => bail_on!(expr, "no match arms handles a combination"),
                MatchComboStatus::Abstract(candidates) => {
                    if candidates.len() != 1 {
                        bail_on!(expr, "ambiguous abstract match arms");
                    }
                    candidates.into_iter().next().unwrap()
                }
                MatchComboStatus::Concrete(i, combo) => (i, combo),
            };

            all_combinations.push(combo);
            map_arms.entry(i).and_modify(|c| *c += 1);
        }

        // check that every match arm is useful
        if map_arms.values().any(|v| *v == 0) {
            bail_on!(expr, "unused match arms");
        }

        Ok(all_combinations)
    }
}
