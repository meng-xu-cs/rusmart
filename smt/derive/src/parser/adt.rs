use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use syn::{
    ExprMatch, ExprPath, FieldPat, Member, Pat, PatOr, PatStruct, PatTupleStruct, Path,
    PathArguments, PathSegment, Result,
};

use crate::parser::err::{bail_if_exists, bail_if_missing, bail_on};
use crate::parser::expr::{CtxtForExpr, Expr, ExprParserRoot, MatchCombo, MatchVariant, Unpack};
use crate::parser::name::{UsrTypeName, VarName};
use crate::parser::ty::{EnumVariant, TypeBody, TypeTag};
use crate::parser::util::PatUtil;

/// An identifier for a ADT variant
#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct ADTBranch {
    adt: UsrTypeName,
    branch: String,
}

impl ADTBranch {
    /// Build manually
    pub fn new(adt: UsrTypeName, branch: String) -> Self {
        Self { adt, branch }
    }

    /// Extract a call target from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let Path {
            leading_colon,
            segments,
        } = path;
        bail_if_exists!(leading_colon);

        let mut iter = segments.iter().rev();
        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "branch");
        if !matches!(arguments, PathArguments::None) {
            bail_on!(arguments, "unexpected arguments");
        }
        let variant_name = ident.to_string();

        let PathSegment { ident, arguments } = bail_if_missing!(iter.next(), path, "type");
        if !matches!(arguments, PathArguments::None) {
            bail_on!(arguments, "unexpected arguments");
        }
        let type_name = ident.try_into()?;
        bail_if_exists!(iter.next());

        // done
        Ok(Self {
            adt: type_name,
            branch: variant_name,
        })
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
pub struct MatchAnalyzer<'a, 'ctx: 'a> {
    /// context provider
    ctxt: &'a ExprParserRoot<'ctx>,
    /// An accumulation of match arms
    arms: Vec<MatchArm>,
}

impl<'a, 'ctx: 'a> MatchAnalyzer<'a, 'ctx> {
    /// Creating a new context for analyzing a match expression
    pub fn new(ctxt: &'a ExprParserRoot<'ctx>) -> Self {
        Self { ctxt, arms: vec![] }
    }

    /// Retrieve a type variant by path
    fn get_adt_variant_by_path(&self, path: &Path) -> Result<(ADTBranch, &EnumVariant)> {
        let branch = ADTBranch::from_path(path)?;

        // look-up
        let type_body = match self.ctxt.get_type_def(&branch.adt) {
            None => bail_on!(path, "no such type"),
            Some(def) => def.body(),
        };

        let variant_def = match type_body {
            TypeBody::Enum(adt) => match adt.variants().get(&branch.branch) {
                None => bail_on!(path, "no such variant"),
                Some(variant) => variant,
            },
            _ => bail_on!(path, "not an ADT"),
        };

        // done
        Ok((branch, variant_def))
    }

    /// Analyze a pattern for: match arm -> head -> case -> binding
    fn analyze_pat_match_binding(&self, pat: &Pat) -> Result<Option<VarName>> {
        let binding = match pat {
            Pat::Wild(_) => None,
            _ => Some(PatUtil::expect_name(pat)?),
        };
        Ok(binding)
    }

    /// Analyze a pattern for: match arm -> head -> case
    fn analyze_pat_match_case(
        &self,
        pat: &Pat,
    ) -> Result<(ADTBranch, Unpack, BTreeMap<VarName, TypeTag>)> {
        let mut bindings = BTreeMap::new();

        let (branch, unpack) = match pat {
            Pat::Path(pat_path) => {
                let ExprPath {
                    attrs: _,
                    qself,
                    path,
                } = pat_path;
                bail_if_exists!(qself.as_ref().map(|q| &q.ty));

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    EnumVariant::Unit => (),
                    _ => bail_on!(pat, "unexpected pattern"),
                }
                (branch, Unpack::Unit)
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

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    EnumVariant::Tuple(def_tuple) => {
                        let slots = def_tuple.slots();
                        if elems.len() != slots.len() {
                            bail_on!(elems, "number of slots mismatch");
                        }

                        let mut unpack = BTreeMap::new();
                        for (i, (elem, slot)) in elems.iter().zip(slots.iter()).enumerate() {
                            match self.analyze_pat_match_binding(elem)? {
                                None => (),
                                Some(var) => {
                                    match bindings.insert(var.clone(), slot.clone()) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(elem, "duplicated name");
                                        }
                                    }
                                    unpack.insert(i, var);
                                }
                            }
                        }
                        (branch, Unpack::Tuple(unpack))
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

                let (branch, variant) = self.get_adt_variant_by_path(path)?;
                match variant {
                    EnumVariant::Record(def_record) => {
                        let records = def_record.fields();
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

                            match self.analyze_pat_match_binding(pat)? {
                                None => (),
                                Some(var) => {
                                    match bindings.insert(var.clone(), field_type.clone()) {
                                        None => (),
                                        Some(_) => {
                                            bail_on!(pat, "duplicated name");
                                        }
                                    }
                                    unpack.insert(field_name, var);
                                }
                            }
                        }
                        (branch, Unpack::Record(unpack))
                    }
                    _ => bail_on!(pat, "unexpected pattern"),
                }
            }
            _ => bail_on!(pat, "invalid case pattern"),
        };

        Ok((branch, unpack, bindings))
    }

    /// Analyze a pattern for: match arm -> head
    pub fn analyze_pat_match_head(
        &self,
        pat: &Pat,
    ) -> Result<(MatchAtom, BTreeMap<VarName, TypeTag>)> {
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
                let (branch, unpack, ref_bindings) = self.analyze_pat_match_case(pat_case)?;
                if variants.insert(branch, unpack).is_some() {
                    bail_on!(cases, "duplicated adt variant");
                }

                for pat_case in iter.by_ref() {
                    let (branch, unpack, new_bindings) = self.analyze_pat_match_case(pat_case)?;
                    if variants.insert(branch, unpack).is_some() {
                        bail_on!(cases, "duplicated adt variant");
                    }
                    if ref_bindings != new_bindings {
                        bail_on!(pat_case, "case patterns do not bind the same variable set");
                    }
                }

                (MatchAtom::Binding(variants), ref_bindings)
            }
            _ => {
                let (branch, unpack, bindings) = self.analyze_pat_match_case(pat)?;
                (
                    MatchAtom::Binding(std::iter::once((branch, unpack)).collect()),
                    bindings,
                )
            }
        };
        Ok((atom, bindings))
    }

    /// Add a match arm
    pub fn add_arm(&mut self, atoms: Vec<MatchAtom>, body: Expr) {
        self.arms.push(MatchArm { atoms, body })
    }

    /// Organize the arms into per-combo-by-permutation format
    pub fn into_organized(
        self,
        expr: &ExprMatch,
        heads: &[(UsrTypeName, BTreeSet<String>)],
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
                            if adt_name != &branch.adt {
                                bail_on!(expr, "atoms and heads ADT name mismatch");
                            }
                            if !adt_variants.contains(&branch.branch) {
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
                .map(|(variant_name, (type_name, _))| {
                    ADTBranch::new(type_name.clone(), variant_name.clone())
                })
                .collect();

            // go over each arm and check which is the match
            let mut found = MatchComboStatus::None;
            for (i, arm) in self.arms.iter().enumerate() {
                let mut variants = vec![];
                let mut is_matched = true;
                let mut is_abstract = false;
                for (combo_branch, arm_atom) in combo_as_branch.iter().zip(arm.atoms.iter()) {
                    match arm_atom {
                        MatchAtom::Default => {
                            is_abstract = true;
                        }
                        MatchAtom::Binding(binding) => match binding.get(combo_branch) {
                            None => {
                                is_matched = false;
                                break;
                            }
                            Some(unpack) => {
                                let variant = MatchVariant::new(
                                    combo_branch.adt.clone(),
                                    combo_branch.branch.to_string(),
                                    unpack.clone(),
                                );
                                variants.push(variant);
                            }
                        },
                    }
                }

                // check if everything matches
                if !is_matched {
                    continue;
                }

                // assign the combo
                let combo = MatchCombo::new(variants, arm.body.clone());
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
