use std::collections::{BTreeSet, VecDeque};

use crate::parser::generics::{Generics, GenericsInstPartial, Monomorphization, PartialInst};
use crate::parser::infer::{TIError, TypeRef, TypeUnifier};

/// Unify two (partial) instantiations and see if new instantiations appear
fn self_interference(
    generics: &Generics,
    lhs: &Monomorphization,
    rhs: &Monomorphization,
) -> Option<Monomorphization> {
    // unify the arguments in an asymmetric setting
    let mut unifier = TypeUnifier::new();

    let base_lhs = GenericsInstPartial::new_with_mono(generics, lhs).complete(&mut unifier);
    let base_rhs = GenericsInstPartial::new_with_mono(generics, rhs).complete(&mut unifier);
    let inst_lhs = base_lhs.vec();
    let inst_rhs = base_rhs.vec();

    let mut unifies = true;
    for (lhs, rhs) in inst_lhs.iter().zip(inst_rhs.iter()) {
        match unifier.unify(&lhs, &rhs) {
            Ok(None) => {
                unifies = false;
                break;
            }
            Ok(_) => (),
            Err(TIError::CyclicUnification) => {
                panic!("type unification error: cyclic type unification")
            }
        }
    }
    if !unifies {
        return None;
    }

    // collect the unified results from both sides
    let ty_to_inst = |ty| {
        let refreshed = unifier.refresh_type(&ty);
        match refreshed.reverse() {
            None => {
                let var = match refreshed {
                    TypeRef::Var(v) => v,
                    _ => panic!("type parameter must be either assigned or variadic"),
                };
                let tp_name = match base_lhs.reverse(&var).or_else(|| base_rhs.reverse(&var)) {
                    None => panic!("unable to find the origin of type var {}", var),
                    Some((n, _)) => n.clone(),
                };
                PartialInst::Unassigned(tp_name)
            }
            Some(tag) => PartialInst::Assigned(tag),
        }
    };

    let refreshed_lhs: Vec<_> = inst_lhs.into_iter().map(ty_to_inst).collect();
    let refreshed_rhs: Vec<_> = inst_rhs.into_iter().map(ty_to_inst).collect();

    // sanity check before returning the new instance
    if refreshed_lhs != refreshed_rhs {
        panic!("monomorphization of two partial instantiations yields different results");
    }
    Some(Monomorphization {
        args: refreshed_lhs,
    })
}

/// Probe for more instantiations to add
fn probe_instantiations(
    generics: &Generics,
    existing: &BTreeSet<Monomorphization>,
    addition: &Monomorphization,
    extended: &mut VecDeque<Monomorphization>,
) {
    for inst in existing {
        match self_interference(generics, addition, inst) {
            None => continue,
            Some(mono) => {
                if !existing.contains(&mono) && !extended.contains(&mono) {
                    extended.push_back(mono);
                }
            }
        }
    }
}

/// Add a new (partial) instantiation to the set
pub fn add_instantiation(
    generics: &Generics,
    existing: &mut BTreeSet<Monomorphization>,
    addition: Monomorphization,
) -> Vec<Monomorphization> {
    // nothing to add if this mono is already processed
    if existing.contains(&addition) {
        return vec![];
    }

    let mut extended = VecDeque::new();
    extended.push_back(addition);

    // loop until nothing left in the queue
    let mut incremental = vec![];
    while !extended.is_empty() {
        let inst = extended.pop_front().unwrap();
        probe_instantiations(generics, existing, &inst, &mut extended);
        existing.insert(inst.clone());
        incremental.push(inst);
    }
    incremental
}
