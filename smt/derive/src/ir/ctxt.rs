use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};

use crate::ir::sort::{SmtSortName, Sort};
use crate::parser::ctxt::{ContextWithFunc, Refinement};
use crate::parser::infer::TypeRef;
use crate::parser::name::TypeParamName;

/// A context manager for building around a refinement relation
pub struct IRBuilder<'a> {
    /// context provider
    ctxt: &'a ContextWithFunc,
    /// type arguments
    ty_args: BTreeMap<TypeParamName, SmtSortName>,
}

impl<'a> IRBuilder<'a> {
    /// Create a new IR builder
    pub fn new(ctxt: &'a ContextWithFunc) -> Self {
        Self {
            ctxt,
            ty_args: BTreeMap::new(),
        }
    }

    /// Initialize it with a new refinement relation
    pub fn build(mut self, rel: &Refinement) -> Result<()> {
        // get the pair
        let (fn_impl, fn_spec) = self.ctxt.get_relation(rel);

        // initialize uninterpreted sorts
        let generics_impl = &fn_impl.head.generics.params;
        let generics_spec = &fn_spec.head.generics.params;
        if generics_impl != generics_spec {
            bail!("generics mismatch");
        }
        for ty_param in generics_impl {
            let sort_name = SmtSortName::new(ty_param);
            match self.ty_args.insert(ty_param.clone(), sort_name) {
                None => (),
                Some(_) => bail!("duplicated type parameter {}", ty_param),
            }
        }

        // process function parameters
        let params_impl = &fn_impl.head.params;
        let params_spec = &fn_impl.head.params;
        if params_impl.len() != params_spec.len() {
            bail!("parameter mismatch");
        }
        for ((_, param_impl), (_, param_spec)) in params_impl.iter().zip(params_spec) {
            if param_impl != param_spec {
                bail!("parameter mismatch");
            }
        }

        // done
        Ok(())
    }

    /// Register a type ref to the builder and pull its dependencies into the builder if needed
    fn register_type(&mut self, ty: &TypeRef) -> Result<Sort> {
        let sort = match ty {
            TypeRef::Var(_) => bail!("incomplete type"),
            TypeRef::Boolean => Sort::Boolean,
            TypeRef::Integer => Sort::Integer,
            TypeRef::Rational => Sort::Rational,
            TypeRef::Text => Sort::Text,
            TypeRef::Cloak(sub) => {
                // unwrap the cloak
                self.register_type(sub.as_ref())?
            }
            TypeRef::Seq(sub) => Sort::Seq(self.register_type(sub.as_ref())?.into()),
            TypeRef::Set(sub) => Sort::Set(self.register_type(sub.as_ref())?.into()),
            TypeRef::Map(key, val) => Sort::Map(
                self.register_type(key.as_ref())?.into(),
                self.register_type(val.as_ref())?.into(),
            ),
            TypeRef::Error => Sort::Error,
            TypeRef::User(..) => todo!(),
            TypeRef::Pack(elems) => Sort::Pack(
                elems
                    .iter()
                    .map(|e| self.register_type(e))
                    .collect::<Result<_>>()?,
            ),
            TypeRef::Parameter(name) => {
                let sort_name = self
                    .ty_args
                    .get(name)
                    .ok_or_else(|| anyhow!("no such type parameter {}", name))?;
                Sort::Uninterpreted(sort_name.clone())
            }
        };
        Ok(sort)
    }
}
