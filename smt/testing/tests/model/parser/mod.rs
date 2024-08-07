mod adt_enum_ok;
mod adt_enum_variant_record_no_fields;
mod adt_enum_variant_tuple_no_slots;
mod adt_struct_no_fields;
mod adt_struct_ok;
mod adt_tuple_no_slots;
mod adt_tuple_ok;
mod attr_ok;
mod axiom_not_boolean_return;
mod axiom_ok;
mod duplicated_axiom;
mod duplicated_impl;
mod duplicated_spec;
mod duplicated_type;
mod expr_call_default_by_sys_ty;
mod expr_call_default_by_ty_param;
mod expr_call_default_by_usr_ty;
mod expr_call_func_cast_ok;
mod expr_call_func_plain_nonexist;
mod expr_call_func_plain_ok;
mod expr_call_sys_func_ok;
mod expr_enum_ok;
mod expr_field_ok;
mod expr_if_let;
mod expr_if_not_bool_deref;
mod expr_if_ok;
mod expr_local_bind_enum_unpack;
mod expr_local_bind_paren;
mod expr_local_bind_struct_unpack;
mod expr_local_ok;
mod expr_local_type_array;
mod expr_local_type_infer;
mod expr_local_type_paren;
mod expr_local_type_ref;
mod expr_local_without_binding;
mod expr_match_if_in_arm;
mod expr_match_naming_conflict_field;
mod expr_match_naming_conflict_slot;
mod expr_match_ok;
mod expr_match_vague_arm;
mod expr_match_wildcard_arm;
mod expr_method_sys_func_ok;
mod expr_pack_ok;
mod expr_paren_ok;
mod expr_path_ok;
mod expr_path_with_namespace;
mod expr_struct_ok;
mod func_no_return_type;
mod generics_ok;
mod method_derive_ok;
mod name_conflict_on_impl_and_spec;
mod name_conflict_on_type_and_axiom;
mod stmt_naming_conflict_local_local;
mod stmt_naming_conflict_local_param;
mod stmt_naming_conflict_scope;
mod stmt_ok;
mod type_never;
mod type_not_smt_in_adt;
mod type_not_smt_in_func;
mod type_paran_in_adt;
mod type_paran_in_func;
mod vc_impl_target_invalid;
mod vc_pair_incompatible;
mod vc_spec_target_invalid;
