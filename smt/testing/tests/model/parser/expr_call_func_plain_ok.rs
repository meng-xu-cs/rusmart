use rusmart_smt_remark_derive::{smt_impl, smt_type};
use rusmart_smt_stdlib::{Boolean, SMT};

#[smt_impl]
fn foo() -> Boolean {
    Boolean::from(false)
}

#[smt_impl]
fn bar() -> Boolean {
    foo()
}

#[smt_type]
struct S(Boolean);

#[smt_impl]
fn wrap_t_generic<T: SMT>(t: T) -> T {
    t
}

#[smt_impl]
fn call_wrap_t_sys_ty_no_arg() -> Boolean {
    wrap_t_generic(Boolean::from(false))
}

#[smt_impl]
fn call_wrap_t_sys_ty_with_arg() -> Boolean {
    wrap_t_generic::<Boolean>(Boolean::from(false))
}

#[smt_impl]
fn call_wrap_t_sys_ty_infer_arg() -> Boolean {
    wrap_t_generic::<_>(Boolean::from(false))
}

#[smt_impl]
fn call_wrap_t_usr_ty_no_arg() -> S {
    wrap_t_generic(S(Boolean::from(false)))
}

#[smt_impl]
fn call_wrap_t_usr_ty_with_arg() -> S {
    wrap_t_generic::<S>(S(Boolean::from(false)))
}

#[smt_impl]
fn call_wrap_t_usr_ty_infer_arg() -> S {
    wrap_t_generic::<_>(S(Boolean::from(false)))
}

#[smt_impl]
fn call_wrap_t_param_ty_no_arg<X: SMT>(x: X) -> X {
    wrap_t_generic(x)
}

#[smt_impl]
fn call_wrap_t_param_ty_with_arg<X: SMT>(x: X) -> X {
    wrap_t_generic::<X>(x)
}

#[smt_impl]
fn call_wrap_t_param_ty_infer_arg<X: SMT>(x: X) -> X {
    wrap_t_generic::<_>(x)
}
