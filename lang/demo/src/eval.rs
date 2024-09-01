use rusmart_smt_remark::{smt_axiom, smt_impl, smt_spec, smt_type};
use rusmart_smt_stdlib::dt::{Boolean, Cloak, Error, Integer, Map, SMT};

//
// Expression evaluation
//

#[smt_type]
pub enum Value {
    Null,
    Boolean(Boolean),
    Integer(Integer),
}

#[smt_type]
pub enum Expr {
    Undef,
    Value(Value),
    Error(Error),
}

#[smt_impl(method = lt)]
fn lt_value(lhs: Value, rhs: Value) -> Expr {
    match (lhs, rhs) {
        // null will make every operation null
        (Value::Null, Value::Null) => Expr::Value(Value::Null),
        (Value::Null, Value::Boolean(_)) => Expr::Value(Value::Null),
        (Value::Null, Value::Integer(_)) => Expr::Value(Value::Null),
        (Value::Boolean(_), Value::Null) => Expr::Value(Value::Null),
        (Value::Integer(_), Value::Null) => Expr::Value(Value::Null),

        // boolean leads to a type error
        (Value::Boolean(_), Value::Boolean(_)) => Expr::Error(Error::new()),
        (Value::Boolean(_), Value::Integer(_)) => Expr::Error(Error::new()),
        (Value::Integer(_), Value::Boolean(_)) => Expr::Error(Error::new()),

        // integer
        (Value::Integer(l), Value::Integer(r)) => Expr::Value(Value::Boolean(l.lt(r))),
    }
}

#[smt_impl(method = lt)]
pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
    match (lhs, rhs) {
        // undef will cause an error
        (Expr::Undef, Expr::Undef) => Expr::Error(Error::new()),
        (Expr::Undef, Expr::Value(_)) => Expr::Error(Error::new()),
        (Expr::Undef, Expr::Error(e)) => Expr::Error(Error::new().merge(e)),
        (Expr::Value(_), Expr::Undef) => Expr::Error(Error::new()),
        (Expr::Error(e), Expr::Undef) => Expr::Error(Error::new().merge(e)),

        // error will be propagated
        (Expr::Error(l), Expr::Error(r)) => Expr::Error(l.merge(r)),
        (Expr::Error(e), Expr::Value(_)) => Expr::Error(e),
        (Expr::Value(_), Expr::Error(e)) => Expr::Error(e),

        // value op is delegated
        (Expr::Value(l), Expr::Value(r)) => l.lt(r),
    }
}

#[smt_impl(method = le)]
fn le_value(lhs: Value, rhs: Value) -> Expr {
    match (lhs, rhs) {
        // null will make every operation null
        (Value::Null, _) => Expr::Value(Value::Null),
        (_, Value::Null) => Expr::Value(Value::Null),

        // integer
        (Value::Integer(l), Value::Integer(r)) => Expr::Value(Value::Boolean(l.lt(r))),

        // all others lead to type error
        (_, _) => Expr::Error(Error::new()),
    }
}

#[smt_impl(method = le)]
pub fn le(lhs: Expr, rhs: Expr) -> Expr {
    match (lhs, rhs) {
        // error will be propagated
        (Expr::Error(l), Expr::Error(r)) => Expr::Error(l.merge(r)),
        (Expr::Error(e), Expr::Undef) => Expr::Error(Error::new().merge(e)),
        (Expr::Undef, Expr::Error(e)) => Expr::Error(Error::new().merge(e)),
        (Expr::Error(e), _) => Expr::Error(e),
        (_, Expr::Error(e)) => Expr::Error(e),

        // undef will cause an error
        (Expr::Undef, _) => Expr::Error(Error::new()),
        (_, Expr::Undef) => Expr::Error(Error::new()),

        // value op is delegated
        (Expr::Value(l), Expr::Value(r)) => l.le(r),
    }
}

//
// Statement evaluation
//

#[smt_type]
pub struct Variable(Integer); // TODO: this should be modeled as a finite-domain sort

#[smt_type]
pub struct State {
    ns: Map<Variable, Expr>,
}

#[smt_impl(method = assign)]
pub fn assign(state: State, var: Variable, expr: Expr) -> State {
    State {
        ns: state.ns.put_unchecked(var, expr),
    }
}

#[smt_impl(method = assign_if)]
pub fn assign_if(state: State, var: Variable, expr: Expr, cond: Expr) -> State {
    let assigned = match (expr, cond) {
        // error propagates
        (Expr::Error(e1), Expr::Error(e2)) => Expr::Error(e1.merge(e2)),
        (Expr::Error(e), Expr::Undef) => Expr::Error(Error::new().merge(e)),
        (Expr::Undef, Expr::Error(e)) => Expr::Error(Error::new().merge(e)),
        (Expr::Error(e), _) => Expr::Error(e),
        (_, Expr::Error(e)) => Expr::Error(e),

        // undef causes error
        (Expr::Undef, _) => Expr::Error(Error::new()),
        (_, Expr::Undef) => Expr::Error(Error::new()),

        // intended semantics when the condition is a boolean
        (_, Expr::Value(Value::Boolean(v))) => {
            if *v {
                expr
            } else {
                Expr::Value(Value::Null)
            }
        }

        // all other case should cause an error
        (_, Expr::Value(_)) => Expr::Error(Error::new()),
    };
    assign(state, var, assigned)
}

#[smt_impl(method = retrieve)]
pub fn retrieve(state: State, var: Variable) -> Expr {
    if *state.ns.contains_key(var) {
        state.ns.get_unchecked(var)
    } else {
        Expr::Undef
    }
}

//
// Program evaluation
//

#[smt_type]
pub enum Operator {
    LitBool(Boolean),
    LitInt(Integer),
    VarRef(Variable),
    Add(Cloak<Operator>, Cloak<Operator>),
    Sub(Cloak<Operator>, Cloak<Operator>),
    Mul(Cloak<Operator>, Cloak<Operator>),
    Div(Cloak<Operator>, Cloak<Operator>),
    Eq(Cloak<Operator>, Cloak<Operator>),
    Ne(Cloak<Operator>, Cloak<Operator>),
    Lt(Cloak<Operator>, Cloak<Operator>),
    Le(Cloak<Operator>, Cloak<Operator>),
    Ge(Cloak<Operator>, Cloak<Operator>),
    Gt(Cloak<Operator>, Cloak<Operator>),
    And(Cloak<Operator>, Cloak<Operator>),
    Or(Cloak<Operator>, Cloak<Operator>),
    Xor(Cloak<Operator>, Cloak<Operator>),
    Block(Cloak<BlockHead>, Cloak<Operator>),
}

#[smt_type]
pub enum Statement {
    Assign(Variable, Operator),
    AssignIf(Variable, Operator, Operator),
}

#[smt_type]
pub enum BlockHead {
    None,
    Stmt(Statement, Cloak<BlockHead>),
}

#[smt_type]
pub struct Program {
    body: Operator,
}

#[smt_impl]
pub fn evaluate_operator(state: State, op: Operator) -> (State, Expr) {
    match op {
        Operator::LitBool(v) => (state, Expr::Value(Value::Boolean(v))),
        Operator::LitInt(v) => (state, Expr::Value(Value::Integer(v))),
        Operator::VarRef(v) => (state, retrieve(state, v)),
        Operator::Lt(lhs, rhs) => {
            let (t1, l) = evaluate_operator(state, lhs.reveal());
            let (t2, r) = evaluate_operator(t1, rhs.reveal());
            (t2, l.lt(r))
        }
        Operator::Le(lhs, rhs) => {
            let (t1, l) = evaluate_operator(state, lhs.reveal());
            let (t2, r) = evaluate_operator(t1, rhs.reveal());
            (t2, l.le(r))
        }
        Operator::Block(head, body) => {
            let t = evaluate_block_head(state, head.reveal());
            evaluate_operator(t, body.reveal())
        }
        _ => todo!(),
    }
}

#[smt_impl]
pub fn evaluate_statement(state: State, stmt: Statement) -> State {
    match stmt {
        Statement::Assign(v, op) => {
            let (t, e) = evaluate_operator(state, op);
            assign(t, v, e)
        }
        Statement::AssignIf(v, op1, op2) => {
            let (t1, expr) = evaluate_operator(state, op1);
            let (t2, cond) = evaluate_operator(t1, op2);
            assign_if(t2, v, expr, cond)
        }
    }
}

#[smt_impl]
pub fn evaluate_block_head(state: State, head: BlockHead) -> State {
    match head {
        BlockHead::None => state,
        BlockHead::Stmt(stmt, rest) => {
            let t = evaluate_statement(state, stmt);
            evaluate_block_head(t, rest.reveal())
        }
    }
}

#[smt_impl]
pub fn evaluate_program(prog: Program) -> (State, Expr) {
    let state = State::default();
    evaluate_operator(state, prog.body)
}

//
// High-level properties about the evaluator
//

#[smt_spec(impls = [evaluate_program])]
pub fn spec_evaluate_program(_prog: Program) -> (State, Expr) {
    unimplemented!()
}

#[smt_axiom]
pub fn axiom1(p1: Program, p2: Program) -> Boolean {
    p1.eq(p2)
        .implies(spec_evaluate_program(p1).eq(spec_evaluate_program(p2)))
}

// DCE
// linting
// error patterns (based on data)
// K-framework for operation semantics correctness (maybe some test suites)
// common sense properties
