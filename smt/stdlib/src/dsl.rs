#[macro_export]
macro_rules! smt_expr {
    // literals
    ((b true)) => {
        $crate::dt::Boolean::from(true)
    };
    ((b false)) => {
        $crate::dt::Boolean::from(false)
    };
    ((i $l:literal)) => {
        $crate::dt::Integer::from($l)
    };
    ((r $l:literal)) => {
        $crate::dt::Integer::from($l)
    };
    ((t $l:literal)) => {
        $crate::dt::Text::from($l)
    };
    // boolean
    ((& $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::and(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((| $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitor(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((^ $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitxor(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    // integer
    ((==i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::eq(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((!=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ne(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::lt(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::le(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((>=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ge(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((>i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::gt(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((+i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::add(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((-i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::sub(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((*i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::mul(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((/i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::div(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((%i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::rem(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    // rational
    ((==r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::eq(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((!=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ne(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::lt(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::le(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((>=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ge(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((>r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::gt(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((+r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::add(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((-r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::sub(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((*r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::mul(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((/r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::div(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    // text
    ((==t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::eq(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((!=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::ne(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::lt(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    ((<=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::le(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    // seq
    ((vec)) => {
        $crate::dt::Seq::empty()
    };
    ((len_vec $v:tt)) => {
        $crate::dt::Seq::length($v)
    };
    ((+vec $v:tt [$e:tt])) => {
        $crate::dt::Seq::append(
            $crate::dsl::smt_stmt!($v),
            $crate::dsl::smt_stmt!($e),
        )
    };
    ((in_vec $v:tt $e:tt)) => {
        $crate::dt::Seq::contains(
            $crate::dsl::smt_stmt!($v),
            $crate::dsl::smt_stmt!($e),
        )
    };
    ((get_vec $v:tt $i:tt)) => {
        $crate::dt::Seq::at_unchecked(
            $crate::dsl::smt_stmt!($v),
            $crate::dsl::smt_stmt!($i),
        )
    };
    // set
    ((set)) => {
        $crate::dt::Set::empty()
    };
    ((len_set $s:tt)) => {
        $crate::dt::Set::length($s)
    };
    ((+set $s:tt [$e:tt])) => {
        $crate::dt::Set::insert(
            $crate::dsl::smt_stmt!($s),
            $crate::dsl::smt_stmt!($e),
        )
    };
    ((in_set $s:tt $e:tt)) => {
        $crate::dt::Set::contains(
            $crate::dsl::smt_stmt!($s),
            $crate::dsl::smt_stmt!($e),
        )
    };
    // map
    ((map)) => {
        $crate::dt::Map::empty()
    };
    ((len_map $m:tt)) => {
        $crate::dt::Map::length($m)
    };
    ((+map $m:tt $k:tt $v:tt)) => {
        $crate::dt::Map::put_unchecked(
            $crate::dsl::smt_stmt!($m),
            $crate::dsl::smt_stmt!($k),
            $crate::dsl::smt_stmt!($v),
        )
    };
    ((get_map $m:tt $k:tt)) => {
        $crate::dt::Map::get_unchecked(
            $crate::dsl::smt_stmt!($m),
            $crate::dsl::smt_stmt!($k),
        )
    };
    ((in_map $m:tt $k:tt)) => {
        $crate::dt::Map::contains_key(
            $crate::dsl::smt_stmt!($m),
            $crate::dsl::smt_stmt!($k),
        )
    };
    // error
    (err) => {
        $crate::dt::Error::fresh()
    };
    ((++ $lhs:tt $rhs:tt)) => {
        $crate::dt::Error::merge(
            $crate::dsl::smt_stmt!($lhs),
            $crate::dsl::smt_stmt!($rhs),
        )
    };
    // user-defined function
    (($f:ident $($a:tt)*)) => {
        $f($($a,)*)
    };
    // control-flow
    ((? $c1:tt $v1:tt : $(? $cn:tt $vn:tt :)* | $d:tt)) => {
        if *$crate::dsl::smt_stmt!($c1) {
            $crate::dsl::smt_stmt!($v1)
        } $(else if *$crate::dsl::smt_stmt!($cn) {
            $crate::dsl::smt_stmt!($vn)
        })* else {
            $crate::dsl::smt_stmt!($d)
        }
    };
    ((~ $v:tt $(| $p1:pat => $e1:tt)+ $(_ => $d:tt)?)) => {
        match $v {
            $($p1 => $crate::dsl::smt_stmt!($e1),)+
            $(_ => $crate::dsl::smt_stmt!($d),)?
        }
    };
}
pub use smt_expr;

/// Construct an SMT statement (with or without block)
#[macro_export]
macro_rules! smt_stmt {
    // ident
    ($i:ident) => {
        $i
    };
    // operation
    (( $($exp:tt)+ )) => {
        $crate::dsl::smt_expr!(($($exp)+))
    };
    // block
    ({ $(# $v:ident = $e:tt;)* @ $exp:tt }) => {
        $(let $v = $crate::dsl::smt_stmt!($e);)*
        $crate::dsl::smt_expr!($exp)
    };
}
pub use smt_stmt;

fn test() -> crate::dt::Boolean {
    smt_expr! (
        (? (b false) (b false) :? (b true) (b true) :| (b false))
    )
}

fn test2() -> crate::dt::Boolean {
    smt_stmt! {{
        # a = (b false);
        # b = (b false);
        @ (? a (b false) :? b (b true) :| (b false))
    }}
}
