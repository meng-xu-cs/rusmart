#[macro_export]
macro_rules! smt_expr {
    // ident
    ($i:ident) => {
        $i
    };
    // literals
    ((b $l:literal)) => {
        $crate::dt::Boolean::from($l)
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
        $crate::dt::Boolean::bitand(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((| $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitor(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((^ $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitxor(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    // integer
    ((==i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::eq(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((!=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ne(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::lt(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::le(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((>=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ge(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((>i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::gt(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((+i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::add(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((-i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::sub(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((*i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::mul(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((/i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::div(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((%i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::rem(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    // rational
    ((==r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::eq(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((!=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ne(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::lt(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::le(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((>=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ge(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((>r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::gt(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((+r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::add(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((-r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::sub(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((*r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::mul(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((/r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::div(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    // text
    ((==t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::eq(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((!=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::ne(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::lt(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    ((<=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::le(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    // seq
    ([]) => {
        $crate::dt::Seq::empty()
    };
    ([|$v:tt|]) => {
        $crate::dt::Seq::length($v)
    };
    ([++ $v:tt $e:tt]) => {
        $crate::dt::Seq::append(
            $crate::dsl::smt_expr!($v), $crate::dsl::smt_expr!($e)
        )
    };
    ([in $v:tt $e:tt]) => {
        $crate::dt::Seq::contains(
            $crate::dsl::smt_expr!($v), $crate::dsl::smt_expr!($e)
        )
    };
    ([at $v:tt $i:tt]) => {
        $crate::dt::Seq::at_unchecked(
            $crate::dsl::smt_expr!($v), $crate::dsl::smt_expr!($i)
        )
    };
    // set
    (()) => {
        $crate::dt::Set::empty()
    };
    ((|$s:tt|)) => {
        $crate::dt::Set::length($s)
    };
    ((add $s:tt $e:tt)) => {
        $crate::dt::Set::insert(
            $crate::dsl::smt_expr!($s), $crate::dsl::smt_expr!($e)
        )
    };
    ((in $s:tt $e:tt)) => {
        $crate::dt::Set::contains(
            $crate::dsl::smt_expr!($s), $crate::dsl::smt_expr!($e)
        )
    };
    // map
    ({}) => {
        $crate::dt::Map::empty()
    };
    ({|$m:tt|}) => {
        $crate::dt::Map::length($m)
    };
    ({put $m:tt $k:tt $v:tt}) => {
        $crate::dt::Map::put_unchecked(
            $crate::dsl::smt_expr!($m), $crate::dsl::smt_expr!($k), $crate::dsl::smt_expr!($v)
        )
    };
    ({get $m:tt $k:tt}) => {
        $crate::dt::Map::get_unchecked(
            $crate::dsl::smt_expr!($m), $crate::dsl::smt_expr!($k)
        )
    };
    ({in $m:tt $k:tt}) => {
        $crate::dt::Map::contains_key(
            $crate::dsl::smt_expr!($m), $crate::dsl::smt_expr!($k)
        )
    };
    // error
    (err) => {
        $crate::dt::Error::fresh()
    };
    ((# $lhs:tt $rhs:tt)) => {
        $crate::dt::Error::merge(
            $crate::dsl::smt_expr!($lhs), $crate::dsl::smt_expr!($rhs)
        )
    };
    // user-defined function
    (($f:ident $($a:tt)*)) => {
        $f($($a,)*)
    };
    // control-flow
    ((? $c1:tt $v1:tt : $(? $cn:tt $vn:tt :)* | $d:tt)) => {
        if *$crate::dsl::smt_expr!($c1) {
            $crate::dsl::smt_expr!($v1)
        } $(else if *$crate::dsl::smt_expr!($cn) {
            $crate::dsl::smt_expr!($vn)
        })* else {
            $crate::dsl::smt_expr!($d)
        }
    };
    ((~ $v:tt $(| $p1:pat => $e1:tt)+ $(_ => $d:tt)?)) => {
        match $v {
            $($p1 => $crate::dsl::smt_expr!($e1),)+
            $(_ => $crate::dsl::smt_expr!($d),)?
        }
    };
}
pub use smt_expr;

#[macro_export]
macro_rules! smt_stmt {
    ({ $(# $v:ident = $e:tt;)* @ $exp:tt }) => {
        $(let $v = $crate::dsl::smt_stmt!($e);)*
        $crate::dsl::smt_stmt!($exp)
    };
    (( $($exp:tt)+ )) => {
        $crate::dsl::smt_expr!(($($exp)+))
    }
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
