#[macro_export]
macro_rules! smt {
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
        $crate::dt::Boolean::bitand(smt!($lhs), smt!($rhs))
    };
    ((| $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitor(smt!($lhs), smt!($rhs))
    };
    ((^ $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::bitxor(smt!($lhs), smt!($rhs))
    };
    // integer
    ((==i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::eq(smt!($lhs), smt!($rhs))
    };
    ((!=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ne(smt!($lhs), smt!($rhs))
    };
    ((<i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::lt(smt!($lhs), smt!($rhs))
    };
    ((<=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::le(smt!($lhs), smt!($rhs))
    };
    ((>=i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::ge(smt!($lhs), smt!($rhs))
    };
    ((>i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::gt(smt!($lhs), smt!($rhs))
    };
    ((+i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::add(smt!($lhs), smt!($rhs))
    };
    ((-i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::sub(smt!($lhs), smt!($rhs))
    };
    ((*i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::mul(smt!($lhs), smt!($rhs))
    };
    ((/i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::div(smt!($lhs), smt!($rhs))
    };
    ((%i $lhs:tt $rhs:tt)) => {
        $crate::dt::Integer::rem(smt!($lhs), smt!($rhs))
    };
    // rational
    ((==r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::eq(smt!($lhs), smt!($rhs))
    };
    ((!=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ne(smt!($lhs), smt!($rhs))
    };
    ((<r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::lt(smt!($lhs), smt!($rhs))
    };
    ((<=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::le(smt!($lhs), smt!($rhs))
    };
    ((>=r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::ge(smt!($lhs), smt!($rhs))
    };
    ((>r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::gt(smt!($lhs), smt!($rhs))
    };
    ((+r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::add(smt!($lhs), smt!($rhs))
    };
    ((-r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::sub(smt!($lhs), smt!($rhs))
    };
    ((*r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::mul(smt!($lhs), smt!($rhs))
    };
    ((/r $lhs:tt $rhs:tt)) => {
        $crate::dt::Rational::div(smt!($lhs), smt!($rhs))
    };
    // text
    ((==t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::eq(smt!($lhs), smt!($rhs))
    };
    ((!=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::ne(smt!($lhs), smt!($rhs))
    };
    ((<t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::lt(smt!($lhs), smt!($rhs))
    };
    ((<=t $lhs:tt $rhs:tt)) => {
        $crate::dt::Text::le(smt!($lhs), smt!($rhs))
    };
    // seq
    ([]) => {
        $crate::dt::Seq::empty()
    };
    ([|$v:tt|]) => {
        $crate::dt::Seq::length($v)
    };
    ([++ $v:tt $e:tt]) => {
        $crate::dt::Seq::append(smt!($v), smt!($e))
    };
    ([in $v:tt $e:tt]) => {
        $crate::dt::Seq::contains(smt!($v), smt!($e))
    };
    ([at $v:tt $i:tt]) => {
        $crate::dt::Seq::at_unchecked(smt!($v), smt!($i))
    };
    // set
    (()) => {
        $crate::dt::Set::empty()
    };
    ((|$s:tt|)) => {
        $crate::dt::Set::length($s)
    };
    ((add $s:tt $e:tt)) => {
        $crate::dt::Set::append(smt!($s), smt!($e))
    };
    ((in $s:tt $e:tt)) => {
        $crate::dt::Set::contains(smt!($s), smt!($e))
    };
    // map
    ({}) => {
        $crate::dt::Map::empty()
    };
    ({|$m:tt|}) => {
        $crate::dt::Map::length($m)
    };
    ({put $m:tt $k:tt $v:tt}) => {
        $crate::dt::Map::put_unchecked(smt!($m), smt!($k), smt!($v))
    };
    ({get $m:tt $k:tt}) => {
        $crate::dt::Map::get_unchecked(smt!($m), smt!($k))
    };
    ({in $m:tt $k:tt}) => {
        $crate::dt::Map::contains_key(smt!($m), smt!($k))
    };
    // error
    (err) => {
        $crate::dt::Error::fresh()
    };
    ((# $lhs:tt $rhs:tt)) => {
        $crate::dt::Error::merge(smt!($lhs), smt!($rhs))
    };
    // user-defined function
    (($f:ident $($a:tt)*)) => {
        $f($($a,)*)
    };
    // type-inference enabled
    ($l:literal) => {
        $l.into()
    };
    ((== $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) == smt!($rhs))
    };
    ((!= $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) != smt!($rhs))
    };
    ((< $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) < smt!($rhs))
    };
    ((<= $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) <= smt!($rhs))
    };
    ((>= $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) >= smt!($rhs))
    };
    ((> $lhs:tt $rhs:tt)) => {
        $crate::dt::Boolean::from(smt!($lhs) > smt!($rhs))
    };
    ((+ $lhs:tt $rhs:tt)) => {
        smt!($lhs) + smt!($rhs)
    };
    ((- $lhs:tt $rhs:tt)) => {
        smt!($lhs) - smt!($rhs)
    };
    ((* $lhs:tt $rhs:tt)) => {
        smt!($lhs) * smt!($rhs)
    };
    ((/ $lhs:tt $rhs:tt)) => {
        smt!($lhs) / smt!($rhs)
    };
    ((% $lhs:tt $rhs:tt)) => {
        smt!($lhs) & smt!($rhs)
    };
}
pub use smt;

#[macro_export]
macro_rules! smt_stmt {
    () => {};
}
