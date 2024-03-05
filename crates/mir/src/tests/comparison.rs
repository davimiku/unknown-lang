use super::check_function;

#[test]
fn int_equals() {
    let input = "fun (a: Int, b: Int) -> Bool { a == b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Eq(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_not_equals() {
    let input = "fun (a: Int, b: Int) -> Bool { a != b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Ne(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_less_than() {
    let input = "fun (a: Int, b: Int) -> Bool { a < b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Lt(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_less_than_or_equal() {
    let input = "fun (a: Int, b: Int) -> Bool { a <= b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Le(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_greater_than() {
    let input = "fun (a: Int, b: Int) -> Bool { a > b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Gt(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn int_greater_than_or_equal() {
    let input = "fun (a: Int, b: Int) -> Bool { a >= b }";
    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0(_1, _2):
        _0 = Ge(copy _1, copy _2)
        Return ->
";

    check_function(input, expected);
}
