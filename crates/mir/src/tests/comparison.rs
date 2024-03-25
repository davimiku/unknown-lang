use super::check_module;

#[test]
fn int_equals() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a == b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Eq(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn int_not_equals() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a != b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Ne(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn int_less_than() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a < b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Lt(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn int_less_than_or_equal() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a <= b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Le(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn int_greater_than() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a > b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Gt(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn int_greater_than_or_equal() {
    let input = "let main = fun (a: Int, b: Int) -> Bool { a >= b }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _0 = Ge(copy _1, copy _2)
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn is_even() {
    let input = "let main = fun (a: Int) -> { a % 2 == 0 }";

    let expected = "
fun main:
    params: _1
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _2 = Rem(copy _1, const 2)
        _0 = Eq(copy _2, const 0)
        Return _0 ->
";

    check_module(input, expected);
}
