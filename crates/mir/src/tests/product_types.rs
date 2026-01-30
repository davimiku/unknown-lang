use super::check_module;

#[test]
fn simple_record_literal() {
    let input = "let main = fun () -> {
        [ x = 1, y = 2 ]
    }";
    let expected = "
fun main:
    params: {none}
    mut _0: [ x : 1, y : 2 ]

    BB0():
        _0 := [ x = const 1, y = const 2 ]
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn simple_record_literal_with_assignment() {
    let input = "let main = fun () -> {
        let point = [ x = 1, y = 2 ]
        point
    }";
    let expected = "
fun main:
    params: {none}
    mut _0: [ x : 1, y : 2 ]
    _1: [ x : 1, y : 2 ]

    BB0():
        _1 := [ x = const 1, y = const 2 ]
        _0 := copy _1
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn record_literal_with_expressions() {
    let input = "let main = fun (a: Int, b: Int) -> {
        [ x = a + 1, y = b * 2 ]
    }";
    let expected = "
fun main:
    params: _1, _2
    mut _0: [ x : Int, y : Int ]
    _1: Int
    _2: Int
    _3: Int
    _4: Int

    BB0():
        _3 := Add(copy _1, const 1)
        _4 := Mul(copy _2, const 2)
        _0 := [ x = copy _3, y = copy _4 ]
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn pull_field_from_product_type() {
    let input = "let main = fun () -> {
        let record = [ t = 1 ]
        record.t
        }";
    let expected = "
fun main:

    BB0():
        ";

    check_module(input, expected);
}
