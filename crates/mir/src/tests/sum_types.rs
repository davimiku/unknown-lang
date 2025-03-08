use super::check_module;

#[test]
fn always_returns_true() {
    let input = "let main = fun () -> { true }";
    let expected = "
fun main:
    params: {none}
    mut _0: Bool~0.3
    
    BB0():
        _0 := const 1
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn define_and_pass_through_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let expected = "
fun main:
    params: _1
    mut _0: Color~1.0
    _1: Color~1.0
    
    BB0():
        _0 := copy _1
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn define_and_use_sum_type() {
    let input = "
type Color = (red | green | blue)

let main = fun () -> { Color.green }";

    let expected = "
fun main:
    params: {none}
    mut _0: Color~1.0
    
    BB0():
        _0 := const 1
        Return _0 ->";

    check_module(input, expected);
}
