use super::{check_function, check_module};

#[test]
#[ignore = "builtin calls not implemented yet"]
fn print_call() {
    let input = "fun (i: Int) -> { print i }";

    let expected = "
fun {anonymous}:
    params: _1
    mut _0: ()
    _1: Int
    
    BB0():
        Call -> [TODO]
        ";

    check_function(input, expected);
}

#[test]
fn main_call_is_even() {
    let input = "
let is_even = fun (a: Int) -> { a % 2 == 0 }

let main = fun (a: Int) -> {
    is_even a
}
";

    let expected = "
fun is_even:
    params: _1
    mut _0: Bool
    _1: Int
    _2: Int
    
    BB0():
        _2 = Rem(copy _1, const 2)
        _0 = Eq(copy _2, const 0)
        Return _0 ->
    
fun main:
    params: _1
    mut _0: Bool
    _1: Int
    
    BB0():
        _0 = \\f:1:0 (copy _1) -> [return: BB1, unwind -> TODO]
    BB1(_0):
        Return _0 ->
";
    // TODO: need the reverse mapping between FuncId and symbol

    check_module(input, expected);
}
