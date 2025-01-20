use super::check_module;

#[test]
#[ignore = "builtin calls not implemented yet"]
fn builtin_call() {
    let input = "let main = fun (i: Int) -> { print i }";

    let expected = "
fun main:
    params: _1
    mut _0: ()
    _1: Int
    
    BB0():
        Call -> [TODO]
        ";

    check_module(input, expected);
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
    mut _0: (false | true)
    _1: Int
    _2: Int
    
    BB0():
        _2 := Rem(copy _1, const 2)
        _0 := Eq(copy _2, const 0)
        Return _0 ->
    
fun main:
    params: _1
    mut _0: (false | true)
    _1: Int
    
    BB0():
        _0 = is_even (copy _1) -> [return: BB1(), unwind -> TODO]
    BB1():
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn main_call_is_even_conditional() {
    let input = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Int {
    if is_even a {
        16
    } else {
        7
    }
}
";

    let expected = "
fun is_even:
    params: _1
    mut _0: (false | true)
    _1: Int
    _2: Int
    
    BB0():
        _2 := Rem(copy _1, const 2)
        _0 := Eq(copy _2, const 0)
        Return _0 ->
    
fun main:
    params: _1
    mut _0: Int
    _1: Int
    _2: (false | true)
    
    BB0():
        _2 = is_even (copy _1) -> [return: BB1(), unwind -> TODO]
    BB1():
        BranchInt(copy _2): [0 -> BB3(), else -> BB2()]
    BB2():
        _0 := const 16
        Jump -> BB4(_0)
    BB3():
        _0 := const 7
        Jump -> BB4(_0)
    BB4(_0):
        Return _0 ->
";

    check_module(input, expected);
}
