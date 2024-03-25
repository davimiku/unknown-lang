use super::check_module;

#[test]
fn basic_if_else() {
    let input = "
let main = fun (condition: Bool) -> Float { 
    if condition {
        16.0
    } else {
        8.0
    }
}";
    let expected = "
fun main:
    params: _1
    mut _0: Float
    _1: Bool
    
    BB0():
        SwitchInt(copy _1): [0 -> BB2, else -> BB1]
    BB1():
        _0 = const 16.0
        Jump -> BB3
    BB2():
        _0 = const 8.0
        Jump -> BB3
    BB3(_0):
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn loop_increment() {
    let input = "
let main = fun () -> Int { 
    let mut i = 0
    loop {
        if i > 5 { break }
        i += 1
    }
    i
}";
    let expected = "
fun main:
    params:
    mut _0: Int
    _1: Int
    
    BB0():
        _1 = const 0
    BB1(_1):
        _2 = Gt(copy _1, const 5)
        SwitchInt(copy _2): [5 -> BB3, else -> BB2]
    BB2(_1?):
        _1 = Add(copy _1, const 1)
        Jump -> BB1
    BB3(_1):
        _0 = copy _1
        Return _0 ->
";

    check_module(input, expected);
}
