use super::check_module;

#[test]
fn mutable_int() {
    let input = "
let main = fun () -> { 
    let mut i = 0
    i = i + 2
    i
}";
    let expected = "
fun main:
    params: {none}
    mut _0: Int
    mut _1: Int
    _2: Int
    
    BB0():
        _1 := const 0
        _2 := Add(copy _1, const 2)
        _1 <- copy _2
        _0 := copy _1
        Return _0 ->
";

    check_module(input, expected);
}
