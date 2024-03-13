use super::check_function;

#[test]
fn basic_if_else() {
    let input = "
fun (condition: Bool) -> Float { 
    if condition {
        16.0
    } else {
        8.0
    }
}";
    let expected = "
fun {anonymous}:
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

    check_function(input, expected);
}
