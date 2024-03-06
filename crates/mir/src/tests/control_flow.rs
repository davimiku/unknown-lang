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
    // TODO: the duplicate _2 variable should be eliminated at
    // some point by adding some logic to constructing the condition
    // Operand to look for variables that are already locals directly
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Float
    _1: Bool
    _2: Bool
    
    BB0():
        _2 = copy _1
        SwitchInt(copy _2): [0 -> BB2, else -> BB1]
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
