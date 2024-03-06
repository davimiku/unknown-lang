use super::check_function;

#[test]
fn identity_int() {
    let input = "fun (i: Int) -> { i }";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0():
        _0 = copy _1
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_variable_from_const() {
    let input = "
fun () -> { 
    let a = 16
    a
}";
    let expected = "
fun {anonymous}:
    params: {none}
    mut _0: 16
    _1: 16
    
    BB0():
        _1 = const 16
        _0 = copy _1
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_variable_from_param() {
    let input = "
fun (a: Int) -> { 
    let b = a
    b
}";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    _2: Int
    
    BB0():
        _2 = copy _1
        _0 = copy _2
        Return _0 ->
";

    check_function(input, expected);
}
