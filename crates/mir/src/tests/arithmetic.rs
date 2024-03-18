use super::check_function;

#[test]
fn basic_arithmetic() {
    let input = "fun () -> { 12 + 4 }";
    let expected = "
fun {anonymous}:
    params: {none}
    mut _0: Int
    
    BB0():
        _0 = Add(const 12, const 4)
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_add_param_and_constant() {
    let input = "fun (i: Int) -> { i + 16 }";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0():
        _0 = Add(copy _1, const 16)
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_sub_param_and_constant() {
    let input = "fun (i: Int) -> { i - 16 }";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0():
        _0 = Sub(copy _1, const 16)
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_mul_param_and_constant() {
    let input = "fun (i: Int) -> { i * 16 }";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    
    BB0():
        _0 = Mul(copy _1, const 16)
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn int_variable_with_addition() {
    let input = "
fun (a: Int) -> { 
    let b = a + 16
    b
}";
    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Int
    _1: Int
    _2: Int
    
    BB0():
        _2 = Add(copy _1, const 16)
        _0 = copy _2
        Return _0 ->
";

    check_function(input, expected);
}

#[test]
fn much_arithmetic() {
    let input = "fun (a: Int, b: Int) -> { a + 2 * b - 7 }";

    let expected = "
fun {anonymous}:
    params: _1, _2
    mut _0: Int
    _1: Int
    _2: Int
    _3: Int
    _4: Int
    
    BB0():
        _4 = Mul(const 2, copy _2)
        _3 = Add(copy _1, copy _4)
        _0 = Sub(copy _3, const 7)
        Return _0 ->
";

    check_function(input, expected);
}
