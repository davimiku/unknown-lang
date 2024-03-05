use super::check_function;

#[test]
fn nested_scopes() {
    let input = "
fun (a: Float) -> {
    let b = 4.0
    {
        let c = a + b
        {
            let d = c * 2.5
        }
    }
}";

    let expected = "
fun {anonymous}:
    params: _1
    mut _0: ()
    _1: Float
    _2: 4.0
    _3: Float
    _4: Float
    
    BB0():
        _2 = const 4.0
        _3 = Add(copy _1, copy _2)
        _4 = Mul(copy _3, const 2.5)
        Return ->
";

    check_function(input, expected);
}

#[test]
fn nested_scopes_with_return() {
    let input = "
fun (a: Float) -> {
    let b = 4.0
    {
        let c = a + b
        {
            let d = c * 2.5
            {
                d
            }
        }
    }
}";

    let expected = "
fun {anonymous}:
    params: _1
    mut _0: Float
    _1: Float
    _2: 4.0
    _3: Float
    _4: Float
    
    BB0():
        _2 = const 4.0
        _3 = Add(copy _1, copy _2)
        _4 = Mul(copy _3, const 2.5)
        _0 = copy _4
        Return ->
";

    check_function(input, expected);
}
