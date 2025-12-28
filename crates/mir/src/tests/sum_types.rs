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

#[test]
fn sum_type_with_data() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (i: Int) -> { Number.int i }
";

    let expected = "
fun main:
    params: _1
    mut _0: Number~1.0
    _1: Int

    BB0():
        _0 := Number.int$0(copy _1)
        Return _0 ->";

    check_module(input, expected);

    let input = "
type Number = (int: Int | float: Float)

let main = fun (f: Float) -> { Number.float f }
";

    let expected = "
fun main:
    params: _1
    mut _0: Number~1.0
    _1: Float

    BB0():
        _0 := Number.float$1(copy _1)
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn unwrap_add_then_rewrap() {
    let input = "type Number = (int: Int | float: Float)

let main = fun (n: Number) -> {
    match n {
        .int i -> { Number.int (i + 16) }
        .float f -> { Number.float (f + 16.0) }
    }
}";

    let expected = "fun main:
    params: _1
    mut _0: Number~1.0
    _1: Number~1.0
    _2: Int
    _3: Int
    _4: Int
    _5: Float
    _6: Float

    BB0():
        _2 := discriminant(_1)
        BranchInt(copy _2): [0 -> BB1(), 1 -> BB2(), else -> BB1()]
    BB1():
        _3 := copy _1.int
        _4 := Add(copy _3, const 16)
        _0 := Number.int$0(copy _4)
        Jump -> BB3()
    BB2():
        _5 := copy _1.float
        _6 := Add(copy _5, const 16.0)
        _0 := Number.float$1(copy _6)
        Jump -> BB3()
    BB3():
        Return _0 ->";

    check_module(input, expected);
}
