use super::check_module;

#[test]
fn basic_match() {
    let input = "
let main = fun (condition: Bool) -> Int {
    match condition {
        .false -> { 8 }
        .true -> { 16 }
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: Int
    _1: Bool~0.3
    _2: Int
    
    BB0():
        _2 := discriminant(_1)
        BranchInt(copy _2): [0 -> BB1(), 1 -> BB2()]
    BB1():
        _0 := const 8
        Jump -> BB3()
    BB2():
        _0 := const 16
        Jump -> BB3()
    BB3():
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn match_with_basic_union() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        .green -> { 16 }
        .blue -> { 24 }
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: Int
    _1: Color~1.0
    _2: Int
    
    BB0():
        _2 := discriminant(_1)
        BranchInt(copy _2): [0 -> BB1(), 1 -> BB2(), 2 -> BB3()]
    BB1():
        _0 := const 8
        Jump -> BB4()
    BB2():
        _0 := const 16
        Jump -> BB4()
    BB3():
        _0 := const 24
        Jump -> BB4()
    BB4():
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn match_with_catch_all() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Int {
    match condition {
        .red -> { 8 }
        otherwise -> { 16 }
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: Int
    _1: Color~1.0
    _2: Int
    _3: Color~1.0
    
    BB0():
        _2 := discriminant(_1)
        BranchInt(copy _2): [0 -> BB1(), else -> BB2()]
    BB1():
        _0 := const 8
        Jump -> BB3()
    BB2():
        _3 := copy _1
        _0 := const 16
        Jump -> BB3()
    BB3():
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn match_with_otherwise_using_bound_otherwise() {
    let input = "
type Color = (red | green | blue)
let main = fun (condition: Color) -> Color {
    match condition {
        .green -> { Color.green }
        otherwise -> { otherwise }
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: Color~1.0
    _1: Color~1.0
    _2: Int
    _3: Color~1.0
    
    BB0():
        _2 := discriminant(_1)
        BranchInt(copy _2): [1 -> BB1(), else -> BB2()]
    BB1():
        _0 := const 1
        Jump -> BB3()
    BB2():
        _3 := copy _1
        _0 := copy _3
        Jump -> BB3()
    BB3():
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn basic_if_else() {
    let input = "
let main = fun (condition: Bool) -> Int { 
    if condition {
        16
    } else {
        8
    }
}";
    let expected = "
fun main:
    params: _1
    mut _0: Int
    _1: Bool~0.3
    
    BB0():
        BranchInt(copy _1): [0 -> BB2(), else -> BB1()]
    BB1():
        _0 := const 16
        Jump -> BB3(_0)
    BB2():
        _0 := const 8
        Jump -> BB3(_0)
    BB3(_0):
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn if_else_with_addition_after() {
    let input = "
let main = fun (condition: Bool, b: Float) -> Float { 
    let a = if condition {
        16.0
    } else {
        8.0
    }
    a + b
}";

    let expected = "
fun main:
    params: _1, _2
    mut _0: Float
    _1: Bool~0.3
    _2: Float
    _3: Float
    
    BB0():
        BranchInt(copy _1): [0 -> BB2(), else -> BB1()]
    BB1():
        _3 := const 16.0
        Jump -> BB3(_3)
    BB2():
        _3 := const 8.0
        Jump -> BB3(_3)
    BB3(_3):
        _0 := Add(copy _3, copy _2)
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn empty_then() {
    let input = "
let main = fun (condition: Bool, i: Int) -> Int {
    if condition {}
    i
}";

    let expected = "
fun main:
    params: _1, _2
    mut _0: Int
    _1: Bool~0.3
    _2: Int
    
    BB0():
        _0 := copy _2
        Return _0 ->
    ";

    check_module(input, expected);
}

#[test]
fn no_else_block() {
    let input = "
let main = fun (condition: Bool, i: Int) -> Int {
    if condition {
        let j = 1
    }
    i
}";

    let expected = "
fun main:
    params: _1, _2
    mut _0: Int
    _1: Bool~0.3
    _2: Int
    _3: 1
    
    BB0():
        BranchInt(copy _1): [0 -> BB2(), else -> BB1()]
    BB1():
        _3 := const 1
        Jump -> BB2()
    BB2():
        _0 := copy _2
        Return _0 ->
";

    check_module(input, expected);
}
