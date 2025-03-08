use super::check_module;

#[test]
fn empty_loop() {
    let input = "
let main = fun () -> { 
    loop { }
}";

    let expected = "
fun main:
    params: {none}
    mut _0: {bottom}
    
    BB0():
        Jump -> BB1()
    BB1():
        Jump -> BB1()
";

    check_module(input, expected);
}

#[test]
fn loop_immediately_break() {
    let input = "
let main = fun () -> { 
    loop { break }
}";

    let expected = "
fun main:
    params: {none}
    mut _0: ()
    
    BB0():
        Jump -> BB1()
    BB1():
        Jump -> BB2()
    BB2():
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn loop_from_param() {
    let input = "
let main = fun (i: Int) -> {
    loop {
        if i > 5 { break }
        i = i + 1
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: ()
    _1: Int
    _2: Bool~0.3
    _3: Int
    
    BB0():
        Jump -> BB1()
    BB1():
        _2 := Gt(copy _1, const 5)
        BranchInt(copy _2): [0 -> BB4(), else -> BB3()]
    BB2():
        Return _0 ->
    BB3():
        Jump -> BB2()
    BB4():
        _3 := Add(copy _1, const 1)
        _1 <- copy _3
        Jump -> BB1()";

    check_module(input, expected);
}

#[test]
fn loop_from_param_with_else() {
    let input = "
let main = fun (i: Int) -> {
    loop {
        if i > 5 { 
            break 
        } else {
            i = i + 1
        }
    }
}";

    let expected = "
fun main:
    params: _1
    mut _0: ()
    _1: Int
    _2: Bool~0.3
    _3: Int
    
    BB0():
        Jump -> BB1()
    BB1():
        _2 := Gt(copy _1, const 5)
        BranchInt(copy _2): [0 -> BB4(), else -> BB3()]
    BB2():
        Return _0 ->
    BB3():
        Jump -> BB2()
    BB4():
        _3 := Add(copy _1, const 1)
        _1 <- copy _3
        Jump -> BB5(_0)
    BB5(_0):
        Jump -> BB1()";

    check_module(input, expected);
}

#[test]
fn loop_full_example() {
    let input = "
let main = fun () -> {
    let mut i = 0
    loop {
        if i > 5 { break }
        i = i + 1
    }
    i
}";

    let expected = "
fun main:
    params: {none}
    mut _0: Int
    mut _1: Int
    _2: Bool~0.3
    _3: Int
    
    BB0():
        _1 := const 0
        Jump -> BB1()
    BB1():
        _2 := Gt(copy _1, const 5)
        BranchInt(copy _2): [0 -> BB4(), else -> BB3()]
    BB2():
        _0 := copy _1
        Return _0 ->
    BB3():
        Jump -> BB2()
    BB4():
        _3 := Add(copy _1, const 1)
        _1 <- copy _3
        Jump -> BB1()
";

    check_module(input, expected);
}
