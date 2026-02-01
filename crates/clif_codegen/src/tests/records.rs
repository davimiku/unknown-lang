use crate::builtins::{XBool, XFloat, XInt, FALSE, TRUE};
use crate::tests::{
    compile_main, to_fn, to_fn_one_param_sret, to_fn_three_param_sret, to_fn_two_param_sret,
    to_fn_zero_param_sret,
};

#[test]
fn define_and_pass_through_product_type() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { p }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq, Clone, Copy)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);

    // Function signature: fn(*const Point, *mut Point) -> ()
    let code_fn = unsafe { to_fn_one_param_sret::<*const Point, Point>(code_ptr) };

    let input = Point { x: 123, y: 456 };
    let mut output = Point::default();
    code_fn(&input, &mut output);

    assert_eq!(input, output);
}

#[test]
fn return_field_of_product_type() {
    let input = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { p.y }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn::<(*const Point,), XInt>(code_ptr) };

    let input = Point { x: 123, y: 456 };
    let output = code_fn((&input,));

    assert_eq!(456, output);
}

#[test]
fn pass_record_return_unit() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(*const Point,), ()>(code_ptr) };

    let input = Point { x: 42, y: 99 };
    let output = code_fn((&input,));

    assert_eq!((), output);
}

#[test]
fn return_hardcoded_record() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun () -> { [ x = 10, y = 20 ] }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_zero_param_sret::<Point>(code_ptr) };

    let mut output = Point::default();
    code_fn(&mut output);

    assert_eq!(Point { x: 10, y: 20 }, output);
}

#[test]
fn construct_record_from_scalar_params() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (a: Int, b: Int) -> { [ x = a, y = b ] }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_two_param_sret::<XInt, XInt, Point>(code_ptr) };

    let mut output = Point::default();
    code_fn(7, 13, &mut output);

    assert_eq!(Point { x: 7, y: 13 }, output);
}

#[test]
fn record_with_mixed_scalar_types() {
    let code = "
type Mixed = [ flag: Bool, count: Int, ratio: Float ]

let main = fun (m: Mixed) -> { m }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq, Clone, Copy)]
    struct Mixed {
        count: i64,
        flag: XBool,
        ratio: f64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_one_param_sret::<Mixed, Mixed>(code_ptr) };

    let input = Mixed {
        count: 42,
        flag: TRUE,
        ratio: 3.14,
    };
    let mut output = Mixed::default();
    code_fn(input, &mut output);

    assert_eq!(input, output);
}

#[test]
fn access_multiple_fields() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { p.x + p.y }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(*const Point,), XInt>(code_ptr) };

    let input = Point { x: 100, y: 50 };
    let output = code_fn((&input,));

    assert_eq!(150, output);
}

#[test]
fn record_with_many_fields() {
    let code = "
type Data = [ a: Int, b: Int, c: Int, d: Int, e: Int ]

let main = fun (data: Data) -> { data }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq, Clone, Copy)]
    struct Data {
        a: i64,
        b: i64,
        c: i64,
        d: i64,
        e: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_one_param_sret::<*const Data, Data>(code_ptr) };

    let input = Data {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
        e: 5,
    };
    let mut output = Data::default();
    code_fn(&input, &mut output);

    assert_eq!(input, output);
}

#[test]
fn construct_record_with_expressions() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (n: Int) -> { [ x = n * 2, y = n + 10 ] }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_one_param_sret::<XInt, Point>(code_ptr) };

    let mut output = Point::default();
    code_fn(5, &mut output);

    assert_eq!(Point { x: 10, y: 15 }, output);
}

#[test]
fn record_field_in_computation() {
    let code = "
type Box = [ width: Int, height: Int ]

let main = fun (b: Box) -> { b.width * b.height }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Box {
        width: i64,
        height: i64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(*const Box,), XInt>(code_ptr) };

    let input = Box {
        width: 7,
        height: 8,
    };
    let output = code_fn((&input,));

    assert_eq!(56, output);
}

#[test]
#[ignore = "boolean logic not implemented yet"]
fn record_with_bool_fields() {
    let code = "
type Flags = [ enabled: Bool, active: Bool ]

let main = fun (f: Flags) -> { f.enabled && f.active }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Flags {
        enabled: XBool,
        active: XBool,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(*const Flags,), XBool>(code_ptr) };

    let input = Flags {
        enabled: TRUE,
        active: TRUE,
    };
    let output = code_fn((&input,));

    assert_eq!(TRUE, output);
}

#[test]
fn record_with_float_computation() {
    let code = "
type Circle = [ radius: Float ]

let main = fun (c: Circle) -> { c.radius * 2.0 }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Circle {
        radius: f64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn::<(*const Circle,), XFloat>(code_ptr) };

    let input = Circle { radius: 5.0 };
    let output = code_fn((&input,));

    assert_eq!(10.0, output);
}

#[test]
fn construct_record_from_mixed_type_params() {
    let code = "
type Mixed = [ flag: Bool, count: Int, ratio: Float ]

let main = fun (b: Bool, n: Int, f: Float) -> { [ flag = b, count = n, ratio = f ] }";

    #[repr(C)]
    #[derive(Debug, Default, PartialEq)]
    struct Mixed {
        count: i64,
        flag: XBool,
        ratio: f64,
    }

    let code_ptr = compile_main(code);
    let code_fn = unsafe { to_fn_three_param_sret::<XBool, XInt, XFloat, Mixed>(code_ptr) };

    let mut output = Mixed::default();
    code_fn(TRUE, 99, 2.718, &mut output);

    assert_eq!(
        Mixed {
            count: 99,
            flag: TRUE,
            ratio: 2.718
        },
        output
    );
}
