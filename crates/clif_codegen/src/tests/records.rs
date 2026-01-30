use crate::builtins::{XInt, XTag};
use crate::tests::{compile_main, to_fn, to_fn_one_param_sret, to_fn_zero_param_sret};

#[test]
fn define_and_pass_through_product_type() {
    let code = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { p }";

    #[repr(C)]
    #[derive(Debug, PartialEq, Clone, Copy)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(code);

    // Function signature: fn(Point, *mut Point) -> ()
    let code_fn = unsafe { to_fn_one_param_sret::<Point, Point>(code_ptr) };

    let input = Point { x: 123, y: 456 };
    let mut output = Point { x: 0, y: 0 };
    code_fn(input, &mut output);

    assert_eq!(input, output);
}

#[test]
fn return_field_of_product_type() {
    let input = "
type Point = [ x: Int, y: Int ]

let main = fun (p: Point) -> { p.x }";

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i64,
        y: i64,
    }

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn::<(Point,), XInt>(code_ptr) };

    let input = Point { x: 123, y: 456 };
    let output = code_fn((input,));

    assert_eq!(123, output);
}
