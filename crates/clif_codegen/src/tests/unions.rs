use crate::builtins::{XFloat, XInt};
use crate::tests::{compile_main, to_fn};

#[test]
fn define_and_pass_through_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((0,)), 0);
    assert_eq!(code_fn((1,)), 1);
    assert_eq!(code_fn((2,)), 2);
}

#[test]
fn define_and_use_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun () -> { Color.green }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 1);
}

#[test]
fn construct_union_with_int_data() {
    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_c 32 }
";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), (XInt, XInt)>(code_ptr) };

    assert_eq!(code_fn((100_000,)), (2, 32));

    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_c i }
";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), (XInt, XInt)>(code_ptr) };

    assert_eq!(code_fn((16,)), (2, 16));
}

#[test]
fn construct_union_with_float_data() {
    let input = "
type CoolFloat = (float_a: Float | float_b: Float | float_c: Float)

let main = fun (f: Float) -> { CoolFloat.float_c f }
";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XFloat,), (XInt, XFloat)>(code_ptr) };

    assert_eq!(code_fn((1.23,)), (2, 1.23));
}

#[test]
fn construct_union_with_int_float_data_literal() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun () -> { Number.float 1.23 }
";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), (XInt, XInt)>(code_ptr) };

    let result = code_fn(());
    println!("{result:?}");
    println!("{}", result.0);
    println!("{}", bytemuck::cast::<i64, f64>(result.1));

    // assert_eq!(code_fn((1.23,)), (1, 0, 1.23));
}

#[test]
fn construct_union_with_int_float_data() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (f: Float) -> { Number.float f }
";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XFloat,), (XInt, XInt)>(code_ptr) };

    let result = code_fn((1.23_f64,));
    println!("{result:?}");
    println!("{}", result.0);
    println!("{}", result.1);

    // assert_eq!(code_fn((1.23,)), (1, 0, 1.23));
}
